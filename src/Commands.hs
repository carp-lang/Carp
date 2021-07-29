module Commands where

import ColorText
import Context
import Control.Exception
import Control.Monad (join, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits (finiteBitSize)
import Data.Functor ((<&>))
import Data.Hashable (hash)
import Data.List (elemIndex, foldl')
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Emit
import qualified Env as E
import Info
import qualified Map
import qualified Meta
import Obj
import Parsing (parse)
import Path
import Project
import Reify
import RenderDocs
import System.Directory (makeAbsolute)
import System.Environment (getEnv, setEnv)
import System.Exit (ExitCode (..), exitSuccess)
import System.IO (IOMode (..), hClose, hPutStr, hSetEncoding, openFile, utf8)
import System.Info (arch, os)
import System.Process (callCommand, spawnProcess, waitForProcess)
import TypeError
import Types
import Util
import Prelude hiding (abs)

data CarpException
  = ShellOutException {shellOutMessage :: String, returnCode :: Int}
  | CancelEvaluationException
  | EvalException EvalError
  deriving (Eq, Show)

instance Exception CarpException

-- | A lot of commands need to return nil, which signifies a side effecting function and no printing of the result.
dynamicNil :: Either a XObj
dynamicNil = Right (XObj (Lst []) (Just dummyInfo) (Just UnitTy)) -- TODO: Remove/unwrap (Right ...) to a XObj

boolToXObj :: Bool -> XObj
boolToXObj b = if b then trueXObj else falseXObj

addCmd :: SymPath -> CommandFunctionType -> String -> String -> (String, Binder)
addCmd path callback doc example =
  (filename, Binder meta cmd)
  where
    SymPath _ filename = path
    exampleUsage = "Example Usage:\n```\n" ++ example ++ "\n```\n"
    docString = doc ++ "\n\n" ++ exampleUsage
    meta = Meta.set "doc" (XObj (Str docString) Nothing Nothing) emptyMeta
    cmd =
      XObj
        ( Lst
            [ XObj (Command callback) (Just dummyInfo) Nothing,
              XObj (Sym path Symbol) Nothing Nothing,
              XObj (Arr args) Nothing Nothing
            ]
        )
        (Just dummyInfo {infoFile = "Core Commands"})
        (Just DynamicTy)
    args = (\x -> XObj (Sym (SymPath [] x) Symbol) Nothing Nothing) <$> argnames
    argnames = case callback of
      NullaryCommandFunction _ -> []
      UnaryCommandFunction _ -> ["x"]
      BinaryCommandFunction _ -> ["x", "y"]
      TernaryCommandFunction _ -> ["x", "y", "z"]
      VariadicCommandFunction _ -> []

-- | Use this function to register nullary commands in the environment.
addNullaryCommand :: SymPath -> NullaryCommandCallback -> String -> String -> (String, Binder)
addNullaryCommand p = addCmd p . NullaryCommandFunction

-- | Use this function to register unary commands in the environment.
addUnaryCommand :: SymPath -> UnaryCommandCallback -> String -> String -> (String, Binder)
addUnaryCommand p = addCmd p . UnaryCommandFunction

-- | Use this function to register binary commands in the environment.
addBinaryCommand :: SymPath -> BinaryCommandCallback -> String -> String -> (String, Binder)
addBinaryCommand p = addCmd p . BinaryCommandFunction

-- | Use this function to register ternary commands in the environment.
addTernaryCommand :: SymPath -> TernaryCommandCallback -> String -> String -> (String, Binder)
addTernaryCommand p = addCmd p . TernaryCommandFunction

-- | Use this function to register n-ary commands in the environment.
addVariadicCommand :: SymPath -> VariadicCommandCallback -> String -> String -> (String, Binder)
addVariadicCommand p = addCmd p . VariadicCommandFunction

presentErrorWithLabel :: MonadIO m => String -> String -> a -> m a
presentErrorWithLabel label msg ret =
  liftIO $ do
    emitErrorWithLabel label msg
    pure ret

presentError :: MonadIO m => String -> a -> m a
presentError msg ret =
  liftIO $ do
    emitError msg
    pure ret

-- | Command for changing various project settings.
commandProjectConfig :: BinaryCommandCallback
commandProjectConfig ctx xobj@(XObj (Str key) _ _) value = do
  let proj = contextProj ctx
      newProj = case key of
        "cflag" -> do
          cflag <- unwrapStringXObj value
          pure (proj {projectCFlags = addIfNotPresent cflag (projectCFlags proj)})
        "libflag" -> do
          libflag <- unwrapStringXObj value
          pure (proj {projectLibFlags = addIfNotPresent libflag (projectLibFlags proj)})
        "pkgconfigflag" -> do
          pkgconfigflag <- unwrapStringXObj value
          pure (proj {projectPkgConfigFlags = addIfNotPresent pkgconfigflag (projectPkgConfigFlags proj)})
        "cmod" -> do
          cmod <- unwrapStringXObj value
          pure (proj {projectCModules = addIfNotPresent cmod (projectCModules proj)})
        "prompt" -> do
          prompt <- unwrapStringXObj value
          pure (proj {projectPrompt = prompt})
        "search-path" -> do
          searchPath <- unwrapStringXObj value
          pure (proj {projectCarpSearchPaths = addIfNotPresent searchPath (projectCarpSearchPaths proj)})
        "print-ast" -> do
          printAST <- unwrapBoolXObj value
          pure (proj {projectPrintTypedAST = printAST})
        "echo-c" -> do
          echoC <- unwrapBoolXObj value
          pure (proj {projectEchoC = echoC})
        "echo-compiler-cmd" -> do
          echoCompilerCmd <- unwrapBoolXObj value
          pure (proj {projectEchoCompilationCommand = echoCompilerCmd})
        "compiler" -> do
          compiler <- unwrapStringXObj value
          pure (proj {projectCompiler = compiler})
        "target" -> do
          target <- unwrapStringXObj value
          pure (proj {projectTarget = Target target})
        "title" -> do
          title <- unwrapStringXObj value
          pure (proj {projectTitle = title})
        "output-directory" -> do
          outDir <- unwrapStringXObj value
          pure (proj {projectOutDir = outDir})
        "docs-directory" -> do
          docsDir <- unwrapStringXObj value
          pure (proj {projectDocsDir = docsDir})
        "docs-generate-index" ->
          do
            docsGenerateIndex <- unwrapBoolXObj value
            pure (proj {projectDocsGenerateIndex = docsGenerateIndex})
        "docs-logo" -> do
          logo <- unwrapStringXObj value
          pure (proj {projectDocsLogo = logo})
        "docs-prelude" -> do
          prelude <- unwrapStringXObj value
          pure (proj {projectDocsPrelude = prelude})
        "docs-url" -> do
          url <- unwrapStringXObj value
          pure (proj {projectDocsURL = url})
        "docs-styling" -> do
          url <- unwrapStringXObj value
          pure (proj {projectDocsStyling = url})
        "file-path-print-length" -> do
          len <- unwrapStringXObj value
          case len of
            "short" -> pure (proj {projectFilePathPrintLength = ShortPath})
            "full" -> pure (proj {projectFilePathPrintLength = ShortPath})
            _ -> Left ("Project.config can't understand the value '" ++ len ++ "' for key 'file-path-print-length.")
        "generate-only" -> do
          generateOnly <- unwrapBoolXObj value
          pure (proj {projectGenerateOnly = generateOnly})
        "paren-balance-hints" ->
          do
            balanceHints <- unwrapBoolXObj value
            pure (proj {projectBalanceHints = balanceHints})
        "force-reload" -> do
          forceReload <- unwrapBoolXObj value
          pure (proj {projectForceReload = forceReload})
        _ -> Left ("Project.config can't understand the key '" ++ key ++ "' at " ++ prettyInfoFromXObj xobj ++ ".")
  case newProj of
    Left errorMessage -> presentErrorWithLabel "CONFIG ERROR" errorMessage (ctx, dynamicNil)
    Right ok -> pure (ctx {contextProj = ok}, dynamicNil)
commandProjectConfig ctx faultyKey _ =
  presentError ("First argument to 'Project.config' must be a string: " ++ pretty faultyKey) (ctx, dynamicNil)

-- | Command for changing various project settings.
commandProjectGetConfig :: UnaryCommandCallback
commandProjectGetConfig ctx xobj@(XObj (Str key) _ _) =
  let proj = contextProj ctx
      xstr s = XObj s (Just dummyInfo) (Just StringTy)
      getVal _ = case key of
        "cflag" -> Right $ Str $ show $ projectCFlags proj
        "libflag" -> Right $ Str $ show $ projectLibFlags proj
        "pkgconfigflag" -> Right $ Arr $ xstr . Str <$> projectPkgConfigFlags proj
        "load-stack" -> Right $ Arr $ xstr . Str <$> projectLoadStack proj
        "prompt" -> Right $ Str $ projectPrompt proj
        "search-path" -> Right $ Str $ show $ projectCarpSearchPaths proj
        "print-ast" -> Right $ Bol $ projectPrintTypedAST proj
        "echo-c" -> Right $ Bol $ projectEchoC proj
        "echo-compiler-cmd" -> Right $ Bol $ projectEchoCompilationCommand proj
        "compiler" -> Right $ Str $ projectCompiler proj
        "target" -> Right $ Str $ show $ projectTarget proj
        "title" -> Right $ Str $ projectTitle proj
        "output-directory" -> Right $ Str $ projectOutDir proj
        "docs-directory" -> Right $ Str $ projectDocsDir proj
        "docs-logo" -> Right $ Str $ projectDocsLogo proj
        "docs-prelude" -> Right $ Str $ projectDocsPrelude proj
        "docs-url" -> Right $ Str $ projectDocsURL proj
        "docs-generate-index" -> Right $ Bol $ projectDocsGenerateIndex proj
        "docs-styling" -> Right $ Str $ projectDocsStyling proj
        "file-path-print-length" -> Right $ Str $ show (projectFilePathPrintLength proj)
        "generate-only" -> Right $ Bol $ projectGenerateOnly proj
        "paren-balance-hints" -> Right $ Bol $ projectBalanceHints proj
        _ -> Left key
   in pure $ case getVal ctx of
        Right val -> (ctx, Right $ xstr val)
        Left k -> evalError ctx (labelStr "CONFIG ERROR" ("Project.get-config can't understand the key '" ++ k)) (xobjInfo xobj)
commandProjectGetConfig ctx faultyKey =
  presentError ("First argument to 'Project.config' must be a string: " ++ pretty faultyKey) (ctx, dynamicNil)

-- | Command for exiting the REPL/compiler
commandQuit :: NullaryCommandCallback
commandQuit ctx =
  do
    _ <- liftIO exitSuccess
    pure (ctx, dynamicNil)

-- | Command for printing the generated C output (in out/main.c)
commandCat :: NullaryCommandCallback
commandCat ctx = do
  let outDir = projectOutDir (contextProj ctx)
      outMain = outDir </> "main.c"
  liftIO $ do
    callCommand ("cat -n " ++ outMain)
    pure (ctx, dynamicNil)

-- | Command for running the executable generated by the 'build' command.
commandRunExe :: NullaryCommandCallback
commandRunExe ctx = commandRunWithArgs ctx []

-- | Command for running the executable generated by the 'build' command.
commandRunWithArgs :: VariadicCommandCallback
commandRunWithArgs ctx args =
  if projectCanExecute prj
    then runExeWithArgs ctx exe (pretty <$> args)
    else liftIO $ do
      putStrLnWithColor Red "Can't call the 'run' command, need to build an executable first (requires a 'main' function)."
      pure (ctx, dynamicNil)
  where
    prj = contextProj ctx
    dir = projectOutDir prj
    exe = dir </> projectTitle prj

commandRunExeWithArgs :: VariadicCommandCallback
commandRunExeWithArgs ctx xs =
  case xs of
    XObj (Str exe) _ _ : args -> runExeWithArgs ctx exe (pretty <$> args)
    x : _ -> pure $ evalError ctx ("Can't call the 'run-exe-with-args', the first argument must be the path of the executable: " ++ pretty x) (xobjInfo x)
    [] -> pure $ evalError ctx "Can't call the 'run-exe-with-args' without arguments." (Just dummyInfo)

runExeWithArgs :: Context -> FilePath -> [String] -> IO (Context, Either EvalError XObj)
runExeWithArgs ctx exe args = liftIO $ do
  hndl <- spawnProcess exe args
  exitCode <- waitForProcess hndl
  case exitCode of
    ExitSuccess -> pure (ctx, Right (XObj (Num IntTy 0) (Just dummyInfo) (Just IntTy)))
    ExitFailure i -> throw (ShellOutException ("'" ++ exe ++ "' exited with return value " ++ show i ++ ".") i)

-- | Command for building the project, producing an executable binary or a shared library.
commandBuild :: VariadicCommandCallback
commandBuild ctx [] = commandBuild ctx [falseXObj]
commandBuild ctx [XObj (Bol shutUp) _ _] = do
  let env = removeSpecials (contextGlobalEnv ctx)
      typeEnv = contextTypeEnv ctx
      proj = contextProj ctx
      execMode = contextExecMode ctx
      src = do
        typeDecl <- typeEnvToDeclarations typeEnv env
        decl <- envToDeclarations typeEnv env
        c <- envToC env Functions
        initGlobals <- fmap (wrapInInitFunction (projectCore proj)) (globalsToC env)
        pure
          ( "//Types:\n" ++ typeDecl
              ++ "\n\n//Declarations:\n"
              ++ decl
              ++ "\n\n//Init globals:\n"
              ++ initGlobals
              ++ "\n\n//Definitions:\n"
              ++ c
          )
  case src of
    Left err ->
      pure (evalError ctx ("I encountered an error when emitting code:\n\n" ++ show err) Nothing)
    Right okSrc ->
      do
        let compiler = projectCompiler proj
            echoCompilationCommand = projectEchoCompilationCommand proj
            incl = projectIncludesToC proj
            preproc = projectPreprocToC proj
            includeCorePath = projectCarpDir proj ++ "/core/ "
            cModules = projectCModules proj
            flags = projectFlags proj
            outDir = projectOutDir proj
            outMain = outDir </> "main.c"
            outExe = outDir </> projectTitle proj
            generateOnly = projectGenerateOnly proj
            compile hasMain =
              do
                let cmd =
                      joinWithSpace $
                        [ compiler,
                          if hasMain then "" else "-shared",
                          "-o",
                          outExe,
                          "-I",
                          includeCorePath,
                          flags,
                          outMain
                        ]
                          ++ cModules
                 in liftIO $ do
                      when echoCompilationCommand (putStrLn cmd)
                      callCommand cmd
                      when
                        (execMode == Repl && not shutUp)
                        (putStrLn ("Compiled to '" ++ outExe ++ (if hasMain then "' (executable)" else "' (shared library)")))
                      pure (setProjectCanExecute hasMain ctx, dynamicNil)
        liftIO $ createDirectoryIfMissing False outDir
        outputHandle <- openFile outMain WriteMode
        hSetEncoding outputHandle utf8
        hPutStr outputHandle (incl ++ preproc ++ okSrc)
        hClose outputHandle
        if generateOnly
          then pure (ctx, dynamicNil)
          else case Map.lookup "main" (envBindings env) of
            Just (Binder _ (XObj _ _ (Just (FuncTy _ IntTy _)))) -> compile True
            Just (Binder _ (XObj _ _ (Just (FuncTy _ UnitTy _)))) -> compile True
            -- we do not check for non-generic versions of main, these are already caught during concretization
            Just (Binder _ (XObj _ _ (Just (FuncTy _ _ _)))) -> pure (evalError ctx "`main` was generic at build time, it must return either `Int` or `()`." Nothing)
            Just (Binder _ (XObj _ _ (Just t))) -> pure (evalError ctx ("`main` is expected to be a function, but was `" ++ show t ++ "`.") Nothing)
            Just (Binder _ (XObj _ _ Nothing)) -> pure (evalError ctx ("The type of `main` is not set at build time. This should not happen, consider raising an issue with the code that caused it.") Nothing)
            Nothing -> compile False
  where
    removeSpecials env =
      let binds = Map.filterWithKey filterSpecials (envBindings env)
       in env {envBindings = binds}
    filterSpecials k _ =
      not (isSpecialSym (XObj (Sym (SymPath [] k) Symbol) Nothing Nothing))
commandBuild ctx [arg] =
  pure (evalError ctx ("`build` expected a boolean argument, but got `" ++ pretty arg ++ "`.") (xobjInfo arg))
commandBuild ctx args =
  pure (evalError ctx ("`build` expected a single boolean argument, but got `" ++ unwords (map pretty args) ++ "`.") (xobjInfo (head args)))

setProjectCanExecute :: Bool -> Context -> Context
setProjectCanExecute value ctx =
  let proj = contextProj ctx
      proj' = proj {projectCanExecute = value}
   in ctx {contextProj = proj'}

-- | Command for printing all the bindings in the current environment.
commandListBindings :: NullaryCommandCallback
commandListBindings ctx =
  liftIO $ do
    putStrLn "Types:\n"
    putStrLn (prettyEnvironment (getTypeEnv (contextTypeEnv ctx)))
    putStrLn "\nGlobal environment:\n"
    putStrLn (prettyEnvironment (contextGlobalEnv ctx))
    putStrLn ""
    pure (ctx, dynamicNil)

-- | Command for printing information about the current project.
commandProject :: NullaryCommandCallback
commandProject ctx = do
  liftIO (print (contextProj ctx))
  pure (ctx, dynamicNil)

-- | Command for getting the filename of the operating system you're on.
commandHostOS :: NullaryCommandCallback
commandHostOS ctx =
  pure (ctx, Right (XObj (Str os) (Just dummyInfo) (Just StringTy)))

-- | Command for getting the native architecture.
commandHostArch :: NullaryCommandCallback
commandHostArch ctx =
  pure (ctx, Right (XObj (Str arch) (Just dummyInfo) (Just StringTy)))

-- | Command for adding a header file include to the project.
commandAddInclude :: (String -> Includer) -> UnaryCommandCallback
commandAddInclude includerConstructor ctx x =
  case x of
    XObj (Str file) _ _ -> do
      let proj = contextProj ctx
          includer = includerConstructor file
          includers = projectIncludes proj
          includers' =
            if includer `elem` includers
              then includers
              else includers ++ [includer] -- Add last to preserve include order
          proj' = proj {projectIncludes = includers'}
      pure (ctx {contextProj = proj'}, dynamicNil)
    _ ->
      pure (evalError ctx ("Argument to 'include' must be a string, but was `" ++ pretty x ++ "`") (xobjInfo x))

-- | Command for adding preprocessing directives to emitted C output.
-- All of the directives will be emitted after the project includes and before any other code.
commandPreproc :: UnaryCommandCallback
commandPreproc ctx (XObj (C c) _ _) =
  let proj = contextProj ctx
      preprocs = (projectPreproc proj) ++ [c]
      proj' = proj {projectPreproc = preprocs}
   in pure (replaceProject ctx proj', dynamicNil)
commandPreproc ctx x =
  pure (evalError ctx ("Argument to 'preproc' must be C code, but was `" ++ pretty x ++ "`") (xobjInfo x))

commandAddSystemInclude :: UnaryCommandCallback
commandAddSystemInclude = commandAddInclude SystemInclude

commandAddRelativeInclude :: UnaryCommandCallback
commandAddRelativeInclude ctx x =
  case x of
    XObj (Str file) i@(Just info) t ->
      let compiledFile = infoFile info
       in commandAddInclude
            RelativeInclude
            ctx
            (XObj (Str $ takeDirectory compiledFile </> file) i t)
    _ ->
      pure (evalError ctx ("Argument to 'include' must be a string, but was `" ++ pretty x ++ "`") (xobjInfo x))

commandArray :: VariadicCommandCallback
commandArray ctx args =
  pure (ctx, Right (XObj (Arr args) (Just dummyInfo) Nothing))

commandList :: VariadicCommandCallback
commandList ctx args =
  pure (ctx, Right (XObj (Lst args) (Just dummyInfo) Nothing))

commandLength :: UnaryCommandCallback
commandLength ctx x =
  pure $ case x of
    XObj (Lst lst) _ _ ->
      (ctx, Right (XObj (Num IntTy (Integral (length lst))) Nothing Nothing))
    XObj (Arr arr) _ _ ->
      (ctx, Right (XObj (Num IntTy (Integral (length arr))) Nothing Nothing))
    _ -> evalError ctx ("Applying 'length' to non-list: " ++ pretty x) (xobjInfo x)

commandCar :: UnaryCommandCallback
commandCar ctx x =
  pure $ case x of
    XObj (Lst (car : _)) _ _ -> (ctx, Right car)
    XObj (Arr (car : _)) _ _ -> (ctx, Right car)
    _ -> evalError ctx ("Applying 'car' to non-list: " ++ pretty x) (xobjInfo x)

commandCdr :: UnaryCommandCallback
commandCdr ctx x =
  pure $ case x of
    XObj (Lst (_ : cdr)) i _ -> (ctx, Right (XObj (Lst cdr) i Nothing))
    XObj (Arr (_ : cdr)) i _ -> (ctx, Right (XObj (Arr cdr) i Nothing))
    _ -> evalError ctx "Applying 'cdr' to non-list or empty list" (xobjInfo x)

commandLast :: UnaryCommandCallback
commandLast ctx x =
  pure $ case x of
    XObj (Lst lst@(_ : _)) _ _ -> (ctx, Right (last lst))
    XObj (Arr arr@(_ : _)) _ _ -> (ctx, Right (last arr))
    _ -> evalError ctx "Applying 'last' to non-list or empty list." (xobjInfo x)

commandAllButLast :: UnaryCommandCallback
commandAllButLast ctx x =
  pure $ case x of
    XObj (Lst lst) i _ -> (ctx, Right (XObj (Lst (init lst)) i Nothing))
    XObj (Arr arr) i _ -> (ctx, Right (XObj (Arr (init arr)) i Nothing))
    _ -> evalError ctx "Applying 'all-but-last' to non-list or empty list." (xobjInfo x)

commandCons :: BinaryCommandCallback
commandCons ctx x xs =
  pure $ case xs of
    XObj (Lst lst) _ _ ->
      (ctx, Right (XObj (Lst (x : lst)) (xobjInfo x) (xobjTy x))) -- TODO: probably not correct to just copy 'i' and 't'?
    XObj (Arr arr) _ _ -> (ctx, Right (XObj (Arr (x : arr)) (xobjInfo x) (xobjTy x)))
    _ -> evalError ctx "Applying 'cons' to non-list or empty list." (xobjInfo xs)

commandConsLast :: BinaryCommandCallback
commandConsLast ctx x xs =
  pure $ case xs of
    XObj (Lst lst) i t ->
      (ctx, Right (XObj (Lst (lst ++ [x])) i t)) -- TODO: should they get their own i:s and t:s
    _ -> evalError ctx "Applying 'cons-last' to non-list or empty list." (xobjInfo xs)

commandAppend :: BinaryCommandCallback
commandAppend ctx xs ys =
  pure $ case (xs, ys) of
    (XObj (Lst lst1) i t, XObj (Lst lst2) _ _) ->
      (ctx, Right (XObj (Lst (lst1 ++ lst2)) i t)) -- TODO: should they get their own i:s and t:s
    (XObj (Arr arr1) i t, XObj (Arr arr2) _ _) -> (ctx, Right (XObj (Arr (arr1 ++ arr2)) i t))
    _ -> evalError ctx "Applying 'append' to non-array/list or empty list." (xobjInfo xs)

commandMacroError :: UnaryCommandCallback
commandMacroError ctx msg =
  pure $ case msg of
    XObj (Str smsg) _ _ -> evalError ctx smsg (xobjInfo msg)
    x -> evalError ctx (pretty x) (xobjInfo msg)

commandMacroLog :: VariadicCommandCallback
commandMacroLog ctx msgs = do
  liftIO (mapM_ (putStr . logify) msgs)
  liftIO (putStr "\n")
  pure (ctx, dynamicNil)
  where
    logify m =
      case m of
        XObj (Str msg) _ _ -> msg
        x -> pretty x

commandEq :: BinaryCommandCallback
commandEq ctx a b =
  pure (ctx, Right (boolToXObj (cmp (a, b))))
  where
    cmp (XObj (Sym sa _) _ _, XObj (Sym sb _) _ _) = sa == sb
    cmp (XObj (Num _ na) _ _, XObj (Num _ nb) _ _) = na == nb
    cmp (XObj (Lst elemsA) _ _, XObj (Lst elemsB) _ _) =
      length elemsA == length elemsB && all cmp (zip elemsA elemsB)
    cmp (XObj (Arr elemsA) _ _, XObj (Arr elemsB) _ _) =
      length elemsA == length elemsB && all cmp (zip elemsA elemsB)
    cmp (XObj x _ _, XObj y _ _) = x == y

commandComp :: (Number -> Number -> Bool) -> String -> BinaryCommandCallback
commandComp op _ ctx (XObj (Num _ aNum) _ _) (XObj (Num _ bNum) _ _) = pure (ctx, Right (boolToXObj (op aNum bNum)))
commandComp _ opname ctx a b = pure $ evalError ctx ("Can't compare (" ++ opname ++ ") " ++ pretty a ++ " with " ++ pretty b) (xobjInfo a)

commandLt :: BinaryCommandCallback
commandLt = commandComp (<) "<"

commandGt :: BinaryCommandCallback
commandGt = commandComp (>) ">"

commandRound :: UnaryCommandCallback
commandRound ctx (XObj (Num _ (Floating i)) _ _) =
  pure (ctx, Right (XObj (Num IntTy (Integral (round i))) (Just dummyInfo) (Just IntTy)))
commandRound ctx i@(XObj (Num _ (Integral _)) _ _) =
  pure (ctx, Right i)
commandRound ctx a =
  pure $ evalError ctx ("Can’t call round with " ++ pretty a) (xobjInfo a)

commandCharAt :: BinaryCommandCallback
commandCharAt ctx a b =
  pure $ case (a, b) of
    (XObj (Str s) _ _, XObj (Num IntTy (Integral i)) _ _) ->
      if length s > i
        then (ctx, Right (XObj (Chr (s !! i)) (Just dummyInfo) (Just IntTy)))
        else evalError ctx ("Can't call char-at with " ++ pretty a ++ " and " ++ show i ++ ", index too large") (xobjInfo a)
    _ -> evalError ctx ("Can't call char-at with " ++ pretty a ++ " and " ++ pretty b) (xobjInfo a)

commandIndexOf :: BinaryCommandCallback
commandIndexOf ctx a b =
  pure $ case (a, b) of
    (XObj (Str s) _ _, XObj (Chr c) _ _) ->
      (ctx, Right (XObj (Num IntTy (Integral (getIdx c s))) (Just dummyInfo) (Just IntTy)))
    _ -> evalError ctx ("Can't call index-of with " ++ pretty a ++ " and " ++ pretty b) (xobjInfo a)
  where
    getIdx c s = fromMaybe (-1) $ elemIndex c s

commandSubstring :: TernaryCommandCallback
commandSubstring ctx a b c =
  pure $ case (a, b, c) of
    (XObj (Str s) _ _, XObj (Num IntTy (Integral f)) _ _, XObj (Num IntTy (Integral t)) _ _) ->
      if f > t
        then evalError ctx ("Can't call substring with indices " ++ show f ++ " and " ++ show t ++ ". The stop index must be higher than the start index") (xobjInfo a)
        else (ctx, Right (XObj (Str (take (t - f) (drop f s))) (Just dummyInfo) (Just StringTy)))
    _ -> evalError ctx ("Can't call substring with " ++ pretty a ++ ", " ++ pretty b ++ " and " ++ pretty c) (xobjInfo a)

commandStringLength :: UnaryCommandCallback
commandStringLength ctx a =
  pure $ case a of
    XObj (Str s) _ _ ->
      (ctx, Right (XObj (Num IntTy (Integral (length s))) (Just dummyInfo) (Just IntTy)))
    _ -> evalError ctx ("Can't call length with " ++ pretty a) (xobjInfo a)

commandStringConcat :: UnaryCommandCallback
commandStringConcat ctx a =
  pure $ case a of
    XObj (Arr strings) _ _ ->
      case mapM unwrapStringXObj strings of
        Left err -> evalError ctx err (xobjInfo a)
        Right result -> (ctx, Right (XObj (Str (join result)) (Just dummyInfo) (Just StringTy)))
    _ -> evalError ctx ("Can't call concat with " ++ pretty a) (xobjInfo a)

commandStringSplitOn :: BinaryCommandCallback
commandStringSplitOn ctx (XObj (Str sep) _ _) (XObj (Str s) _ _) =
  pure (ctx, Right (XObj (Arr (xstr <$> splitOn sep s)) (Just dummyInfo) Nothing))
  where
    xstr o = XObj (Str o) (Just dummyInfo) (Just StringTy)
commandStringSplitOn ctx sep s =
  pure $ evalError ctx ("Can't call split-on with " ++ pretty sep ++ ", " ++ pretty s) (xobjInfo sep)

commandSymConcat :: UnaryCommandCallback
commandSymConcat ctx a =
  pure $ case a of
    XObj (Arr syms) _ _ ->
      case mapM unwrapSymPathXObj syms of
        Left err -> evalError ctx err (xobjInfo a)
        Right result -> (ctx, Right (XObj (Sym (SymPath [] (join (map show result))) (LookupGlobal CarpLand AVariable)) (Just dummyInfo) Nothing))
    _ -> evalError ctx ("Can't call concat with " ++ pretty a) (xobjInfo a)

commandSymPrefix :: BinaryCommandCallback
commandSymPrefix ctx (XObj (Sym (SymPath [] prefix) _) _ _) (XObj (Sym (SymPath [] suffix) st) i t) =
  pure (ctx, Right (XObj (Sym (SymPath [prefix] suffix) st) i t))
commandSymPrefix ctx (XObj (Sym (SymPath ps prefix) _) _ _) (XObj (Sym (SymPath [] suffix) st) i t) =
  pure (ctx, Right (XObj (Sym (SymPath (ps++[prefix]) suffix) st) i t))
commandSymPrefix ctx x (XObj (Sym (SymPath [] _) _) _ _) =
  pure $ evalError ctx ("Can’t call `prefix` with " ++ pretty x) (xobjInfo x)
commandSymPrefix ctx _ x =
  pure $ evalError ctx ("Can’t call `prefix` with " ++ pretty x) (xobjInfo x)

commandSymFrom :: UnaryCommandCallback
commandSymFrom ctx x@(XObj (Sym _ _) _ _) = pure (ctx, Right x)
commandSymFrom ctx (XObj (Str s) i t) = pure (ctx, Right $ XObj (sFrom_ s) i t)
commandSymFrom ctx (XObj (Pattern s) i t) = pure (ctx, Right $ XObj (sFrom_ s) i t)
commandSymFrom ctx (XObj (Chr c) i t) = pure (ctx, Right $ XObj (sFrom_ (show c)) i t)
commandSymFrom ctx (XObj (Num _ v) i t) = pure (ctx, Right $ XObj (sFrom_ (show v)) i t)
commandSymFrom ctx (XObj (Bol b) i t) = pure (ctx, Right $ XObj (sFrom_ (show b)) i t)
commandSymFrom ctx x =
  pure $ evalError ctx ("Can’t call `from` with " ++ pretty x) (xobjInfo x)

commandSymStr :: UnaryCommandCallback
commandSymStr ctx (XObj (Sym s _) i _) =
  pure (ctx, Right $ XObj (Str (show s)) i (Just StringTy))
commandSymStr ctx x =
  pure $ evalError ctx ("Can’t call `str` with " ++ pretty x) (xobjInfo x)

sFrom_ :: String -> Obj
sFrom_ s = Sym (SymPath [] s) (LookupGlobal CarpLand AVariable)

commandPathDirectory :: UnaryCommandCallback
commandPathDirectory ctx a =
  pure $ case a of
    XObj (Str s) _ _ ->
      (ctx, Right (XObj (Str (takeDirectory s)) (Just dummyInfo) (Just StringTy)))
    _ -> evalError ctx ("Can't call `directory` with " ++ pretty a) (xobjInfo a)

commandPathAbsolute :: UnaryCommandCallback
commandPathAbsolute ctx a =
  case a of
    XObj (Str s) _ _ -> do
      abs <- makeAbsolute s
      pure (ctx, Right (XObj (Str abs) (Just dummyInfo) (Just StringTy)))
    _ -> pure $ evalError ctx ("Can't call `absolute` with " ++ pretty a) (xobjInfo a)

commandArith :: (Number -> Number -> Number) -> String -> BinaryCommandCallback
commandArith op _ ctx (XObj (Num aTy aNum) _ _) (XObj (Num bTy bNum) _ _) =
  let newTy = promoteNumber aTy bTy
   in pure (ctx, Right (XObj (Num newTy (op aNum bNum)) (Just dummyInfo) (Just newTy)))
commandArith _ opname ctx a b = pure $ evalError ctx ("Can't call " ++ opname ++ " with " ++ pretty a ++ " and " ++ pretty b) (xobjInfo a)

commandPlus :: BinaryCommandCallback
commandPlus = commandArith (+) "+"

commandMinus :: BinaryCommandCallback
commandMinus = commandArith (-) "-"

commandDiv :: BinaryCommandCallback
commandDiv ctx p@(XObj (Num _ (Integral _)) _ _) q@(XObj (Num _ (Integral _)) _ _) = commandArith div "/" ctx p q
commandDiv ctx p q = commandArith (/) "/" ctx p q

commandMul :: BinaryCommandCallback
commandMul = commandArith (*) "*"

commandStr :: VariadicCommandCallback
commandStr ctx xs =
  pure (ctx, Right (XObj (Str (join (map f xs))) (Just dummyInfo) (Just StringTy)))
  where
    -- TODO: Is there a better function to call here than some exceptions + 'pretty'?
    f (XObj (Str s) _ _) = s
    f (XObj (Sym path _) _ _) = show path
    f x = escape $ pretty x
    escape [] = []
    escape ('\\' : y) = "\\\\" ++ escape y
    escape (x : y) = x : escape y

commandNot :: UnaryCommandCallback
commandNot ctx x =
  pure $ case x of
    XObj (Bol ab) _ _ -> (ctx, Right (boolToXObj (not ab)))
    _ -> evalError ctx ("can't perform logical operation (not) on " ++ pretty x) (xobjInfo x)

commandReadFile :: UnaryCommandCallback
commandReadFile ctx filename =
  case filename of
    XObj (Str fname) _ _ -> do
      exceptional <- liftIO ((try $ slurp fname) :: (IO (Either IOException String)))
      pure $ case exceptional of
        Right contents -> (ctx, Right (XObj (Str contents) (Just dummyInfo) (Just StringTy)))
        Left _ -> evalError ctx ("The argument to `read-file` `" ++ fname ++ "` does not exist") (xobjInfo filename)
    _ -> pure (evalError ctx ("The argument to `read-file` must be a string, I got `" ++ pretty filename ++ "`") (xobjInfo filename))

commandWriteFile :: BinaryCommandCallback
commandWriteFile ctx filename contents =
  case filename of
    XObj (Str fname) _ _ ->
      case contents of
        XObj (Str s) _ _ -> do
          exceptional <- liftIO ((try $ writeFile fname s) :: (IO (Either IOException ())))
          pure $ case exceptional of
            Right () -> (ctx, dynamicNil)
            Left _ -> evalError ctx ("Cannot write to argument to `" ++ fname ++ "`, an argument to `write-file`") (xobjInfo filename)
        _ -> pure (evalError ctx ("The second argument to `write-file` must be a string, I got `" ++ pretty contents ++ "`") (xobjInfo contents))
    _ -> pure (evalError ctx ("The first argument to `write-file` must be a string, I got `" ++ pretty filename ++ "`") (xobjInfo filename))

commandGetEnv :: UnaryCommandCallback
commandGetEnv ctx (XObj (Str var) _ _) = do
  exceptional <- liftIO ((try $ getEnv var) :: (IO (Either IOException String)))
  pure $ case exceptional of
    Right v -> (ctx, Right (XObj (Str v) (Just dummyInfo) (Just StringTy)))
    Left _ -> (ctx, dynamicNil)
commandGetEnv ctx notString =
  pure (evalError ctx ("The argument to `get-env` must be a string, I got `" ++ pretty notString ++ "`") (xobjInfo notString))

commandSetEnv :: BinaryCommandCallback
commandSetEnv ctx (XObj (Str var) _ _) (XObj (Str val) _ _) = do
  liftIO $ setEnv var val
  pure (ctx, dynamicNil)
commandSetEnv ctx notString (XObj (Str _) _ _) =
  pure (evalError ctx ("The first argument to `set-env` must be a string, I got `" ++ pretty notString ++ "`") (xobjInfo notString))
commandSetEnv ctx _ notString =
  pure (evalError ctx ("The second argument to `set-env` must be a string, I got `" ++ pretty notString ++ "`") (xobjInfo notString))

commandHostBitWidth :: NullaryCommandCallback
commandHostBitWidth ctx =
  let bitSize = Integral (finiteBitSize (undefined :: Int))
   in pure (ctx, Right (XObj (Num IntTy bitSize) (Just dummyInfo) (Just IntTy)))

commandSaveDocsEx :: BinaryCommandCallback
commandSaveDocsEx ctx modulePaths filePaths = do
  case modulesAndGlobals of
    Left err -> pure (ctx, Left err)
    Right ok -> saveDocs ctx ok
  where
    globalEnv = contextGlobalEnv ctx
    modulesAndGlobals =
      let (_, mods) = modules
          (_, globs) = filesWithGlobals
       in do
            okMods <- mods
            okGlobs <- globs
            pure (okMods ++ okGlobs)
    modules :: (Context, Either EvalError [(SymPath, Binder)])
    modules = do
      case modulePaths of
        XObj (Arr xobjs) _ _ ->
          case mapM unwrapSymPathXObj xobjs of
            Left err -> evalError ctx err (xobjInfo modulePaths)
            Right okPaths ->
              case mapM (getEnvironmentBinderForDocumentation globalEnv) okPaths of
                Left err -> evalError ctx err (xobjInfo modulePaths)
                Right okEnvBinders -> (ctx, Right (zip okPaths okEnvBinders))
        x ->
          evalError ctx ("Invalid first arg to save-docs-internal (expected array of symbols): " ++ pretty x) (xobjInfo modulePaths)
    filesWithGlobals :: (Context, Either EvalError [(SymPath, Binder)])
    filesWithGlobals = do
      case filePaths of
        XObj (Arr xobjs) _ _ ->
          case mapM unwrapStringXObj xobjs of
            Left err -> evalError ctx err (xobjInfo filePaths)
            Right okPaths ->
              let globalBinders = map (getGlobalBindersForDocumentation globalEnv) okPaths
                  fauxModules = zipWith createFauxModule okPaths globalBinders
               in (ctx, Right fauxModules)
        x ->
          evalError ctx ("Invalid second arg to save-docs-internal (expected array of strings containing filenames): " ++ pretty x) (xobjInfo filePaths)
    createFauxModule :: String -> Map.Map String Binder -> (SymPath, Binder)
    createFauxModule filename binders =
      let moduleName = "Globals in " ++ filename
          fauxGlobalModule = E.new Nothing (Just moduleName)
          fauxGlobalModuleWithBindings = fauxGlobalModule {envBindings = binders}
          fauxTypeEnv = E.new Nothing Nothing
       in (SymPath [] moduleName, Binder emptyMeta (XObj (Mod fauxGlobalModuleWithBindings fauxTypeEnv) Nothing Nothing))
    getEnvironmentBinderForDocumentation :: Env -> SymPath -> Either String Binder
    getEnvironmentBinderForDocumentation env path =
      case E.searchValueBinder env path of
        Right foundBinder@(Binder _ (XObj (Mod _ _) _ _)) ->
          Right foundBinder
        Right (Binder _ x) ->
          Left ("I can’t generate documentation for `" ++ pretty x ++ "` because it isn’t a module")
        Left _ ->
          Left ("I can’t find the module `" ++ show path ++ "`")
    getGlobalBindersForDocumentation :: Env -> String -> Map.Map String Binder
    getGlobalBindersForDocumentation env filename =
      Map.filter (\bind -> (binderFilename bind) == filename) (envBindings env)
    binderFilename :: Binder -> String
    binderFilename = takeFileName . fromMaybe "" . fmap infoFile . xobjInfo . binderXObj

-- | Command for emitting literal C code from Carp.
-- The string passed to this function will be emitted as is.
-- This is necessary in some C interop contexts, e.g. calling macros that only accept string literals:
--   (static-assert 0 (emit-c "\"foo\""))
-- Also used in combination with the preproc command.
commandEmitC :: UnaryCommandCallback
commandEmitC ctx (XObj (Str c) i _) =
  pure (ctx, Right (XObj (C c) i (Just CTy)))
commandEmitC ctx xobj =
  pure (evalError ctx ("Invalid argument to emit-c (expected a string):" ++ pretty xobj) (xobjInfo xobj))

saveDocs :: Context -> [(SymPath, Binder)] -> IO (Context, Either a XObj)
saveDocs ctx pathsAndEnvBinders = do
  liftIO (saveDocsForEnvs (contextProj ctx) pathsAndEnvBinders)
  pure (ctx, dynamicNil)

commandSexpression :: VariadicCommandCallback
commandSexpression ctx [xobj, XObj (Bol b) _ _] =
  commandSexpressionInternal ctx xobj b
commandSexpression ctx [xobj] =
  commandSexpressionInternal ctx xobj False
commandSexpression ctx xobj =
  pure $ evalError ctx ("s-expr expects a symbol argument and an optional bool, but got: " ++ unwords (map pretty xobj)) (Just dummyInfo)

commandSexpressionInternal :: Context -> XObj -> Bool -> IO (Context, Either EvalError XObj)
commandSexpressionInternal ctx xobj bol =
  let tyEnv = contextTypeEnv ctx
   in case xobj of
        (XObj (Lst [inter@(XObj (Interface ty _) _ _), path]) i t) ->
          pure (ctx, Right (XObj (Lst [toSymbols inter, path, reify ty]) i t))
        (XObj (Lst forms) i t) ->
          pure (ctx, Right (XObj (Lst (map toSymbols forms)) i t))
        mdl@(XObj (Mod e _) _ _) ->
          if bol
            then getMod
            else case E.getTypeBinder tyEnv (fromMaybe "" (envModuleName e)) of
              Right (Binder _ (XObj (Lst forms) i t)) ->
                pure (ctx, Right (XObj (Lst (map toSymbols forms)) i t))
              Right (Binder _ xobj') ->
                pure (ctx, Right (toSymbols xobj'))
              Left _ ->
                getMod
          where
            getMod =
              case toSymbols mdl of
                x@(XObj (Lst _) _ _) ->
                  bindingSyms e (ctx, Right x)
                _ -> error "getmod"
              where
                bindingSyms env start =
                  mapM
                    (commandSexpression ctx . pure . snd)
                    ( Map.toList $ Map.map binderXObj (envBindings env)
                    )
                    <&> foldl' combine start
                combine (c, Right (XObj (Lst xs) i t)) (_, Right y@(XObj (Lst _) _ _)) =
                  (c, Right (XObj (Lst (xs ++ [y])) i t))
                combine _ (c, Left err) =
                  (c, Left err)
                combine (c, Left err) _ =
                  (c, Left err)
                combine _ _ = error "combine"
        _ ->
          pure $ evalError ctx ("can't get an s-expression for: " ++ pretty xobj ++ " is it a bound symbol or literal s-expression?") (Just dummyInfo)

toSymbols :: XObj -> XObj
toSymbols (XObj (Mod e _) i t) =
  XObj
    ( Lst
        [ XObj (Sym (SymPath [] "defmodule") Symbol) i t,
          XObj (Sym (SymPath [] (fromMaybe "" (envModuleName e))) Symbol) i t
        ]
    )
    i
    t
toSymbols (XObj (Defn _) i t) = XObj (Sym (SymPath [] "defn") Symbol) i t
toSymbols (XObj Def i t) = XObj (Sym (SymPath [] "def") Symbol) i t
toSymbols (XObj (Deftype _) i t) = XObj (Sym (SymPath [] "deftype") Symbol) i t
toSymbols (XObj (DefSumtype _) i t) = XObj (Sym (SymPath [] "deftype") Symbol) i t
toSymbols (XObj (Interface _ _) i t) = XObj (Sym (SymPath [] "definterface") Symbol) i t
toSymbols (XObj Macro i t) = XObj (Sym (SymPath [] "defmacro") Symbol) i t
toSymbols (XObj (Command _) i t) = XObj (Sym (SymPath [] "command") Symbol) i t
toSymbols (XObj (Primitive _) i t) = XObj (Sym (SymPath [] "primitive") Symbol) i t
toSymbols (XObj (External _) i t) = XObj (Sym (SymPath [] "external") Symbol) i t
toSymbols x = x

commandHash :: UnaryCommandCallback
commandHash ctx v = pure (ctx, Right (XObj (Num LongTy (Integral (hash v))) (Just dummyInfo) (Just LongTy)))

commandParse :: UnaryCommandCallback
commandParse ctx (XObj (Str s) i _) =
  pure $ case parse s "command:parse" of
    Left e -> evalError ctx (show e) i
    Right [] -> evalError ctx "parse did not return an object" i
    Right [e] -> (ctx, Right e)
    Right (_ : _) -> evalError ctx "parse returned multiple objects" i
commandParse ctx x =
  pure (evalError ctx ("Argument to `parse` must be a string, but was `" ++ pretty x ++ "`") (xobjInfo x))

commandType :: UnaryCommandCallback
commandType ctx (XObj x _ _) =
  pure (ctx, Right (XObj (Sym (SymPath [] (typeOf x)) Symbol) Nothing Nothing))
  where
    typeOf (C _) = "C"
    typeOf (Str _) = "string"
    typeOf (Sym _ _) = "symbol"
    typeOf (MultiSym _ _) = "multi-symbol"
    typeOf (InterfaceSym _) = "interface-symbol"
    typeOf (Arr _) = "array"
    typeOf (StaticArr _) = "static-array"
    typeOf (Lst _) = "list"
    typeOf (Num IntTy _) = "int"
    typeOf (Num LongTy _) = "long"
    typeOf (Num ByteTy _) = "byte"
    typeOf (Num FloatTy _) = "float"
    typeOf (Num DoubleTy _) = "double"
    typeOf (Num _ _) = error "invalid number type for `type` command!"
    typeOf (Pattern _) = "pattern"
    typeOf (Chr _) = "char"
    typeOf (Bol _) = "bool"
    typeOf (Dict _) = "map"
    typeOf (Closure _ _) = "closure"
    typeOf (Defn _) = "defn"
    typeOf Def = "def"
    typeOf (Fn _ _) = "fn"
    typeOf Do = "do"
    typeOf Let = "let"
    typeOf LocalDef = "local-def"
    typeOf While = "while"
    typeOf Break = "dreak"
    typeOf If = "if"
    typeOf (Match _) = "matxch"
    typeOf (Mod _ _) = "module"
    typeOf (Deftype _) = "deftype"
    typeOf (DefSumtype _) = "def-sum-type"
    typeOf With = "with"
    typeOf (External _) = "external"
    typeOf (ExternalType _) = "external-type"
    typeOf MetaStub = "meta-stub"
    typeOf (Deftemplate _) = "deftemplate"
    typeOf (Instantiate _) = "instantiate"
    typeOf (Defalias _) = "defalias"
    typeOf SetBang = "set!"
    typeOf Macro = "macro"
    typeOf Dynamic = "dynamic"
    typeOf DefDynamic = "defdynamic"
    typeOf (Command _) = "command"
    typeOf (Primitive _) = "primitive"
    typeOf The = "the"
    typeOf Ref = "ref"
    typeOf Deref = "deref"
    typeOf (Interface _ _) = "interface"
    typeOf (Protocol _ _) = "protocol"

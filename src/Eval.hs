{-# LANGUAGE LambdaCase #-}

module Eval where

import ColorText
import Commands
import Context
import Control.Applicative
import Control.Exception
import Control.Monad.State
import Data.Bits ((.&.))
import Data.Either (fromRight)
import Data.List (isSuffixOf)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Emit
import qualified Env as E
import EvalError
import EvalIR (EvalIR (..), lowerExpr)
import EvalTypes (LookupPreference (..))
import EvalVM (runEvalIRVM)
import Expand
import Infer
import Info
import qualified Map
import qualified Meta
import Obj
import Parsing
import Path
import Primitives
import Project
import Qualify
import qualified Set
import System.Exit (ExitCode (..), exitSuccess, exitWith)
import System.Process (readProcessWithExitCode)
import qualified Text.Parsec as Parsec
import TypeError
import Types
import Util
import Prelude hiding (exp, mod)

-- Prefer dynamic bindings
evalDynamic :: Context -> XObj -> IO (Context, Either EvalError XObj)
evalDynamic ctx xobj = evalIR ctx (lowerExpr ctx xobj) PreferDynamic

-- Prefer global bindings
evalStatic :: Context -> XObj -> IO (Context, Either EvalError XObj)
evalStatic ctx xobj = evalIR ctx (lowerExpr ctx xobj) PreferGlobal

-- | IR entry point for evaluator execution.
evalIR :: Context -> EvalIR -> LookupPreference -> IO (Context, Either EvalError XObj)
evalIR = runEvalIRVM

-- | Public evaluator entry now routes through IR by default.
eval :: Context -> XObj -> LookupPreference -> IO (Context, Either EvalError XObj)
eval ctx xobj preference = evalIR ctx (lowerExpr ctx xobj) preference

macroExpand :: Context -> XObj -> IO (Context, Either EvalError XObj)
macroExpand ctx xobj = expand MacroExpandOnly evalDynamic ctx xobj

-- | Parses a string and then converts the resulting forms to commands, which are evaluated in order.
executeString :: Bool -> Bool -> Context -> String -> String -> IO Context
executeString = executeStringAtLine 1

executeStringAtLine :: Int -> Bool -> Bool -> Context -> String -> String -> IO Context
executeStringAtLine line doCatch printResult ctx input fileName =
  if doCatch then catch exec (catcher ctx) else exec
  where
    exec = case parseAtLine line input fileName of
      Left parseError ->
        let sourcePos = Parsec.errorPos parseError
            parseErrorXObj =
              XObj
                (Lst [])
                ( Just
                    dummyInfo
                      { infoFile = fileName,
                        infoLine = Parsec.sourceLine sourcePos,
                        infoColumn = Parsec.sourceColumn sourcePos
                      }
                )
                Nothing
         in do
              _ <- liftIO $ treatErr ctx (replaceChars (Map.fromList [('\n', " ")]) (show parseError)) parseErrorXObj
              pure ctx
      Right xobjs -> do
        (res, ctx') <-
          foldM
            interactiveFolder
            (XObj (Lst []) (Just dummyInfo) (Just UnitTy), ctx)
            xobjs
        when
          (printResult && xobjTy res /= Just UnitTy)
          (putStrLnWithColor Yellow ("=> " ++ prettyDynamic res))
        pure ctx'
    interactiveFolder (_, context) =
      executeCommand context
    treatErr ctx' e xobj = do
      let fppl = projectFilePathPrintLength (contextProj ctx')
      case contextExecMode ctx' of
        Check -> putStrLn (machineReadableInfoFromXObj fppl xobj ++ " " ++ e)
        _ -> emitErrorWithLabel "PARSE ERROR" e
      throw CancelEvaluationException

-- | Like 'pretty', but detects dynamic map bucket-lists and formats them as {k v ...}.
prettyDynamic :: XObj -> String
prettyDynamic = visit
  where
    visit xobj =
      case xobjObj xobj of
        Lst lst
          | isDynMapBuckets lst ->
            "{" ++ joinWithSpace (concatMap pairsFrom lst) ++ "}"
        Lst lst -> "(" ++ joinWithSpace (map visit lst) ++ ")"
        Arr arr -> "[" ++ joinWithSpace (map visit arr) ++ "]"
        StaticArr arr -> "$[" ++ joinWithSpace (map visit arr) ++ "]"
        _ -> pretty xobj
    isDynMapBuckets lst =
      let n = length lst
       in n >= 16
            && (n .&. (n - 1)) == 0
            && all isBucket lst
    isBucket (XObj (Lst pairs) _ _) = all isPair pairs
    isBucket _ = False
    isPair (XObj (Lst [_, _]) _ _) = True
    isPair _ = False
    pairsFrom (XObj (Lst pairs) _ _) = concatMap pairElems pairs
    pairsFrom _ = []
    pairElems (XObj (Lst [k, v]) _ _) = [visit k, visit v]
    pairElems _ = []

-- | Used by functions that has a series of forms to evaluate and need to fold over them (producing a new Context in the end)
folder :: Context -> XObj -> IO Context
folder context xobj = do
  (_, ctx) <- executeCommand context xobj
  pure ctx

-- | Take a repl command and execute it.
executeCommand :: Context -> XObj -> IO (XObj, Context)
executeCommand ctx@(Context env _ _ _ _ _ _ _ _) xobj =
  do
    when (isJust (envModuleName env)) $
      error ("Global env module name is " ++ fromJust (envModuleName env) ++ " (should be Nothing).")
    -- The s-expression command is a special case that prefers global/static bindings over dynamic bindings
    -- when given a naked binding (no path) as an argument; (s-expr inc)
    (newCtx, result) <- if xobjIsSexp xobj then evalStatic ctx xobj else evalDynamic ctx xobj
    case result of
      Left e@EvalError {} -> do
        reportExecutionError newCtx (show e)
        pure (xobj, newCtx)
      -- special case: calling something static at the repl
      Right (XObj (Lst (XObj (Lst (XObj (Defn _) _ _ : (XObj (Sym (SymPath [] "main") _) _ _) : _)) _ _ : _)) _ _) ->
        executeCommand newCtx (withBuildAndRun (XObj (Lst []) (Just dummyInfo) Nothing))
      Left (HasStaticCall _ _) ->
        callFromRepl newCtx xobj
      Right res -> pure (res, newCtx)
  where
    callFromRepl newCtx xobj' = do
      (nc, r) <- annotateWithinContext newCtx xobj'
      case r of
        Right (ann, deps) -> do
          ctxWithDeps <- liftIO $ foldM (define True) nc (map Qualified deps)
          executeCommand ctxWithDeps (withBuildAndRun (buildMainFunction ann))
        Left err -> do
          reportExecutionError nc (show err)
          pure (xobj', nc)
    withBuildAndRun xobj' =
      XObj
        ( Lst
            [ XObj Do (Just dummyInfo) Nothing,
              xobj',
              XObj
                (Lst [XObj (Sym (SymPath [] "build") Symbol) (Just dummyInfo) Nothing, trueXObj])
                (Just dummyInfo)
                Nothing,
              XObj
                (Lst [XObj (Sym (SymPath [] "run") Symbol) (Just dummyInfo) Nothing])
                (Just dummyInfo)
                Nothing,
              (XObj (Lst []) (Just dummyInfo) (Just UnitTy))
            ]
        )
        (Just dummyInfo)
        Nothing
    xobjIsSexp (XObj (Lst (XObj (Sym (SymPath [] "s-expr") Symbol) _ _ : _)) _ _) = True
    xobjIsSexp _ = False

reportExecutionError :: Context -> String -> IO ()
reportExecutionError ctx errorMessage =
  case contextExecMode ctx of
    Check -> putStrLn errorMessage
    _ ->
      do
        emitErrorBare errorMessage
        throw CancelEvaluationException

-- | Decides what to do when the evaluation fails for some reason.
catcher :: Context -> CarpException -> IO Context
catcher ctx exception =
  case exception of
    (ShellOutException message rc) -> emitErrorWithLabel "RUNTIME ERROR" message >> stop rc
    CancelEvaluationException -> stop 1
    EvalException err -> emitError (show err) >> stop 1
  where
    stop rc =
      case contextExecMode ctx of
        Repl -> pure ctx
        Build -> exitWith (ExitFailure rc)
        Install _ -> exitWith (ExitFailure rc)
        BuildAndRun -> exitWith (ExitFailure rc)
        Check -> exitSuccess

specialCommandWith :: Context -> XObj -> SymPath -> [XObj] -> IO (Context, Either EvalError XObj)
specialCommandWith ctx _ path forms = do
  let env = fromMaybe (contextGlobalEnv ctx) $ contextInternalEnv ctx <|> maybeId (innermostModuleEnv ctx) <|> Just (contextGlobalEnv ctx)
      useThese = envUseModules env
      env' = env {envUseModules = Set.insert path useThese}
      ctx' = replaceGlobalEnv ctx env'
  ctxAfter <- liftIO $ foldM folder ctx' forms
  let envAfter = fromMaybe (contextGlobalEnv ctxAfter) (contextInternalEnv ctxAfter <|> maybeId (innermostModuleEnv ctxAfter) <|> Just (contextGlobalEnv ctxAfter))
      -- undo ALL use:s made inside the 'with'.
      ctxAfter' = replaceGlobalEnv ctx (envAfter {envUseModules = useThese})
  pure (ctxAfter', dynamicNil)

specialCommandDefine :: Context -> XObj -> IO (Context, Either EvalError XObj)
specialCommandDefine ctx xobj =
  do
    (newCtx, result) <- annotateWithinContext ctx xobj
    case result of
      Right (annXObj, annDeps) ->
        do
          ctxWithDeps <- liftIO $ foldM (define True) newCtx (map Qualified annDeps)
          ctxWithDef <- liftIO $ define False ctxWithDeps (Qualified annXObj)
          pure (ctxWithDef, dynamicNil)
      Left err ->
        pure (ctx, Left err)

specialCommandWhile :: Context -> XObj -> XObj -> IO (Context, Either EvalError XObj)
specialCommandWhile ctx cond body = do
  (newCtx, evd) <- evalDynamic ctx cond
  case evd of
    Right c ->
      case xobjObj c of
        Bol b ->
          if b
            then do
              (newCtx', _) <- evalDynamic newCtx body
              specialCommandWhile newCtx' cond body
            else pure (newCtx, dynamicNil)
        _ ->
          pure (throwErr (WhileContainsNonBool c) ctx (xobjInfo c))
    Left e -> pure (newCtx, Left e)

getSigFromDefnOrDef :: Context -> XObj -> Either EvalError (Maybe (Ty, XObj))
getSigFromDefnOrDef ctx xobj =
  let pathStrings = contextPath ctx
      globalEnv = contextGlobalEnv ctx
      fppl = projectFilePathPrintLength (contextProj ctx)
      path = normalizePath (fromMaybe (getPath xobj) (pathFromDefLike xobj))
      fullPath = case path of
        (SymPath [] _) -> consPath pathStrings path
        (SymPath _ _) -> path
      metaData = either (const emptyMeta) id (E.lookupMeta globalEnv fullPath)
   in case Meta.get "sig" metaData of
        Just foundSignature ->
          case xobjToTy foundSignature of
            Just t ->
              let sigToken = XObj (Sym (SymPath [] "sig") Symbol) Nothing Nothing
                  nameToken = XObj (Sym (SymPath [] (getName xobj)) Symbol) Nothing Nothing
                  recreatedSigForm = XObj (Lst [sigToken, nameToken, foundSignature]) Nothing (Just MacroTy)
               in Right (Just (t, recreatedSigForm))
            Nothing -> Left (EvalError ("Can't use '" ++ pretty foundSignature ++ "' as a type signature") (contextHistory ctx) fppl (xobjInfo xobj))
        Nothing -> Right Nothing
  where
    pathFromDefLike :: XObj -> Maybe SymPath
    pathFromDefLike (XObj (Lst (XObj (Defn _) _ _ : XObj (Sym p _) _ _ : _)) _ _) = Just p
    pathFromDefLike (XObj (Lst (XObj Def _ _ : XObj (Sym p _) _ _ : _)) _ _) = Just p
    pathFromDefLike (XObj (Lst (XObj (Sym (SymPath _ headName) _) _ _ : XObj (Sym p _) _ _ : _)) _ _)
      | headName == "defn" || headName == "def" = Just p
    pathFromDefLike _ = Nothing
    normalizePath :: SymPath -> SymPath
    normalizePath p@(SymPath mods name)
      | null mods && '.' `elem` name =
        case splitOn "." name of
          [] -> p
          [single] -> SymPath [] single
          parts -> SymPath (init parts) (last parts)
      | otherwise = p

annotateWithinContext :: Context -> XObj -> IO (Context, Either EvalError (XObj, [XObj]))
annotateWithinContext ctx xobj = do
  let globalEnv = contextGlobalEnv ctx
      typeEnv = contextTypeEnv ctx
      sig = getSigFromDefnOrDef ctx xobj
      fppl = projectFilePathPrintLength (contextProj ctx)
  case sig of
    Left err -> pure (ctx, Left err)
    Right okSig -> do
      (_, expansionResult) <- expandAll (evalDynamic) ctx xobj
      case expansionResult of
        Left err -> pure (ctx, Left err)
        Right expanded ->
          let xobjFullSymbols = qualify ctx expanded
           in case xobjFullSymbols of
                Left err -> pure (evalError ctx (show err) (xobjInfo xobj))
                Right xs ->
                  case annotate typeEnv globalEnv xs okSig of
                    Left err ->
                      -- TODO: Replace this with a single call to evalError (which already checks the execution mode)
                      case contextExecMode ctx of
                        Check -> pure (evalError ctx (joinLines (machineReadableErrorStrings fppl err)) Nothing)
                        _ -> pure (evalError ctx (show err) (xobjInfo xobj))
                    Right ok -> pure (ctx, Right ok)

primitiveDefmodule :: VariadicPrimitiveCallback
primitiveDefmodule xobj ctx@(Context env i tenv pathStrings _ _ _ _ _) (XObj (Sym (SymPath [] moduleName) _) si _ : innerExpressions) =
  -- N.B. The `envParent` rewrite at the end of this line is important!
  -- lookups delve into parent envs by default, which is normally what we want, but in this case it leads to problems
  -- when submodules happen to share a name with an existing module or type at the global level.
  either (const (defineNewModule emptyMeta)) updateExistingModule (E.searchBinder ((fromRight env (E.getInnerEnv env pathStrings)) {envParent = Nothing}) (SymPath [] moduleName))
    >>= defineModuleBindings
    >>= \(newCtx, result) ->
      let updater c = replaceInternalEnvMaybe c (E.parent =<< contextInternalEnv c)
       in case result of
            Left err -> pure (newCtx, Left err)
            Right _ -> pure (updater (popModulePath newCtx), dynamicNil)
  where
    --------------------------------------------------------------------------------
    -- Update an existing module by modifying its environment parents and updating the current context path.
    updateExistingModule :: Binder -> IO (Context, Either EvalError XObj)
    updateExistingModule (Binder _ (XObj (Mod innerEnv _) _ _)) =
      let updateContext =
            replacePath' (contextPath ctx ++ [moduleName])
              . replaceInternalEnv' (innerEnv {envParent = i})
       in pure (updateContext ctx, dynamicNil)
    updateExistingModule (Binder meta (XObj (Lst [XObj MetaStub _ _, _]) _ _)) =
      defineNewModule meta
    updateExistingModule _ =
      pure (throwErr (ModuleRedefinition moduleName) ctx (xobjInfo xobj))
    --------------------------------------------------------------------------------
    -- Define a brand new module with a context's current environments as its parents.
    defineNewModule :: MetaData -> IO (Context, Either EvalError XObj)
    defineNewModule meta =
      pure (fromRight ctx (updater ctx), dynamicNil)
      where
        moduleDefs = E.new (Just (fromRight env (E.getInnerEnv env pathStrings))) (Just moduleName)
        moduleTypes = E.new (Just tenv) (Just moduleName)
        newModule = XObj (Mod moduleDefs moduleTypes) si (Just ModuleTy)
        updater = \c ->
          insertInGlobalEnv' (markQualified (SymPath pathStrings moduleName)) (Binder meta newModule) c
            >>= pure . replaceInternalEnv' (moduleDefs {envParent = i})
            >>= pure . replacePath' (contextPath ctx ++ [moduleName])
    --------------------------------------------------------------------------------
    -- Define bindings for the module.
    defineModuleBindings :: (Context, Either EvalError XObj) -> IO (Context, Either EvalError XObj)
    defineModuleBindings (context, Left e) = pure (context, Left e)
    defineModuleBindings (context, _) =
      foldM step (context, dynamicNil) innerExpressions
    step :: (Context, Either EvalError XObj) -> XObj -> IO (Context, Either EvalError XObj)
    step (ctx', Left e) _ = pure (ctx', Left e)
    step (ctx', Right _) expressions =
      macroExpand ctx' expressions
        >>= \(ctx'', res) -> case res of
          Left err -> pure (ctx'', Left err)
          Right r -> evalDynamic ctx'' r
primitiveDefmodule _ ctx (x : _) =
  pure (throwErr (DefmoduleContainsNonSymbol x) ctx (xobjInfo x))
primitiveDefmodule xobj ctx [] =
  pure (throwErr DefmoduleNoArgs ctx (xobjInfo xobj))

-- | "NORMAL" COMMANDS (just like the ones in Command.hs, but these need access to 'eval', etc.)

-- | Command for loading a Carp file.
commandLoad :: VariadicCommandCallback
commandLoad ctx [xobj@(XObj (Str path) i _), XObj (Str toLoad) _ _] =
  loadInternal ctx xobj path i (Just toLoad) DoesReload
commandLoad ctx [XObj (Str _) _ _, x] =
  pure $ throwErr (loadInvalidArgs [x]) ctx (xobjInfo x)
commandLoad ctx [x, _] =
  pure $ throwErr (loadInvalidArgs [x]) ctx (xobjInfo x)
commandLoad ctx [xobj@(XObj (Str path) i _)] =
  loadInternal ctx xobj path i Nothing DoesReload
commandLoad ctx x =
  pure $ throwErr (loadInvalidArgs x) ctx Nothing

commandLoadOnce :: VariadicCommandCallback
commandLoadOnce ctx [xobj@(XObj (Str path) i _), XObj (Str toLoad) _ _] =
  loadInternal ctx xobj path i (Just toLoad) Frozen
commandLoadOnce ctx [XObj (Str _) _ _, x] =
  pure $ throwErr (loadOnceInvalidArgs [x]) ctx (xobjInfo x)
commandLoadOnce ctx [x, _] =
  pure $ throwErr (loadOnceInvalidArgs [x]) ctx (xobjInfo x)
commandLoadOnce ctx [xobj@(XObj (Str path) i _)] =
  loadInternal ctx xobj path i Nothing Frozen
commandLoadOnce ctx x =
  pure $ throwErr (loadOnceInvalidArgs x) ctx Nothing

loadInternal :: Context -> XObj -> String -> Maybe Info -> Maybe String -> ReloadMode -> IO (Context, Either EvalError XObj)
loadInternal ctx xobj path i fileToLoad reloadMode = do
  let proj = contextProj ctx
  libDir <- liftIO $ cachePath $ projectLibDir proj
  let relativeTo = case i of
        Just ii ->
          case infoFile ii of
            "REPL" -> "."
            file -> takeDirectory file
        Nothing -> "."
      carpDir = projectCarpDir proj
      fullSearchPaths =
        path :
        (relativeTo </> path) :
        map (</> path) (projectCarpSearchPaths proj) -- the path from the file that contains the '(load)', or the current directory if not loading from a file (e.g. the repl)
          ++ [carpDir </> "core" </> path] -- user defined search paths
          ++ [libDir </> path]
      firstM _ [] = pure Nothing
      firstM p (x : xs) = do
        q <- p x
        if q
          then pure $ Just x
          else firstM p xs
  existingPath <- liftIO $ firstM doesFileExist fullSearchPaths
  case existingPath of
    Nothing ->
      if '@' `elem` path
        then tryInstall path
        else pure $ invalidPath ctx path
    Just firstPathFound ->
      do
        canonicalPath <- liftIO (canonicalizePath firstPathFound)
        fileThatLoads <- liftIO (canonicalizePath $ maybe "" infoFile i)
        if canonicalPath == fileThatLoads
          then pure $ cantLoadSelf ctx path
          else do
            let alreadyLoaded = projectAlreadyLoaded proj ++ frozenPaths proj
            if canonicalPath `elem` alreadyLoaded
              then pure (ctx, dynamicNil)
              else do
                contents <- liftIO $ slurp canonicalPath
                let files = projectFiles proj
                    files' =
                      if canonicalPath `elem` map fst files
                        then files
                        else files ++ [(canonicalPath, reloadMode)]
                    prevStack = projectLoadStack proj
                    proj' =
                      proj
                        { projectFiles = files',
                          projectAlreadyLoaded = canonicalPath : alreadyLoaded,
                          projectLoadStack = canonicalPath : prevStack
                        }
                newCtx <- liftIO $ executeString True False (replaceProject ctx proj') contents canonicalPath
                pure (replaceProject newCtx (contextProj newCtx) {projectLoadStack = prevStack}, dynamicNil)
  where
    frozenPaths proj =
      if projectForceReload proj
        then [] -- No paths are Frozen when the "force reload" project setting is true.
        else map fst $ filter (isFrozen . snd) (projectFiles proj)
    isFrozen Frozen = True
    isFrozen _ = False
    invalidPath ctx' path' =
      throwErr (LoadFileNotFound path') ctx' (xobjInfo xobj)
    invalidPathWith ctx' path' stderrOutput cleanup cleanupPath = do
      _ <- liftIO $ when cleanup (removeDirectoryRecursive cleanupPath)
      pure $
        throwErr (LoadGitFailure path' stderrOutput) ctx' (xobjInfo xobj)
    replaceC _ _ [] = []
    replaceC c s (a : b) = if a == c then s ++ replaceC c s b else a : replaceC c s b
    cantLoadSelf ctx' path' =
      throwErr (LoadRecursiveLoad path') ctx' (xobjInfo xobj)
    tryInstall path' =
      let split = splitOn "@" path'
       in tryInstallWithCheckout (joinWith "@" (init split)) (last split)
    fromURL url =
      let split = splitOn "/" (replaceC ':' "_COLON_" url)
          first = head split
       in if first `elem` ["https_COLON_", "http_COLON_"]
            then joinWith "/" (tail (tail split))
            else
              if '@' `elem` first
                then joinWith "/" (joinWith "@" (tail (splitOn "@" first)) : tail split)
                else url
    tryInstallWithCheckout path' toCheckout = do
      let proj = contextProj ctx
      fpath <- liftIO $ cachePath $ projectLibDir proj </> fromURL path' </> toCheckout
      cur <- liftIO getCurrentDirectory
      pathExists <- liftIO $ doesPathExist fpath
      let cleanup = not pathExists
      _ <- liftIO $ createDirectoryIfMissing True fpath
      _ <- liftIO $ setCurrentDirectory fpath
      (_, txt, _) <- liftIO $ readProcessWithExitCode "git" ["rev-parse", "--abbrev-ref=loose", "HEAD"] ""
      if txt == "HEAD\n"
        then do
          _ <- liftIO $ setCurrentDirectory cur
          doGitLoad path' fpath
        else do
          _ <- liftIO $ readProcessWithExitCode "git" ["init"] ""
          _ <- liftIO $ readProcessWithExitCode "git" ["remote", "add", "origin", path'] ""
          (x0, _, stderr0) <- liftIO $ readProcessWithExitCode "git" ["fetch", "--all", "--tags"] ""
          case x0 of
            ExitFailure _ -> do
              _ <- liftIO $ setCurrentDirectory cur
              invalidPathWith ctx path' stderr0 cleanup fpath
            ExitSuccess -> do
              (x1, _, stderr1) <- liftIO $ readProcessWithExitCode "git" ["checkout", toCheckout] ""
              _ <- liftIO $ setCurrentDirectory cur
              case x1 of
                ExitSuccess -> doGitLoad path' fpath
                ExitFailure _ -> invalidPathWith ctx path' stderr1 cleanup fpath
    doGitLoad path' fpath =
      case fileToLoad of
        Just file -> commandLoad ctx [XObj (Str (fpath </> file)) Nothing Nothing]
        Nothing ->
          -- we’re guessing what file to use here
          let fName = last (splitOn "/" path')
              realName' =
                if ".git" `isSuffixOf` fName
                  then take (length fName - 4) fName
                  else fName
              realName =
                if ".carp" `isSuffixOf` realName'
                  then realName'
                  else realName' ++ ".carp"
              fileToLoad' = fpath </> realName
              mainToLoad = fpath </> "main.carp"
           in do
                (newCtx, res) <- commandLoad ctx [XObj (Str fileToLoad') Nothing Nothing]
                case res of
                  ret@(Right _) -> pure (newCtx, ret)
                  Left _ -> commandLoad ctx [XObj (Str mainToLoad) Nothing Nothing]

-- | Load several files in order.
loadFiles :: Context -> [FilePath] -> IO Context
loadFiles = loadFilesExt commandLoad

loadFilesOnce :: Context -> [FilePath] -> IO Context
loadFilesOnce = loadFilesExt commandLoadOnce

loadFilesExt :: VariadicCommandCallback -> Context -> [FilePath] -> IO Context
loadFilesExt loadCmd = foldM load
  where
    load :: Context -> FilePath -> IO Context
    load ctx file = do
      (newCtx, ret) <- loadCmd ctx [XObj (Str file) Nothing Nothing]
      case ret of
        Left err -> throw (EvalException err)
        Right _ -> pure newCtx

-- | Command for reloading all files in the project (= the files that has been loaded before).
commandReload :: NullaryCommandCallback
commandReload ctx = do
  let paths = projectFiles (contextProj ctx)
      f :: Context -> (FilePath, ReloadMode) -> IO Context
      f context (_, Frozen) | not (projectForceReload (contextProj context)) = pure context
      f context (filepath, _) =
        do
          let proj = contextProj context
              alreadyLoaded = projectAlreadyLoaded proj
          if filepath `elem` alreadyLoaded
            then pure context
            else do
              contents <- slurp filepath
              let proj' = proj {projectAlreadyLoaded = filepath : alreadyLoaded}
              executeString False False (replaceProject context proj') contents filepath
  newCtx <- liftIO (foldM f ctx paths)
  pure (newCtx, dynamicNil)

-- | Command for expanding a form and its macros.
commandExpand :: UnaryCommandCallback
commandExpand = macroExpand

-- | This function will show the resulting C code from an expression.
-- | i.e. (Int.+ 2 3) => "_0 = 2 + 3"
commandC :: UnaryCommandCallback
commandC ctx xobj = do
  (newCtx, result) <- expandAll (evalDynamic) ctx xobj
  case result of
    Left err -> pure (newCtx, Left err)
    Right expanded -> do
      (_, annotated) <- annotateWithinContext newCtx expanded
      case annotated of
        Left err -> pure $ evalError newCtx (show err) (xobjInfo xobj)
        Right (annXObj, annDeps) ->
          do
            let cXObj = printC annXObj
                cDeps = concatMap printC annDeps
                c = cDeps ++ cXObj
            liftIO (putStr c)
            pure (newCtx, dynamicNil)

-- | This function will return the compiled AST.
commandExpandCompiled :: UnaryCommandCallback
commandExpandCompiled ctx xobj = do
  (newCtx, result) <- expandAll (evalDynamic) ctx xobj
  case result of
    Left err -> pure (newCtx, Left err)
    Right expanded -> do
      (_, annotated) <- annotateWithinContext newCtx expanded
      case annotated of
        Left err -> pure $ evalError newCtx (show err) (xobjInfo xobj)
        Right (annXObj, _) -> pure (newCtx, Right annXObj)

-- | Helper function for commandC
printC :: XObj -> String
printC xobj =
  case checkForUnresolvedSymbols xobj of
    Left e ->
      strWithColor Red (show e ++ ", can't print resulting code.\n")
    Right _ ->
      strWithColor Green (toC All (Binder emptyMeta xobj))

buildMainFunction :: XObj -> XObj
buildMainFunction xobj =
  XObj
    ( Lst
        [ XObj (Defn Nothing) di Nothing,
          XObj (Sym (SymPath [] "main") Symbol) di Nothing,
          XObj (Arr []) di Nothing,
          XObj
            ( Lst
                [ XObj Do di Nothing,
                  case xobjTy xobj of
                    Nothing -> error "buildmainfunction"
                    Just UnitTy -> xobj
                    Just (RefTy _ _) ->
                      XObj
                        (Lst [XObj (Sym (SymPath [] "println*") Symbol) di Nothing, xobj])
                        di
                        (Just UnitTy)
                    Just _ ->
                      XObj
                        ( Lst
                            [ XObj (Sym (SymPath [] "println*") Symbol) di Nothing,
                              XObj
                                (Lst [XObj Ref di Nothing, xobj])
                                di
                                (Just UnitTy)
                            ]
                        )
                        di
                        (Just UnitTy),
                  XObj (Num IntTy 0) di Nothing
                ]
            )
            di
            Nothing
        ]
    )
    di
    (Just (FuncTy [] UnitTy StaticLifetimeTy))
  where
    di = Just dummyInfo

primitiveDefdynamic :: BinaryPrimitiveCallback
primitiveDefdynamic _ ctx (XObj (Sym (SymPath [] name) _) _ _) value = do
  (newCtx, result) <- evalDynamic ctx value
  case result of
    Left err -> pure (newCtx, Left err)
    Right evaledBody ->
      dynamicOrMacroWith newCtx (\path -> [XObj DefDynamic Nothing Nothing, XObj (Sym path Symbol) Nothing Nothing, evaledBody]) DynamicTy name value
primitiveDefdynamic _ ctx notName _ =
  pure (throwErr (DefnDynamicInvalidName notName) ctx (xobjInfo notName))

specialCommandSet :: Context -> [XObj] -> IO (Context, Either EvalError XObj)
specialCommandSet ctx [orig@(XObj (Sym path@(SymPath _ _) _) _ _), val] =
  let lookupInternal =
        maybe (Left "") Right (contextInternalEnv ctx)
          >>= \e ->
            unwrapErr (E.searchBinder e path)
              >>= \binder -> pure (binder, setInternal, e)
      lookupGlobal =
        Right (contextGlobalEnv ctx)
          >>= \e ->
            unwrapErr (E.searchBinder e path)
              >>= \binder -> pure (binder, setGlobal, e)
   in either
        ((const (pure $ (throwErr (SetVarNotFound orig) ctx (xobjInfo orig)))))
        (\(binder', setter', env') -> evalAndSet binder' setter' env')
        (lookupInternal <> lookupGlobal)
  where
    evalAndSet :: Binder -> (Context -> Env -> Either EvalError XObj -> Binder -> IO (Context, Either EvalError XObj)) -> Env -> IO (Context, Either EvalError XObj)
    evalAndSet binder setter env =
      case xobjTy (binderXObj binder) of
        -- don't type check dynamic or untyped bindings
        -- TODO: Figure out why untyped cases are sometimes coming into set!
        Just DynamicTy -> handleUnTyped
        Nothing -> handleUnTyped
        _ ->
          evalDynamic ctx val
            >>= \(newCtx, result) ->
              case result of
                Right evald -> typeCheckValueAgainstBinder newCtx evald binder >>= \(nctx, typedVal) -> setter nctx env typedVal binder
                left -> pure (newCtx, left)
      where
        handleUnTyped :: IO (Context, Either EvalError XObj)
        handleUnTyped =
          evalDynamic ctx val
            >>= \(newCtx, result) -> setter newCtx env result binder
    setGlobal :: Context -> Env -> Either EvalError XObj -> Binder -> IO (Context, Either EvalError XObj)
    setGlobal ctx' env value binder =
      pure $ either (failure ctx' orig) (success ctx') value
      where
        success c xo = (replaceGlobalEnv c (setStaticOrDynamicVar path env binder xo), dynamicNil)
    setInternal :: Context -> Env -> Either EvalError XObj -> Binder -> IO (Context, Either EvalError XObj)
    setInternal ctx' env value binder =
      pure $ either (failure ctx' orig) (success ctx') value
      where
        success c xo = (replaceInternalEnv c (setStaticOrDynamicVar path env binder xo), dynamicNil)
specialCommandSet ctx [notName, _] =
  pure (throwErr (SetInvalidVarName notName) ctx (xobjInfo notName))
specialCommandSet ctx args =
  pure (throwErr (setInvalidArgs args) ctx (if null args then Nothing else xobjInfo (head args)))

-- | Convenience method for signifying failure in a given context.
failure :: Context -> XObj -> EvalError -> (Context, Either EvalError a)
failure ctx orig err = evalError ctx (show err) (xobjInfo orig)

-- | Given a context, value XObj and an existing binder, check whether or not
-- the given value has a type matching the binder's in the given context.
typeCheckValueAgainstBinder :: Context -> XObj -> Binder -> IO (Context, Either EvalError XObj)
typeCheckValueAgainstBinder ctx val binder = do
  (ctx', typedValue) <- annotateWithinContext ctx val
  pure $ case typedValue of
    Right (val', _) -> go ctx' binderTy val'
    Left err -> (ctx', Left err)
  where
    path = getPath (binderXObj binder)
    binderTy = xobjTy (binderXObj binder)
    typeErr x = throwErr (SetTypeMismatch path (fromJust (xobjTy x)) (fromJust binderTy)) ctx (xobjInfo x)
    go ctx'' (Just DynamicTy) x = (ctx'', Right x)
    go ctx'' t x@(XObj _ _ t') = if t == t' then (ctx'', Right x) else typeErr x

-- | Sets a variable, checking whether or not it is static or dynamic, and
-- assigns an appropriate type to the variable.
-- Returns a new environment containing the assignment.
setStaticOrDynamicVar :: SymPath -> Env -> Binder -> XObj -> Env
setStaticOrDynamicVar path@(SymPath _ name) env binder value =
  case binder of
    (Binder meta (XObj (Lst (def@(XObj Def _ _) : sym : _)) _ t)) ->
      fromRight env (E.insert env path (Binder meta (XObj (Lst [def, sym, value]) (xobjInfo value) t)))
    (Binder meta (XObj (Lst (defdy@(XObj DefDynamic _ _) : sym : _)) _ _)) ->
      fromRight env (E.insert env path (Binder meta (XObj (Lst [defdy, sym, value]) (xobjInfo value) (Just DynamicTy))))
    (Binder meta (XObj (Lst (lett@(XObj LocalDef _ _) : sym : _)) _ t)) ->
      fromRight (error "FAILED!") (E.replaceInPlace env name (Binder meta (XObj (Lst [lett, sym, value]) (xobjInfo value) t)))
    -- shouldn't happen, errors are thrown at call sites.
    -- TODO: Return an either here to propagate error.
    _ -> env

primitiveEval :: UnaryPrimitiveCallback
primitiveEval _ ctx val = do
  -- primitives don’t evaluate their arguments, so this needs to double-evaluate
  (newCtx, arg) <- evalDynamic ctx val
  case arg of
    Left err -> pure (newCtx, Left err)
    Right evald -> do
      (newCtx', expanded) <- macroExpand newCtx evald
      case expanded of
        Left err -> pure (newCtx', Left err)
        Right ok -> do
          (finalCtx, res) <- evalDynamic newCtx' ok
          pure $ case res of
            Left (HasStaticCall x i) -> throwErr (StaticCall x) ctx i
            _ -> (finalCtx, res)

dynamicOrMacro :: Context -> Obj -> Ty -> String -> XObj -> XObj -> IO (Context, Either EvalError XObj)
dynamicOrMacro ctx pat ty name params body =
  dynamicOrMacroWith ctx (\path -> [XObj pat Nothing Nothing, XObj (Sym path Symbol) Nothing Nothing, params, body]) ty name body

primitiveDefndynamic :: TernaryPrimitiveCallback
primitiveDefndynamic _ ctx (XObj (Sym (SymPath [] name) _) _ _) params body =
  dynamicOrMacro ctx Dynamic DynamicTy name params body
primitiveDefndynamic _ ctx notName _ _ =
  argumentErr ctx "defndynamic" "a name" "first" notName

primitiveDefmacro :: TernaryPrimitiveCallback
primitiveDefmacro _ ctx (XObj (Sym (SymPath [] name) _) _ _) params body =
  dynamicOrMacro ctx Macro MacroTy name params body
primitiveDefmacro _ ctx notName _ _ =
  argumentErr ctx "defmacro" "a name" "first" notName

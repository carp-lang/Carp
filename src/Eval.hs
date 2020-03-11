{-# LANGUAGE LambdaCase #-}
module Eval where

import Control.Concurrent (forkIO)
import Control.Exception
import Control.Monad.State
import Control.Monad.State.Lazy (StateT(..), runStateT, liftIO, modify, get, put)
import Data.Foldable (foldlM, foldrM)
import Data.List (foldl', null, isSuffixOf, intercalate)
import Data.List.Split (splitOn, splitWhen)
import Data.Maybe (fromJust, mapMaybe, isJust, Maybe(..))
import System.Exit (exitSuccess, exitFailure, exitWith, ExitCode(..))
import System.Process (readProcessWithExitCode)
import qualified Data.Map as Map
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Error as ParsecError

import Parsing
import Emit
import Obj
import Types
import Infer
import Deftype
import Sumtypes
import ColorText
import Template
import Util
import Commands
import Expand
import Lookup
import Qualify
import TypeError
import Concretize
import Path
import Primitives

-- | Dynamic (REPL) evaluation of XObj:s (s-expressions)
eval :: Env -> XObj -> StateT Context IO (Either EvalError XObj)
eval env xobj@(XObj o i t) = do
  ctx <- get
  case o of
    Lst body   -> eval' body
    Sym path@(SymPath p n) _ -> do
      let fppl = projectFilePathPrintLength (contextProj ctx)
      case lookupInEnv (SymPath ("Dynamic" : p) n) env of
        Just (_, Binder _ found) -> return (Right (resolveDef found))
        Nothing ->
          case lookupInEnv path env of
            Just (_, Binder _ found) -> return (Right (resolveDef found))
            Nothing ->
              return (evalError ctx ("Can't find symbol '" ++ show path ++ "'") i)
    Arr objs  -> do
      evaled <- fmap sequence (mapM (eval env) objs)
      return $ do ok <- evaled
                  Right (XObj (Arr ok) i t)
    _        -> return (Right xobj)
  where
    resolveDef (XObj (Lst [XObj DefDynamic _ _, _, value]) _ _) = value
    resolveDef x = x
    eval' form = do
      ctx <- get
      case form of
       [XObj (Sym (SymPath ["Dynamic"] "and") _) _ _, a, b] -> do
         evaledA <- eval env a
         evaledB <- eval env b
         return $ do okA <- evaledA
                     case okA of
                       XObj (Bol ab) _ _ ->
                         if ab
                           then do okB <- evaledB
                                   case okB of
                                     XObj (Bol bb) _ _ ->
                                       if bb then Right trueXObj else Right falseXObj
                                     _ ->
                                       evalError ctx ("Can’t perform call `and` on " ++ pretty okB) (info okB)
                           else Right falseXObj
                       _ ->
                         evalError ctx ("Can’t call `and` on " ++ pretty okA) (info okA)

       [XObj (Sym (SymPath ["Dynamic"] "or") _) _ _, a, b] -> do
         evaledA <- eval env a
         evaledB <- eval env b
         return $ do okA <- evaledA
                     case okA of
                       XObj (Bol ab) _ _ ->
                         if ab
                           then Right trueXObj
                           else do okB <- evaledB
                                   case okB of
                                     XObj (Bol bb) _ _ ->
                                       if bb then Right trueXObj else Right falseXObj
                                     _ ->
                                       evalError ctx ("Can’t call `or` on " ++ pretty okB) (info okB)
                       _ ->
                         evalError ctx ("Can’t call `or` on " ++ pretty okA) (info okA)

       [XObj If _ _, mcond, mtrue, mfalse] -> do
         evd <- eval env mcond
         case evd of
           Right cond ->
             case obj cond of
               Bol b -> eval env (if b then mtrue else mfalse)
               _     ->
                 return (evalError ctx
                          ("This `if` condition contains the non-boolean value `" ++
                           pretty cond ++ "`") (info cond))
           Left e -> return (Left e)

       XObj If _ _:_ ->
         return (evalError ctx
                  ("I didn’t understand this `if`.\n\n Got:\n```\n" ++ pretty xobj ++
                   "\n```\n\nExpected the form:\n```\n(if cond then else)\n```\n") (info xobj))

       [XObj (Defn _) _ _, name, args@(XObj (Arr a) _ _), body] ->
         case obj name of
           (Sym (SymPath [] _) _) ->
               if all isUnqualifiedSym a
               then specialCommandDefine xobj
               else return (evalError ctx
                 ("`defn` requires all arguments to be unqualified symbols, but it got `" ++
                  pretty args ++ "`") (info xobj))
           _                      -> return (evalError ctx
             ("`defn` identifiers must be unqualified symbols, but it got `" ++
              pretty name ++ "`") (info xobj))

       [XObj (Defn _) _ _, name, invalidArgs, _] ->
         return (evalError ctx
           ("`defn` requires an array of symbols as argument list, but it got `" ++
            pretty invalidArgs ++ "`") (info xobj))

       (defn@(XObj (Defn _) _ _) : _) ->
           return (evalError ctx
             ("I didn’t understand the `defn` at " ++ prettyInfoFromXObj xobj ++
              ":\n\n" ++ pretty xobj ++
              "\n\nIs it valid? Every `defn` needs to follow the form `(defn name [arg] body)`.")
              (info defn))

       [def@(XObj Def _ _), name, expr] ->
         if isUnqualifiedSym name
         then specialCommandDefine xobj
         else return (evalError ctx
           ("`def` identifiers must be unqualified symbols, but it got `" ++
            pretty name ++ "`") (info xobj))

       [the@(XObj The _ _), ty, value] ->
         do evaledValue <- expandAll eval env value -- TODO: Why expand all here?
            return $ do okValue <- evaledValue
                        Right (XObj (Lst [the, ty, okValue]) i t)

       (XObj The _ _: _) ->
           return (evalError ctx
             ("I didn’t understand the `the` at " ++ prettyInfoFromXObj xobj ++
              ":\n\n" ++ pretty xobj ++
              "\n\nIs it valid? Every `the` needs to follow the form `(the type expression)`.")
              (info xobj))

       [XObj Let _ _x, XObj (Arr bindings) bindi bindt, body]
         | odd (length bindings) -> return (evalError ctx
             ("Uneven number of forms in `let`: " ++ pretty xobj)
             (info xobj)) -- Unreachable?
         | not (all isSym (evenIndices bindings)) -> return (evalError ctx
             ("`let` identifiers must be symbols, but it got `" ++
              joinWithSpace (map pretty bindings) ++ "`") (info xobj))
         | otherwise ->
             do let innerEnv = Env Map.empty (Just env) (Just "LET") [] InternalEnv 0
                let binds = unwrapVar (pairwise bindings) []
                eitherEnv <- foldrM successiveEval (Right innerEnv) binds
                case eitherEnv of
                   Left err -> return $ Left err
                   Right envWithBindings -> do
                          evaledBody <- eval envWithBindings body
                          return $ do okBody <- evaledBody
                                      Right okBody
         where unwrapVar [] acc = acc
               unwrapVar ((XObj (Sym (SymPath [] x) _) _ _,y):xs) acc = unwrapVar xs ((x,y):acc)
               successiveEval (n, x) =
                 \case
                   err@(Left _) -> return err
                   Right e ->
                     eval e x >>= \case
                       Right okX -> return $ Right $ extendEnv e n okX
                       Left err -> return $ Left err

       l@[XObj Fn{} _ _, args@(XObj (Arr a) _ _), f] ->
         if all isUnqualifiedSym a
         then return (Right (XObj (Closure (XObj (Lst l) i t) (CEnv env)) i t))
         else return (evalError ctx ("`fn` requires all arguments to be unqualified symbols, but it got `" ++ pretty args ++ "`") (info args))
       XObj (Closure (XObj (Lst [XObj (Fn _ _) _ _, XObj (Arr params) _ _, body]) _ _) (CEnv e)) i _:args ->
         case checkArity params args of
           Left err ->
             return (evalError ctx err (info xobj))
           Right () ->
             do evaledArgs <- fmap sequence (mapM (eval env) args)
                case evaledArgs of
                  Right okArgs -> apply e body params okArgs
                  Left err -> return (Left err)

       XObj (Lst [XObj Dynamic _ _, _, XObj (Arr params) _ _, body]) i _:args ->
         case checkArity params args of
           Left err ->
             return (evalError ctx err i)
           Right () ->
             do evaledArgs <- fmap sequence (mapM (eval env) args)
                case evaledArgs of
                  Right okArgs -> apply env body params okArgs
                  Left err -> return (Left err)

       XObj (Lst [XObj Macro _ _, _, XObj (Arr params) _ _, body]) i _:args -> do
         put (pushFrame ctx xobj)
         case checkArity params args of
           Left err -> do
             put ctx
             return (evalError ctx err i)
           Right () -> do
             -- Replace info so that the macro which is called gets the source location info of the expansion site.
             --let replacedBody = replaceSourceInfoOnXObj (info xobj) body
             res <- apply env body params args
             case res of
              Right xobj -> do
                _ <- force (eval env xobj)
                newCtx <- get
                put (popFrame newCtx)
                return res
              Left err -> do
                put ctx
                return (Left err)

       XObj (Lst [XObj (Command callback) _ _, _]) _ _:args ->
         do evaledArgs <- fmap sequence (mapM (eval env) args)
            case evaledArgs of
              Right okArgs -> getCommand callback okArgs
              Left err -> return (Left err)

       XObj (Lst (XObj (Defn _) _ _:_)) _ _:_ -> return (Right xobj)

       l@(XObj (Lst _) i t):args -> do
         put (pushFrame ctx xobj)
         f <- eval env l
         case f of
            Right fun -> do
             res <- eval env (XObj (Lst (fun:args)) i t)
             newCtx <- get
             put (popFrame newCtx)
             return res
            x -> do
             put ctx
             return x

       x@(XObj sym@(Sym s _) i _):args -> do
         put (pushFrame ctx xobj)
         case Map.lookup s primitives of
           Just prim -> do
             res <- prim x env args
             newCtx <- get
             put (popFrame newCtx)
             return res
           Nothing -> do
             f <- eval env x
             case f of
               Right fun -> do
                 res <- eval env (XObj (Lst (fun:args)) i t)
                 newCtx <- get
                 put (popFrame newCtx)
                 return res
               Left err -> do
                 put ctx
                 return (Left err)
       XObj With _ _ : xobj@(XObj (Sym path _) _ _) : forms ->
         specialCommandWith xobj path forms
       XObj With _ _ : _ ->
         return (evalError ctx ("Invalid arguments to `with`: " ++ pretty xobj) (info xobj))
       [XObj Do _ _] ->
         return (evalError ctx "No forms in do" (info xobj))
       XObj Do _ _ : rest -> do
         evaled <- foldlM (successiveEval env) dynamicNil rest
         case evaled of
           Left e -> return (Left e)
           Right evald -> return (Right evald)
        where successiveEval e acc x =
                 case acc of
                   err@(Left _) -> return err
                   Right _ -> do
                    res <- eval e x
                    case res of
                      Left err -> return (Left err)
                      Right x -> return (Right x)
       [] -> return dynamicNil
       x ->
         return (evalError ctx ("I did not understand the form `" ++ show x ++ "`.") (info xobj))
    force x = seq x x
    checkArity params args =
      let la = length args
          withRest = any ((":rest" ==) . getName) params
          lp = length params - (if withRest then 2 else 0)
      in if lp == la  || (withRest && la >= lp)
         then Right ()
         else if la < lp
              then Left ("expected " ++ show lp ++
                         " arguments but received only " ++ show la ++
                         ".\n\nYou’ll have to provide " ++
                         intercalate ", " (map pretty (drop la params)) ++
                         " as well.")
              else Left ("expected " ++ show lp ++ " arguments, but received " ++
                         show la ++ ".\n\nThe arguments " ++
                         intercalate ", " (map pretty (drop lp args)) ++
                         " are not needed.")

apply :: Env -> XObj -> [XObj] -> [XObj] -> StateT Context IO (Either EvalError XObj)
apply env body params args =
  let allParams = map getName params
  in case splitWhen (":rest" ==) allParams of
       [a, b] -> callWith a b
       [a] -> callWith a []
       _ -> do
        ctx <- get
        return (evalError ctx
                 ("I didn’t understand this macro’s argument split, got `" ++
                  joinWith "," allParams ++
                  "`, but expected exactly one `:rest` separator.") Nothing)
  where callWith proper rest =
          let n = length proper
              insideEnv = Env Map.empty (Just env) Nothing [] InternalEnv 0
              insideEnv' = foldl' (\e (p, x) -> extendEnv e p x) insideEnv
                                  (zip proper (take n args))
              insideEnv'' = if null rest
                             then insideEnv'
                             else extendEnv insideEnv'
                                   (head rest)
                                   (XObj (Lst (drop n args)) Nothing Nothing)
          in eval insideEnv'' body

-- LEGACY STUFF

-- | Parses a string and then converts the resulting forms to commands, which are evaluated in order.
executeString :: Bool -> Bool -> Context -> String -> String -> IO Context
executeString doCatch printResult ctx input fileName =
  if doCatch then catch exec (catcher ctx) else exec
  where exec = case parse input fileName of
                 Left parseError ->
                   let sourcePos = Parsec.errorPos parseError
                       parseErrorXObj = XObj (Lst []) (Just dummyInfo { infoFile = fileName
                                                                      , infoLine = Parsec.sourceLine sourcePos
                                                                      , infoColumn = Parsec.sourceColumn sourcePos
                                                                      }) Nothing
                   in do
                    liftIO $ treatErr ctx (replaceChars (Map.fromList [('\n', " ")]) (show parseError)) parseErrorXObj
                    return ctx
                 Right xobjs -> do
                  (res, ctx) <- foldM interactiveFolder
                                    (XObj (Lst []) (Just dummyInfo) (Just UnitTy), ctx)
                                    xobjs
                  when (printResult && ty res /= Just UnitTy)
                    (putStrLnWithColor Yellow ("=> " ++ pretty res))
                  return ctx
        interactiveFolder (_, context) xobj =
          executeCommand context xobj
        treatErr ctx e xobj = do
          let msg =  "[PARSE ERROR] " ++ e
              fppl = projectFilePathPrintLength (contextProj ctx)
          case contextExecMode ctx of
            Check -> putStrLn (machineReadableInfoFromXObj fppl xobj ++ " " ++ msg)
            _ -> putStrLnWithColor Red msg
          throw CancelEvaluationException

-- | Used by functions that has a series of forms to evaluate and need to fold over them (producing a new Context in the end)
folder :: Context -> XObj -> IO Context
folder context xobj = do
     (_, ctx) <- executeCommand context xobj
     return ctx

-- | Take a repl command and execute it.
executeCommand :: Context -> XObj -> IO (XObj, Context)
executeCommand ctx s@(XObj (Sym _ _) _ _) =
  executeCommand ctx
    (XObj (Lst [ (XObj (Sym (SymPath [] "info") Symbol) (Just dummyInfo) Nothing)
               , s]) (Just dummyInfo) Nothing)
executeCommand ctx@(Context env typeEnv pathStrings proj lastInput execMode _) xobj =
  do when (isJust (envModuleName env)) $
       error ("Global env module name is " ++ fromJust (envModuleName env) ++ " (should be Nothing).")
     (result, newCtx) <- runStateT (eval env xobj) ctx
     case result of
       Left e -> do
         reportExecutionError newCtx (show e)
         return (xobj, newCtx)
       -- special case: calling a function at the repl
       Right (XObj (Lst (XObj (Lst (XObj (Defn _) _ _:name:_)) _ _:args)) i _) -> do
        (r, nc) <- runStateT (annotateWithinContext False (XObj (Lst (name:args)) i Nothing)) newCtx
        case r of
          Right (ann, _) -> executeCommand nc (withBuildAndRun (buildMainFunction ann))
          Left err -> do
           reportExecutionError nc (show err)
           return (xobj, nc)
       Right result -> return (result, newCtx)
  where withBuildAndRun xobj =
          XObj (Lst [ XObj Do (Just dummyInfo) Nothing
                    , xobj
                    , XObj (Lst [XObj (Sym (SymPath [] "build") Symbol) (Just dummyInfo) Nothing])
                           (Just dummyInfo) Nothing
                    , XObj (Lst [XObj (Sym (SymPath [] "run") Symbol) (Just dummyInfo) Nothing])
                           (Just dummyInfo) Nothing
                    ]) (Just dummyInfo) Nothing

reportExecutionError :: Context -> String -> IO ()
reportExecutionError ctx errorMessage =
  case contextExecMode ctx of
    Check -> putStrLn errorMessage
    _ ->
      do putStrLnWithColor Red errorMessage
         throw CancelEvaluationException

-- | Decides what to do when the evaluation fails for some reason.
catcher :: Context -> CarpException -> IO Context
catcher ctx exception =
  case exception of
    (ShellOutException message returnCode) ->
      do putStrLnWithColor Red ("[RUNTIME ERROR] " ++ message)
         stop returnCode
    CancelEvaluationException ->
      stop 1
    EvalException evalError ->
      do putStrLnWithColor Red (show evalError)
         stop 1
  where stop returnCode =
          case contextExecMode ctx of
            Repl -> return ctx
            Build -> exitWith (ExitFailure returnCode)
            Install _ -> exitWith (ExitFailure returnCode)
            BuildAndRun -> exitWith (ExitFailure returnCode)
            Check -> exitSuccess

specialCommandWith :: XObj -> SymPath -> [XObj] -> StateT Context IO (Either EvalError XObj)
specialCommandWith xobj path forms =
  do ctx <- get
     let pathStrings = contextPath ctx
         env = contextGlobalEnv ctx
         typeEnv = contextTypeEnv ctx
         useThese = envUseModules env
         env' = if path `elem` useThese then env else env { envUseModules = path : useThese }
         ctx' = ctx { contextGlobalEnv = env' }
     ctxAfter <- liftIO $ foldM folder ctx' forms
     let envAfter = contextGlobalEnv ctxAfter
         ctxAfter' = ctx { contextGlobalEnv = envAfter { envUseModules = useThese } } -- This will undo ALL use:s made inside the 'with'.
     put ctxAfter'
     return dynamicNil

specialCommandDefine :: XObj -> StateT Context IO (Either EvalError XObj)
specialCommandDefine xobj =
  do result <- annotateWithinContext True xobj
     case result of
       Right (annXObj, annDeps) ->
         do ctxAfterExpansion <- get
            ctxWithDeps <- liftIO $ foldM (define True) ctxAfterExpansion annDeps
            ctxWithDef <- liftIO $ define False ctxWithDeps annXObj
            put ctxWithDef
            return dynamicNil
       Left err ->
         return (Left err)

getSigFromDefnOrDef :: Context -> Env -> FilePathPrintLength -> XObj -> StateT Context IO (Either EvalError (Maybe (Ty, XObj)))
getSigFromDefnOrDef ctx globalEnv fppl xobj =
  let metaData = existingMeta globalEnv xobj
  in  case Map.lookup "sig" (getMeta metaData) of
        Just foundSignature ->
          case xobjToTy foundSignature of
            Just t -> let sigToken = XObj (Sym (SymPath [] "sig") Symbol) Nothing Nothing
                          nameToken = XObj (Sym (SymPath [] (getName xobj)) Symbol) Nothing Nothing
                          recreatedSigForm = XObj (Lst [sigToken, nameToken, foundSignature]) Nothing (Just MacroTy)
                      in return (Right (Just (t, recreatedSigForm)))
            Nothing -> return (evalError ctx ("Can't use '" ++ pretty foundSignature ++ "' as a type signature") (info xobj))
        Nothing -> return (Right Nothing)

annotateWithinContext :: Bool -> XObj -> StateT Context IO (Either EvalError (XObj, [XObj]))
annotateWithinContext qualifyDefn xobj =
  do ctx <- get
     let pathStrings = contextPath ctx
         fppl = projectFilePathPrintLength (contextProj ctx)
         globalEnv = contextGlobalEnv ctx
         typeEnv = contextTypeEnv ctx
         innerEnv = getEnv globalEnv pathStrings
     sig <- getSigFromDefnOrDef ctx globalEnv fppl xobj
     expansionResult <- expandAll eval globalEnv xobj
     ctxAfterExpansion <- get
     case expansionResult of
       Left err -> return (evalError ctx (show err) Nothing)
       Right expanded ->
         let xobjFullPath = if qualifyDefn then setFullyQualifiedDefn expanded (SymPath pathStrings (getName xobj)) else expanded
             xobjFullSymbols = setFullyQualifiedSymbols typeEnv globalEnv innerEnv xobjFullPath
         in case annotate typeEnv globalEnv xobjFullSymbols of
              Left err ->
                case contextExecMode ctx of
                  Check ->
                    let fppl = projectFilePathPrintLength (contextProj ctx)
                    in  return (evalError ctx (joinWith "\n" (machineReadableErrorStrings fppl err)) Nothing)
                  _ ->
                    return (evalError ctx (show err) (info xobj))
              Right ok ->
                return (Right ok)

primitiveDefmodule :: Primitive
primitiveDefmodule xobj env (XObj (Sym (SymPath [] moduleName) _) _ _:innerExpressions) = do
  ctx@(Context _ typeEnv pathStrings proj lastInput execMode history) <- get
  let fppl = projectFilePathPrintLength proj

      defineIt :: MetaData -> StateT Context IO (Either EvalError XObj)
      defineIt meta = do let parentEnv = getEnv env pathStrings
                             innerEnv = Env (Map.fromList []) (Just parentEnv) (Just moduleName) [] ExternalEnv 0
                             newModule = XObj (Mod innerEnv) (info xobj) (Just ModuleTy)
                             globalEnvWithModuleAdded = envInsertAt env (SymPath pathStrings moduleName) (Binder meta newModule)
                             ctx' = Context globalEnvWithModuleAdded typeEnv (pathStrings ++ [moduleName]) proj lastInput execMode history
                         ctxAfterModuleDef <- liftIO $ foldM folder ctx' innerExpressions
                         put (popModulePath ctxAfterModuleDef)
                         return dynamicNil

  result <- case lookupInEnv (SymPath pathStrings moduleName) env of
              Just (_, Binder _ (XObj (Mod _) _ _)) ->
                do let ctx' = Context env typeEnv (pathStrings ++ [moduleName]) proj lastInput execMode history -- use { = } syntax instead
                   ctxAfterModuleAdditions <- liftIO $ foldM folder ctx' innerExpressions
                   put (popModulePath ctxAfterModuleAdditions)
                   return dynamicNil -- TODO: propagate errors...
              Just (_, Binder existingMeta (XObj (Lst [XObj DocStub _ _, _]) _ _)) ->
                defineIt existingMeta
              Just (_, Binder _ x) ->
                return (evalError ctx ("Can't redefine '" ++ moduleName ++ "' as module") (info xobj))
              Nothing ->
                defineIt emptyMeta

  case result of
    Left err -> return (Left err)
    Right _ -> return dynamicNil

-- | "NORMAL" COMMANDS (just like the ones in Command.hs, but these need access to 'eval', etc.)

-- | Command for loading a Carp file.
commandLoad :: CommandCallback
commandLoad [xobj@(XObj (Str path) i _)] =
  do ctx <- get
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
           (relativeTo </> path) :                         -- the path from the file that contains the '(load)', or the current directory if not loading from a file (e.g. the repl)
           map (</> path) (projectCarpSearchPaths proj) ++ -- user defined search paths
           [carpDir </> "core" </> path] ++
           [libDir </> path]
         firstM _ [] = return Nothing
         firstM p (x:xs) = do
           q <- p x
           if q
             then return $ Just x
             else firstM p xs
     existingPath <- liftIO $ firstM doesFileExist fullSearchPaths
     case existingPath of
       Nothing ->
        if '@' `elem` path
          then tryInstall path
          else return $ invalidPath ctx path
       Just firstPathFound ->
         do canonicalPath <- liftIO (canonicalizePath firstPathFound)
            fileThatLoads <- liftIO (canonicalizePath (case i of
                                                         Just ii -> infoFile ii
                                                         Nothing -> ""))
            if canonicalPath == fileThatLoads
              then return $ cantLoadSelf ctx path
              else do let alreadyLoaded = projectAlreadyLoaded proj
                      if canonicalPath `elem` alreadyLoaded
                        then
                             return ()
                        else do contents <- liftIO $ slurp canonicalPath
                                let files = projectFiles proj
                                    files' = if canonicalPath `elem` files
                                             then files
                                             else files ++ [canonicalPath]
                                    proj' = proj { projectFiles = files', projectAlreadyLoaded = canonicalPath : alreadyLoaded }
                                newCtx <- liftIO $ executeString True False (ctx { contextProj = proj' }) contents canonicalPath
                                put newCtx
                      return dynamicNil
  where
    fppl ctx =
      projectFilePathPrintLength (contextProj ctx)
    invalidPath ctx path =
      evalError ctx
        ((case contextExecMode ctx of
          Check ->
            machineReadableInfoFromXObj (fppl ctx) xobj ++ " I can't find a file named: '" ++ path ++ "'"
          _ -> "I can't find a file named: '" ++ path ++ "'") ++
        "\n\nIf you tried loading an external package, try appending a version string (like `@master`)") (info xobj)
    invalidPathWith ctx path stderr cleanup cleanupPath = do
      _ <- liftIO $ when cleanup (removeDirectoryRecursive cleanupPath)
      return $ evalError ctx
        ((case contextExecMode ctx of
          Check ->
            machineReadableInfoFromXObj (fppl ctx) xobj ++ " I can't find a file named: '" ++ path ++ "'"
          _ -> "I can't find a file named: '" ++ path ++ "'") ++
        "\n\nI tried interpreting the statement as a git import, but got: " ++ stderr)
        (info xobj)
    replaceC c s [] = []
    replaceC c s (a:b) = if a == c then s ++ replaceC c s b else a : replaceC c s b
    cantLoadSelf ctx path =
      case contextExecMode ctx of
        Check ->
          evalError ctx (machineReadableInfoFromXObj (fppl ctx) xobj ++ " A file can't load itself: '" ++ path ++ "'") (info xobj)
        _ ->
          evalError ctx ("A file can't load itself: '" ++ path ++ "'") (info xobj)
    tryInstall path =
      let split = splitOn "@" path
      in tryInstallWithCheckout (joinWith "@" (init split)) (last split)
    fromURL url =
      let split = splitOn "/" (replaceC ':' "_COLON_" url)
          fst = head split
      in if fst `elem` ["https_COLON_", "http_COLON_"]
        then joinWith "/" (tail (tail split))
        else
          if '@' `elem` fst
            then joinWith "/" (joinWith "@" (tail (splitOn "@" fst)) : tail split)
            else url
    tryInstallWithCheckout path toCheckout = do
      ctx <- get
      let proj = contextProj ctx
      fpath <- liftIO $ cachePath $ projectLibDir proj </> fromURL path </> toCheckout
      cur <- liftIO getCurrentDirectory
      pathExists <- liftIO $ doesPathExist fpath
      let cleanup = not pathExists
      _ <- liftIO $ createDirectoryIfMissing True fpath
      _ <- liftIO $ setCurrentDirectory fpath
      (_, txt, _) <- liftIO $ readProcessWithExitCode "git" ["rev-parse", "--abbrev-ref=loose", "HEAD"] ""
      if txt == "HEAD\n"
      then do
        _ <- liftIO $ setCurrentDirectory cur
        doGitLoad path fpath
      else do
        _ <- liftIO $ readProcessWithExitCode "git" ["init"] ""
        _ <- liftIO $ readProcessWithExitCode "git" ["remote", "add", "origin", path] ""
        (x0, _, stderr0) <- liftIO $ readProcessWithExitCode "git" ["fetch", "--all", "--tags"] ""
        case x0 of
          ExitFailure _ -> do
            _ <- liftIO $ setCurrentDirectory cur
            invalidPathWith ctx path stderr0 cleanup fpath
          ExitSuccess -> do
            (x1, _, stderr1) <- liftIO $ readProcessWithExitCode "git" ["checkout", toCheckout] ""
            _ <- liftIO $ setCurrentDirectory cur
            case x1 of
              ExitSuccess -> doGitLoad path fpath
              ExitFailure _ -> invalidPathWith ctx path stderr1 cleanup fpath
    doGitLoad path fpath =
      let fName = last (splitOn "/" path)
          realName' = if ".git" `isSuffixOf` fName
                       then take (length fName - 4) fName
                       else fName
          realName = if ".carp" `isSuffixOf` realName'
                      then realName'
                      else realName' ++ ".carp"
          fileToLoad = fpath </> realName
          mainToLoad = fpath </> "main.carp"
      in do
        res <- commandLoad [XObj (Str fileToLoad) Nothing Nothing]
        case res of
          ret@(Right _) -> return ret
          Left _ ->  commandLoad [XObj (Str mainToLoad) Nothing Nothing]
commandLoad [x] = do
  ctx <- get
  return $ evalError ctx ("Invalid args to `load`: " ++ pretty x) (info x)

-- | Load several files in order.
loadFiles :: Context -> [FilePath] -> IO Context
loadFiles ctxStart filesToLoad = foldM folder ctxStart filesToLoad
  where folder :: Context -> FilePath -> IO Context
        folder ctx file = do
         (ret, newCtx) <- runStateT (commandLoad [XObj (Str file) Nothing Nothing]) ctx
         let fppl = projectFilePathPrintLength (contextProj newCtx)
         case ret of
           Left err -> throw (EvalException err)
           Right _ -> return newCtx

-- | Command for reloading all files in the project (= the files that has been loaded before).
commandReload :: CommandCallback
commandReload args =
  do ctx <- get
     let paths = projectFiles (contextProj ctx)
         f :: Context -> FilePath -> IO Context
         f context filepath = do let proj = contextProj context
                                     alreadyLoaded = projectAlreadyLoaded proj
                                 if filepath `elem` alreadyLoaded
                                   then
                                        return context
                                   else do
                                           contents <- slurp filepath
                                           let proj' = proj { projectAlreadyLoaded = filepath : alreadyLoaded }
                                           executeString False False (context { contextProj = proj' }) contents filepath
     newCtx <- liftIO (foldM f ctx paths)
     put newCtx
     return dynamicNil

-- | Command for expanding a form and its macros.
commandExpand :: CommandCallback
commandExpand [xobj] =
  do ctx <- get
     result <- expandAll eval (contextGlobalEnv ctx) xobj
     case result of
       Left e -> return (Left e)
       Right expanded ->
         liftIO $ do putStrLnWithColor Yellow (pretty expanded)
                     return dynamicNil

-- | This function will show the resulting C code from an expression.
-- | i.e. (Int.+ 2 3) => "_0 = 2 + 3"
commandC :: CommandCallback
commandC [xobj] =
  do ctx <- get
     let globalEnv = contextGlobalEnv ctx
         typeEnv = contextTypeEnv ctx
     result <- expandAll eval globalEnv xobj
     case result of
       Left err -> return $ Left err
       Right expanded ->
         case annotate typeEnv globalEnv (setFullyQualifiedSymbols typeEnv globalEnv globalEnv expanded) Nothing of
           Left err -> return $ evalError ctx (show err) (info xobj)
           Right (annXObj, annDeps) ->
             do let cXObj = printC annXObj
                    cDeps = concatMap printC annDeps
                    c = cDeps ++ cXObj
                liftIO (putStr c)
                return dynamicNil

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
  XObj (Lst [ XObj (Defn Nothing) (Just dummyInfo) Nothing
            , XObj (Sym (SymPath [] "main") Symbol) (Just dummyInfo) Nothing
            , XObj (Arr []) (Just dummyInfo) Nothing
            , case ty xobj of
                Just UnitTy -> xobj
                Just (RefTy _ _) -> XObj (Lst [XObj (Sym (SymPath [] "println*") Symbol) (Just dummyInfo) Nothing, xobj])
                                       (Just dummyInfo) (Just UnitTy)
                Just _ -> XObj (Lst [XObj (Sym (SymPath [] "println*") Symbol) (Just dummyInfo) Nothing,
                                     XObj (Lst [XObj Ref (Just dummyInfo) Nothing, xobj])
                                           (Just dummyInfo) (Just UnitTy)])
                               (Just dummyInfo) (Just UnitTy)
            ]) (Just dummyInfo) (Just (FuncTy [] UnitTy StaticLifetimeTy))

primitiveDefdynamic :: Primitive
primitiveDefdynamic _ _ [XObj (Sym (SymPath [] name) _) _ _, value] =
  do env <- gets contextGlobalEnv
     result <- eval env value
     case result of
       Left err -> return (Left err)
       Right evaledBody ->
         dynamicOrMacroWith (\path -> [XObj DefDynamic Nothing Nothing, XObj (Sym path Symbol) Nothing Nothing, evaledBody]) DynamicTy name value
primitiveDefdynamic _ _ [notName, body] = do
  ctx <- get
  return (evalError ctx ("`defndynamic` expected a name as first argument, but got " ++ pretty notName) (info notName))

primitiveEval :: Primitive
primitiveEval _ env [val] = do
  -- primitives don’t evaluate their arguments, so this needs to double-evaluate
  arg <- eval env val
  case arg of
    Left err -> return (Left err)
    Right ok -> eval env ok

primitives :: Map.Map SymPath Primitive
primitives = Map.fromList
  [ makePrim "quote" 1 "(quote x) ; where x is an actual symbol" (\_ _ [x] -> return (Right x))
  , makeVarPrim "file" "(file mysymbol)" primitiveFile
  , makeVarPrim "line" "(line mysymbol)" primitiveLine
  , makeVarPrim "column" "(column mysymbol)" primitiveColumn
  , makePrim "info" 1 "(info mysymbol)" primitiveInfo
  , makeVarPrim "register-type" "(register-type Name <optional: members>)" primitiveRegisterType
  , makePrim "defmacro" 3 "(defmacro name [args :rest restargs] body)" primitiveDefmacro
  , makePrim "defndynamic" 3 "(defndynamic name [args] body)" primitiveDefndynamic
  , makePrim "defdynamic" 2 "(defdynamic name value)" primitiveDefdynamic
  , makePrim "type" 1 "(type mysymbol)" primitiveType
  , makePrim "members" 1 "(type mysymbol)" primitiveMembers
  , makeVarPrim "defmodule" "(defmodule MyModule <expressions>)" primitiveDefmodule
  , makePrim "meta-set!" 3 "(meta-set! mysymbol \"mykey\" \"myval\")" primitiveMetaSet
  , makePrim "meta" 2 "(meta mysymbol \"mykey\")" primitiveMeta
  , makePrim "definterface" 2 "(definterface myfunction MyType)" primitiveDefinterface
  , makeVarPrim "register" "(register name <signature> <optional: override>)" primitiveRegister
  , makeVarPrim "deftype" "(deftype name <members>)" primitiveDeftype
  , makePrim "use" 1 "(use MyModule)" primitiveUse
  , makePrim "eval" 1 "(evaluate mycode)" primitiveEval
  , makePrim "defined?" 1 "(evaluate mycode)" primitiveEval
  ]

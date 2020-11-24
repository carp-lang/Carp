{-# LANGUAGE LambdaCase #-}
module Eval where

import Control.Applicative
import Control.Exception
import Control.Monad.State
import Data.Foldable (foldlM, foldrM)
import Data.List (foldl', isSuffixOf, intercalate)
import Data.List.Split (splitOn, splitWhen)
import Data.Maybe (fromJust, isJust, fromMaybe)
import System.Exit (exitSuccess, exitWith, ExitCode(..))
import System.Process (readProcessWithExitCode)
import qualified Data.Map as Map
import qualified Text.Parsec as Parsec

import Parsing
import Emit
import Obj
import Project
import Types
import Infer
import ColorText
import Util
import Commands
import Expand
import Lookup
import Qualify
import TypeError
import Path
import Primitives
import Info
import qualified Meta

data LookupPreference = PreferDynamic
                      | PreferGlobal

-- Prefer dynamic bindings
evalDynamic :: Context -> XObj -> IO (Context, Either EvalError XObj)
evalDynamic ctx xobj = eval ctx xobj PreferDynamic

-- Prefer global bindings
evalStatic :: Context -> XObj -> IO (Context, Either EvalError XObj)
evalStatic ctx xobj = eval ctx xobj PreferGlobal

-- | Dynamic (REPL) evaluation of XObj:s (s-expressions)
-- Note: You might find a bunch of code of the following form both here and in
-- macroExpand:
--
-- pure (ctx, do res <- <something>
--               Right <something else with res>)
--
-- This might a little weird to you, and rightfully so. Through the nested do
-- we ensure that an evaluation is forced where it needs to be, since we depend
-- on the state here; eval is inherently stateful (because it carries around
-- the compiler’s context, which might change after each macro expansion), and
-- it gets real weird with laziness. (Note to the note: this code is mostly a
-- remnant of us using StateT, and might not be necessary anymore since we
-- switched to more explicit state-passing.)
eval :: Context -> XObj -> LookupPreference -> IO (Context, Either EvalError XObj)
eval ctx xobj@(XObj o i t) preference =
  case o of
    Lst body   -> eval' body
    Sym path@(SymPath p n) _ ->
      pure
      $ fromMaybe (evalError ctx ("Can't find symbol '" ++ show n ++ "'") i) -- all else failed, error.
      -- Certain contexts prefer looking up bindings in the dynamic environment (e.g. defdyanmic) while others
      -- prefer the static global environment.
      ((case preference of
          PreferDynamic -> tryDynamicLookup
          PreferGlobal -> (tryLookup path <|> tryDynamicLookup))
      <|> (if null p then tryInternalLookup path else tryLookup path))
      where tryDynamicLookup =
              (lookupInEnv (SymPath ("Dynamic" : p) n) (contextGlobalEnv ctx)
              >>= \(_, Binder _ found) -> pure (ctx, Right (resolveDef found)))
            tryInternalLookup path =
              (contextInternalEnv ctx
              >>= lookupInEnv path
              >>= \(_ , Binder _ found) -> pure (ctx, Right (resolveDef found)))
              <|> tryLookup path -- fallback
            tryLookup path =
                (lookupInEnv path (contextGlobalEnv ctx)
                  >>= \(_, Binder meta found) -> checkPrivate meta found)
                <|> (lookupInEnv path (getTypeEnv (contextTypeEnv ctx))
                    >>= \(_, Binder _ found) -> pure (ctx, Right (resolveDef found)))
            checkPrivate meta found =
              pure $ if metaIsTrue meta "private"
                     then evalError ctx ("The binding: " ++ show (getPath found) ++ " is private; it may only be used within the module that defines it.") i
                     else (ctx, Right (resolveDef found))
    Arr objs  -> do
      (newCtx, evaled) <- foldlM successiveEval (ctx, Right []) objs
      pure (newCtx, do ok <- evaled
                       Right (XObj (Arr ok) i t))
    StaticArr objs  -> do
      (newCtx, evaled) <- foldlM successiveEval (ctx, Right []) objs
      pure (newCtx, do ok <- evaled
                       Right (XObj (StaticArr ok) i t))
    _ ->  do (nctx, res) <- annotateWithinContext False ctx xobj
             pure $ case res of
               Left e -> (nctx, Left e)
               Right (val, deps) -> (nctx, Right val)
  where
    resolveDef (XObj (Lst [XObj DefDynamic _ _, _, value]) _ _) = value
    resolveDef (XObj (Lst [XObj LetDef _ _, _, value]) _ _) = value
    resolveDef x = x
    eval' form =
      case form of

       [XObj If _ _, mcond, mtrue, mfalse] -> do
         (newCtx, evd) <- eval ctx mcond preference
         case evd of
           Right cond ->
             case obj cond of
               Bol b -> eval newCtx (if b then mtrue else mfalse) preference
               _     ->
                 pure (evalError ctx
                          ("This `if` condition contains the non-boolean value `" ++
                           pretty cond ++ "`") (info cond))
           Left e -> pure (newCtx, Left e)

       XObj If _ _:_ ->
         pure (evalError ctx
                  ("I didn’t understand this `if`.\n\n Got:\n```\n" ++ pretty xobj ++
                   "\n```\n\nExpected the form:\n```\n(if cond then else)\n```\n") (info xobj))

       [XObj (Defn _) _ _, name, args@(XObj (Arr a) _ _), body] ->
         case obj name of
           (Sym (SymPath [] _) _) ->
               if all isUnqualifiedSym a
               then specialCommandDefine ctx xobj
               else pure (evalError ctx
                 ("`defn` requires all arguments to be unqualified symbols, but it got `" ++
                  pretty args ++ "`") (info xobj))
           _                      -> pure (evalError ctx
             ("`defn` identifiers must be unqualified symbols, but it got `" ++
              pretty name ++ "`") (info xobj))

       [XObj (Defn _) _ _, name, invalidArgs, _] ->
         pure (evalError ctx
           ("`defn` requires an array of symbols as argument list, but it got `" ++
            pretty invalidArgs ++ "`") (info xobj))

       (defn@(XObj (Defn _) _ _) : _) ->
           pure (evalError ctx
             ("I didn’t understand the `defn` at " ++ prettyInfoFromXObj xobj ++
              ":\n\n" ++ pretty xobj ++
              "\n\nIs it valid? Every `defn` needs to follow the form `(defn name [arg] body)`.")
              (info defn))

       [def@(XObj Def _ _), name, expr] ->
         if isUnqualifiedSym name
         then specialCommandDefine ctx xobj
         else pure (evalError ctx
           ("`def` identifiers must be unqualified symbols, but it got `" ++
            pretty name ++ "`") (info xobj))

       [the@(XObj The _ _), ty, value] ->
         do (newCtx, evaledValue) <- expandAll evalDynamic ctx value -- TODO: Why expand all here?
            pure (newCtx, do okValue <- evaledValue
                             Right (XObj (Lst [the, ty, okValue]) i t))

       (XObj The _ _: _) ->
           pure (evalError ctx
             ("I didn’t understand the `the` at " ++ prettyInfoFromXObj xobj ++
              ":\n\n" ++ pretty xobj ++
              "\n\nIs it valid? Every `the` needs to follow the form `(the type expression)`.")
              (info xobj))

       [XObj Let _ _, XObj (Arr bindings) bindi bindt, body]
         | odd (length bindings) -> pure (evalError ctx
             ("Uneven number of forms in `let`: " ++ pretty xobj)
             (info xobj)) -- Unreachable?
         | not (all isSym (evenIndices bindings)) -> pure (evalError ctx
             ("`let` identifiers must be symbols, but it got `" ++
              joinWithSpace (map pretty bindings) ++ "`") (info xobj))
         | otherwise ->
             do let binds = unwrapVar (pairwise bindings) []
                    i = contextInternalEnv ctx
                    ni = Env Map.empty i Nothing [] InternalEnv 0
                eitherCtx <- foldrM successiveEval (Right ctx{contextInternalEnv=Just ni}) binds
                case eitherCtx of
                   Left err -> pure (ctx, Left err)
                   Right newCtx -> do
                          (finalCtx, evaledBody) <- eval newCtx body preference
                          let Just e = contextInternalEnv finalCtx
                          pure (finalCtx{contextInternalEnv=envParent e},
                                  do okBody <- evaledBody
                                     Right okBody)
         where unwrapVar [] acc = acc
               unwrapVar ((XObj (Sym (SymPath [] x) _) _ _,y):xs) acc = unwrapVar xs ((x,y):acc)
               successiveEval (n, x) =
                 \case
                   err@(Left _) -> pure err
                   Right ctx -> do
                     (newCtx, res) <- eval ctx x preference
                     case res of
                       Right okX -> do
                        let binder = Binder emptyMeta (XObj (Lst [(XObj LetDef Nothing Nothing), XObj (Sym (SymPath [] n) Symbol) Nothing Nothing, okX]) Nothing (ty okX))
                            Just e = contextInternalEnv ctx
                        pure $ Right (newCtx {contextInternalEnv=Just (envInsertAt e (SymPath [] n) binder)})
                       Left err -> pure $ Left err

       l@[XObj Fn{} _ _, args@(XObj (Arr a) _ _), f] ->
         pure $ if all isUnqualifiedSym a
                then (ctx, Right (XObj (Closure (XObj (Lst l) i t) (CCtx ctx)) i t))
                else evalError ctx ("`fn` requires all arguments to be unqualified symbols, but it got `" ++ pretty args ++ "`") (info args)
       XObj (Closure (XObj (Lst [XObj (Fn _ _) _ _, XObj (Arr params) _ _, body]) _ _) (CCtx c)) i _:args ->
         case checkArity params args of
           Left err -> pure (evalError ctx err (info xobj))
           Right () ->
             do (newCtx, evaledArgs) <- foldlM successiveEval (ctx, Right []) args
                case evaledArgs of
                  Right okArgs -> do
                    (_, res) <- apply c body params okArgs
                    pure (newCtx, res)
                  Left err -> pure (newCtx, Left err)

       XObj (Lst [XObj Dynamic _ _, _, XObj (Arr params) _ _, body]) i _:args ->
         case checkArity params args of
           Left err ->
             pure (evalError ctx err i)
           Right () ->
             do (newCtx, evaledArgs) <- foldlM successiveEval (ctx, Right []) args
                case evaledArgs of
                  Right okArgs -> apply newCtx body params okArgs
                  Left err -> pure (newCtx, Left err)

       XObj (Lst [XObj Macro _ _, _, XObj (Arr params) _ _, body]) i _:args ->
         case checkArity params args of
           Left err -> pure (evalError ctx err i)
           Right () -> do
             -- Replace info so that the macro which is called gets the source location info of the expansion site.
             --let replacedBody = replaceSourceInfoOnXObj (info xobj) body
             (ctx', res) <- apply ctx body params args
             case res of
              Right xobj -> macroExpand ctx' xobj
              Left err -> pure (ctx, res)

       XObj (Lst [XObj (Command callback) _ _, _, _]) _ _:args ->
         do (newCtx, evaledArgs) <- foldlM successiveEval (ctx, Right []) args
            case evaledArgs of
              Right okArgs -> getCommand callback ctx okArgs
              Left err -> pure (ctx, Left err)

       x@(XObj (Lst [XObj (Primitive prim) _ _, _, _]) _ _):args -> (getPrimitive prim) x ctx args

       XObj (Lst (XObj (Defn _) _ _:_)) _ _:_ -> pure (ctx, Left (HasStaticCall xobj i))
       XObj (Lst (XObj (Interface _ _) _ _:_)) _ _:_ -> pure (ctx, Left (HasStaticCall xobj i))
       XObj (Lst (XObj (Instantiate _) _ _:_)) _ _:_ -> pure (ctx, Left (HasStaticCall xobj i))
       XObj (Lst (XObj (Deftemplate _) _ _:_)) _ _:_ -> pure (ctx, Left (HasStaticCall xobj i))
       XObj (Lst (XObj (External _) _ _:_)) _ _:_ -> pure (ctx, Left (HasStaticCall xobj i))
       XObj (Match _) _ _:_ -> pure (ctx, Left (HasStaticCall xobj i))
       [XObj Ref _ _, _] -> pure (ctx, Left (HasStaticCall xobj i))

       l@(XObj (Lst _) i t):args -> do
         (newCtx, f) <- eval ctx l preference
         case f of
            Right fun -> do
             (newCtx', res) <- eval (pushFrame newCtx xobj) (XObj (Lst (fun:args)) i t) preference
             pure (popFrame newCtx', res)
            x -> pure (newCtx, x)

       x@(XObj sym@(Sym s _) i _):args -> do
         (newCtx, f) <- eval ctx x preference
         case f of
           Right fun -> do
             (newCtx', res) <- eval (pushFrame ctx xobj) (XObj (Lst (fun:args)) i t) preference
             pure (popFrame newCtx', res)
           Left err -> pure (newCtx, Left err)

       XObj With _ _ : xobj@(XObj (Sym path _) _ _) : forms ->
         specialCommandWith ctx xobj path forms
       XObj With _ _ : _ ->
         pure (evalError ctx ("Invalid arguments to `with`: " ++ pretty xobj) (info xobj))
       XObj SetBang _ _ :args -> specialCommandSet ctx args
       [XObj Do _ _] ->
         pure (evalError ctx "No forms in do" (info xobj))
       XObj Do _ _ : rest -> foldlM successiveEval (ctx, dynamicNil) rest
        where successiveEval (ctx, acc) x =
               case acc of
                 err@(Left _) -> pure (ctx, err)
                 Right _ -> eval ctx x preference
       [XObj While _ _, cond, body] ->
         specialCommandWhile ctx cond body
       [XObj Address _ _, value] ->
         specialCommandAddress ctx value
       [] -> pure (ctx, dynamicNil)
       x -> do
        pure (evalError ctx ("I did not understand the form `" ++ pretty xobj ++ "`") (info xobj))
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
    successiveEval (ctx, acc) x =
     case acc of
       Left _ -> pure (ctx, acc)
       Right l -> do
        (newCtx, evald) <- eval ctx x preference
        pure $ case evald of
          Right res -> (newCtx, Right (l ++ [res]))
          Left err -> (newCtx, Left err)

macroExpand :: Context -> XObj -> IO (Context, Either EvalError XObj)
macroExpand ctx xobj =
  case xobj of
    XObj (Arr objs) i t -> do
      (newCtx, expanded) <- foldlM successiveExpand (ctx, Right []) objs
      pure (newCtx, do ok <- expanded
                       Right (XObj (Arr ok) i t))
    XObj (StaticArr objs) i t -> do
      (newCtx, expanded) <- foldlM successiveExpand (ctx, Right []) objs
      pure (newCtx, do ok <- expanded
                       Right (XObj (StaticArr ok) i t))
    XObj (Lst [XObj (Lst (XObj Macro _ _:_)) _ _]) _ _ -> evalDynamic ctx xobj
    XObj (Lst (x@(XObj sym@(Sym s _) _ _):args)) i t -> do
      (newCtx, f) <- evalDynamic ctx x
      case f of
        Right m@(XObj (Lst (XObj Macro _ _:_)) _ _) -> do
          (newCtx', res) <- evalDynamic ctx (XObj (Lst (m:args)) i t)
          pure (newCtx', res)
        _ -> do
          (newCtx, expanded) <- foldlM successiveExpand (ctx, Right []) args
          pure (newCtx, do ok <- expanded
                           Right (XObj (Lst (x:ok)) i t))
    XObj (Lst objs) i t -> do
      (newCtx, expanded) <- foldlM successiveExpand (ctx, Right []) objs
      pure (newCtx, do ok <- expanded
                       Right (XObj (Lst ok) i t))
    _ -> pure (ctx, Right xobj)
  where successiveExpand (ctx, acc) x =
         case acc of
           Left _ -> pure (ctx, acc)
           Right l -> do
            (newCtx, expanded) <- macroExpand ctx x
            pure $ case expanded of
              Right res -> (newCtx, Right (l ++ [res]))
              Left err -> (newCtx, Left err)

apply :: Context -> XObj -> [XObj] -> [XObj] -> IO (Context, Either EvalError XObj)
apply ctx@Context{contextInternalEnv=internal} body params args =
  let env = contextEnv ctx
      allParams = map getName params
  in case splitWhen (":rest" ==) allParams of
       [a, b] -> callWith env a b
       [a] -> callWith env a []
       _ ->
        pure (evalError ctx
                 ("I didn’t understand this macro’s argument split, got `" ++
                  joinWith "," allParams ++
                  "`, but expected exactly one `:rest` separator.") Nothing)
  where callWith env proper rest = do
          let n = length proper
              insideEnv = Env Map.empty internal Nothing [] InternalEnv 0
              insideEnv' = foldl' (\e (p, x) -> extendEnv e p x) insideEnv
                                  (zip proper (take n args))
              insideEnv'' = if null rest
                             then insideEnv'
                             else extendEnv insideEnv'
                                   (head rest)
                                   (XObj (Lst (drop n args)) Nothing Nothing)
          (c, r) <- evalDynamic (ctx {contextInternalEnv=Just insideEnv''}) body
          pure (c{contextInternalEnv=internal}, r)

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
                    _ <- liftIO $ treatErr ctx (replaceChars (Map.fromList [('\n', " ")]) (show parseError)) parseErrorXObj
                    pure ctx
                 Right xobjs -> do
                  (res, ctx) <- foldM interactiveFolder
                                    (XObj (Lst []) (Just dummyInfo) (Just UnitTy), ctx)
                                    xobjs
                  when (printResult && ty res /= Just UnitTy)
                    (putStrLnWithColor Yellow ("=> " ++ pretty res))
                  pure ctx
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
     pure ctx

-- | Take a repl command and execute it.
executeCommand :: Context -> XObj -> IO (XObj, Context)
executeCommand ctx s@(XObj (Sym _ _) _ _) =
  executeCommand ctx
    (XObj (Lst [ XObj (Sym (SymPath [] "info") Symbol) (Just dummyInfo) Nothing
               , s]) (Just dummyInfo) Nothing)
executeCommand ctx@(Context env _ _ _ _ _ _ _) xobj =
  do when (isJust (envModuleName env)) $
       error ("Global env module name is " ++ fromJust (envModuleName env) ++ " (should be Nothing).")
     -- The s-expression command is a special case that prefers global/static bindings over dynamic bindings
     -- when given a naked binding (no path) as an argument; (s-expr inc)
     (newCtx, result) <- if (xobjIsSexp xobj) then evalStatic ctx xobj else evalDynamic ctx xobj
     case result of
       Left e@(EvalError _ _ _ _) -> do
         reportExecutionError newCtx (show e)
         pure (xobj, newCtx)

       -- special case: calling something static at the repl
       Right (XObj (Lst (XObj (Lst (XObj (Defn _) _ _:(XObj (Sym (SymPath [] "main") _) _ _):_)) _ _:_)) _ _) ->
        executeCommand newCtx (withBuildAndRun (XObj (Lst []) (Just dummyInfo) Nothing))
       Left (HasStaticCall _ _) ->
        callFromRepl newCtx xobj

       Right result -> pure (result, newCtx)
  where callFromRepl newCtx xobj = do
          (nc, r) <- annotateWithinContext False newCtx xobj
          case r of
            Right (ann, deps) -> do
              ctxWithDeps <- liftIO $ foldM (define True) nc deps
              executeCommand ctxWithDeps (withBuildAndRun (buildMainFunction ann))
            Left err -> do
             reportExecutionError nc (show err)
             pure (xobj, nc)
        withBuildAndRun xobj =
          XObj (Lst [ XObj Do (Just dummyInfo) Nothing
                    , xobj
                    , XObj (Lst [XObj (Sym (SymPath [] "build") Symbol) (Just dummyInfo) Nothing])
                           (Just dummyInfo) Nothing
                    , XObj (Lst [XObj (Sym (SymPath [] "run") Symbol) (Just dummyInfo) Nothing])
                           (Just dummyInfo) Nothing
                    ]) (Just dummyInfo) Nothing
        xobjIsSexp (XObj (Lst (XObj (Sym (SymPath [] "s-expr") Symbol) _ _:_)) _ _) = True
        xobjIsSexp _ = False

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
            Repl -> pure ctx
            Build -> exitWith (ExitFailure returnCode)
            Install _ -> exitWith (ExitFailure returnCode)
            BuildAndRun -> exitWith (ExitFailure returnCode)
            Check -> exitSuccess

specialCommandWith :: Context -> XObj -> SymPath -> [XObj] -> IO (Context, Either EvalError XObj)
specialCommandWith ctx xobj path forms = do
  let pathStrings = contextPath ctx
      env = contextEnv ctx
      typeEnv = contextTypeEnv ctx
      useThese = envUseModules env
      env' = if path `elem` useThese then env else env { envUseModules = path : useThese }
      ctx' = ctx { contextGlobalEnv = env' }
  ctxAfter <- liftIO $ foldM folder ctx' forms
  let envAfter = contextEnv ctxAfter
      ctxAfter' = ctx { contextGlobalEnv = envAfter { envUseModules = useThese } } -- This will undo ALL use:s made inside the 'with'.
  pure (ctxAfter', dynamicNil)

specialCommandDefine :: Context -> XObj -> IO (Context, Either EvalError XObj)
specialCommandDefine ctx xobj =
  do (newCtx, result) <- annotateWithinContext True ctx xobj
     case result of
       Right (annXObj, annDeps) ->
         do ctxWithDeps <- liftIO $ foldM (define True) newCtx annDeps
            ctxWithDef <- liftIO $ define False ctxWithDeps annXObj
            pure (ctxWithDef, dynamicNil)
       Left err ->
         pure (ctx, Left err)

specialCommandAddress :: Context -> XObj -> IO (Context, Either EvalError XObj)
specialCommandAddress ctx xobj =
  case xobj of
    XObj (Sym path _) _ _ ->
      do (newCtx, result) <- annotateWithinContext False ctx xobj
         case result of
           Right (annXObj, annDeps) -> return (newCtx, Right annXObj)
           Left err ->
             return (ctx, Left err)
    _ -> return (evalError ctx ("Can't get the address of non-symbol " ++ pretty xobj) (info xobj))
specialCommandWhile :: Context -> XObj -> XObj -> IO (Context, Either EvalError XObj)
specialCommandWhile ctx cond body = do
  (newCtx, evd) <- evalDynamic ctx cond
  case evd of
    Right c ->
      case obj c of
        Bol b -> if b
          then do
            (newCtx, _) <- evalDynamic newCtx body
            specialCommandWhile newCtx cond body
          else
            pure (newCtx, dynamicNil)
        _ ->
          pure (evalError ctx ("This `while` condition contains the non-boolean value '" ++
                  pretty c ++ "`") (info c))
    Left e -> pure (newCtx, Left e)

getSigFromDefnOrDef :: Context -> Env -> FilePathPrintLength -> XObj -> (Either EvalError (Maybe (Ty, XObj)))
getSigFromDefnOrDef ctx globalEnv fppl xobj@(XObj _ i t) =
  let pathStrings = contextPath ctx
      path = (getPath xobj)
      fullPath = case path of
                   (SymPath [] n) -> consPath pathStrings path
                   (SymPath quals n) -> path
      metaData = existingMeta globalEnv (XObj (Sym fullPath Symbol) i t)
  in  case Meta.get "sig" metaData of
        Just foundSignature ->
          case xobjToTy foundSignature of
            Just t -> let sigToken = XObj (Sym (SymPath [] "sig") Symbol) Nothing Nothing
                          nameToken = XObj (Sym (SymPath [] (getName xobj)) Symbol) Nothing Nothing
                          recreatedSigForm = XObj (Lst [sigToken, nameToken, foundSignature]) Nothing (Just MacroTy)
                      in Right (Just (t, recreatedSigForm))
            Nothing -> Left (EvalError ("Can't use '" ++ pretty foundSignature ++ "' as a type signature") (contextHistory ctx) fppl (info xobj))
        Nothing -> Right Nothing

annotateWithinContext :: Bool -> Context -> XObj -> IO (Context, Either EvalError (XObj, [XObj]))
annotateWithinContext qualifyDefn ctx xobj = do
     let pathStrings = contextPath ctx
         fppl = projectFilePathPrintLength (contextProj ctx)
         globalEnv = contextGlobalEnv ctx
         typeEnv = contextTypeEnv ctx
         innerEnv = getEnv globalEnv pathStrings
     let sig = getSigFromDefnOrDef ctx globalEnv fppl xobj
     case sig of
       Left err -> pure (ctx, Left err)
       Right okSig -> do
         (ctxAfterExpansion, expansionResult) <- expandAll evalDynamic ctx xobj
         case expansionResult of
           Left err -> pure (evalError ctx (show err) Nothing)
           Right expanded ->
             let xobjFullPath = if qualifyDefn then setFullyQualifiedDefn expanded (SymPath pathStrings (getName xobj)) else expanded
                 xobjFullSymbols = setFullyQualifiedSymbols typeEnv globalEnv innerEnv xobjFullPath
             in case annotate typeEnv globalEnv xobjFullSymbols okSig of
                  Left err ->
                    case contextExecMode ctx of
                      Check ->
                        let fppl = projectFilePathPrintLength (contextProj ctx)
                        in  pure (evalError ctx (joinLines (machineReadableErrorStrings fppl err)) Nothing)
                      _ ->
                        pure (evalError ctx (show err) (info xobj))
                  Right ok -> pure (ctx, Right ok)

primitiveDefmodule :: Primitive
primitiveDefmodule xobj ctx@(Context env i typeEnv pathStrings proj lastInput execMode history) (XObj (Sym (SymPath [] moduleName) _) _ _:innerExpressions) = do
  let fppl = projectFilePathPrintLength proj

      defineIt :: MetaData -> IO (Context, Either EvalError XObj)
      defineIt meta = do
        let parentEnv = getEnv env pathStrings
            innerEnv = Env (Map.fromList []) (Just parentEnv) (Just moduleName) [] ExternalEnv 0
            newModule = XObj (Mod innerEnv) (info xobj) (Just ModuleTy)
            globalEnvWithModuleAdded = envInsertAt env (SymPath pathStrings moduleName) (Binder meta newModule)
            ctx' = Context globalEnvWithModuleAdded (Just (innerEnv{envParent=i})) typeEnv (pathStrings ++ [moduleName]) proj lastInput execMode history
        (ctxAfterModuleDef, res) <- liftIO $ foldM folder (ctx', dynamicNil) innerExpressions
        pure (popModulePath ctxAfterModuleDef{contextInternalEnv=i}, res)

  (newCtx, result) <-
    case lookupInEnv (SymPath pathStrings moduleName) env of
      Just (_, Binder _ (XObj (Mod innerEnv) _ _)) -> do
        let ctx' = Context env (Just innerEnv) typeEnv (pathStrings ++ [moduleName]) proj lastInput execMode history -- TODO: use { = } syntax instead
        (ctxAfterModuleAdditions, res) <- liftIO $ foldM folder (ctx', dynamicNil) innerExpressions
        pure (popModulePath ctxAfterModuleAdditions{contextInternalEnv=i}, res) -- TODO: propagate errors...
      Just (_, Binder existingMeta (XObj (Lst [XObj DocStub _ _, _]) _ _)) ->
        defineIt existingMeta
      Just (_, Binder _ x) ->
        pure (evalError ctx ("Can't redefine '" ++ moduleName ++ "' as module") (info xobj))
      Nothing ->
        defineIt emptyMeta

  pure $ case result of
    Left err -> (newCtx, Left err)
    Right _ -> (newCtx, dynamicNil)
  where folder (ctx, r) x =
         case r of
           Left err -> pure (ctx, r)
           Right _ -> do
             (newCtx, result) <- macroExpand ctx x
             case result of
               Left err -> pure (newCtx, Left err)
               Right e -> do
                 (newCtx, result) <- evalDynamic newCtx e
                 case result of
                   Left err -> pure (newCtx, Left err)
                   Right _ -> pure (newCtx, r)
primitiveDefmodule _ ctx (x:_) =
  pure (evalError ctx ("`defmodule` expects a symbol, got '" ++ pretty x ++ "' instead.") (info x))
primitiveDefmodule _ ctx [] =
  pure (evalError ctx "`defmodule` requires at least a symbol, received none." (Just dummyInfo))

-- | "NORMAL" COMMANDS (just like the ones in Command.hs, but these need access to 'eval', etc.)

-- | Command for loading a Carp file.
commandLoad :: CommandCallback
commandLoad ctx [xobj@(XObj (Str path) i _)] =
  loadInternal ctx xobj path i DoesReload
commandLoad ctx [x] =
  pure $ evalError ctx ("Invalid args to `load`: " ++ pretty x) (info x)

commandLoadOnce :: CommandCallback
commandLoadOnce ctx [xobj@(XObj (Str path) i _)] =
  loadInternal ctx xobj path i Frozen
commandLoadOnce ctx [x] =
  pure $ evalError ctx ("Invalid args to `load-once`: " ++ pretty x) (info x)

loadInternal :: Context -> XObj -> String -> Maybe Info -> ReloadMode -> IO (Context, Either EvalError XObj)
loadInternal ctx xobj path i reloadMode = do
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
      firstM _ [] = pure Nothing
      firstM p (x:xs) = do
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
      do canonicalPath <- liftIO (canonicalizePath firstPathFound)
         fileThatLoads <- liftIO (canonicalizePath (case i of
                                                      Just ii -> infoFile ii
                                                      Nothing -> ""))
         if canonicalPath == fileThatLoads
           then pure $ cantLoadSelf ctx path
           else do let alreadyLoaded = projectAlreadyLoaded proj ++ frozenPaths proj
                   if canonicalPath `elem` alreadyLoaded
                     then pure (ctx, dynamicNil)
                     else do
                      contents <- liftIO $ slurp canonicalPath
                      let files = projectFiles proj
                          files' = if canonicalPath `elem` (map fst files)
                                   then files
                                   else files ++ [(canonicalPath, reloadMode)]
                          prevStack = projectLoadStack proj
                          proj' = proj { projectFiles = files'
                                       , projectAlreadyLoaded = canonicalPath : alreadyLoaded
                                       , projectLoadStack = canonicalPath : prevStack
                                       }
                      newCtx <- liftIO $ executeString True False (ctx { contextProj = proj' }) contents canonicalPath

                      pure (newCtx { contextProj = (contextProj newCtx) { projectLoadStack = prevStack } }, dynamicNil)
  where
    frozenPaths proj =
      if projectForceReload proj
      then [] -- No paths are Frozen when the "force reload" project setting is true.
      else map fst $ filter (isFrozen . snd) (projectFiles proj)

    isFrozen Frozen = True
    isFrozen _ = False

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
      pure $ evalError ctx
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
        (newCtx, res) <- commandLoad ctx [XObj (Str fileToLoad) Nothing Nothing]
        case res of
          ret@(Right _) -> pure (newCtx, ret)
          Left _ ->  commandLoad ctx [XObj (Str mainToLoad) Nothing Nothing]

-- | Load several files in order.
loadFiles :: Context -> [FilePath] -> IO Context
loadFiles = loadFilesExt commandLoad

loadFilesOnce :: Context -> [FilePath] -> IO Context
loadFilesOnce = loadFilesExt commandLoadOnce

loadFilesExt :: CommandCallback -> Context -> [FilePath] -> IO Context
loadFilesExt loadCmd ctxStart filesToLoad = foldM folder ctxStart filesToLoad
  where folder :: Context -> FilePath -> IO Context
        folder ctx file = do
         (newCtx, ret) <- loadCmd ctx [XObj (Str file) Nothing Nothing]
         let fppl = projectFilePathPrintLength (contextProj newCtx)
         case ret of
           Left err -> throw (EvalException err)
           Right _ -> pure newCtx

-- | Command for reloading all files in the project (= the files that has been loaded before).
commandReload :: CommandCallback
commandReload ctx args = do
  let paths = projectFiles (contextProj ctx)
      f :: Context -> (FilePath, ReloadMode) -> IO Context
      f context (_, Frozen) | not (projectForceReload (contextProj context)) = pure context
      f context (filepath, _) =
        do let proj = contextProj context
               alreadyLoaded = projectAlreadyLoaded proj
           if filepath `elem` alreadyLoaded
             then
                  pure context
             else do
                     contents <- slurp filepath
                     let proj' = proj { projectAlreadyLoaded = filepath : alreadyLoaded }
                     executeString False False (context { contextProj = proj' }) contents filepath
  newCtx <- liftIO (foldM f ctx paths)
  pure (newCtx, dynamicNil)

-- | Command for expanding a form and its macros.
commandExpand :: CommandCallback
commandExpand ctx [xobj] = macroExpand ctx xobj

-- | This function will show the resulting C code from an expression.
-- | i.e. (Int.+ 2 3) => "_0 = 2 + 3"
commandC :: CommandCallback
commandC ctx [xobj] = do
  let globalEnv = contextGlobalEnv ctx
      typeEnv = contextTypeEnv ctx
  (newCtx, result) <- expandAll evalDynamic ctx xobj
  case result of
    Left err -> pure (newCtx, Left err)
    Right expanded ->
      case annotate typeEnv globalEnv (setFullyQualifiedSymbols typeEnv globalEnv globalEnv expanded) Nothing of
        Left err -> pure $ evalError newCtx (show err) (info xobj)
        Right (annXObj, annDeps) ->
          do let cXObj = printC annXObj
                 cDeps = concatMap printC annDeps
                 c = cDeps ++ cXObj
             liftIO (putStr c)
             pure (newCtx, dynamicNil)

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
  XObj (Lst [ XObj (Defn Nothing) di Nothing
            , XObj (Sym (SymPath [] "main") Symbol) di Nothing
            , XObj (Arr []) di Nothing
            , XObj (Lst [ XObj Do di Nothing
                        , case ty xobj of
                            Just UnitTy -> xobj
                            Just (RefTy _ _) -> XObj (Lst [XObj (Sym (SymPath [] "println*") Symbol) di Nothing, xobj])
                                                di (Just UnitTy)
                            Just _ -> XObj (Lst [XObj (Sym (SymPath [] "println*") Symbol) di Nothing,
                                                 XObj (Lst [XObj Ref di Nothing, xobj])
                                                 di (Just UnitTy)])
                                      di (Just UnitTy)
                        , XObj (Num IntTy 0) di Nothing
                        ]) di Nothing]) di (Just (FuncTy [] UnitTy StaticLifetimeTy))
  where di = Just dummyInfo

primitiveDefdynamic :: Primitive
primitiveDefdynamic _ ctx [XObj (Sym (SymPath [] name) _) _ _, value] = do
  (newCtx, result) <- evalDynamic ctx value
  case result of
    Left err -> pure (newCtx, Left err)
    Right evaledBody ->
      dynamicOrMacroWith newCtx (\path -> [XObj DefDynamic Nothing Nothing, XObj (Sym path Symbol) Nothing Nothing, evaledBody]) DynamicTy name value
primitiveDefdynamic _ ctx [notName, body] =
  pure (evalError ctx ("`defndynamic` expected a name as first argument, but got " ++ pretty notName) (info notName))

specialCommandSet :: Context -> [XObj] -> IO (Context, Either EvalError XObj)
specialCommandSet ctx [x@(XObj (Sym path@(SymPath mod n) _) _ _), value] = do
  (newCtx, result) <- evalDynamic ctx value
  case result of
    Left err -> pure (newCtx, Left err)
    Right evald -> do
      let globalEnv = contextGlobalEnv ctx
      case contextInternalEnv ctx of
        Nothing -> setGlobal newCtx globalEnv evald
        Just env -> setInternal newCtx env evald
    where setGlobal ctx env value =
            case lookupInEnv path env of
              Just (e, binder) -> do
                (ctx', typedVal) <- typeCheckValueAgainstBinder ctx value binder
                pure $ either (failure ctx) (success ctx') typedVal
                where success c xo = (c{contextGlobalEnv = setStaticOrDynamicVar path env binder xo}, dynamicNil)
              Nothing -> pure (ctx, Right value)
          setInternal ctx env value =
            case lookupInEnv path env of
              Just (e, binder) -> do
                -- TODO: Type check local bindings.
                -- At the moment, let bindings are not structured the same as global defs or dynamic defs.
                -- This makes calls to the type check problematic, as we cannot work against a common binding form.
                -- Once we better support let bindings, type check them.
                (ctx', typedVal) <- typeCheckValueAgainstBinder ctx value binder
                pure $ if contextPath ctx == mod
                         then either (failure ctx) (success ctx') typedVal
                         else (ctx', dynamicNil)
                where success c xo = (c{contextInternalEnv = Just (setStaticOrDynamicVar (SymPath [] n) env binder xo)}, dynamicNil)
              -- If the def isn't found in the internal environment, check the global environment.
              Nothing -> setGlobal ctx (contextGlobalEnv ctx) value
specialCommandSet ctx [notName, body] =
  pure (evalError ctx ("`set!` expected a name as first argument, but got " ++ pretty notName) (info notName))
specialCommandSet ctx args =
  pure (evalError ctx ("`set!` takes a name and a value, but got `" ++ intercalate " " (map pretty args)) (if null args then Nothing else info (head args)))

-- | Convenience method for signifying failure in a given context.
failure :: Context -> EvalError -> (Context, Either EvalError a)
failure ctx err = (ctx, Left err)

-- | Given a context, value XObj and an existing binder, check whether or not
-- the given value has a type matching the binder's in the given context.
typeCheckValueAgainstBinder :: Context -> XObj -> Binder -> IO (Context, (Either EvalError XObj))
typeCheckValueAgainstBinder ctx val binder = do
  (ctx', typedValue) <- annotateWithinContext False ctx val
  pure $ case typedValue of
    Right (val', deps) -> go ctx' binderTy val'
    Left err -> (ctx', Left err)
  where path = (getPath (binderXObj binder))
        binderTy = ty (binderXObj binder)
        typeErr x = evalError ctx ("can't `set!` " ++ show path ++ " to a value of type " ++ show (fromJust (ty x)) ++ ", " ++ show path ++ " has type " ++ show (fromJust binderTy)) (info x)
        go ctx (Just DynamicTy) x = (ctx, Right x)
        go ctx t x@(XObj _ _ t') = if t == t' then (ctx, Right x) else typeErr x

-- | Sets a variable, checking whether or not it is static or dynamic, and
-- assigns an appropriate type to the variable.
-- Returns a new environment containing the assignment.
setStaticOrDynamicVar :: SymPath -> Env -> Binder -> XObj -> Env
setStaticOrDynamicVar path env binder value =
    case binder of
      (Binder meta (XObj (Lst (def@(XObj Def _ _) : sym : val)) i t)) ->
        envReplaceBinding path (Binder meta (XObj (Lst [def, sym, value]) (info value) t)) env
      (Binder meta (XObj (Lst (defdy@(XObj DefDynamic _ _) : sym : val)) i t)) ->
        envReplaceBinding path (Binder meta (XObj (Lst [defdy, sym, value]) (info value) (Just DynamicTy))) env
      (Binder meta (XObj (Lst (lett@(XObj LetDef _ _) : sym : val)) i t)) ->
        envReplaceBinding path (Binder meta (XObj (Lst [lett, sym, value]) (info value) t)) env
      -- shouldn't happen, errors are thrown at call sites.
      -- TODO: Return an either here to propagate error.
      _ -> env

primitiveEval :: Primitive
primitiveEval _ ctx [val] = do
  -- primitives don’t evaluate their arguments, so this needs to double-evaluate
  (newCtx, arg) <- evalDynamic ctx val
  case arg of
    Left err -> pure (newCtx, Left err)
    Right evald -> do
      (newCtx', expanded) <- macroExpand ctx evald
      case expanded of
        Left err -> pure (newCtx', Left err)
        Right ok -> do
          (finalCtx, res) <- evalDynamic newCtx' ok
          pure $ case res of
            Left (HasStaticCall x i) -> evalError ctx ("Unexpected static call in " ++ pretty x) i
            _ -> (finalCtx, res)

dynamicOrMacro :: Context -> Obj -> Ty -> String -> XObj -> XObj -> IO (Context, Either EvalError XObj)
dynamicOrMacro ctx pat ty name params body = do
  (ctx', exp) <- macroExpand ctx body
  case exp of
    Right expanded ->
      dynamicOrMacroWith ctx' (\path -> [XObj pat Nothing Nothing, XObj (Sym path Symbol) Nothing Nothing, params, expanded]) ty name body
    Left err -> pure (ctx, exp)

primitiveDefndynamic :: Primitive
primitiveDefndynamic _ ctx [XObj (Sym (SymPath [] name) _) _ _, params, body] =
  dynamicOrMacro ctx Dynamic DynamicTy name params body
primitiveDefndynamic _ ctx [notName, params, body] =
  argumentErr ctx "defndynamic" "a name" "first" notName

primitiveDefmacro :: Primitive
primitiveDefmacro _ ctx [XObj (Sym (SymPath [] name) _) _ _, params, body] =
  dynamicOrMacro ctx Macro MacroTy name params body
primitiveDefmacro _ ctx [notName, params, body] =
  argumentErr ctx "defmacro" "a name" "first" notName

primitiveAnd :: Primitive
primitiveAnd _ ctx [a, b] = do
 (newCtx, evaledA) <- evalDynamic ctx a
 case evaledA of
   Left e -> pure (ctx, Left e)
   Right (XObj (Bol ab) _ _) ->
     if ab
       then do
         (newCtx', evaledB) <- evalDynamic newCtx b
         pure $ case evaledB of
           Left e -> (newCtx, Left e)
           Right (XObj (Bol bb) _ _) ->
             (newCtx', Right (boolToXObj bb))
           Right b -> evalError ctx ("Can’t call `or` on " ++ pretty b) (info b)
       else pure (newCtx, Right falseXObj)
   Right a -> pure (evalError ctx ("Can’t call `or` on " ++ pretty a) (info a))

primitiveOr :: Primitive
primitiveOr _ ctx [a, b] = do
 (newCtx, evaledA) <- evalDynamic ctx a
 case evaledA of
   Left e -> pure (ctx, Left e)
   Right (XObj (Bol ab) _ _) ->
     if ab
       then pure (newCtx, Right trueXObj)
       else do
         (newCtx', evaledB) <- evalDynamic newCtx b
         pure $ case evaledB of
           Left e -> (newCtx, Left e)
           Right (XObj (Bol bb) _ _) ->
             (newCtx', Right (boolToXObj bb))
           Right b -> evalError ctx ("Can’t call `or` on " ++ pretty b) (info b)
   Right a -> pure (evalError ctx ("Can’t call `or` on " ++ pretty a) (info a))

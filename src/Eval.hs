{-# LANGUAGE LambdaCase #-}

module Eval where

import ColorText
import Commands
import Control.Applicative
import Control.Exception
import Control.Monad.State
import Data.Foldable (foldlM, foldrM)
import Data.List (foldl', intercalate, isSuffixOf)
import Data.List.Split (splitOn, splitWhen)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Emit
import Env
import Expand
import Infer
import Info
import Lookup
import qualified Map
import qualified Meta
import Obj
import Parsing
import Path
import Primitives
import Project
import Qualify
import System.Exit (ExitCode (..), exitSuccess, exitWith)
import System.Process (readProcessWithExitCode)
import qualified Text.Parsec as Parsec
import TypeError
import Types
import Util
import Prelude hiding (exp, mod)

data LookupPreference
  = PreferDynamic
  | PreferGlobal

-- Prefer dynamic bindings
evalDynamic :: Bool -> Context -> XObj -> IO (Context, Either EvalError XObj)
evalDynamic shouldResolve ctx xobj = eval ctx xobj PreferDynamic shouldResolve

-- Prefer global bindings
evalStatic :: Bool -> Context -> XObj -> IO (Context, Either EvalError XObj)
evalStatic shouldResolve ctx xobj = eval ctx xobj PreferGlobal shouldResolve

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
eval :: Context -> XObj -> LookupPreference -> Bool -> IO (Context, Either EvalError XObj)
eval ctx xobj@(XObj o info ty) preference shouldResolve =
  case o of
    Lst body -> eval' body
    Sym spath@(SymPath p n) _ ->
      pure $
        if shouldResolve
        then unwrapLookup (tryAllLookups >>= checkStatic)
        else unwrapLookup tryAllLookups
      where
        checkStatic (_, (Right (XObj (Lst ((XObj Def _ _) : _)) _ _))) =
          pure (ctx, Left (HasStaticCall xobj info))
        checkStatic (_, (Right (XObj (Lst ((XObj (Defn _) _ _) : _)) _ _))) =
          pure (ctx, Left (HasStaticCall xobj info))
        checkStatic (_, (Right (XObj (Lst ((XObj (External _) _ _) : _)) _ _))) =
          pure (ctx, Left (HasStaticCall xobj info))
        checkStatic v = pure v
        unwrapLookup v =
          fromMaybe
            (evalError ctx ("Can't find symbol '" ++ show n ++ "'") info) -- all else failed, error.
            v
        tryAllLookups =
          ( case preference of
                PreferDynamic -> tryDynamicLookup
                PreferGlobal -> (tryLookup spath <|> tryDynamicLookup)
          )
            <|> (if null p then tryInternalLookup spath else tryLookup spath)
        tryDynamicLookup =
          ( lookupBinder (SymPath ("Dynamic" : p) n) (contextGlobalEnv ctx)
              >>= \(Binder _ found) -> pure (ctx, Right (resolveDef found))
          )
        tryInternalLookup path =
          ( contextInternalEnv ctx
              >>= lookupBinder path
              >>= \(Binder _ found) -> pure (ctx, Right (resolveDef found))
          )
            <|> tryLookup path -- fallback
        tryLookup path =
          ( lookupBinder path (contextGlobalEnv ctx)
              >>= \(Binder meta found) -> checkPrivate meta found
          )
            <|> ( lookupBinder path (getTypeEnv (contextTypeEnv ctx))
                    >>= \(Binder _ found) -> pure (ctx, Right (resolveDef found))
                )
        checkPrivate meta found =
          pure $
            if metaIsTrue meta "private"
              then evalError ctx ("The binding: " ++ show (getPath found) ++ " is private; it may only be used within the module that defines it.") info
              else (ctx, Right (resolveDef found))
    Arr objs -> do
      (newCtx, evaled) <- foldlM successiveEval (ctx, Right []) objs
      pure
        ( newCtx,
          do
            ok <- evaled
            Right (XObj (Arr ok) info ty)
        )
    StaticArr objs -> do
      (newCtx, evaled) <- foldlM successiveEval (ctx, Right []) objs
      pure
        ( newCtx,
          do
            ok <- evaled
            Right (XObj (StaticArr ok) info ty)
        )
    _ -> do
      (nctx, res) <- annotateWithinContext False ctx xobj
      pure $ case res of
        Left e -> (nctx, Left e)
        Right (val, _) -> (nctx, Right val)
  where
    resolveDef (XObj (Lst [XObj DefDynamic _ _, _, value]) _ _) = value
    resolveDef (XObj (Lst [XObj LetDef _ _, _, value]) _ _) = value
    resolveDef x = x
    eval' form =
      case form of
        [XObj If _ _, mcond, mtrue, mfalse] -> do
          (newCtx, evd) <- eval ctx mcond preference False
          case evd of
            Right cond ->
              case xobjObj cond of
                Bol b -> eval newCtx (if b then mtrue else mfalse) preference False
                _ ->
                  pure
                    ( evalError
                        ctx
                        ( "This `if` condition contains the non-boolean value `"
                            ++ pretty cond
                            ++ "`"
                        )
                        (xobjInfo cond)
                    )
            Left e -> pure (newCtx, Left e)
        XObj If _ _ : _ ->
          pure
            ( evalError
                ctx
                ( "I didn’t understand this `if`.\n\n Got:\n```\n" ++ pretty xobj
                    ++ "\n```\n\nExpected the form:\n```\n(if cond then else)\n```\n"
                )
                (xobjInfo xobj)
            )
        [XObj (Defn _) _ _, name, args@(XObj (Arr a) _ _), _] ->
          case xobjObj name of
            (Sym (SymPath [] _) _) ->
              if all isUnqualifiedSym a
                then specialCommandDefine ctx xobj
                else
                  pure
                    ( evalError
                        ctx
                        ( "`defn` requires all arguments to be unqualified symbols, but it got `"
                            ++ pretty args
                            ++ "`"
                        )
                        (xobjInfo xobj)
                    )
            _ ->
              pure
                ( evalError
                    ctx
                    ( "`defn` identifiers must be unqualified symbols, but it got `"
                        ++ pretty name
                        ++ "`"
                    )
                    (xobjInfo xobj)
                )
        [XObj (Defn _) _ _, _, invalidArgs, _] ->
          pure
            ( evalError
                ctx
                ( "`defn` requires an array of symbols as argument list, but it got `"
                    ++ pretty invalidArgs
                    ++ "`"
                )
                (xobjInfo xobj)
            )
        (defn@(XObj (Defn _) _ _) : _) ->
          pure
            ( evalError
                ctx
                ( "I didn’t understand the `defn` at " ++ prettyInfoFromXObj xobj
                    ++ ":\n\n"
                    ++ pretty xobj
                    ++ "\n\nIs it valid? Every `defn` needs to follow the form `(defn name [arg] body)`."
                )
                (xobjInfo defn)
            )
        [(XObj Def _ _), name, _] ->
          if isUnqualifiedSym name
            then specialCommandDefine ctx xobj
            else
              pure
                ( evalError
                    ctx
                    ( "`def` identifiers must be unqualified symbols, but it got `"
                        ++ pretty name
                        ++ "`"
                    )
                    (xobjInfo xobj)
                )
        [the@(XObj The _ _), t, value] ->
          do
            (newCtx, evaledValue) <- expandAll (evalDynamic False) ctx value -- TODO: Why expand all here?
            pure
              ( newCtx,
                do
                  okValue <- evaledValue
                  Right (XObj (Lst [the, t, okValue]) info ty)
              )
        (XObj The _ _ : _) ->
          pure
            ( evalError
                ctx
                ( "I didn’t understand the `the` at " ++ prettyInfoFromXObj xobj
                    ++ ":\n\n"
                    ++ pretty xobj
                    ++ "\n\nIs it valid? Every `the` needs to follow the form `(the type expression)`."
                )
                (xobjInfo xobj)
            )
        [XObj Let _ _, XObj (Arr bindings) _ _, body]
          | odd (length bindings) ->
            pure
              ( evalError
                  ctx
                  ("Uneven number of forms in `let`: " ++ pretty xobj)
                  (xobjInfo xobj) -- Unreachable?
              )
          | not (all isSym (evenIndices bindings)) ->
            pure
              ( evalError
                  ctx
                  ( "`let` identifiers must be symbols, but it got `"
                      ++ joinWithSpace (map pretty bindings)
                      ++ "`"
                  )
                  (xobjInfo xobj)
              )
          | otherwise ->
            do
              let binds = unwrapVar (pairwise bindings) []
                  ni = Env Map.empty (contextInternalEnv ctx) Nothing [] InternalEnv 0
              eitherCtx <- foldrM successiveEval' (Right ctx {contextInternalEnv = Just ni}) binds
              case eitherCtx of
                Left err -> pure (ctx, Left err)
                Right newCtx -> do
                  (finalCtx, evaledBody) <- eval newCtx body preference False
                  let Just e = contextInternalEnv finalCtx
                  pure
                    ( finalCtx {contextInternalEnv = envParent e},
                      do
                        okBody <- evaledBody
                        Right okBody
                    )
          where
            unwrapVar [] acc = acc
            unwrapVar ((XObj (Sym (SymPath [] x) _) _ _, y) : xs) acc = unwrapVar xs ((x, y) : acc)
            unwrapVar _ _ = error "unwrapvar"
            successiveEval' (n, x) =
              \case
                err@(Left _) -> pure err
                Right ctx' -> do
                  (newCtx, res) <- eval ctx' x preference False
                  case res of
                    Right okX -> do
                      let binder = Binder emptyMeta (XObj (Lst [(XObj LetDef Nothing Nothing), XObj (Sym (SymPath [] n) Symbol) Nothing Nothing, okX]) Nothing (xobjTy okX))
                          Just e = contextInternalEnv newCtx
                      pure $ Right (newCtx {contextInternalEnv = Just (envInsertAt e (SymPath [] n) binder)})
                    Left err -> pure $ Left err
        l@[XObj Fn {} _ _, args@(XObj (Arr a) _ _), _] ->
          pure $
            if all isUnqualifiedSym a
              then (ctx, Right (XObj (Closure (XObj (Lst l) info ty) (CCtx ctx)) info ty))
              else evalError ctx ("`fn` requires all arguments to be unqualified symbols, but it got `" ++ pretty args ++ "`") (xobjInfo args)
        XObj (Closure (XObj (Lst [XObj (Fn _ _) _ _, XObj (Arr params) _ _, body]) _ _) (CCtx c)) _ _ : args ->
          case checkArity "<closure>" params args of
            Left err -> pure (evalError ctx err (xobjInfo xobj))
            Right () ->
              do
                (newCtx, evaledArgs) <- foldlM successiveEval (ctx, Right []) args
                case evaledArgs of
                  Right okArgs -> do
                    (_, res) <- apply c body params okArgs
                    pure (newCtx, res)
                  Left err -> pure (newCtx, Left err)
        XObj (Lst [XObj Dynamic _ _, sym, XObj (Arr params) _ _, body]) i _ : args ->
          case checkArity (getName sym) params args of
            Left err ->
              pure (evalError ctx err i)
            Right () ->
              do
                (newCtx, evaledArgs) <- foldlM successiveEval (ctx, Right []) args
                case evaledArgs of
                  Right okArgs -> apply newCtx body params okArgs
                  Left err -> pure (newCtx, Left err)
        XObj (Lst [XObj Macro _ _, sym, XObj (Arr params) _ _, body]) i _ : args ->
          case checkArity (getName sym) params args of
            Left err -> pure (evalError ctx err i)
            Right () -> do
              -- Replace info so that the macro which is called gets the source location info of the expansion site.
              --let replacedBody = replaceSourceInfoOnXObj (info xobj) body
              (ctx', res) <- apply ctx body params args
              case res of
                Right xobj' -> macroExpand ctx' xobj'
                Left _ -> pure (ctx, res)
        [XObj (Lst [XObj (Command (NullaryCommandFunction nullary)) _ _, _, _]) _ _] ->
          do
            (_, evaledArgs) <- foldlM successiveEval (ctx, Right []) []
            case evaledArgs of
              Right [] -> nullary ctx
              Right _ -> error "eval nullary"
              Left err -> pure (ctx, Left err)
        [XObj (Lst [XObj (Command (UnaryCommandFunction unary)) _ _, _, _]) _ _, x] ->
          do
            (_, evaledArgs) <- foldlM successiveEval (ctx, Right []) [x]
            case evaledArgs of
              Right [x'] -> unary ctx x'
              Right _ -> error "eval unary"
              Left err -> pure (ctx, Left err)
        [XObj (Lst [XObj (Command (BinaryCommandFunction binary)) _ _, _, _]) _ _, x, y] ->
          do
            (_, evaledArgs) <- foldlM successiveEval (ctx, Right []) [x, y]
            case evaledArgs of
              Right [x', y'] -> binary ctx x' y'
              Right _ -> error "eval binary"
              Left err -> pure (ctx, Left err)
        [XObj (Lst [XObj (Command (TernaryCommandFunction ternary)) _ _, _, _]) _ _, x, y, z] ->
          do
            (_, evaledArgs) <- foldlM successiveEval (ctx, Right []) [x, y, z]
            case evaledArgs of
              Right [x', y', z'] -> ternary ctx x' y' z'
              Right _ -> error "eval ternary"
              Left err -> pure (ctx, Left err)
        XObj (Lst [XObj (Command (VariadicCommandFunction variadic)) _ _, _, _]) _ _ : args ->
          do
            (_, evaledArgs) <- foldlM successiveEval (ctx, Right []) args
            case evaledArgs of
              Right xs -> variadic ctx xs
              Left err -> pure (ctx, Left err)
        XObj (Lst [XObj (Command _) _ _, sym, XObj (Arr params) _ _]) i _ : args ->
          badArity (getName sym) params args i
        x@(XObj (Lst [XObj (Primitive prim) _ _, _, _]) _ _) : args -> getPrimitive prim x ctx args
        XObj (Lst (XObj (Defn _) _ _ : _)) _ _ : _ -> pure (ctx, Left (HasStaticCall xobj info))
        XObj (Lst (XObj (Interface _ _) _ _ : _)) _ _ : _ -> pure (ctx, Left (HasStaticCall xobj info))
        XObj (Lst (XObj (Instantiate _) _ _ : _)) _ _ : _ -> pure (ctx, Left (HasStaticCall xobj info))
        XObj (Lst (XObj (Deftemplate _) _ _ : _)) _ _ : _ -> pure (ctx, Left (HasStaticCall xobj info))
        XObj (Lst (XObj (External _) _ _ : _)) _ _ : _ -> pure (ctx, Left (HasStaticCall xobj info))
        XObj (Match _) _ _ : _ -> pure (ctx, Left (HasStaticCall xobj info))
        [XObj Ref _ _, _] -> pure (ctx, Left (HasStaticCall xobj info))
        l@(XObj (Lst _) i t) : args -> do
          (newCtx, f) <- eval ctx l preference False
          case f of
            Right fun -> do
              (newCtx', res) <- eval (pushFrame newCtx xobj) (XObj (Lst (fun : args)) i t) preference False
              pure (popFrame newCtx', res)
            x -> pure (newCtx, x)
        x@(XObj (Sym _ _) i _) : args -> do
          (newCtx, f) <- eval ctx x preference False
          case f of
            Right fun -> do
              (newCtx', res) <- eval (pushFrame ctx xobj) (XObj (Lst (fun : args)) i ty) preference False
              pure (popFrame newCtx', res)
            Left err -> pure (newCtx, Left err)
        XObj With _ _ : xobj'@(XObj (Sym path _) _ _) : forms ->
          specialCommandWith ctx xobj' path forms
        XObj With _ _ : _ ->
          pure (evalError ctx ("Invalid arguments to `with`: " ++ pretty xobj) (xobjInfo xobj))
        XObj SetBang _ _ : args -> specialCommandSet ctx args
        [XObj Do _ _] ->
          pure (evalError ctx "No forms in do" (xobjInfo xobj))
        XObj Do _ _ : rest -> foldlM successiveEval' (ctx, dynamicNil) rest
          where
            successiveEval' (ctx', acc) x =
              case acc of
                err@(Left _) -> pure (ctx', err)
                Right _ -> eval ctx' x preference False
        [XObj While _ _, cond, body] ->
          specialCommandWhile ctx cond body
        [XObj Address _ _, value] ->
          specialCommandAddress ctx value
        [] -> pure (ctx, dynamicNil)
        _ -> pure (evalError ctx ("I did not understand the form `" ++ pretty xobj ++ "`") (xobjInfo xobj))
    badArity name params args i = case checkArity name params args of
      Left err -> pure (evalError ctx err i)
      Right () -> error "badarity"
    checkArity name params args =
      let la = length args
          withRest = any ((":rest" ==) . getName) params
          lp = length params - (if withRest then 2 else 0)
       in if lp == la || (withRest && la >= lp)
            then Right ()
            else
              if la < lp
                then
                  Left
                    ( name
                        ++ " expected "
                        ++ show lp
                        ++ " arguments but received only "
                        ++ show la
                        ++ ".\n\nYou’ll have to provide "
                        ++ intercalate ", " (map pretty (drop la params))
                        ++ " as well."
                    )
                else
                  Left
                    ( name
                        ++ " expected "
                        ++ show lp
                        ++ " arguments, but received "
                        ++ show la
                        ++ ".\n\nThe arguments "
                        ++ intercalate ", " (map pretty (drop lp args))
                        ++ " are not needed."
                    )
    successiveEval (ctx', acc) x =
      case acc of
        Left _ -> pure (ctx', acc)
        Right l -> do
          (newCtx, evald) <- eval ctx' x preference False
          pure $ case evald of
            Right res -> (newCtx, Right (l ++ [res]))
            Left err -> (newCtx, Left err)

macroExpand :: Context -> XObj -> IO (Context, Either EvalError XObj)
macroExpand ctx xobj =
  case xobj of
    XObj (Arr objs) i t -> do
      (newCtx, expanded) <- foldlM successiveExpand (ctx, Right []) objs
      pure
        ( newCtx,
          do
            ok <- expanded
            Right (XObj (Arr ok) i t)
        )
    XObj (StaticArr objs) i t -> do
      (newCtx, expanded) <- foldlM successiveExpand (ctx, Right []) objs
      pure
        ( newCtx,
          do
            ok <- expanded
            Right (XObj (StaticArr ok) i t)
        )
    XObj (Lst [XObj (Lst (XObj Macro _ _ : _)) _ _]) _ _ -> evalDynamic False ctx xobj
    XObj (Lst (x@(XObj (Sym _ _) _ _) : args)) i t -> do
      (next, f) <- evalDynamic False ctx x
      case f of
        Right m@(XObj (Lst (XObj Macro _ _ : _)) _ _) -> do
          (newCtx', res) <- evalDynamic False ctx (XObj (Lst (m : args)) i t)
          pure (newCtx', res)
        -- TODO: Determine a way to eval primitives generally and remove this special case.
        Right p@(XObj (Lst [(XObj (Primitive prim) _ _), (XObj (Sym (SymPath _ "defmodule") _) _ _), _]) _ _) ->
          getPrimitive prim p next args
        _ -> do
          (newCtx, expanded) <- foldlM successiveExpand (ctx, Right []) args
          pure
            ( newCtx,
              do
                ok <- expanded
                Right (XObj (Lst (x : ok)) i t)
            )
    XObj (Lst objs) i t -> do
      (newCtx, expanded) <- foldlM successiveExpand (ctx, Right []) objs
      pure
        ( newCtx,
          do
            ok <- expanded
            Right (XObj (Lst ok) i t)
        )
    _ -> pure (ctx, Right xobj)
  where
    successiveExpand (ctx', acc) x =
      case acc of
        Left _ -> pure (ctx', acc)
        Right l -> do
          (newCtx, expanded) <- macroExpand ctx' x
          pure $ case expanded of
            Right res -> (newCtx, Right (l ++ [res]))
            Left err -> (newCtx, Left err)

apply :: Context -> XObj -> [XObj] -> [XObj] -> IO (Context, Either EvalError XObj)
apply ctx@Context {contextInternalEnv = internal} body params args =
  let env = contextEnv ctx
      allParams = map getName params
   in case splitWhen (":rest" ==) allParams of
        [a, b] -> callWith env a b
        [a] -> callWith env a []
        _ ->
          pure
            ( evalError
                ctx
                ( "I didn’t understand this macro’s argument split, got `"
                    ++ joinWith "," allParams
                    ++ "`, but expected exactly one `:rest` separator."
                )
                Nothing
            )
  where
    callWith _ proper rest = do
      let n = length proper
          insideEnv = Env Map.empty internal Nothing [] InternalEnv 0
          insideEnv' =
            foldl'
              (\e (p, x) -> extendEnv e p x)
              insideEnv
              (zip proper (take n args))
          insideEnv'' =
            if null rest
              then insideEnv'
              else
                extendEnv
                  insideEnv'
                  (head rest)
                  (XObj (Lst (drop n args)) Nothing Nothing)
      (c, r) <- evalDynamic False (ctx {contextInternalEnv = Just insideEnv''}) body
      pure (c {contextInternalEnv = internal}, r)

-- | Parses a string and then converts the resulting forms to commands, which are evaluated in order.
executeString :: Bool -> Bool -> Context -> String -> String -> IO Context
executeString doCatch printResult ctx input fileName =
  if doCatch then catch exec (catcher ctx) else exec
  where
    exec = case parse input fileName of
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
          (putStrLnWithColor Yellow ("=> " ++ pretty res))
        pure ctx'
    interactiveFolder (_, context) xobj =
      executeCommand context xobj
    treatErr ctx' e xobj = do
      let fppl = projectFilePathPrintLength (contextProj ctx')
      case contextExecMode ctx' of
        Check -> putStrLn (machineReadableInfoFromXObj fppl xobj ++ " " ++ e)
        _ -> emitErrorWithLabel "PARSE ERROR" e
      throw CancelEvaluationException

-- | Used by functions that has a series of forms to evaluate and need to fold over them (producing a new Context in the end)
folder :: Context -> XObj -> IO Context
folder context xobj = do
  (_, ctx) <- executeCommand context xobj
  pure ctx

-- | Take a repl command and execute it.
executeCommand :: Context -> XObj -> IO (XObj, Context)
executeCommand ctx@(Context env _ _ _ _ _ _ _) xobj =
  do
    when (isJust (envModuleName env)) $
      error ("Global env module name is " ++ fromJust (envModuleName env) ++ " (should be Nothing).")
    -- The s-expression command is a special case that prefers global/static bindings over dynamic bindings
    -- when given a naked binding (no path) as an argument; (s-expr inc)
    (newCtx, result) <- if (xobjIsSexp xobj) then evalStatic True ctx xobj else evalDynamic True ctx xobj
    case result of
      Left e@(EvalError _ _ _ _) -> do
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
      (nc, r) <- annotateWithinContext False newCtx xobj'
      case r of
        Right (ann, deps) -> do
          ctxWithDeps <- liftIO $ foldM (define True) nc deps
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
                (Lst [XObj (Sym (SymPath [] "build") Symbol) (Just dummyInfo) Nothing])
                (Just dummyInfo)
                Nothing,
              XObj
                (Lst [XObj (Sym (SymPath [] "run") Symbol) (Just dummyInfo) Nothing])
                (Just dummyInfo)
                Nothing
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
  let env = contextEnv ctx
      useThese = envUseModules env
      env' = if path `elem` useThese then env else env {envUseModules = path : useThese}
      ctx' = ctx {contextGlobalEnv = env'}
  ctxAfter <- liftIO $ foldM folder ctx' forms
  let envAfter = contextEnv ctxAfter
      ctxAfter' = ctx {contextGlobalEnv = envAfter {envUseModules = useThese}} -- This will undo ALL use:s made inside the 'with'.
  pure (ctxAfter', dynamicNil)

specialCommandDefine :: Context -> XObj -> IO (Context, Either EvalError XObj)
specialCommandDefine ctx xobj =
  do
    (newCtx, result) <- annotateWithinContext True ctx xobj
    case result of
      Right (annXObj, annDeps) ->
        do
          ctxWithDeps <- liftIO $ foldM (define True) newCtx annDeps
          ctxWithDef <- liftIO $ define False ctxWithDeps annXObj
          pure (ctxWithDef, dynamicNil)
      Left err ->
        pure (ctx, Left err)

specialCommandAddress :: Context -> XObj -> IO (Context, Either EvalError XObj)
specialCommandAddress ctx xobj =
  case xobj of
    XObj (Sym _ _) _ _ ->
      do
        (newCtx, result) <- annotateWithinContext False ctx xobj
        case result of
          Right (annXObj, _) -> return (newCtx, Right annXObj)
          Left err ->
            return (ctx, Left err)
    _ -> return (evalError ctx ("Can't get the address of non-symbol " ++ pretty xobj) (xobjInfo xobj))

specialCommandWhile :: Context -> XObj -> XObj -> IO (Context, Either EvalError XObj)
specialCommandWhile ctx cond body = do
  (newCtx, evd) <- evalDynamic False ctx cond
  case evd of
    Right c ->
      case xobjObj c of
        Bol b ->
          if b
            then do
              (newCtx', _) <- evalDynamic False newCtx body
              specialCommandWhile newCtx' cond body
            else pure (newCtx, dynamicNil)
        _ ->
          pure
            ( evalError
                ctx
                ( "This `while` condition contains the non-boolean value '"
                    ++ pretty c
                    ++ "`"
                )
                (xobjInfo c)
            )
    Left e -> pure (newCtx, Left e)

getSigFromDefnOrDef :: Context -> Env -> FilePathPrintLength -> XObj -> Either EvalError (Maybe (Ty, XObj))
getSigFromDefnOrDef ctx globalEnv fppl xobj =
  let pathStrings = contextPath ctx
      path = getPath xobj
      fullPath = case path of
        (SymPath [] _) -> consPath pathStrings path
        (SymPath _ _) -> path
      metaData = lookupMeta fullPath globalEnv
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
      (_, expansionResult) <- expandAll (evalDynamic False) ctx xobj
      case expansionResult of
        Left err -> pure (evalError ctx (show err) Nothing)
        Right expanded ->
          let xobjFullPath = if qualifyDefn then setFullyQualifiedDefn expanded (SymPath pathStrings (getName xobj)) else expanded
              xobjFullSymbols = setFullyQualifiedSymbols typeEnv globalEnv innerEnv xobjFullPath
           in case annotate typeEnv globalEnv xobjFullSymbols okSig of
                Left err ->
                  case contextExecMode ctx of
                    Check ->
                      pure (evalError ctx (joinLines (machineReadableErrorStrings fppl err)) Nothing)
                    _ ->
                      pure (evalError ctx (show err) (xobjInfo xobj))
                Right ok -> pure (ctx, Right ok)

primitiveDefmodule :: Primitive
primitiveDefmodule xobj ctx@(Context env i _ pathStrings _ _ _ _) (XObj (Sym (SymPath [] moduleName) _) _ _ : innerExpressions) =
  do
    -- N.B. The `envParent` rewrite at the end of this line is important!
    -- lookups delve into parent envs by default, which is normally what we want, but in this case it leads to problems
    -- when submodules happen to share a name with an existing module or type at the global level.
    maybe (defineNewModule emptyMeta) updateExistingModule (lookupBinder (SymPath [] moduleName) ((getEnv env pathStrings) {envParent = Nothing}))
    >>= defineModuleBindings
    >>= \(newCtx, result) ->
      case result of
        Left err -> pure (newCtx, Left err)
        Right _ -> pure (popModulePath (newCtx {contextInternalEnv = (join (fmap envParent (contextInternalEnv newCtx)))}), dynamicNil)
  where
    updateExistingModule :: Binder -> IO (Context, Either EvalError XObj)
    updateExistingModule (Binder _ (XObj (Mod innerEnv) _ _)) =
      let ctx' =
            ctx
              { contextInternalEnv = Just innerEnv {envParent = i},
                contextPath = ((contextPath ctx) ++ [moduleName])
              }
       in (pure (ctx', dynamicNil))
    updateExistingModule (Binder meta (XObj (Lst [XObj MetaStub _ _, _]) _ _)) =
      defineNewModule meta
    updateExistingModule _ =
      pure (evalError ctx ("Can't redefine '" ++ moduleName ++ "' as module") (xobjInfo xobj))

    defineNewModule :: MetaData -> IO (Context, Either EvalError XObj)
    defineNewModule meta =
      pure (ctx', dynamicNil)
      where
        moduleEnv = Env (Map.fromList []) (Just (getEnv env pathStrings)) (Just moduleName) [] ExternalEnv 0
        newModule = XObj (Mod moduleEnv) (xobjInfo xobj) (Just ModuleTy)
        updatedGlobalEnv = envInsertAt env (SymPath pathStrings moduleName) (Binder meta newModule)
        -- The parent of the internal env needs to be set to i here for contextual `use` calls to work.
        -- In theory this shouldn't be necessary; but for now it is.
        ctx' = ctx {contextGlobalEnv = updatedGlobalEnv, contextInternalEnv = Just moduleEnv {envParent = i}, contextPath = ((contextPath ctx) ++ [moduleName])}

    defineModuleBindings :: (Context, Either EvalError XObj) -> IO (Context, Either EvalError XObj)
    defineModuleBindings (context, Left e) = pure (context, Left e)
    defineModuleBindings (context, _) =
      foldM step (context, dynamicNil) innerExpressions

    step :: (Context, Either EvalError XObj) -> XObj -> IO (Context, Either EvalError XObj)
    step (ctx', Left e) _ = pure (ctx', Left e)
    step (ctx', Right _) expressions =
      (macroExpand ctx' expressions)
        >>= \(ctx'', res) -> case res of
          Left _ -> pure (ctx'', res)
          Right r -> evalDynamic False ctx'' r
primitiveDefmodule _ ctx (x : _) =
  pure (evalError ctx ("`defmodule` expects a symbol, got '" ++ pretty x ++ "' instead.") (xobjInfo x))
primitiveDefmodule _ ctx [] =
  pure (evalError ctx "`defmodule` requires at least a symbol, received none." (Just dummyInfo))

-- | "NORMAL" COMMANDS (just like the ones in Command.hs, but these need access to 'eval', etc.)

-- | Command for loading a Carp file.
commandLoad :: VariadicCommandCallback
commandLoad ctx [xobj@(XObj (Str path) i _), XObj (Str toLoad) _ _] =
  loadInternal ctx xobj path i (Just toLoad) DoesReload
commandLoad ctx [XObj (Str _) _ _, x] =
  pure $ evalError ctx ("Invalid args to `load`: " ++ pretty x) (xobjInfo x)
commandLoad ctx [x, _] =
  pure $ evalError ctx ("Invalid args to `load`: " ++ pretty x) (xobjInfo x)
commandLoad ctx [xobj@(XObj (Str path) i _)] =
  loadInternal ctx xobj path i Nothing DoesReload
commandLoad ctx [x] =
  pure $ evalError ctx ("Invalid args to `load`: " ++ pretty x) (xobjInfo x)
commandLoad ctx _ =
  pure $ evalError ctx "Invalid args to `load`, expected (load str optional:fileFromRepo)" Nothing

commandLoadOnce :: VariadicCommandCallback
commandLoadOnce ctx [xobj@(XObj (Str path) i _), XObj (Str toLoad) _ _] =
  loadInternal ctx xobj path i (Just toLoad) Frozen
commandLoadOnce ctx [XObj (Str _) _ _, x] =
  pure $ evalError ctx ("Invalid args to `load-once`: " ++ pretty x) (xobjInfo x)
commandLoadOnce ctx [x, _] =
  pure $ evalError ctx ("Invalid args to `load-once`: " ++ pretty x) (xobjInfo x)
commandLoadOnce ctx [xobj@(XObj (Str path) i _)] =
  loadInternal ctx xobj path i Nothing Frozen
commandLoadOnce ctx [x] =
  pure $ evalError ctx ("Invalid args to `load-once`: " ++ pretty x) (xobjInfo x)
commandLoadOnce ctx _ =
  pure $ evalError ctx "Invalid args to `load-once`, expected `(load-once str optional:fileFromRepo)`" Nothing

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
        fileThatLoads <-
          liftIO
            ( canonicalizePath
                ( case i of
                    Just ii -> infoFile ii
                    Nothing -> ""
                )
            )
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
                      if canonicalPath `elem` (map fst files)
                        then files
                        else files ++ [(canonicalPath, reloadMode)]
                    prevStack = projectLoadStack proj
                    proj' =
                      proj
                        { projectFiles = files',
                          projectAlreadyLoaded = canonicalPath : alreadyLoaded,
                          projectLoadStack = canonicalPath : prevStack
                        }
                newCtx <- liftIO $ executeString True False (ctx {contextProj = proj'}) contents canonicalPath
                pure (newCtx {contextProj = (contextProj newCtx) {projectLoadStack = prevStack}}, dynamicNil)
  where
    frozenPaths proj =
      if projectForceReload proj
        then [] -- No paths are Frozen when the "force reload" project setting is true.
        else map fst $ filter (isFrozen . snd) (projectFiles proj)
    isFrozen Frozen = True
    isFrozen _ = False
    fppl ctx' =
      projectFilePathPrintLength (contextProj ctx')
    invalidPath ctx' path' =
      evalError
        ctx'
        ( ( case contextExecMode ctx' of
              Check ->
                machineReadableInfoFromXObj (fppl ctx') xobj ++ " I can't find a file named: '" ++ path' ++ "'"
              _ -> "I can't find a file named: '" ++ path' ++ "'"
          )
            ++ "\n\nIf you tried loading an external package, try appending a version string (like `@master`)"
        )
        (xobjInfo xobj)
    invalidPathWith ctx' path' stderr cleanup cleanupPath = do
      _ <- liftIO $ when cleanup (removeDirectoryRecursive cleanupPath)
      pure $
        evalError
          ctx'
          ( ( case contextExecMode ctx' of
                Check ->
                  machineReadableInfoFromXObj (fppl ctx') xobj ++ " I can't find a file named: '" ++ path' ++ "'"
                _ -> "I can't find a file named: '" ++ path' ++ "'"
            )
              ++ "\n\nI tried interpreting the statement as a git import, but got: "
              ++ stderr
          )
          (xobjInfo xobj)
    replaceC _ _ [] = []
    replaceC c s (a : b) = if a == c then s ++ replaceC c s b else a : replaceC c s b
    cantLoadSelf ctx' path' =
      case contextExecMode ctx' of
        Check ->
          evalError ctx' (machineReadableInfoFromXObj (fppl ctx') xobj ++ " A file can't load itself: '" ++ path' ++ "'") (xobjInfo xobj)
        _ ->
          evalError ctx' ("A file can't load itself: '" ++ path' ++ "'") (xobjInfo xobj)
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
loadFilesExt loadCmd ctxStart filesToLoad = foldM load ctxStart filesToLoad
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
              executeString False False (context {contextProj = proj'}) contents filepath
  newCtx <- liftIO (foldM f ctx paths)
  pure (newCtx, dynamicNil)

-- | Command for expanding a form and its macros.
commandExpand :: UnaryCommandCallback
commandExpand ctx xobj = macroExpand ctx xobj

-- | This function will show the resulting C code from an expression.
-- | i.e. (Int.+ 2 3) => "_0 = 2 + 3"
commandC :: UnaryCommandCallback
commandC ctx xobj = do
  let globalEnv = contextGlobalEnv ctx
      typeEnv = contextTypeEnv ctx
  (newCtx, result) <- expandAll (evalDynamic False) ctx xobj
  case result of
    Left err -> pure (newCtx, Left err)
    Right expanded ->
      case annotate typeEnv globalEnv (setFullyQualifiedSymbols typeEnv globalEnv globalEnv expanded) Nothing of
        Left err -> pure $ evalError newCtx (show err) (xobjInfo xobj)
        Right (annXObj, annDeps) ->
          do
            let cXObj = printC annXObj
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

primitiveDefdynamic :: Primitive
primitiveDefdynamic _ ctx [XObj (Sym (SymPath [] name) _) _ _, value] = do
  (newCtx, result) <- evalDynamic False ctx value
  case result of
    Left err -> pure (newCtx, Left err)
    Right evaledBody ->
      dynamicOrMacroWith newCtx (\path -> [XObj DefDynamic Nothing Nothing, XObj (Sym path Symbol) Nothing Nothing, evaledBody]) DynamicTy name value
primitiveDefdynamic _ ctx [notName, _] =
  pure (evalError ctx ("`defndynamic` expected a name as first argument, but got " ++ pretty notName) (xobjInfo notName))
primitiveDefdynamic _ _ _ = error "primitivedefdynamic"

specialCommandSet :: Context -> [XObj] -> IO (Context, Either EvalError XObj)
specialCommandSet ctx [(XObj (Sym path@(SymPath mod n) _) _ _), val] = do
  (newCtx, result) <- evalDynamic False ctx val
  case result of
    Left err -> pure (newCtx, Left err)
    Right evald -> do
      let globalEnv = contextGlobalEnv ctx
      case contextInternalEnv ctx of
        Nothing -> setGlobal newCtx globalEnv evald
        Just env -> setInternal newCtx env evald
  where
    setGlobal ctx' env value =
      case lookupBinder path env of
        Just binder -> do
          (ctx'', typedVal) <- typeCheckValueAgainstBinder ctx' value binder
          pure $ either (failure ctx'') (success ctx'') typedVal
          where
            success c xo = (c {contextGlobalEnv = setStaticOrDynamicVar path env binder xo}, dynamicNil)
        Nothing -> pure (ctx, Right value)
    setInternal ctx' env value =
      case lookupInEnv path env of
        Just (_, binder) -> do
          -- TODO: Type check local bindings.
          -- At the moment, let bindings are not structured the same as global defs or dynamic defs.
          -- This makes calls to the type check problematic, as we cannot work against a common binding form.
          -- Once we better support let bindings, type check them.
          (ctx'', typedVal) <- typeCheckValueAgainstBinder ctx' value binder
          pure $
            if contextPath ctx'' == mod
              then either (failure ctx'') (success ctx'') typedVal
              else (ctx'', dynamicNil)
          where
            success c xo = (c {contextInternalEnv = Just (setStaticOrDynamicVar (SymPath [] n) env binder xo)}, dynamicNil)
        -- If the def isn't found in the internal environment, check the global environment.
        Nothing -> setGlobal ctx' (contextGlobalEnv ctx') value
specialCommandSet ctx [notName, _] =
  pure (evalError ctx ("`set!` expected a name as first argument, but got " ++ pretty notName) (xobjInfo notName))
specialCommandSet ctx args =
  pure (evalError ctx ("`set!` takes a name and a value, but got `" ++ intercalate " " (map pretty args)) (if null args then Nothing else xobjInfo (head args)))

-- | Convenience method for signifying failure in a given context.
failure :: Context -> EvalError -> (Context, Either EvalError a)
failure ctx err = (ctx, Left err)

-- | Given a context, value XObj and an existing binder, check whether or not
-- the given value has a type matching the binder's in the given context.
typeCheckValueAgainstBinder :: Context -> XObj -> Binder -> IO (Context, (Either EvalError XObj))
typeCheckValueAgainstBinder ctx val binder = do
  (ctx', typedValue) <- annotateWithinContext False ctx val
  pure $ case typedValue of
    Right (val', _) -> go ctx' binderTy val'
    Left err -> (ctx', Left err)
  where
    path = (getPath (binderXObj binder))
    binderTy = xobjTy (binderXObj binder)
    typeErr x = evalError ctx ("can't `set!` " ++ show path ++ " to a value of type " ++ show (fromJust (xobjTy x)) ++ ", " ++ show path ++ " has type " ++ show (fromJust binderTy)) (xobjInfo x)
    go ctx'' (Just DynamicTy) x = (ctx'', Right x)
    go ctx'' t x@(XObj _ _ t') = if t == t' then (ctx'', Right x) else typeErr x

-- | Sets a variable, checking whether or not it is static or dynamic, and
-- assigns an appropriate type to the variable.
-- Returns a new environment containing the assignment.
setStaticOrDynamicVar :: SymPath -> Env -> Binder -> XObj -> Env
setStaticOrDynamicVar path env binder value =
  case binder of
    (Binder meta (XObj (Lst (def@(XObj Def _ _) : sym : _)) _ t)) ->
      envReplaceBinding path (Binder meta (XObj (Lst [def, sym, value]) (xobjInfo value) t)) env
    (Binder meta (XObj (Lst (defdy@(XObj DefDynamic _ _) : sym : _)) _ _)) ->
      envReplaceBinding path (Binder meta (XObj (Lst [defdy, sym, value]) (xobjInfo value) (Just DynamicTy))) env
    (Binder meta (XObj (Lst (lett@(XObj LetDef _ _) : sym : _)) _ t)) ->
      envReplaceBinding path (Binder meta (XObj (Lst [lett, sym, value]) (xobjInfo value) t)) env
    -- shouldn't happen, errors are thrown at call sites.
    -- TODO: Return an either here to propagate error.
    _ -> env

primitiveEval :: Primitive
primitiveEval _ ctx [val] = do
  -- primitives don’t evaluate their arguments, so this needs to double-evaluate
  (newCtx, arg) <- evalDynamic False ctx val
  case arg of
    Left err -> pure (newCtx, Left err)
    Right evald -> do
      (newCtx', expanded) <- macroExpand newCtx evald
      case expanded of
        Left err -> pure (newCtx', Left err)
        Right ok -> do
          (finalCtx, res) <- evalDynamic False newCtx' ok
          pure $ case res of
            Left (HasStaticCall x i) -> evalError ctx ("Unexpected static call in " ++ pretty x) i
            _ -> (finalCtx, res)
primitiveEval _ _ _ = error "primitiveeval"

dynamicOrMacro :: Context -> Obj -> Ty -> String -> XObj -> XObj -> IO (Context, Either EvalError XObj)
dynamicOrMacro ctx pat ty name params body = do
  (ctx', exp) <- macroExpand ctx body
  case exp of
    Right expanded ->
      dynamicOrMacroWith ctx' (\path -> [XObj pat Nothing Nothing, XObj (Sym path Symbol) Nothing Nothing, params, expanded]) ty name body
    Left _ -> pure (ctx, exp)

primitiveDefndynamic :: Primitive
primitiveDefndynamic _ ctx [XObj (Sym (SymPath [] name) _) _ _, params, body] =
  dynamicOrMacro ctx Dynamic DynamicTy name params body
primitiveDefndynamic _ ctx [notName, _, _] =
  argumentErr ctx "defndynamic" "a name" "first" notName
primitiveDefndynamic _ _ _ = error "primitivedefndynamic"

primitiveDefmacro :: Primitive
primitiveDefmacro _ ctx [XObj (Sym (SymPath [] name) _) _ _, params, body] =
  dynamicOrMacro ctx Macro MacroTy name params body
primitiveDefmacro _ ctx [notName, _, _] =
  argumentErr ctx "defmacro" "a name" "first" notName
primitiveDefmacro _ _ _ = error "primitivedefmacro"

primitiveAnd :: Primitive
primitiveAnd _ ctx [a, b] = do
  (newCtx, evaledA) <- evalDynamic False ctx a
  case evaledA of
    Left e -> pure (ctx, Left e)
    Right (XObj (Bol ab) _ _) ->
      if ab
        then do
          (newCtx', evaledB) <- evalDynamic False newCtx b
          pure $ case evaledB of
            Left e -> (newCtx, Left e)
            Right (XObj (Bol bb) _ _) ->
              (newCtx', Right (boolToXObj bb))
            Right b' -> evalError ctx ("Can’t call `or` on " ++ pretty b') (xobjInfo b')
        else pure (newCtx, Right falseXObj)
    Right a' -> pure (evalError ctx ("Can’t call `or` on " ++ pretty a') (xobjInfo a'))
primitiveAnd _ _ _ = error "primitiveand"

primitiveOr :: Primitive
primitiveOr _ ctx [a, b] = do
  (newCtx, evaledA) <- evalDynamic False ctx a
  case evaledA of
    Left e -> pure (ctx, Left e)
    Right (XObj (Bol ab) _ _) ->
      if ab
        then pure (newCtx, Right trueXObj)
        else do
          (newCtx', evaledB) <- evalDynamic False newCtx b
          pure $ case evaledB of
            Left e -> (newCtx, Left e)
            Right (XObj (Bol bb) _ _) ->
              (newCtx', Right (boolToXObj bb))
            Right o -> err o
    Right o -> pure (err o)
  where
    err o = evalError ctx ("Can’t call `or` on " ++ pretty o) (xobjInfo o)
primitiveOr _ _ _ = error "primitiveor"

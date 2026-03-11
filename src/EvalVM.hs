{-# LANGUAGE LambdaCase #-}

module EvalVM
  ( compileEvalIR,
    runEvalCode,
    runEvalIRVM,
  )
where

import Context
import Control.Applicative ((<|>))
import Data.Array ((!))
import Data.Either (fromRight)
import Data.Foldable (foldlM, foldrM)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (foldl', intercalate)
import Data.List.Split (splitWhen)
import Data.Maybe (fromMaybe)
import qualified Env as E
import EvalBound (BoundRef (..), refToSymPath)
import EvalCode
import EvalError
import EvalIR (EvalIR (..), lowerExpr, raiseExpr)
import EvalSlotLowering (findUnresolvedLocalRefs, slotifyCallableLocals)
import EvalTypes
import Expand
import Forms (checkAppArity)
import Infer (annotate)
import Info (FilePathPrintLength (FullPath), Info (..), dummyInfo)
import qualified Map
import qualified Meta
import Obj
import Primitives (define, dynamicOrMacroWith)
import Project (projectFilePathPrintLength)
import Qualify (Qualified (..), qualify, qualifyPath, unqualify)
import qualified Set
import System.IO.Unsafe (unsafePerformIO)
import TypeError (evalError, machineReadableErrorStrings)
import Types
import Util (joinLines, pairwise)

type CodeBuilder = [EvalInstr] -> [EvalInstr]

compileEvalIR :: EvalIR -> EvalCode
compileEvalIR ir = mkEvalCode (fst (compileWithState emptyState ir) [IHalt])
  where
    emptyState = CompileState 0 Map.empty
    internSymbol :: BoundRef -> SymbolMode -> CompileState -> (SymbolId, CompileState)
    internSymbol ref mode st =
      case Map.lookup (ref, mode) (internedSymbols st) of
        Just sid -> (sid, st)
        Nothing ->
          let sid = nextSymbolId st
           in (sid, st {nextSymbolId = sid + 1, internedSymbols = Map.insert (ref, mode) sid (internedSymbols st)})
    compileList :: CompileState -> [EvalIR] -> (CodeBuilder, CompileState)
    compileList st [] = (id, st)
    compileList st (x : xs) =
      let (cx, st1) = compileWithState st x
          (cxs, st2) = compileList st1 xs
       in (cx . cxs, st2)
    compileWithState :: CompileState -> EvalIR -> (CodeBuilder, CompileState)
    compileWithState st node =
      case node of
        IRLiteral x ->
          ((IPushConst x :), st)
        IRArray items i t ->
          let (prefix, st') = compileList st items
           in (prefix . (IMakeArray (length items) i t :), st')
        IRStaticArray items i t ->
          let (prefix, st') = compileList st items
           in (prefix . (IMakeStaticArray (length items) i t :), st')
        IRIf cond trueBranch falseBranch i _ ->
          let (condCode, st1) = compileWithState st cond
              (trueCode, st2) = compileWithState st1 trueBranch
              (falseCode, st3) = compileWithState st2 falseBranch
              trueLen = length (trueCode [])
              falseLen = length (falseCode [])
              jumpToElse = trueLen + 1
              jumpToEnd = falseLen
           in ( condCode
                  . (IJumpIfFalseRel jumpToElse i :)
                  . trueCode
                  . (IJumpRel jumpToEnd :)
                  . falseCode,
                st3
              )
        IRDo [] _ _ ->
          ((IPushConst (XObj (Lst []) Nothing Nothing) :), st)
        IRDo forms _ _ ->
          let go (acc, st', ix) form =
                let (compiled, st'') = compileWithState st' form
                    withDrop = if ix < length forms - 1 then compiled . (IDrop :) else compiled
                 in (acc . withDrop, st'', ix + 1)
              (result, stFinal, _) = foldl' go (id, st, 0 :: Int) forms
           in (result, stFinal)
        IRList [] _ _ ->
          ((IPushConst (XObj (Lst []) Nothing Nothing) :), st)
        IRList (IRSymbol ref mode _ _ : args) i t ->
          let (sid, st') = internSymbol ref mode st
              handle = resolveHandleFromRef ref
              argCodes = map compileEvalIR args
           in ( ( ( if isSpecialFormRef ref
                      then IExecCall (IRSymbol ref mode i t) args argCodes i t
                      else IExecCallSymbol sid handle ref mode args argCodes i t
                  )
                    :
                ),
                st'
              )
        IRList (fun : args) i t ->
          ((IExecCall fun args (map compileEvalIR args) i t :), st)
        IRSymbol ref mode i t ->
          let (sid, st') = internSymbol ref mode st
              handle = resolveHandleFromRef ref
           in ((IResolveSymbol sid handle ref mode i t :), st')
        IRLet bindings body i t ->
          ((IExecLet bindings body i t :), st)
        IRFn args body i t ->
          ((IExecFn args body i t :), st)
        IRWhile cond body i t ->
          ((IExecWhile cond body i t :), st)
        IRWith sym forms i t ->
          ((IExecWith sym forms i t :), st)
        IRSet target value i t ->
          ((IExecSet target value i t :), st)
        IRCall (IRSymbol ref mode _ _) args i t ->
          let (sid, st') = internSymbol ref mode st
              handle = resolveHandleFromRef ref
              argCodes = map compileEvalIR args
           in ( ( ( if isSpecialFormRef ref
                      then IExecCall (IRSymbol ref mode i t) args argCodes i t
                      else IExecCallSymbol sid handle ref mode args argCodes i t
                  )
                    :
                ),
                st'
              )
        IRCall fun args i t ->
          ((IExecCall fun args (map compileEvalIR args) i t :), st)
    isSpecialFormRef :: BoundRef -> Bool
    isSpecialFormRef ref =
      case refToSymPath ref of
        SymPath [] n -> isSpecialFormName n
        _ -> False
    isSpecialFormName :: String -> Bool
    isSpecialFormName name =
      case name of
        "if" -> True
        "let" -> True
        "fn" -> True
        "do" -> True
        "while" -> True
        "with" -> True
        "set!" -> True
        "def" -> True
        "defn" -> True
        "defmacro" -> True
        "defndynamic" -> True
        "defdynamic" -> True
        "the" -> True
        "ref" -> True
        "deref" -> True
        _ -> False
    resolveHandleFromRef ref =
      case ref of
        BoundLocalSlot slot -> RHLocalSlot slot
        BoundGlobal p -> RHGlobal p
        BoundDynamic p -> RHDynamic p
        BoundUnresolved (SymPath [] name) -> RHUnqualified name
        BoundUnresolved p -> RHQualified p
        BoundInternal p -> RHQualified p

runEvalIRVM :: Context -> EvalIR -> LookupPreference -> IO (Context, Either EvalError XObj)
runEvalIRVM ctx ir preference =
  case evalIRCacheKey ir of
    Nothing -> runEvalCode ctx preference (compileEvalIR ir)
    Just key -> do
      cache <- readIORef evalIRCodeCache
      case Map.lookup key cache of
        Just cached -> runEvalCode ctx preference cached
        Nothing ->
          let compiled = compileEvalIR ir
           in do
                writeIORef evalIRCodeCache (Map.insert key compiled cache)
                runEvalCode ctx preference compiled

evalDynamicVM :: Context -> XObj -> IO (Context, Either EvalError XObj)
evalDynamicVM ctx xobj =
  let ctx' = ensureDynamicUse ctx
   in runEvalIRVM ctx' (lowerExpr ctx' xobj) PreferDynamic

macroExpandVM :: Context -> XObj -> IO (Context, Either EvalError XObj)
macroExpandVM ctx xobj = expand MacroExpandOnly evalDynamicVM ctx xobj

ensureDynamicUse :: Context -> Context
ensureDynamicUse ctx =
  replaceGlobalEnv ctx (E.addUsePath (contextGlobalEnv ctx) (SymPath [] "Dynamic"))

data VMClosurePayload
  = VMPrecompiled EvalCode
  | VMCompileOnCall CompileMode [String] (Maybe String) XObj (Maybe EvalCode)

data CompileMode = CompileModeFunction | CompileModeDynamic | CompileModeMacro
  deriving (Eq, Ord, Show)

data CompileState = CompileState
  { nextSymbolId :: !Int,
    internedSymbols :: !(Map.Map (BoundRef, SymbolMode) SymbolId)
  }

type CacheKey = (Int, SymbolId)

type CallableCache = Map.Map CacheKey XObj

type IRCodeCache = Map.Map Int EvalCode

{-# NOINLINE vmClosureStore #-}
vmClosureStore :: IORef (Int, Map.Map Int VMClosurePayload)
vmClosureStore = unsafePerformIO (newIORef (0, Map.empty))

{-# NOINLINE evalIRCodeCache #-}
evalIRCodeCache :: IORef IRCodeCache
evalIRCodeCache = unsafePerformIO (newIORef Map.empty)

registerVMClosureCode :: EvalCode -> IO Int
registerVMClosureCode code = do
  (nextId, store) <- readIORef vmClosureStore
  let cid = nextId + 1
  writeIORef vmClosureStore (cid, Map.insert cid (VMPrecompiled code) store)
  pure cid

registerVMClosureLazy :: CompileMode -> [String] -> Maybe String -> XObj -> IO Int
registerVMClosureLazy mode proper restName body = do
  (nextId, store) <- readIORef vmClosureStore
  let cid = nextId + 1
  writeIORef vmClosureStore (cid, Map.insert cid (VMCompileOnCall mode proper restName body Nothing) store)
  pure cid

lookupVMClosurePayload :: Int -> IO (Maybe VMClosurePayload)
lookupVMClosurePayload cid = do
  (_, store) <- readIORef vmClosureStore
  pure (Map.lookup cid store)

cacheVMClosureCode :: Int -> EvalCode -> IO ()
cacheVMClosureCode cid code = do
  (nextId, store) <- readIORef vmClosureStore
  case Map.lookup cid store of
    Just (VMCompileOnCall mode proper restName body _) ->
      writeIORef vmClosureStore (nextId, Map.insert cid (VMCompileOnCall mode proper restName body (Just code)) store)
    _ ->
      writeIORef vmClosureStore (nextId, Map.insert cid (VMPrecompiled code) store)

compileCallableCodeForMode :: Context -> CompileMode -> [String] -> Maybe String -> XObj -> IO (Context, Either EvalError EvalCode)
compileCallableCodeForMode ctx compileMode proper restName body = do
  (expCtx, expanded) <- macroExpandVM ctx body
  case expanded of
    Left err -> pure (expCtx, Left err)
    Right okBody -> do
      let lowered0 = lowerExpr expCtx okBody
          lowered =
            case compileMode of
              CompileModeFunction -> slotifyCallableLocals proper restName lowered0
              _ -> lowered0
          unresolvedLocalRefs =
            case compileMode of
              CompileModeFunction -> findUnresolvedLocalRefs proper restName lowered
              _ -> []
      if null unresolvedLocalRefs
        then pure (expCtx, Right (compileEvalIR lowered))
        else
          pure
            ( evalError
                expCtx
                ("Callable local slot lowering failed, unresolved local refs: " ++ intercalate ", " unresolvedLocalRefs)
                (xobjInfo body)
            )

parseParamSpec :: [XObj] -> Either EvalError ([String], Maybe String)
parseParamSpec params =
  let allParams = map getSimpleName params
   in case splitWhen (":rest" ==) allParams of
        [proper, []] -> Right (proper, Nothing)
        [proper, [restName]] -> Right (proper, Just restName)
        [proper] -> Right (proper, Nothing)
        _ -> Left (EvalError "Invalid function parameter list." [] FullPath Nothing)

resolveSymbol ::
  Context ->
  SymPath ->
  Maybe Info ->
  LookupPreference ->
  Maybe (Context, Either EvalError XObj)
resolveSymbol ctx spath@(SymPath p n) info preference =
  tryAllLookups preference
  where
    maybeId = either (const Nothing) Just
    globalEnv = contextGlobalEnv ctx
    contextualRoot = either (const Nothing) Just (E.getInnerEnv globalEnv (contextPath ctx))
    importedEnvs = E.findImportedEnvs globalEnv :: [Env]
    dynamicPath = SymPath ("Dynamic" : p) n
    tryAllLookups PreferDynamic = getDynamic <|> resolveByShape False True
    tryAllLookups PreferGlobal = getGlobal spath <|> resolveByShape False False
    tryAllLookups (PreferLocal localNames localSlots localNameSlots execMode) =
      let isLocalUnqualified = null p && Set.member n localNames
          dynamicAlreadyTried = not isLocalUnqualified
       in localLookup execMode isLocalUnqualified dynamicAlreadyTried localSlots localNameSlots
    localLookup execMode isLocalUnqualified dynamicAlreadyTried localSlots localNameSlots =
      case execMode of
        ExecFunction ->
          if isLocalUnqualified
            then getLocalFast n localSlots localNameSlots
            else getDynamic <|> resolveByShape True dynamicAlreadyTried
        _ ->
          (if isLocalUnqualified then getLocal n <|> getLocalFast n localSlots localNameSlots else getDynamic)
            <|> resolveByShape isLocalUnqualified dynamicAlreadyTried
    resolveByShape skipInternalLookup dynamicAlreadyTried =
      if null p
        then resolveUnqualified skipInternalLookup dynamicAlreadyTried
        else resolveQualified skipInternalLookup dynamicAlreadyTried
    resolveUnqualified skipInternalLookup dynamicAlreadyTried =
      (if dynamicAlreadyTried then Nothing else tryDynamicLookup)
        <|> (if skipInternalLookup then Nothing else tryInternalLookup n)
        <|> tryGlobalLookups spath
    resolveQualified _ dynamicAlreadyTried =
      (if dynamicAlreadyTried then Nothing else tryDynamicLookup)
        <|> tryGlobalLookups spath
    getDynamic =
      do
        (Binder _ found) <- E.findBinderMaybe globalEnv dynamicPath
        resolveFound found
    getGlobal path =
      do
        (Binder meta found) <- E.findBinderMaybe globalEnv path
        checkPrivate meta found
    tryDynamicLookup =
      do
        (Binder meta found) <- E.findBinderMaybe globalEnv dynamicPath
        checkPrivate meta found
    getLocal name =
      do
        (Binder _ found) <- lookupInternalBinder (contextInternalEnv ctx) name
        resolveFound found
    getLocalFast name localSlots localNameSlots =
      Map.lookup name localNameSlots
        >>= \slot ->
          Map.lookup slot localSlots
            >>= \value -> pure (ctx, Right value)
    tryInternalLookup name =
      lookupInternalBinder (contextInternalEnv ctx) name
        >>= \(Binder _ found) -> resolveFound found
    tryGlobalLookups path =
      ( E.findBinderMaybe globalEnv path
          >>= \(Binder meta found) -> checkPrivate meta found
      )
        <|> tryContextualLookup path
        <|> ( maybeId (lookupBinderInTypeEnv ctx path)
                >>= \(Binder _ found) -> pure (ctx, Right (resolveDef found))
            )
        <|> lookupInUsedModules importedEnvs
      where
        tryContextualLookup (SymPath p' n')
          | null (contextPath ctx) = Nothing
          | otherwise =
            contextualRoot
              >>= \root ->
                E.findBinderMaybe root (SymPath p' n')
                  >>= \(Binder meta found) -> checkPrivate meta found
        lookupInUsedModules [] = Nothing
        lookupInUsedModules (used : rest) =
          ( E.findBinderMaybe used (SymPath p n)
              >>= \(Binder meta found) -> checkPrivate meta found
          )
            <|> lookupInUsedModules rest
    checkPrivate meta found =
      if metaIsTrue meta "private"
        then pure (throwErr (PrivateBinding (getPath found)) ctx info)
        else resolveFound found
    lookupInternalBinder = E.lookupBinderInParentChain
    resolveDef (XObj (Lst [XObj DefDynamic _ _, _, value]) _ _) = value
    resolveDef (XObj (Lst [XObj LocalDef _ _, _, value]) _ _) = value
    resolveDef x = x
    resolveFound found = maybe Nothing (\x -> pure (ctx, Right x)) (resolveFoundMaybe found)
    resolveFoundMaybe found =
      let resolved = resolveDef found
       in if isMetaStub resolved then Nothing else Just resolved
    isMetaStub (XObj (Lst (XObj MetaStub _ _ : _)) _ _) = True
    isMetaStub _ = False

specialCommandDefineVM :: Context -> XObj -> IO (Context, Either EvalError XObj)
specialCommandDefineVM ctx xobj = do
  (newCtx, result) <- annotateWithinContextVM ctx xobj
  case result of
    Right (annXObj, annDeps) -> do
      ctxWithDeps <- foldlM (define True) newCtx (map Qualified annDeps)
      ctxWithDef <- define False ctxWithDeps (Qualified annXObj)
      pure (ctxWithDef, Right (XObj (Lst []) Nothing Nothing))
    Left err ->
      pure (ctx, Left err)

annotateWithinContextVM :: Context -> XObj -> IO (Context, Either EvalError (XObj, [XObj]))
annotateWithinContextVM ctx xobj = do
  let ctxDyn = ensureDynamicUse ctx
      globalEnv = contextGlobalEnv ctxDyn
      typeEnv = contextTypeEnv ctxDyn
      sig = getSigFromDefnOrDefVM ctxDyn xobj
      fppl = projectFilePathPrintLength (contextProj ctx)
  case sig of
    Left err -> pure (ctx, Left err)
    Right okSig -> do
      (_, expansionResult) <- expandAll evalDynamicVM ctxDyn xobj
      case expansionResult of
        Left err -> pure (ctx, Left err)
        Right expanded ->
          let xobjFullSymbols = qualify ctxDyn expanded
           in case xobjFullSymbols of
                Left err -> pure (evalError ctx (show err) (xobjInfo xobj))
                Right xs ->
                  let xsWithInfo = fillMissingInfoQualifiedUnique (xobjInfo xobj) xs
                   in case annotate typeEnv globalEnv xsWithInfo okSig of
                        Left err ->
                          case contextExecMode ctx of
                            Check -> pure (evalError ctx (joinLines (machineReadableErrorStrings fppl err)) Nothing)
                            _ -> pure (evalError ctx (show err) (xobjInfo xobj))
                        Right ok -> pure (ctx, Right ok)
  where
    fillMissingInfoQualifiedUnique :: Maybe Info -> Qualified -> Qualified
    fillMissingInfoQualifiedUnique fallback (Qualified root) = Qualified root'
      where
        (root', _) = go fallback root 1
        go :: Maybe Info -> XObj -> Int -> (XObj, Int)
        go parentInfo x nextId =
          let (info', nextId') =
                case xobjInfo x of
                  Just i -> (Just i, nextId)
                  Nothing ->
                    let base = fromMaybe dummyInfo (parentInfo <|> fallback)
                        fresh = base {infoIdentifier = 1000000 + nextId}
                     in (Just fresh, nextId + 1)
              withInfo = maybe x (\i -> x {xobjInfo = Just i}) info'
           in case xobjObj withInfo of
                Lst xs ->
                  let (xs', n') = goList info' xs nextId'
                   in (withInfo {xobjObj = Lst xs'}, n')
                Arr xs ->
                  let (xs', n') = goList info' xs nextId'
                   in (withInfo {xobjObj = Arr xs'}, n')
                StaticArr xs ->
                  let (xs', n') = goList info' xs nextId'
                   in (withInfo {xobjObj = StaticArr xs'}, n')
                _ -> (withInfo, nextId')
        goList :: Maybe Info -> [XObj] -> Int -> ([XObj], Int)
        goList _ [] n = ([], n)
        goList parentInfo (y : ys) n =
          let (y', n1) = go parentInfo y n
              (ys', n2) = goList parentInfo ys n1
           in (y' : ys', n2)

getSigFromDefnOrDefVM :: Context -> XObj -> Either EvalError (Maybe (Ty, XObj))
getSigFromDefnOrDefVM ctx xobj =
  let pathStrings = contextPath ctx
      globalEnv = contextGlobalEnv ctx
      fppl = projectFilePathPrintLength (contextProj ctx)
      path = fromMaybe (getPath xobj) (pathFromDefLike xobj)
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

primitiveDefdynamicVM :: BinaryPrimitiveCallback
primitiveDefdynamicVM _ ctx (XObj (Sym (SymPath [] name) _) _ _) value = do
  (newCtx, result) <- evalDynamicVM ctx value
  case result of
    Left err -> pure (newCtx, Left err)
    Right evaledBody ->
      dynamicOrMacroWith newCtx (\path -> [XObj DefDynamic Nothing Nothing, XObj (Sym path Symbol) Nothing Nothing, evaledBody]) DynamicTy name value
primitiveDefdynamicVM _ ctx notName _ =
  pure (throwErr (DefnDynamicInvalidName notName) ctx (xobjInfo notName))

bindCompiledCallableVM :: Context -> Ty -> String -> XObj -> XObj -> IO (Context, Either EvalError XObj)
bindCompiledCallableVM ctx ty name params body =
  case params of
    XObj (Arr paramForms) _ _ ->
      case parseParamSpec paramForms of
        Left err -> pure (ctx, Left err)
        Right (proper, rest) -> do
          let qpath = qualifyPath ctx (SymPath [] name)
              upath = unqualify qpath
              rootQPath = qualifyPath (replacePath ctx []) (SymPath [] name)
              info = xobjInfo body
              compileMode =
                case ty of
                  DynamicTy -> CompileModeDynamic
                  MacroTy -> CompileModeMacro
                  _ -> CompileModeFunction
          cidResult <-
            case compileMode of
              CompileModeFunction -> do
                (compileCtx, compiled) <- compileCallableCodeForMode ctx compileMode proper rest body
                case compiled of
                  Left err -> pure (Left (compileCtx, err))
                  Right code -> do
                    cid <- registerVMClosureCode code
                    pure (Right (compileCtx, cid))
              _ -> do
                cid <- registerVMClosureLazy compileMode proper rest body
                pure (Right (ctx, cid))
          case cidResult of
            Left (failedCtx, err) -> pure (failedCtx, Left err)
            Right (closureCtx, cid) -> do
              let inner = XObj (VMClosure cid (Just upath) proper rest) info (Just ty)
                  closure = XObj (Closure inner (CCtx closureCtx)) info (Just ty)
                  headObj =
                    case ty of
                      MacroTy -> XObj Macro Nothing Nothing
                      DynamicTy -> XObj Dynamic Nothing Nothing
                      _ -> XObj Dynamic Nothing Nothing
                  definition = XObj (Lst [headObj, XObj (Sym upath Symbol) Nothing Nothing, params, closure]) info (Just ty)
                  meta = fromRight emptyMeta (E.lookupMeta (contextGlobalEnv closureCtx) upath)
              pure $
                case insertInGlobalEnv closureCtx qpath (Binder meta definition) of
                  Left e -> evalError closureCtx (show e) info
                  Right c ->
                    let withRootShadowCleared =
                          case E.findBinder (contextGlobalEnv c) (SymPath [] name) of
                            Right (Binder _ (XObj (Lst (XObj MetaStub _ _ : _)) _ _)) ->
                              case insertInGlobalEnv c rootQPath (Binder meta definition) of
                                Right c' -> c'
                                Left _ -> c
                            _ -> c
                     in (withRootShadowCleared, Right (XObj (Lst []) Nothing Nothing))
    _ ->
      pure (throwErr (InvalidArgs "Invalid args to callable definition, expected parameter array." [params]) ctx (xobjInfo params))

primitiveDefndynamicVM :: TernaryPrimitiveCallback
primitiveDefndynamicVM _ ctx (XObj (Sym (SymPath [] name) _) _ _) params body =
  bindCompiledCallableVM ctx DynamicTy name params body
primitiveDefndynamicVM _ ctx notName _ _ =
  pure (throwErr (DefnDynamicInvalidName notName) ctx (xobjInfo notName))

primitiveDefmacroVM :: TernaryPrimitiveCallback
primitiveDefmacroVM _ ctx (XObj (Sym (SymPath [] name) _) _ _) params body =
  bindCompiledCallableVM ctx MacroTy name params body
primitiveDefmacroVM _ ctx notName _ _ =
  pure (throwErr (InvalidArgs "`defmacro` expected a name as first argument." [notName]) ctx (xobjInfo notName))

specialCommandWithVM :: Context -> SymPath -> [EvalIR] -> IO (Context, Either EvalError XObj)
specialCommandWithVM ctx path forms = do
  let globalEnv = contextGlobalEnv ctx
      useThese = envUseModules globalEnv
      ctx' = replaceGlobalEnv ctx (globalEnv {envUseModules = Set.insert path useThese})
  (ctxAfter, result) <- foldlM step (ctx', Right (XObj (Lst []) Nothing Nothing)) forms
  let envAfter = contextGlobalEnv ctxAfter
      ctxAfter' = replaceGlobalEnv ctxAfter (envAfter {envUseModules = useThese})
  pure $
    case result of
      Left err -> (ctxAfter', Left err)
      Right _ -> (ctxAfter', Right (XObj (Lst []) Nothing Nothing))
  where
    step (accCtx, accRes) form =
      case accRes of
        Left _ -> pure (accCtx, accRes)
        Right _ -> do
          (nextCtx, evaled) <- runEvalIRVM accCtx form PreferDynamic
          pure (nextCtx, evaled)

specialCommandSetVM :: Context -> EvalIR -> EvalIR -> IO (Context, Either EvalError XObj)
specialCommandSetVM ctx targetIR valueIR =
  case raiseExpr targetIR of
    orig@(XObj (Sym path@(SymPath _ _) _) _ _) ->
      let lookupInternal =
            maybe (Left "") Right (contextInternalEnv ctx)
              >>= \e ->
                maybe (Left "") Right (either (const Nothing) Just (E.searchBinder e path))
                  >>= \binder -> pure (binder, setInternal path, e)
          lookupGlobal =
            Right (contextGlobalEnv ctx)
              >>= \e ->
                maybe (Left "") Right (either (const Nothing) Just (E.searchBinder e path))
                  >>= \binder -> pure (binder, setGlobal path, e)
       in either
            (const (pure (throwErr (SetVarNotFound orig) ctx (xobjInfo orig))))
            (\(binder', setter', env') -> evalAndSet binder' setter' env')
            (lookupInternal <> lookupGlobal)
    notName ->
      pure (throwErr (SetInvalidVarName notName) ctx (xobjInfo notName))
  where
    evalAndSet :: Binder -> (Context -> Env -> Either EvalError XObj -> Binder -> IO (Context, Either EvalError XObj)) -> Env -> IO (Context, Either EvalError XObj)
    evalAndSet binder setter env =
      evalDynamicVM ctx (raiseExpr valueIR)
        >>= \(newCtx, result) ->
          case result of
            Right evald ->
              setter newCtx env (Right evald) binder
            left -> setter newCtx env left binder
    setGlobal :: SymPath -> Context -> Env -> Either EvalError XObj -> Binder -> IO (Context, Either EvalError XObj)
    setGlobal path ctx' env value binder =
      pure $ either (failureVM ctx' (raiseExpr targetIR)) (success ctx') value
      where
        success c xo = (replaceGlobalEnv c (setStaticOrDynamicVarVM path env binder xo), Right (XObj (Lst []) Nothing Nothing))
    setInternal :: SymPath -> Context -> Env -> Either EvalError XObj -> Binder -> IO (Context, Either EvalError XObj)
    setInternal path ctx' env value binder =
      pure $ either (failureVM ctx' (raiseExpr targetIR)) (success ctx') value
      where
        success c xo = (replaceInternalEnv c (setStaticOrDynamicVarVM path env binder xo), Right (XObj (Lst []) Nothing Nothing))

failureVM :: Context -> XObj -> EvalError -> (Context, Either EvalError a)
failureVM ctx orig err = evalError ctx (show err) (xobjInfo orig)

setStaticOrDynamicVarVM :: SymPath -> Env -> Binder -> XObj -> Env
setStaticOrDynamicVarVM path@(SymPath _ name) env binder value =
  case binder of
    (Binder meta (XObj (Lst (def@(XObj Def _ _) : sym : _)) _ t)) ->
      fromRight env (E.insert env path (Binder meta (XObj (Lst [def, sym, value]) (xobjInfo value) t)))
    (Binder meta (XObj (Lst (defdy@(XObj DefDynamic _ _) : sym : _)) _ _)) ->
      fromRight env (E.insert env path (Binder meta (XObj (Lst [defdy, sym, value]) (xobjInfo value) (Just DynamicTy))))
    (Binder meta (XObj (Lst (lett@(XObj LocalDef _ _) : sym : _)) _ t)) ->
      fromRight env (E.replaceInPlace env name (Binder meta (XObj (Lst [lett, sym, value]) (xobjInfo value) t)))
    _ -> env

execIRLetBC :: Context -> LookupPreference -> EvalIR -> EvalIR -> Maybe Info -> Maybe Ty -> IO (Context, Either EvalError XObj)
execIRLetBC ctx preference bindings body info _ =
  case bindings of
    IRArray flatBindings _ _ ->
      let binds = unwrapLetBindsBC (pairwise flatBindings) []
          letNamesInOrder = collectBindingNamesInOrder (pairwise flatBindings)
          ni = Env Map.empty (contextInternalEnv ctx) Nothing Set.empty InternalEnv 0
       in do
            eitherCtx <- foldrM (successiveEvalBindBC preference) (Right (replaceInternalEnv ctx ni)) binds
            case eitherCtx of
              Left err -> pure (ctx, Left err)
              Right newCtx -> do
                let (baseLocalNames, baseLocalSlots, baseLocalNameSlots, localExecMode) =
                      case preference of
                        PreferLocal names slots nameSlots execMode -> (names, slots, nameSlots, execMode)
                        _ -> (Set.empty, Map.empty, Map.empty, preferenceExecMode preference)
                    slotBase = nextLocalSlot baseLocalSlots
                    letNameSlots = Map.fromList (zip letNamesInOrder [slotBase ..])
                    letSlots = collectLetLocalSlots newCtx letNameSlots
                    localNamesSet = Set.union baseLocalNames (Set.fromList letNamesInOrder)
                    localSlots = Map.union letSlots baseLocalSlots
                    localNameSlots = Map.union letNameSlots baseLocalNameSlots
                    localPref = PreferLocal localNamesSet localSlots localNameSlots localExecMode
                (finalCtx, evaledBody) <- runEvalIRVM newCtx body localPref
                let e = fromMaybe E.empty $ contextInternalEnv finalCtx
                    parentEnv = envParent e
                pure (replaceInternalEnvMaybe finalCtx parentEnv, evaledBody)
    _ -> pure (throwErr (UnknownForm (raiseExpr (IRLet bindings body info Nothing))) ctx info)

unwrapLetBindsBC :: [(EvalIR, EvalIR)] -> [(String, EvalIR)] -> [(String, EvalIR)]
unwrapLetBindsBC [] acc = acc
unwrapLetBindsBC ((IRSymbol ref _ _ _, valueIR) : xs) acc =
  case refToSymPath ref of
    SymPath [] name -> unwrapLetBindsBC xs ((name, valueIR) : acc)
    _ -> error "Malformed let binding: non-local symbol"
unwrapLetBindsBC _ _ = error "Malformed let binding: expected symbol/value pairs"

collectBindingNamesInOrder :: [(EvalIR, EvalIR)] -> [String]
collectBindingNamesInOrder [] = []
collectBindingNamesInOrder ((nameIR, _) : rest) =
  case nameIR of
    IRSymbol ref _ _ _ ->
      case refToSymPath ref of
        SymPath [] name -> name : collectBindingNamesInOrder rest
        _ -> collectBindingNamesInOrder rest
    _ -> collectBindingNamesInOrder rest

collectLetLocalSlots :: Context -> Map.Map String Int -> Map.Map Int XObj
collectLetLocalSlots ctx nameSlots =
  let internal = contextInternalEnv ctx
      toValue binder =
        case binderXObj binder of
          XObj (Lst [XObj DefDynamic _ _, _, value]) _ _ -> value
          XObj (Lst [XObj LocalDef _ _, _, value]) _ _ -> value
          x -> x
      lookupName slot name =
        case E.lookupBinderInParentChain internal name of
          Just binder -> Just (slot, toValue binder)
          Nothing -> Nothing
   in Map.fromList
        [ (resolvedSlot, value)
          | (name, slot) <- Map.toList nameSlots,
            Just (resolvedSlot, value) <- [lookupName slot name]
        ]

nextLocalSlot :: Map.Map Int XObj -> Int
nextLocalSlot slots
  | null (Map.keys slots) = 0
  | otherwise = 1 + maximum (Map.keys slots)

successiveEvalBindBC :: LookupPreference -> (String, EvalIR) -> Either EvalError Context -> IO (Either EvalError Context)
successiveEvalBindBC _ _ err@(Left _) = pure err
successiveEvalBindBC preference (name, valueIR) (Right ctx') = do
  let valueXObj = raiseExpr valueIR
      origin = contextInternalEnv ctx'
      recFix = E.recursive origin (Just "let-rec-env") 0
      envWithSelf = fromRight recFix $ if isFn valueXObj then E.insertX recFix (SymPath [] name) valueXObj else Right recFix
      ctx'' = replaceInternalEnv ctx' envWithSelf
  (newCtx, res) <- runEvalIRVM ctx'' valueIR preference
  case res of
    Right okX ->
      pure $
        Right
          ( fromRight
              (error "Failed to eval let binding.")
              (bindLetDeclaration (replaceInternalEnvMaybe newCtx origin) name okX)
          )
    Left err -> pure (Left err)

execIRFnBC :: Context -> LookupPreference -> EvalIR -> EvalIR -> Maybe Info -> Maybe Ty -> IO (Context, Either EvalError XObj)
execIRFnBC ctx preference args body info ty = do
  let argsXObj = raiseExpr args
      bodyXObj = raiseExpr body
  case argsXObj of
    XObj (Arr params) _ _ ->
      case parseParamSpec params of
        Left parseErr -> pure (ctx, Left parseErr)
        Right (proper, rest) -> do
          let compileMode = compileModeForPreference preference
          (newCtx, compiled) <- compileCallableCodeForMode ctx compileMode proper rest bodyXObj
          case compiled of
            Left err -> pure (newCtx, Left err)
            Right bodyCode -> do
              cid <- registerVMClosureCode bodyCode
              pure (newCtx, Right (XObj (Closure (XObj (VMClosure cid Nothing proper rest) info ty) (CCtx newCtx)) info ty))
    _ ->
      pure (throwErr (UnknownForm (raiseExpr (IRFn args body info ty))) ctx info)

execIRWhileBC :: Context -> LookupPreference -> EvalIR -> EvalIR -> Maybe Info -> Maybe Ty -> IO (Context, Either EvalError XObj)
execIRWhileBC ctx preference cond body _ _ = loop ctx
  where
    loop loopCtx = do
      (condCtx, condResult) <- runEvalIRVM loopCtx cond preference
      case condResult of
        Left err -> pure (condCtx, Left err)
        Right condValue ->
          case xobjObj condValue of
            Bol False -> pure (condCtx, Right (XObj (Lst []) Nothing Nothing))
            Bol True -> do
              (bodyCtx, bodyResult) <- runEvalIRVM condCtx body preference
              case bodyResult of
                Left err -> pure (bodyCtx, Left err)
                Right _ -> loop bodyCtx
            _ -> pure (throwErr (IfContainsNonBool (raiseExpr cond)) ctx (irInfoBC cond))

execIRWithBC :: Context -> EvalIR -> [EvalIR] -> Maybe Info -> Maybe Ty -> IO (Context, Either EvalError XObj)
execIRWithBC ctx sym forms info _ =
  case symToPathBC sym of
    Just path -> specialCommandWithVM ctx path forms
    Nothing -> pure (throwErr (UnknownForm (raiseExpr (IRWith sym forms info Nothing))) ctx info)

execIRSetBC :: Context -> EvalIR -> EvalIR -> Maybe Info -> Maybe Ty -> IO (Context, Either EvalError XObj)
execIRSetBC ctx target value _ _ = specialCommandSetVM ctx target value

execIRCallBC :: Context -> LookupPreference -> EvalIR -> [EvalIR] -> [EvalCode] -> Maybe Info -> Maybe Ty -> IO (Context, Either EvalError XObj)
execIRCallBC appCtx preference fun args argCodes info ty =
  let refDerefArityMessage name args' =
        let argXObjs = map raiseExpr args'
            count = length argXObjs
         in if count > 1
              then
                name
                  ++ " expected 1 arguments but received "
                  ++ show count
                  ++ ".\n\nThe arguments "
                  ++ intercalate ", " (map pretty (drop 1 argXObjs))
                  ++ " are not needed."
              else
                name
                  ++ " expected 1 arguments but received only "
                  ++ show count
                  ++ "."
   in case keywordNameBC fun of
        Just "ref"
          | length args /= 1 ->
            pure (evalError appCtx (refDerefArityMessage "ref" args) info)
        Just "deref"
          | length args /= 1 ->
            pure (evalError appCtx (refDerefArityMessage "deref" args) info)
        _ ->
          case inlineCallable of
            Just action -> action
            Nothing ->
              case keywordNameBC fun of
                Just "if" ->
                  case args of
                    [a, b, c] -> execIRIfBC appCtx preference a b c
                    _ -> pure (throwErr (UnknownForm (callXObjBC fun args info ty)) appCtx info)
                Just "let" ->
                  case args of
                    [a, b] -> execIRLetBC appCtx preference a b info ty
                    _ -> pure (throwErr (UnknownForm (callXObjBC fun args info ty)) appCtx info)
                Just "fn" ->
                  case args of
                    [a, b] -> execIRFnBC appCtx preference a b info ty
                    _ -> pure (throwErr (UnknownForm (callXObjBC fun args info ty)) appCtx info)
                Just "do" ->
                  execIRDoBC appCtx preference args
                Just "while" ->
                  case args of
                    [a, b] -> execIRWhileBC appCtx preference a b info ty
                    _ -> pure (throwErr (UnknownForm (callXObjBC fun args info ty)) appCtx info)
                Just "with" ->
                  case args of
                    (sym : forms) -> execIRWithBC appCtx sym forms info ty
                    _ -> pure (throwErr (UnknownForm (callXObjBC fun args info ty)) appCtx info)
                Just "set!" ->
                  case args of
                    [a, b] -> execIRSetBC appCtx a b info ty
                    _ -> pure (throwErr (UnknownForm (callXObjBC fun args info ty)) appCtx info)
                Just "def" -> specialCommandDefineVM appCtx (callXObjBC fun args info ty)
                Just "defn" -> specialCommandDefineVM appCtx (callXObjBC fun args info ty)
                Just "defmacro" ->
                  case args of
                    [name, params, body] -> primitiveDefmacroVM (callXObjBC fun args info ty) appCtx (raiseExpr name) (raiseExpr params) (raiseExpr body)
                    _ -> pure (throwErr (UnknownForm (callXObjBC fun args info ty)) appCtx info)
                Just "defndynamic" ->
                  case args of
                    [name, params, body] -> primitiveDefndynamicVM (callXObjBC fun args info ty) appCtx (raiseExpr name) (raiseExpr params) (raiseExpr body)
                    _ -> pure (throwErr (UnknownForm (callXObjBC fun args info ty)) appCtx info)
                Just "defdynamic" ->
                  case args of
                    [name, value] -> primitiveDefdynamicVM (callXObjBC fun args info ty) appCtx (raiseExpr name) (raiseExpr value)
                    _ -> pure (throwErr (UnknownForm (callXObjBC fun args info ty)) appCtx info)
                Just "the" -> pure (evalError appCtx "EvalVM `the` is not implemented yet." info)
                _ -> do
                  (newCtx, fResult) <- runEvalIRVM appCtx fun preference
                  case fResult of
                    Left err -> pure (newCtx, Left err)
                    Right funXObj -> dispatchCallableBC newCtx preference fun args argCodes info ty funXObj
  where
    inlineCallable =
      case raiseExpr fun of
        XObj Def _ _ -> Just (specialCommandDefineVM appCtx (callXObjBC fun args info ty))
        XObj (Defn _) _ _ -> Just (specialCommandDefineVM appCtx (callXObjBC fun args info ty))
        _ -> Nothing

checkCallableArity ::
  Context ->
  XObj ->
  [EvalIR] ->
  [XObj] ->
  Maybe Info ->
  Maybe (Context, Either EvalError XObj)
checkCallableArity appCtx funExpr args params info =
  case checkAppArity funExpr params (map raiseExpr args) of
    Right () -> Nothing
    Left err -> Just (evalError appCtx (show err) info)

paramsFromSpec :: [String] -> Maybe String -> [XObj]
paramsFromSpec proper rest =
  let sym n = XObj (Sym (SymPath [] n) Symbol) Nothing Nothing
      properParams = map sym proper
   in case rest of
        Nothing -> properParams
        Just restName -> properParams ++ [sym ":rest", sym restName]

dispatchCallableBC :: Context -> LookupPreference -> EvalIR -> [EvalIR] -> [EvalCode] -> Maybe Info -> Maybe Ty -> XObj -> IO (Context, Either EvalError XObj)
dispatchCallableBC appCtx preference fun args argCodes info ty funXObj =
  if isStaticDefinitionBC funXObj
    then pure (appCtx, Left (HasStaticCall (callXObjBC fun args info ty) info))
    else dispatchByClass (classifyCallableBC funXObj)
  where
    funExpr = raiseExpr fun
    dispatchByClass callableClass =
      case callableClass of
        ResolvedSymbol spath ->
          resolveAndDispatch spath
        CompiledClosure mode capturedCtx cid proper rest ->
          case checkCallableArity appCtx funExpr args (paramsFromSpec proper rest) info of
            Just err -> pure err
            Nothing -> dispatchCompiledClosure mode capturedCtx cid proper rest
        InlineFn params body ->
          runInlineFnCallBC appCtx preference funExpr params body args argCodes info
        MacroDefForm ->
          dispatchMacroDef
        DynamicDefForm ->
          dispatchDynamicDef
        DefDynamicForm ->
          dispatchDefdynamic
        CommandForm arity ->
          runCommandCallBC appCtx preference arity args argCodes info
        PrimitiveForm prim ->
          runPrimitiveCallBC appCtx funXObj prim args info
        UnknownCallable ->
          pure (evalError appCtx ("Unknown callable object in EvalVM: " ++ pretty funXObj ++ " in call " ++ pretty (callXObjBC fun args info ty)) info)
    resolveAndDispatch spath =
      case resolveSymbol appCtx spath info preference of
        Just (resolvedCtx, Right resolvedFun) -> dispatchCallableBC resolvedCtx preference fun args argCodes info ty resolvedFun
        Just (resolvedCtx, Left err) -> pure (resolvedCtx, Left err)
        Nothing -> pure (throwErr (UnknownForm (callXObjBC fun args info ty)) appCtx info)
    dispatchCompiledClosure mode capturedCtx cid proper rest =
      case mode of
        CallAsMacro ->
          runCompiledMacroClosureCallBC appCtx capturedCtx cid proper rest args
        CallAsDynamic ->
          runCompiledDynamicClosureCallBC appCtx cid proper rest argCodes
        CallAsFunction ->
          runCompiledClosureCallBC appCtx preference capturedCtx cid proper rest argCodes
    dispatchMacroDef =
      case map raiseExpr args of
        [name, params, body] -> primitiveDefmacroVM funXObj appCtx name params body
        actual -> pure (throwErr (InvalidArgs "Invalid args to `macro`, expected `(macro name [args] body)`" actual) appCtx info)
    dispatchDynamicDef =
      case map raiseExpr args of
        [name, params, body] -> primitiveDefndynamicVM funXObj appCtx name params body
        actual -> pure (throwErr (InvalidArgs "Invalid args to `dynamic`, expected `(dynamic name [args] body)`" actual) appCtx info)
    dispatchDefdynamic =
      case map raiseExpr args of
        [name, value] -> primitiveDefdynamicVM funXObj appCtx name value
        actual -> pure (throwErr (InvalidArgs "Invalid args to `defdynamic`, expected `(defdynamic name value)`" actual) appCtx info)

data CompiledCallMode = CallAsFunction | CallAsDynamic | CallAsMacro

data CallableClass
  = ResolvedSymbol SymPath
  | CompiledClosure CompiledCallMode Context Int [String] (Maybe String)
  | InlineFn [XObj] XObj
  | MacroDefForm
  | DynamicDefForm
  | DefDynamicForm
  | CommandForm CommandFunctionType
  | PrimitiveForm PrimitiveFunctionType
  | UnknownCallable

classifyCallableBC :: XObj -> CallableClass
classifyCallableBC xobj =
  case xobjObj xobj of
    Sym spath _ -> ResolvedSymbol spath
    Closure (XObj (VMClosure cid _ proper rest) _ cty) (CCtx capturedCtx) ->
      let mode =
            case cty of
              Just MacroTy -> CallAsMacro
              Just DynamicTy -> CallAsDynamic
              _ -> CallAsFunction
       in CompiledClosure mode capturedCtx cid proper rest
    Lst [XObj Macro _ _, _, XObj (Arr _) _ _, XObj (Closure (XObj (VMClosure cid _ proper rest) _ _) (CCtx capturedCtx)) _ _] ->
      CompiledClosure CallAsMacro capturedCtx cid proper rest
    Lst [XObj Dynamic _ _, _, XObj (Arr _) _ _, XObj (Closure (XObj (VMClosure cid _ proper rest) _ _) (CCtx capturedCtx)) _ _] ->
      CompiledClosure CallAsDynamic capturedCtx cid proper rest
    Lst [XObj (Fn _ _) _ _, XObj (Arr params) _ _, body] ->
      InlineFn params body
    Lst [XObj (Sym (SymPath [] "fn") _) _ _, XObj (Arr params) _ _, body] ->
      InlineFn params body
    Macro -> MacroDefForm
    Dynamic -> DynamicDefForm
    DefDynamic -> DefDynamicForm
    Lst [XObj (Command arity) _ _, _, XObj (Arr _) _ _] ->
      CommandForm arity
    Lst [XObj (Primitive prim) _ _, _, XObj (Arr _) _ _] ->
      PrimitiveForm prim
    _ -> UnknownCallable

runInlineFnCallBC :: Context -> LookupPreference -> XObj -> [XObj] -> XObj -> [EvalIR] -> [EvalCode] -> Maybe Info -> IO (Context, Either EvalError XObj)
runInlineFnCallBC appCtx preference funExpr params body argsToCall argCodes info =
  case parseParamSpec params of
    Left err -> pure (appCtx, Left err)
    Right (proper, rest) -> do
      case checkCallableArity appCtx funExpr argsToCall params info of
        Just err -> pure err
        Nothing -> do
          let compileMode = compileModeForPreference preference
          (ctxAfterCompile, compiled) <- compileCallableCodeForMode appCtx compileMode proper rest body
          case compiled of
            Left cerr -> pure (ctxAfterCompile, Left cerr)
            Right code -> do
              cid <- registerVMClosureCode code
              runCompiledClosureCallBC ctxAfterCompile preference ctxAfterCompile cid proper rest argCodes

runCompiledClosureCallBC :: Context -> LookupPreference -> Context -> Int -> [String] -> Maybe String -> [EvalCode] -> IO (Context, Either EvalError XObj)
runCompiledClosureCallBC appCtx preference capturedCtx cid proper restName argCodes = do
  (ctxWithArgs, evaledArgs) <- evalManyCodeBC appCtx preference argCodes
  case evaledArgs of
    Right okArgs -> do
      let callCtx = replaceInternalEnvMaybe ctxWithArgs (contextInternalEnv capturedCtx)
          compileMode = compileModeForPreference preference
      (ctx', res) <- applyCompiledBC compileMode callCtx cid proper restName okArgs
      pure (replaceInternalEnvMaybe ctx' (contextInternalEnv ctxWithArgs), res)
    Left err -> pure (ctxWithArgs, Left err)

runCompiledDynamicClosureCallBC :: Context -> Int -> [String] -> Maybe String -> [EvalCode] -> IO (Context, Either EvalError XObj)
runCompiledDynamicClosureCallBC appCtx cid proper restName argCodes = do
  (ctxWithArgs, evaledArgs) <- evalManyCodeBC appCtx PreferDynamic argCodes
  case evaledArgs of
    Right okArgs -> do
      (ctx', res) <- applyCompiledBC CompileModeDynamic ctxWithArgs cid proper restName okArgs
      pure (replaceInternalEnvMaybe ctx' (contextInternalEnv ctxWithArgs), res)
    Left err -> pure (ctxWithArgs, Left err)

runCompiledMacroClosureCallBC :: Context -> Context -> Int -> [String] -> Maybe String -> [EvalIR] -> IO (Context, Either EvalError XObj)
runCompiledMacroClosureCallBC appCtx _ cid proper restName argsToCall = do
  (ctx', res) <- applyCompiledBC CompileModeMacro appCtx cid proper restName (map raiseExpr argsToCall)
  case res of
    Right xobj' -> macroExpandVM ctx' xobj'
    Left _ -> pure (appCtx, res)

runCommandCallBC :: Context -> LookupPreference -> CommandFunctionType -> [EvalIR] -> [EvalCode] -> Maybe Info -> IO (Context, Either EvalError XObj)
runCommandCallBC appCtx preference arity callArgs argCodes callInfo =
  case (arity, callArgs) of
    (NullaryCommandFunction nullary, []) -> nullary appCtx
    (UnaryCommandFunction unary, [_]) -> do
      (c, evaledArgs) <- evalManyCodeBC appCtx preference (take 1 argCodes)
      case evaledArgs of
        Right [x'] -> unary c x'
        Left err -> pure (c, Left err)
        _ -> pure (throwErr (UnknownForm (raiseExpr (IRCall (IRLiteral (XObj (Lst []) Nothing Nothing)) callArgs callInfo Nothing))) c callInfo)
    (BinaryCommandFunction binary, [_, _]) -> do
      (c, evaledArgs) <- evalManyCodeBC appCtx preference (take 2 argCodes)
      case evaledArgs of
        Right [x', y'] -> binary c x' y'
        Left err -> pure (c, Left err)
        _ -> pure (throwErr (UnknownForm (raiseExpr (IRCall (IRLiteral (XObj (Lst []) Nothing Nothing)) callArgs callInfo Nothing))) c callInfo)
    (TernaryCommandFunction ternary, [_, _, _]) -> do
      (c, evaledArgs) <- evalManyCodeBC appCtx preference (take 3 argCodes)
      case evaledArgs of
        Right [x', y', z'] -> ternary c x' y' z'
        Left err -> pure (c, Left err)
        _ -> pure (throwErr (UnknownForm (raiseExpr (IRCall (IRLiteral (XObj (Lst []) Nothing Nothing)) callArgs callInfo Nothing))) c callInfo)
    (VariadicCommandFunction variadic, xs) -> do
      (c, evaledArgs) <- evalManyCodeBC appCtx preference (take (length xs) argCodes)
      case evaledArgs of
        Right args' -> variadic c args'
        Left err -> pure (c, Left err)
    _ -> pure (throwErr (UnknownForm (raiseExpr (IRCall (IRLiteral (XObj (Lst []) Nothing Nothing)) callArgs callInfo Nothing))) appCtx callInfo)

runPrimitiveCallBC :: Context -> XObj -> PrimitiveFunctionType -> [EvalIR] -> Maybe Info -> IO (Context, Either EvalError XObj)
runPrimitiveCallBC appCtx fun prim callArgs callInfo =
  let rawArgs = map raiseExpr callArgs
   in case (prim, rawArgs) of
        (NullaryPrimitive nullary, []) -> nullary fun appCtx
        (UnaryPrimitive unary, [x]) -> unary fun appCtx x
        (BinaryPrimitive binary, [x, y]) -> binary fun appCtx x y
        (TernaryPrimitive ternary, [x, y, z]) -> ternary fun appCtx x y z
        (QuaternaryPrimitive quaternary, [x, y, z, w]) -> quaternary fun appCtx x y z w
        (VariadicPrimitive variadic, xs) -> variadic fun appCtx xs
        _ -> pure (throwErr (UnknownForm (raiseExpr (IRCall (IRLiteral fun) callArgs callInfo Nothing))) appCtx callInfo)

applyCompiledBC :: CompileMode -> Context -> Int -> [String] -> Maybe String -> [XObj] -> IO (Context, Either EvalError XObj)
applyCompiledBC compileMode appCtx cid proper restName argVals = do
  mpayload <- lookupVMClosurePayload cid
  case mpayload of
    Nothing ->
      pure (evalError appCtx ("Missing compiled VM closure code for id " ++ show cid) Nothing)
    Just payload ->
      case payload of
        VMCompileOnCall lazyMode lazyProper lazyRestName body maybeCode ->
          case maybeCode of
            Just ready -> runCompiledWith appCtx ready
            Nothing -> do
              (compileCtx, compiled) <- compileCallableCodeForMode appCtx lazyMode lazyProper lazyRestName body
              case compiled of
                Left err -> pure (compileCtx, Left err)
                Right code -> do
                  cacheVMClosureCode cid code
                  runCompiledWith compileCtx code
        VMPrecompiled ready ->
          runCompiledWith appCtx ready
  where
    runCompiledWith compileCtx code = do
      let n = length proper
          properBinds = zip proper (take n argVals)
          restBinds =
            case restName of
              Nothing -> []
              Just rest -> [(rest, XObj (Lst (drop n argVals)) Nothing Nothing)]
          binds = proper ++ maybe [] (: []) restName
          localNames = Set.fromList binds
          properSlots = zip [0 ..] (take n argVals)
          restSlots =
            case restName of
              Nothing -> []
              Just _ -> [(length proper, XObj (Lst (drop n argVals)) Nothing Nothing)]
          localSlots = Map.fromList (properSlots ++ restSlots)
          internal = contextInternalEnv compileCtx
          insideEnv = Env Map.empty internal Nothing Set.empty InternalEnv 0
          localBinders =
            map (\(name, value) -> (name, toBinder (toLocalDef name value))) properBinds
              ++ map (\(name, value) -> (name, toBinder value)) restBinds
          insideEnv' = insideEnv {envBindings = Map.fromList localBinders}
          localCtx = replaceInternalEnv compileCtx insideEnv'
      let localNameSlots =
            Map.fromList
              (zip proper [0 ..] ++ maybe [] (\rest -> [(rest, length proper)]) restName)
      let localExecMode =
            case compileMode of
              CompileModeFunction -> ExecFunction
              CompileModeDynamic -> ExecDynamic
              CompileModeMacro -> ExecMacro
      (finalCtx, result) <- runEvalCode localCtx (PreferLocal localNames localSlots localNameSlots localExecMode) code
      pure (replaceInternalEnvMaybe finalCtx (contextInternalEnv compileCtx), result)

evalManyCodeBC :: Context -> LookupPreference -> [EvalCode] -> IO (Context, Either EvalError [XObj])
evalManyCodeBC startCtx preference xs = do
  (newCtx, evaled) <- foldlM successiveEval (startCtx, Right []) xs
  pure (newCtx, fmap reverse evaled)
  where
    successiveEval (ctx', acc) code =
      case acc of
        Left _ -> pure (ctx', acc)
        Right l -> do
          (nextCtx, evald) <- runEvalCode ctx' preference code
          pure $ case evald of
            Right res -> (nextCtx, Right (res : l))
            Left err -> (nextCtx, Left err)

execIRDoBC :: Context -> LookupPreference -> [EvalIR] -> IO (Context, Either EvalError XObj)
execIRDoBC ctx preference forms =
  foldlM
    ( \(ctx', acc) next ->
        case acc of
          Left _ -> pure (ctx', acc)
          Right _ -> runEvalIRVM ctx' next preference
    )
    (ctx, Right (XObj (Lst []) Nothing Nothing))
    forms

execIRIfBC :: Context -> LookupPreference -> EvalIR -> EvalIR -> EvalIR -> IO (Context, Either EvalError XObj)
execIRIfBC ctx preference cond trueBranch falseBranch = do
  (newCtx, evd) <- runEvalIRVM ctx cond preference
  case evd of
    Right cond' ->
      case xobjObj cond' of
        Bol b -> runEvalIRVM newCtx (if b then trueBranch else falseBranch) preference
        _ -> pure (throwErr (IfContainsNonBool (raiseExpr cond)) ctx (irInfoBC cond))
    Left e -> pure (newCtx, Left e)

callXObjBC :: EvalIR -> [EvalIR] -> Maybe Info -> Maybe Ty -> XObj
callXObjBC fun args info ty = XObj (Lst (raiseExpr fun : map raiseExpr args)) info ty

keywordNameBC :: EvalIR -> Maybe String
keywordNameBC ir' =
  case ir' of
    IRSymbol ref _ _ _ ->
      case refToSymPath ref of
        SymPath [] n -> Just n
        _ -> Nothing
    IRLiteral (XObj If _ _) -> Just "if"
    IRLiteral (XObj Let _ _) -> Just "let"
    IRLiteral (XObj (Fn _ _) _ _) -> Just "fn"
    IRLiteral (XObj Do _ _) -> Just "do"
    IRLiteral (XObj While _ _) -> Just "while"
    IRLiteral (XObj With _ _) -> Just "with"
    IRLiteral (XObj SetBang _ _) -> Just "set!"
    IRLiteral (XObj Def _ _) -> Just "def"
    IRLiteral (XObj (Defn _) _ _) -> Just "defn"
    IRLiteral (XObj The _ _) -> Just "the"
    _ -> Nothing

isStaticDefinitionBC :: XObj -> Bool
isStaticDefinitionBC (XObj (Lst (XObj headObj _ _ : _)) _ _) = isResolvableStaticObj headObj
isStaticDefinitionBC _ = False

irInfoBC :: EvalIR -> Maybe Info
irInfoBC (IRSymbol _ _ info _) = info
irInfoBC (IRLiteral x) = xobjInfo x
irInfoBC (IRArray _ info _) = info
irInfoBC (IRStaticArray _ info _) = info
irInfoBC (IRIf _ _ _ info _) = info
irInfoBC (IRLet _ _ info _) = info
irInfoBC (IRFn _ _ info _) = info
irInfoBC (IRDo _ info _) = info
irInfoBC (IRWhile _ _ info _) = info
irInfoBC (IRWith _ _ info _) = info
irInfoBC (IRSet _ _ info _) = info
irInfoBC (IRCall _ _ info _) = info
irInfoBC (IRList _ info _) = info

evalIRCacheKey :: EvalIR -> Maybe Int
evalIRCacheKey ir =
  case irInfoBC ir of
    Just info ->
      let ident = infoIdentifier info
       in if ident > 0 then Just ident else Nothing
    Nothing -> Nothing

symToPathBC :: EvalIR -> Maybe SymPath
symToPathBC (IRSymbol ref _ _ _) = Just (refToSymPath ref)
symToPathBC _ = Nothing

runEvalCode :: Context -> LookupPreference -> EvalCode -> IO (Context, Either EvalError XObj)
runEvalCode ctx preference code = go ctx preference 0 [] Map.empty Map.empty
  where
    codeLen = evalCodeLen code
    codeArr = evalCodeArray code
    cacheKey :: Context -> SymbolId -> CacheKey
    cacheKey c sid = (contextBindingEpoch c, sid)
    cacheableHandle handle =
      case handle of
        RHGlobal _ -> True
        RHDynamic _ -> True
        RHQualified _ -> True
        _ -> False
    pathForHandle handle =
      case handle of
        RHLocalSlot slot -> SymPath [] ("$slot" ++ show slot)
        RHGlobal p -> p
        RHDynamic p -> p
        RHQualified p -> p
        RHUnqualified n -> SymPath [] n
    resolveFromHandle currentCtx currentPref handle info =
      case handle of
        RHGlobal p -> resolveGlobalExact currentCtx p info
        RHDynamic p -> resolveDynamicExact currentCtx p info
        RHQualified p -> resolveQualifiedExact currentCtx p info currentPref
        RHUnqualified n -> resolveSymbol currentCtx (SymPath [] n) info currentPref
        RHLocalSlot _ -> Nothing
    resolveGlobalExact currentCtx p info =
      case E.findBinderMaybe (contextGlobalEnv currentCtx) p of
        Just (Binder meta found) ->
          if metaIsTrue meta "private"
            then pure (throwErr (PrivateBinding (getPath found)) currentCtx info)
            else resolveFoundValue currentCtx found
        Nothing -> Nothing
    resolveDynamicExact currentCtx p info =
      let dynamicPath =
            case p of
              SymPath path name -> SymPath ("Dynamic" : path) name
       in case E.findBinderMaybe (contextGlobalEnv currentCtx) dynamicPath of
            Just (Binder meta found) ->
              if metaIsTrue meta "private"
                then pure (throwErr (PrivateBinding (getPath found)) currentCtx info)
                else resolveFoundValue currentCtx found
            Nothing -> Nothing
    resolveQualifiedExact currentCtx p info currentPref =
      case resolveSymbol currentCtx p info currentPref of
        Just resolved -> Just resolved
        Nothing ->
          case resolveDynamicExact currentCtx p info of
            Just resolvedDyn -> Just resolvedDyn
            Nothing -> Nothing
    resolveDefValue (XObj (Lst [XObj DefDynamic _ _, _, value]) _ _) = value
    resolveDefValue (XObj (Lst [XObj LocalDef _ _, _, value]) _ _) = value
    resolveDefValue x = x
    resolveFoundValue currentCtx found =
      let resolved = resolveDefValue found
       in if isMetaStubValue resolved then Nothing else pure (currentCtx, Right resolved)
    isMetaStubValue (XObj (Lst (XObj MetaStub _ _ : _)) _ _) = True
    isMetaStubValue _ = False
    go :: Context -> LookupPreference -> Int -> [XObj] -> Map.Map CacheKey XObj -> CallableCache -> IO (Context, Either EvalError XObj)
    go currentCtx currentPref pc stack symbolCache callableCache
      | pc < 0 || pc >= codeLen =
        pure (evalError currentCtx "EvalVM instruction pointer out of range." Nothing)
      | otherwise =
        case codeArr ! pc of
          IPushConst x ->
            go currentCtx currentPref (pc + 1) (x : stack) symbolCache callableCache
          IResolveSymbol sid handle ref _ i _ ->
            case ref of
              BoundLocalSlot slot ->
                case currentPref of
                  PreferLocal _ localSlots _ execMode ->
                    case Map.lookup slot localSlots of
                      Just val -> go currentCtx currentPref (pc + 1) (val : stack) symbolCache callableCache
                      Nothing ->
                        case execMode of
                          ExecFunction ->
                            pure (evalError currentCtx ("EvalVM missing function local slot " ++ show slot ++ ".") i)
                          _ -> resolveAndCache
                  _ -> resolveAndCache
              _ ->
                resolveAndCache
            where
              resolveAndCache =
                let refCacheable = cacheableHandle handle
                    spath = pathForHandle handle
                    ckey = cacheKey currentCtx sid
                 in case if refCacheable then Map.lookup ckey symbolCache else Nothing of
                      Just cached ->
                        go currentCtx currentPref (pc + 1) (cached : stack) symbolCache callableCache
                      Nothing ->
                        case resolveFromHandle currentCtx currentPref handle i of
                          Just (resolvedCtx, Right val) ->
                            let symbolCache' =
                                  if refCacheable
                                    then Map.insert (cacheKey resolvedCtx sid) val symbolCache
                                    else symbolCache
                             in go resolvedCtx currentPref (pc + 1) (val : stack) symbolCache' callableCache
                          Just (resolvedCtx, Left err) ->
                            pure (resolvedCtx, Left err)
                          Nothing ->
                            pure (throwErr (SymbolNotFound spath) currentCtx i)
          IExecCallSymbol sid handle ref mode args argCodes i t ->
            case ref of
              BoundLocalSlot slot ->
                let funIR = IRSymbol ref mode i t
                 in case currentPref of
                      PreferLocal _ localSlots _ execMode ->
                        case Map.lookup slot localSlots of
                          Just funXObj -> do
                            (nextCtx, result) <- dispatchCallableBC currentCtx currentPref funIR args argCodes i t funXObj
                            case result of
                              Right value -> go nextCtx currentPref (pc + 1) (value : stack) symbolCache callableCache
                              Left err -> pure (nextCtx, Left err)
                          Nothing ->
                            case execMode of
                              ExecFunction ->
                                pure (evalError currentCtx ("EvalVM missing function local callable slot " ++ show slot ++ ".") i)
                              _ -> resolveCallableAndDispatch
                      _ -> resolveCallableAndDispatch
              _ ->
                resolveCallableAndDispatch
            where
              resolveCallableAndDispatch =
                let refCacheable = cacheableHandle handle
                    spath = pathForHandle handle
                    ckey = cacheKey currentCtx sid
                    funIR = IRSymbol ref mode i t
                    dispatchWith funXObj = do
                      (nextCtx, result) <- dispatchCallableBC currentCtx currentPref funIR args argCodes i t funXObj
                      case result of
                        Right value ->
                          let callableCache' =
                                if refCacheable
                                  then Map.insert (cacheKey nextCtx sid) funXObj callableCache
                                  else callableCache
                           in go nextCtx currentPref (pc + 1) (value : stack) symbolCache callableCache'
                        Left err -> pure (nextCtx, Left err)
                 in case if refCacheable then Map.lookup ckey callableCache else Nothing of
                      Just cachedCallable ->
                        dispatchWith cachedCallable
                      Nothing ->
                        case resolveFromHandle currentCtx currentPref handle i of
                          Just (resolvedCtx, Right funXObj) -> do
                            let callableCache' =
                                  if refCacheable
                                    then Map.insert (cacheKey resolvedCtx sid) funXObj callableCache
                                    else callableCache
                            (nextCtx, result) <- dispatchCallableBC resolvedCtx currentPref funIR args argCodes i t funXObj
                            case result of
                              Right value -> go nextCtx currentPref (pc + 1) (value : stack) symbolCache callableCache'
                              Left err -> pure (nextCtx, Left err)
                          Just (resolvedCtx, Left err) -> pure (resolvedCtx, Left err)
                          Nothing -> pure (throwErr (SymbolNotFound spath) currentCtx i)
          IMakeArray n i t ->
            case popN n stack of
              Left msg -> pure (evalError currentCtx msg i)
              Right (items, rest) ->
                go currentCtx currentPref (pc + 1) (XObj (Arr (reverse items)) i t : rest) symbolCache callableCache
          IMakeStaticArray n i t ->
            case popN n stack of
              Left msg -> pure (evalError currentCtx msg i)
              Right (items, rest) ->
                go currentCtx currentPref (pc + 1) (XObj (StaticArr (reverse items)) i t : rest) symbolCache callableCache
          IExecCall fun args argCodes i t -> do
            (nextCtx, result) <- execIRCallBC currentCtx currentPref fun args argCodes i t
            case result of
              Right value -> go nextCtx currentPref (pc + 1) (value : stack) symbolCache callableCache
              Left err -> pure (nextCtx, Left err)
          IExecLet bindings body i t -> do
            (nextCtx, result) <- execIRLetBC currentCtx currentPref bindings body i t
            case result of
              Right value -> go nextCtx currentPref (pc + 1) (value : stack) symbolCache callableCache
              Left err -> pure (nextCtx, Left err)
          IExecFn args body i t -> do
            (nextCtx, result) <- execIRFnBC currentCtx currentPref args body i t
            case result of
              Right value -> go nextCtx currentPref (pc + 1) (value : stack) symbolCache callableCache
              Left err -> pure (nextCtx, Left err)
          IExecWhile cond body i t -> do
            (nextCtx, result) <- execIRWhileBC currentCtx currentPref cond body i t
            case result of
              Right value -> go nextCtx currentPref (pc + 1) (value : stack) symbolCache callableCache
              Left err -> pure (nextCtx, Left err)
          IExecWith sym forms i t -> do
            (nextCtx, result) <- execIRWithBC currentCtx sym forms i t
            case result of
              Right value -> go nextCtx currentPref (pc + 1) (value : stack) symbolCache callableCache
              Left err -> pure (nextCtx, Left err)
          IExecSet target value i t -> do
            (nextCtx, result) <- execIRSetBC currentCtx target value i t
            case result of
              Right value' ->
                let pref' = syncLocalSlotAfterSet nextCtx currentPref target
                 in go nextCtx pref' (pc + 1) (value' : stack) symbolCache callableCache
              Left err -> pure (nextCtx, Left err)
          IDrop ->
            case stack of
              [] -> pure (evalError currentCtx "EvalVM stack underflow on drop." Nothing)
              (_ : rest) -> go currentCtx currentPref (pc + 1) rest symbolCache callableCache
          IJumpIfFalseRel offset i ->
            case stack of
              [] -> pure (evalError currentCtx "EvalVM stack underflow on conditional jump." i)
              (top : rest) ->
                case xobjObj top of
                  Bol False -> go currentCtx currentPref (pc + 1 + offset) rest symbolCache callableCache
                  Bol True -> go currentCtx currentPref (pc + 1) rest symbolCache callableCache
                  _ -> pure (evalError currentCtx ("EvalVM expected a Bool on conditional jump, got " ++ pretty top) i)
          IJumpRel offset ->
            go currentCtx currentPref (pc + 1 + offset) stack symbolCache callableCache
          ITrap msg i ->
            pure (evalError currentCtx msg i)
          IHalt ->
            case stack of
              (top : _) -> pure (currentCtx, Right top)
              [] -> pure (currentCtx, Right (XObj (Lst []) Nothing Nothing))
    popN :: Int -> [XObj] -> Either String ([XObj], [XObj])
    popN n xs
      | n < 0 = Left "EvalVM internal error: negative pop length."
      | n == 0 = Right ([], xs)
      | otherwise =
        case xs of
          [] -> Left "EvalVM stack underflow while collecting instruction arguments."
          (y : ys) ->
            case popN (n - 1) ys of
              Left e -> Left e
              Right (collected, rest) -> Right (y : collected, rest)
    syncLocalSlotAfterSet :: Context -> LookupPreference -> EvalIR -> LookupPreference
    syncLocalSlotAfterSet updatedCtx pref target =
      case pref of
        PreferLocal localNames localSlots localNameSlots localExecMode ->
          case target of
            IRSymbol ref _ _ _ ->
              case refToSymPath ref of
                SymPath [] name
                  | Set.member name localNames ->
                    case Map.lookup name localNameSlots of
                      Just slot ->
                        case E.lookupBinderInParentChain (contextInternalEnv updatedCtx) name of
                          Just binder ->
                            let newVal =
                                  case binderXObj binder of
                                    XObj (Lst [XObj DefDynamic _ _, _, value]) _ _ -> value
                                    XObj (Lst [XObj LocalDef _ _, _, value]) _ _ -> value
                                    x -> x
                             in PreferLocal localNames (Map.insert slot newVal localSlots) localNameSlots localExecMode
                          Nothing -> pref
                      Nothing -> pref
                _ -> pref
            _ -> pref
        _ -> pref

preferenceExecMode :: LookupPreference -> EvalExecMode
preferenceExecMode pref =
  case pref of
    PreferDynamic -> ExecDynamic
    PreferGlobal -> ExecFunction
    PreferLocal _ _ _ execMode -> execMode

compileModeForPreference :: LookupPreference -> CompileMode
compileModeForPreference pref =
  case preferenceExecMode pref of
    ExecFunction -> CompileModeFunction
    ExecDynamic -> CompileModeDynamic
    ExecMacro -> CompileModeMacro

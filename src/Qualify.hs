{-# LANGUAGE TupleSections #-}

--------------------------------------------------------------------------------

-- | Defines data, errors, and functions for qualifying symbols in a given
-- context.
module Qualify
  ( QualificationError,
    QualifiedPath,
    Qualified (..),
    qualify,
    qualifyPath,
    unqualify,
    markQualified,
    qualifyNull,
    getQualifiedPath,
  )
where

import Control.Monad (foldM, liftM)
import Data.Bifunctor
import Data.Either (fromRight)
import qualified Env as E
import Info
import qualified Map
import Obj
import qualified Set
import SymPath
import Util

--------------------------------------------------------------------------------
-- Errors

-- | Error qualifying a symbol.
data QualificationError
  = FailedToQualifyDeclarationName XObj
  | FailedToQualifySymbols XObj
  | FailedToQualifyPath SymPath
  | NonVariableInMatch XObj
  | NakedInitForUnnamedModule [String]
  | QualifiedMulti SymPath
  | LocalMulti SymPath [(Env, Binder)]
  | FailedToFindSymbol XObj

instance Show QualificationError where
  show (FailedToQualifyDeclarationName xobj) =
    "Couldn't fully qualify the definition: " ++ pretty xobj
  show (FailedToQualifySymbols xobj) =
    "Couldn't fully qualify the symbols in the form: " ++ pretty xobj
  show (FailedToQualifyPath spath) =
    "Couldn't fully qualify the symbol: " ++ show spath
      ++ "in the given context."
  show (NonVariableInMatch xobj) =
    "Couldn't qualify the xobj: " ++ pretty xobj
      ++ "in a match expression."
  show (NakedInitForUnnamedModule s) =
    "Tried to emit a naked init for an unnamed module: " ++ (show s)
  show (QualifiedMulti spath) =
    "Tried to use a qualified symbol as a multi sym: " ++ (show spath)
  show (LocalMulti spath binders) =
    "Tried to use a symbol that has local bindings as a multi sym: " ++ show spath
      ++ show binders
  show (FailedToFindSymbol xobj) =
    "Couldn't find the xobj: " ++ pretty xobj

--------------------------------------------------------------------------------
-- Data

-- | Denotes an XObj containing only symbols that *have been fully qualified*.
--
-- A fully qualified xobj **must not** be qualified further (e.g. using context
-- paths).
newtype Qualified = Qualified {unQualified :: XObj} deriving (Show)

-- | Denotes a symbol that has been fully qualified.
newtype QualifiedPath = QualifiedPath SymPath
  deriving (Ord, Eq)

instance Show QualifiedPath where
  show (QualifiedPath spath) = show spath

--------------------------------------------------------------------------------
-- Path Qualification Functions

-- | Qualifies a symbol in a given Context.
qualifyPath :: Context -> SymPath -> QualifiedPath
qualifyPath ctx spath =
  let qpath = consPath (contextPath ctx) spath
   in (QualifiedPath qpath)

-- | Transforms a qualified path into an equivalent SymPath.
--
-- Used to cross the qualified/unqualified symbol boundary.
-- This is predominantly used for compatibility with other parts of the
-- codebase once qualification checks have been performed.
unqualify :: QualifiedPath -> SymPath
unqualify (QualifiedPath spath) = spath

-- | Marks a path as fully qualified without performing any transformations.
--
-- Used to indicate a "naked", unprocessed path should be treated qualified as
-- given. For example, `inc` in `(implements inc Module.inc)` should be
-- interpreted as fully qualified as typed and should not be qualified using
-- the overarching context.
markQualified :: SymPath -> QualifiedPath
markQualified = QualifiedPath

-- | Qualify a symbol contextually if it is not qualified, otherwise, mark it
-- qualified.
--
-- This should be used whenever a path entered with *any* initial
-- qualifications should be treated as an absolute reference while symbols
-- without qualifications should be treated as a relative reference.
--
-- For example, `Foo.inc` in `(implements inc Foo.inc)` will be treated as an
-- absolute reference to `Foo.inc`, even in the context of `Foo` and will not
-- be qualified further. Contrarily, the second `inc` in `(implements inc inc)`
-- in the context of `Foo` would be further qualified to `Foo.inc`.
qualifyNull :: Context -> SymPath -> QualifiedPath
qualifyNull ctx spath@(SymPath [] _) = qualifyPath ctx spath
qualifyNull _ spath = markQualified spath

--------------------------------------------------------------------------------
-- XObj Qualification Functions

-- | Gets the qualified path of a fully qualified XObj.
getQualifiedPath :: Qualified -> QualifiedPath
getQualifiedPath = QualifiedPath . getPath . unQualified

-- | Qualifies all symbols in an XObj in the given context.
qualify :: Context -> XObj -> Either QualificationError Qualified
qualify ctx xobj@(XObj obj info ty) =
  -- TODO: Merge this with setFullyQualifiedSymbols
  case obj of
    Lst [defn, (XObj (Sym (SymPath _ name) mode) symi symt), args, body] ->
      inner >>= \i -> setFullyQualifiedSymbols t g i (XObj (Lst [defn, (XObj (Sym (SymPath pathStrings name) mode) symi symt), args, body]) info ty)
    Lst [def, XObj (Sym (SymPath _ name) mode) symi symt, expr] ->
      inner >>= \i -> setFullyQualifiedSymbols t g i (XObj (Lst [def, (XObj (Sym (SymPath pathStrings name) mode) symi symt), expr]) info ty)
    _ -> inner >>= \i -> setFullyQualifiedSymbols t g i xobj
  where
    pathStrings :: [String]
    pathStrings = contextPath ctx
    t :: TypeEnv
    t = contextTypeEnv ctx
    g :: Env
    g = contextGlobalEnv ctx
    inner :: Either QualificationError Env
    inner = replaceLeft (FailedToQualifySymbols xobj) (E.getInnerEnv g pathStrings)

-- | Changes all symbols EXCEPT bound vars (defn names, variable names, etc) to their fully qualified paths.
-- | This must run after the 'setFullyQualifiedDefn' function has fixed the paths of all bindings in the environment.
-- | This function does NOT go into function-body scope environments and the like.
setFullyQualifiedSymbols :: TypeEnv -> Env -> Env -> XObj -> Either QualificationError Qualified
setFullyQualifiedSymbols t g e xobj =
  case qualified of
    Right qualifiedXObj -> Right $ Qualified $ qualifiedXObj
    err -> fmap Qualified err
  where
    qualified :: Either QualificationError XObj
    qualified =
      fmap unQualified $
        case xobjObj xobj of
          Lst ((XObj (Defn _) _ _) : _) ->
            qualifyFunctionDefinition t g e xobj
          Lst ((XObj (Fn _ _) _ _) : _) ->
            qualifyLambda t g e xobj
          Lst ((XObj The _ _) : _) ->
            qualifyThe t g e xobj
          Lst ((XObj Def _ _) : _) ->
            qualifyDef t g e xobj
          Lst ((XObj Let _ _) : _) ->
            qualifyLet t g e xobj
          Lst ((XObj (Match _) _ _) : _) ->
            qualifyMatch t g e xobj
          Lst ((XObj With _ _) : _) ->
            qualifyWith t g e xobj
          Lst _ ->
            qualifyLst t g e xobj
          Sym _ _ ->
            qualifySym t g e xobj
          Arr _ ->
            qualifyArr t g e xobj
          StaticArr _ ->
            qualifyStaticArr t g e xobj
          _ -> Right $ Qualified $ xobj

-- | The type of functions that qualify XObjs (forms/s-expressions).
type Qualifier = TypeEnv -> Env -> Env -> XObj -> Either QualificationError Qualified

-- Note to maintainers: liftM unQualified is used extensively throughout to
-- turn a Qualified XObj back into an XObj for further nesting. Recall that:
--
--     foo <- liftM unQualified x === foo <- pure . unQualified =<< x

-- | Qualify the symbols in a Defn form's body.
qualifyFunctionDefinition :: Qualifier
qualifyFunctionDefinition typeEnv globalEnv env x@(XObj (Lst [defn@(XObj (Defn _) _ _), sym@(XObj (Sym (SymPath _ functionName) _) _ _), args@(XObj (Arr argsArr) _ _), body]) i t) =
  -- For self-recursion, there must be a binding to the function in the inner env.
  -- It is marked as RecursionEnv basically is the same thing as external to not mess up lookup.
  -- Inside the recursion env is the function env that contains bindings for the arguments of the function.
  -- Note: These inner envs is ephemeral since they are not stored in a module or global scope.
  do
    recursionEnv <- fixLeft (pure (E.recursive (Just env) (Just (functionName ++ "-recurse-env")) 0))
    envWithSelf <- fixLeft (E.insertX recursionEnv (SymPath [] functionName) sym)
    -- Copy the use modules from the local env to ensure they are available from the function env.
    functionEnv <- fixLeft (pure ((E.nested (Just envWithSelf) (Just (functionName ++ "-function-env")) 0) {envUseModules = (envUseModules env)}))
    envWithArgs <- fixLeft (foldM (\e arg@(XObj (Sym path _) _ _) -> E.insertX e path arg) functionEnv argsArr)
    qualifiedBody <- liftM unQualified (setFullyQualifiedSymbols typeEnv globalEnv envWithArgs body)
    pure (Qualified (XObj (Lst [defn, sym, args, qualifiedBody]) i t))
  where
    fixLeft = replaceLeft (FailedToQualifyDeclarationName x)
qualifyFunctionDefinition _ _ _ xobj = Left $ FailedToQualifyDeclarationName xobj

-- | Qualify the symbols in a lambda body.
qualifyLambda :: Qualifier
qualifyLambda typeEnv globalEnv env x@(XObj (Lst [fn@(XObj (Fn _ _) _ _), args@(XObj (Arr argsArr) _ _), body]) i t) =
  let lvl = envFunctionNestingLevel env
      functionEnv = Env Map.empty (Just env) Nothing Set.empty InternalEnv (lvl + 1)
   in (replaceLeft (FailedToQualifySymbols x) (foldM (\e arg@(XObj (Sym path _) _ _) -> E.insertX e path arg) functionEnv argsArr))
        >>= \envWithArgs ->
          liftM unQualified (setFullyQualifiedSymbols typeEnv globalEnv envWithArgs body)
            >>= \qualifiedBody -> pure (Qualified (XObj (Lst [fn, args, qualifiedBody]) i t))
qualifyLambda _ _ _ xobj = Left $ FailedToQualifySymbols xobj

-- | Qualify the symbols in a The form's body.
qualifyThe :: Qualifier
qualifyThe typeEnv globalEnv env (XObj (Lst [the@(XObj The _ _), typeX, value]) i t) =
  do
    qualifiedValue <- liftM unQualified (setFullyQualifiedSymbols typeEnv globalEnv env value)
    pure (Qualified (XObj (Lst [the, typeX, qualifiedValue]) i t))
qualifyThe _ _ _ xobj = Left $ FailedToQualifySymbols xobj

-- | Qualify the symbols in a Def form's body.
qualifyDef :: Qualifier
qualifyDef typeEnv globalEnv env (XObj (Lst [def@(XObj Def _ _), sym, expr]) i t) =
  do
    qualifiedExpr <- liftM unQualified (setFullyQualifiedSymbols typeEnv globalEnv env expr)
    pure (Qualified (XObj (Lst [def, sym, qualifiedExpr]) i t))
qualifyDef _ _ _ xobj = Left $ FailedToQualifySymbols xobj

-- | Qualify the symbols in a Let form's bindings and body.
qualifyLet :: Qualifier
qualifyLet typeEnv globalEnv env x@(XObj (Lst [letExpr@(XObj Let _ _), bind@(XObj (Arr bindings) bindi bindt), body]) i t)
  | odd (length bindings) = Right $ Qualified $ XObj (Lst [letExpr, bind, body]) i t -- Leave it untouched for the compiler to find the error.
  | not (all isSym (evenIndices bindings)) = Right $ Qualified $ XObj (Lst [letExpr, bind, body]) i t -- Leave it untouched for the compiler to find the error.
  | otherwise =
    do
      let Just ii = i
          lvl = envFunctionNestingLevel env
          innerEnv = Env Map.empty (Just env) (Just ("let-env-" ++ show (infoIdentifier ii))) Set.empty InternalEnv lvl
      (innerEnv', qualifiedBindings) <- foldM qualifyBinding (innerEnv, []) (pairwise bindings)
      qualifiedBody <- liftM unQualified (setFullyQualifiedSymbols typeEnv globalEnv innerEnv' body)
      pure (Qualified (XObj (Lst [letExpr, XObj (Arr qualifiedBindings) bindi bindt, qualifiedBody]) i t))
  where
    qualifyBinding :: (Env, [XObj]) -> (XObj, XObj) -> Either QualificationError (Env, [XObj])
    qualifyBinding (e, bs) (s@(XObj (Sym path _) _ _), o@(XObj (Lst [(XObj (Fn _ _) _ _), _, _]) _ _)) =
      do
        -- Let bindings to anonymous functions may recursively call themselves,
        -- qualify the symbols appropriately by adding a recursion environment.
        -- e.g. (let [f (fn [x] (if (= x 1) x (f (dec x))))])
        -- Environment parenting is a bit nuanced here; the recursive reference
        -- needs to be stored in a recursive env to mark the symbol correctly.
        -- However, we also need to ensure captured variables are still marked
        -- as such, which is based on env nesting level, and we need to ensure
        -- the recursive reference isn't accidentally captured.
        let Just origin = E.parent e
        recursionEnv <- fixLeft (pure (E.recursive (Just e) (Just ("let-recurse-env")) 0))
        envWithSelf <- fixLeft (E.insertX recursionEnv path s)
        qualified <- liftM unQualified (setFullyQualifiedSymbols typeEnv globalEnv (E.setParent e (E.setParent origin envWithSelf)) o)
        updated <- (replaceLeft (FailedToQualifySymbols x) (E.insertX e path s))
        (pure (updated, bs ++ [s, qualified]))
      where
        fixLeft = replaceLeft (FailedToQualifyDeclarationName x)
    qualifyBinding (e, bs) (s@(XObj (Sym path _) _ _), o) =
      do
        qualified <- liftM unQualified (setFullyQualifiedSymbols typeEnv globalEnv e o)
        updated <- (replaceLeft (FailedToQualifySymbols x) (E.insertX e path s))
        (pure (updated, bs ++ [s, qualified]))
    qualifyBinding _ _ = error "bad let binding"
qualifyLet _ _ _ xobj = Left $ FailedToQualifySymbols xobj

-- | Qualify symbols in a Match form.
qualifyMatch :: Qualifier
qualifyMatch typeEnv globalEnv env (XObj (Lst (matchExpr@(XObj (Match _) _ _) : expr : casesXObjs)) i t)
  -- Leave it untouched for the compiler to find the error.
  | odd (length casesXObjs) = pure $ Qualified $ XObj (Lst (matchExpr : expr : casesXObjs)) i t
  | otherwise =
    do
      qualifiedExpr <- pure . unQualified =<< setFullyQualifiedSymbols typeEnv globalEnv env expr
      qualifiedCases <- pure . map (map unQualified) =<< mapM qualifyCases (pairwise casesXObjs)
      pure (Qualified (XObj (Lst (matchExpr : qualifiedExpr : concat qualifiedCases)) i t))
  where
    Just ii = i
    lvl = envFunctionNestingLevel env
    -- Create an inner environment for each case.
    innerEnv :: Env
    innerEnv = E.nested (Just env) (Just ("case-env-" ++ show (infoIdentifier ii))) lvl
    -- Qualify each case in the match form.
    qualifyCases :: (XObj, XObj) -> Either QualificationError [Qualified]
    qualifyCases (l@(XObj (Lst (_ : xs)) _ _), r) =
      do
        innerEnv' <- foldM foldVars innerEnv xs
        qualifiedLHS <- setFullyQualifiedSymbols typeEnv globalEnv innerEnv' l
        qualifiedRHS <- setFullyQualifiedSymbols typeEnv globalEnv innerEnv' r
        Right [qualifiedLHS, qualifiedRHS]
    qualifyCases (wild@(XObj (Sym (SymPath _ "_") _) _ _), r) =
      do
        qualifiedLHS <- foldVars env wild >>= \e -> setFullyQualifiedSymbols typeEnv globalEnv e wild
        qualifiedRHS <- setFullyQualifiedSymbols typeEnv globalEnv env r
        Right [qualifiedLHS, qualifiedRHS]
    qualifyCases (l, r) =
      do
        qualifiedLHS <- setFullyQualifiedSymbols typeEnv globalEnv env l
        qualifiedRHS <- setFullyQualifiedSymbols typeEnv globalEnv env r
        Right [qualifiedLHS, qualifiedRHS]
    -- Add variables in a case to its environment
    foldVars :: Env -> XObj -> Either QualificationError Env
    foldVars env' v@(XObj (Sym path _) _ _) = (replaceLeft (FailedToQualifySymbols v) (E.insertX env' path v))
    -- Nested sumtypes; fold recursively -- is there a more efficient way?
    foldVars _ (XObj (Lst (_ : ys)) _ _) = foldM foldVars innerEnv ys
    foldVars _ v = Left $ NonVariableInMatch v
qualifyMatch _ _ _ xobj = Left $ FailedToQualifySymbols xobj

-- | Qualify symbols in a With form.
qualifyWith :: Qualifier
qualifyWith typeEnv globalEnv env (XObj (Lst [XObj With _ _, XObj (Sym path _) _ _, expression]) _ _) =
  let useThese = envUseModules env
      env' = env {envUseModules = Set.insert path useThese}
   in setFullyQualifiedSymbols typeEnv globalEnv env' expression
qualifyWith _ _ _ xobj = Left $ FailedToQualifySymbols xobj

-- | Qualify symbols in a generic Lst form.
qualifyLst :: Qualifier
qualifyLst typeEnv globalEnv env (XObj (Lst xobjs) i t) =
  -- TODO: Perhaps this general case can be sufficient? No need with all the cases above..?
  do
    qualifiedXObjs <- liftM (map unQualified) (mapM (setFullyQualifiedSymbols typeEnv globalEnv env) xobjs)
    pure (Qualified (XObj (Lst qualifiedXObjs) i t))
qualifyLst _ _ _ xobj = Left $ FailedToQualifySymbols xobj

-- | Qualify a single symbol.
qualifySym :: Qualifier
-- Unqualified path.
qualifySym typeEnv globalEnv localEnv xobj@(XObj (Sym path@(SymPath _ name) _) i t) =
  ( ( ( replaceLeft
          (FailedToFindSymbol xobj)
          -- TODO: Why do we need getValue here? We should be able to restrict this
          -- search only to direct children of the type environment, but this causes
          -- errors.
          ( fmap (\(e, b) -> ((E.prj typeEnv), (E.prj e, b))) (E.searchType typeEnv path)
              <> fmap (localEnv,) (E.searchValue localEnv path)
              <> fmap (globalEnv,) (E.searchValue globalEnv path)
          )
      )
        >>= \(origin, (e, binder)) ->
          resolve (E.prj origin) (E.prj e) (binderXObj binder)
            >>= pure . Qualified
    )
      <> ((resolveMulti path (E.lookupInUsed localEnv globalEnv path)) >>= pure . Qualified)
      <> ((replaceLeft (FailedToFindSymbol xobj) (E.lookupContextually globalEnv path)) >>= (resolveMulti path) >>= pure . Qualified)
      <> ((resolveMulti path (E.lookupEverywhere globalEnv name)) >>= pure . Qualified)
      <> pure (Qualified xobj)
  )
  where
    resolve :: Env -> Env -> XObj -> Either QualificationError XObj
    resolve _ _ (XObj (Lst (XObj (Interface _ _) _ _ : _)) _ _) =
      -- Before we return an interface, double check that it isn't shadowed by a local let-binding.
      case (E.searchValue localEnv path) of
        Right (e, Binder _ _) ->
          case envMode e of
            InternalEnv -> pure (XObj (Sym (getPath xobj) (LookupLocal (captureOrNot e localEnv))) i t)
            _ -> pure (XObj (InterfaceSym name) i t)
        _ -> pure (XObj (InterfaceSym name) i t)
    resolve _ _ x@(XObj (Lst (XObj (External (Just overrideName)) _ _ : _)) _ _) =
      pure (XObj (Sym (getPath x) (LookupGlobalOverride overrideName)) i t)
    resolve _ _ (XObj (Mod modenv _) _ _) =
      nakedInit modenv
    resolve origin found xobj' =
      if (isTypeDef xobj')
        then
          ( (replaceLeft (FailedToFindSymbol xobj') (fmap (globalEnv,) (E.searchValue globalEnv path)))
              >>= \(origin', (e', binder)) -> resolve (E.prj origin') (E.prj e') (binderXObj binder)
          )
        else case envMode (E.prj found) of
          RecursionEnv -> pure (XObj (Sym (getPath xobj') LookupRecursive) i t)
          InternalEnv -> pure (XObj (Sym (getPath xobj') (LookupLocal (captureOrNot found origin))) i t)
          ExternalEnv -> pure (XObj (Sym (getPath xobj') (LookupGlobal (if isExternalFunction xobj' then ExternalCode else CarpLand) (definitionMode xobj'))) i t)
    resolveMulti :: (Show e, E.Environment e) => SymPath -> [(e, Binder)] -> Either QualificationError XObj
    resolveMulti _ [] =
      Left (FailedToFindSymbol xobj)
    resolveMulti _ [(e, b)] =
      resolve (E.prj e) (E.prj e) (binderXObj b)
    resolveMulti spath xs =
      let localOnly = remove (E.envIsExternal . fst) xs
          paths = map (getModuleSym . (second binderXObj)) xs
       in case localOnly of
            [] -> case spath of
              (SymPath [] _) ->
                Right $ XObj (MultiSym name paths) i t
              _ -> Left (QualifiedMulti spath)
            ys -> Left (LocalMulti spath (map (first E.prj) ys))
    nakedInit :: Env -> Either QualificationError XObj
    nakedInit e =
      maybe
        (Left (NakedInitForUnnamedModule (pathToEnv e)))
        (Right . id)
        ( envModuleName e
            >>= \name' ->
              pure (XObj (Sym (SymPath ((init (pathToEnv e)) ++ [name']) "init") (LookupGlobal CarpLand AFunction)) i t)
        )
    getModuleSym (_, x) =
      case x of
        XObj (Mod ev _) _ _ ->
          fromRight
            (SymPath (init (pathToEnv ev)) name)
            ( (replaceLeft (FailedToFindSymbol x) (E.searchType globalEnv (SymPath (init (pathToEnv ev)) name)))
                >> (fmap getPath (nakedInit ev))
            )
        _ -> (getPath x)
qualifySym _ _ _ xobj = Left $ FailedToQualifySymbols xobj

-- | Determine whether or not this symbol is captured in a local environment (closures).
captureOrNot :: Env -> Env -> CaptureMode
captureOrNot foundEnv localEnv =
  if envFunctionNestingLevel foundEnv < envFunctionNestingLevel localEnv
    then Capture (envFunctionNestingLevel localEnv - envFunctionNestingLevel foundEnv)
    else NoCapture

-- | Qualify an Arr form.
qualifyArr :: Qualifier
qualifyArr typeEnv globalEnv env (XObj (Arr array) i t) =
  do
    qualifiedArr <- liftM (map unQualified) (mapM (setFullyQualifiedSymbols typeEnv globalEnv env) array)
    pure (Qualified (XObj (Arr qualifiedArr) i t))
qualifyArr _ _ _ xobj = Left $ FailedToQualifySymbols xobj

-- | Qualify a StaticArr form.
qualifyStaticArr :: Qualifier
qualifyStaticArr typeEnv globalEnv env (XObj (StaticArr array) i t) =
  do
    qualifiedArr <- liftM (map unQualified) (mapM (setFullyQualifiedSymbols typeEnv globalEnv env) array)
    pure (Qualified (XObj (StaticArr qualifiedArr) i t))
qualifyStaticArr _ _ _ xobj = Left $ FailedToQualifySymbols xobj

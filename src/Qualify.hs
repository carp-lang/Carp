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

import Control.Monad (foldM)
import Data.List (foldl')
import Debug.Trace
import Env
import Info
import Lookup
import qualified Map
import Obj
import qualified Set
import Types
import Util

--------------------------------------------------------------------------------
-- Errors

-- | Error qualifying a symbol.
data QualificationError
  = FailedToQualifyDeclarationName XObj
  | FailedToQualifySymbols XObj
  | FailedToQualifyPath SymPath

instance Show QualificationError where
  show (FailedToQualifyDeclarationName xobj) =
    "Couldn't fully qualify the definition: " ++ pretty xobj
  show (FailedToQualifySymbols xobj) =
    "Couldn't fully qualify the symbols in the form: " ++ pretty xobj
  show (FailedToQualifyPath spath) =
    "Couldn't fully qualify the symbol: " ++ show spath
      ++ "in the given context."

--------------------------------------------------------------------------------
-- Data

-- | Denotes an XObj containing only symbols that *have been fully qualified*.
--
-- A fully qualified xobj **must not** be qualified further (e.g. using context
-- paths).
newtype Qualified = Qualified {unQualified :: XObj}

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
      setFullyQualifiedSymbols t g i (XObj (Lst [defn, (XObj (Sym (SymPath pathStrings name) mode) symi symt), args, body]) info ty)
    Lst [def, XObj (Sym (SymPath _ name) mode) symi symt, expr] ->
      setFullyQualifiedSymbols t g i (XObj (Lst [def, (XObj (Sym (SymPath pathStrings name) mode) symi symt), expr]) info ty)
    _ -> setFullyQualifiedSymbols t g i xobj
  where
    pathStrings :: [String]
    pathStrings = contextPath ctx
    t :: TypeEnv
    t = contextTypeEnv ctx
    g :: Env
    g = contextGlobalEnv ctx
    i :: Env
    i = getEnv g pathStrings

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

-- | Qualify the symbols in a Defn form's body.
qualifyFunctionDefinition :: Qualifier
qualifyFunctionDefinition typeEnv globalEnv env (XObj (Lst [defn@(XObj (Defn _) _ _), sym@(XObj (Sym (SymPath _ functionName) _) _ _), args@(XObj (Arr argsArr) _ _), body]) i t) =
  -- For self-recursion, there must be a binding to the function in the inner env.
  -- It is marked as RecursionEnv basically is the same thing as external to not mess up lookup.
  -- Inside the recursion env is the function env that contains bindings for the arguments of the function.
  -- Note: These inner envs is ephemeral since they are not stored in a module or global scope.
  let recursionEnv = Env Map.empty (Just env) (Just (functionName ++ "-recurse-env")) Set.empty RecursionEnv 0
      envWithSelf = extendEnv recursionEnv functionName sym
      functionEnv = Env Map.empty (Just envWithSelf) Nothing Set.empty InternalEnv 0
      envWithArgs = foldl' (\e arg@(XObj (Sym (SymPath _ argSymName) _) _ _) -> extendEnv e argSymName arg) functionEnv argsArr
   in setFullyQualifiedSymbols typeEnv globalEnv envWithArgs body
        >>= pure . unQualified
        >>= \qualifiedBody -> pure $ Qualified $ XObj (Lst [defn, sym, args, qualifiedBody]) i t
qualifyFunctionDefinition _ _ _ xobj = Left $ FailedToQualifyDeclarationName xobj

-- | Qualify the symbols in a lambda body.
qualifyLambda :: Qualifier
qualifyLambda typeEnv globalEnv env (XObj (Lst [fn@(XObj (Fn _ _) _ _), args@(XObj (Arr argsArr) _ _), body]) i t) =
  let lvl = envFunctionNestingLevel env
      functionEnv = Env Map.empty (Just env) Nothing Set.empty InternalEnv (lvl + 1)
      envWithArgs = foldl' (\e arg@(XObj (Sym (SymPath _ argSymName) _) _ _) -> extendEnv e argSymName arg) functionEnv argsArr
   in setFullyQualifiedSymbols typeEnv globalEnv envWithArgs body
        >>= pure . unQualified
        >>= \qualifiedBody -> pure $ Qualified $ XObj (Lst [fn, args, qualifiedBody]) i t
qualifyLambda _ _ _ xobj = Left $ FailedToQualifySymbols xobj

-- | Qualify the symbols in a The form's body.
qualifyThe :: Qualifier
qualifyThe typeEnv globalEnv env (XObj (Lst [the@(XObj The _ _), typeX, value]) i t) =
  setFullyQualifiedSymbols typeEnv globalEnv env value
    >>= pure . unQualified
    >>= \qualifiedValue -> pure $ Qualified $ XObj (Lst [the, typeX, qualifiedValue]) i t
qualifyThe _ _ _ xobj = Left $ FailedToQualifySymbols xobj

-- | Qualify the symbols in a Def form's body.
qualifyDef :: Qualifier
qualifyDef typeEnv globalEnv env (XObj (Lst [def@(XObj Def _ _), sym, expr]) i t) =
  setFullyQualifiedSymbols typeEnv globalEnv env expr
    >>= pure . unQualified
    >>= \qualifiedExpr -> pure $ Qualified $ XObj (Lst [def, sym, qualifiedExpr]) i t
qualifyDef _ _ _ xobj = Left $ FailedToQualifySymbols xobj

-- | Qualify the symbols in a Let form's bindings and body.
qualifyLet :: Qualifier
qualifyLet typeEnv globalEnv env (XObj (Lst [letExpr@(XObj Let _ _), bind@(XObj (Arr bindings) bindi bindt), body]) i t)
  | odd (length bindings) = Right $ Qualified $ XObj (Lst [letExpr, bind, body]) i t -- Leave it untouched for the compiler to find the error.
  | not (all isSym (evenIndices bindings)) = Right $ Qualified $ XObj (Lst [letExpr, bind, body]) i t -- Leave it untouched for the compiler to find the error.
  | otherwise =
    let Just ii = i
        lvl = envFunctionNestingLevel env
        innerEnv = Env Map.empty (Just env) (Just ("let-env-" ++ show (infoIdentifier ii))) Set.empty InternalEnv lvl
     in foldM qualifyBinding (innerEnv, []) (pairwise bindings)
          >>= \(innerEnv', qualifiedBindings) ->
            setFullyQualifiedSymbols typeEnv globalEnv innerEnv' body
              >>= pure . unQualified
              >>= \qualifiedBody -> pure $ Qualified $ XObj (Lst [letExpr, XObj (Arr qualifiedBindings) bindi bindt, qualifiedBody]) i t
  where
    qualifyBinding :: (Env, [XObj]) -> (XObj, XObj) -> Either QualificationError (Env, [XObj])
    qualifyBinding (e, bs) (s@(XObj (Sym (SymPath _ binderName) _) _ _), o) =
      setFullyQualifiedSymbols typeEnv globalEnv e o
        >>= pure . unQualified
        >>= \qualified -> pure $ (extendEnv e binderName s, bs ++ [s, qualified])
    qualifyBinding _ _ = error "bad let binding"
qualifyLet _ _ _ xobj = Left $ FailedToQualifySymbols xobj

-- | Qualify symbols in a Match form.
qualifyMatch :: Qualifier
qualifyMatch typeEnv globalEnv env (XObj (Lst (matchExpr@(XObj (Match _) _ _) : expr : casesXObjs)) i t)
  | odd (length casesXObjs) = pure $ Qualified $ XObj (Lst (matchExpr : expr : casesXObjs)) i t -- Leave it untouched for the compiler to find the error.
  | otherwise =
    setFullyQualifiedSymbols typeEnv globalEnv env expr
      >>= pure . unQualified
      >>= \qualifiedExpr ->
        mapM qualifyCases (pairwise casesXObjs)
          >>= pure . map (map unQualified)
          >>= \qualifiedCases -> pure $ Qualified $ XObj (Lst (matchExpr : qualifiedExpr : concat qualifiedCases)) i t
  where
    Just ii = i
    lvl = envFunctionNestingLevel env
    innerEnv :: Env
    innerEnv = Env Map.empty (Just env) (Just ("case-env-" ++ show (infoIdentifier ii))) Set.empty InternalEnv lvl
    qualifyCases :: (XObj, XObj) -> Either QualificationError [Qualified]
    qualifyCases (l@(XObj (Lst (_ : xs)) _ _), r) =
      do let innerEnv' = foldl' foldVars innerEnv xs
         qualifiedLHS <- setFullyQualifiedSymbols typeEnv globalEnv env l
         qualifiedRHS <- setFullyQualifiedSymbols typeEnv globalEnv innerEnv' r
         Right [qualifiedLHS, qualifiedRHS]
    qualifyCases (l, r) =
      do qualifiedLHS <- setFullyQualifiedSymbols typeEnv globalEnv env l
         qualifiedRHS <- setFullyQualifiedSymbols typeEnv globalEnv env r
         Right [qualifiedLHS, qualifiedRHS]
    foldVars :: Env -> XObj -> Env
    foldVars env' v@(XObj (Sym (SymPath _ binderName) _) _ _) = extendEnv env' binderName v
    -- Nested sumtypes
    -- fold recursively -- is there a more efficient way?
    foldVars _ (XObj (Lst (_ : ys)) _ _) = foldl' foldVars innerEnv ys
    foldVars _ v = error ("Can't match variable with " ++ show v)
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
  mapM (setFullyQualifiedSymbols typeEnv globalEnv env) xobjs
    >>= pure . map unQualified
    >>= \xobjs' -> pure $ Qualified $ XObj (Lst xobjs') i t
qualifyLst _ _ _ xobj = Left $ FailedToQualifySymbols xobj

-- | Qualify a single symbol.
-- TODO: Clean this up
qualifySym :: Qualifier
qualifySym typeEnv globalEnv localEnv xobj@(XObj (Sym path _) i t) =
  Right $
    Qualified $
      case path of
        -- Unqualified:
        SymPath [] name ->
          case lookupBinder path (getTypeEnv typeEnv) of
            Just (Binder _ (XObj (Lst (XObj (Interface _ _) _ _ : _)) _ _)) ->
              -- Found an interface with the same path!
              -- Have to ensure it's not a local variable with the same name as the interface
              case lookupInEnv path localEnv of
                Just (foundEnv, _) ->
                  if envIsExternal foundEnv
                    then createInterfaceSym name
                    else doesNotBelongToAnInterface False localEnv
                Nothing ->
                  --trace ("Will turn '" ++ show path ++ "' " ++ prettyInfoFromXObj xobj ++ " into an interface symbol.")
                  createInterfaceSym name
            _ ->
              doesNotBelongToAnInterface False localEnv
        -- Qualified:
        _ ->
          doesNotBelongToAnInterface False localEnv
  where
    createInterfaceSym name =
      XObj (InterfaceSym name) i t
    captureOrNot foundEnv =
      if envFunctionNestingLevel foundEnv < envFunctionNestingLevel localEnv
        then Capture (envFunctionNestingLevel localEnv - envFunctionNestingLevel foundEnv)
        else NoCapture
    doesNotBelongToAnInterface :: Bool -> Env -> XObj
    doesNotBelongToAnInterface finalRecurse theEnv =
      let results = multiLookupQualified path theEnv
          results' = removeThoseShadowedByRecursiveSymbol results
       in case results' of
            [] -> case envParent theEnv of
              Just p ->
                doesNotBelongToAnInterface False p
              Nothing ->
                -- OBS! The environment with no parent is the global env but it's an old one without the latest bindings!
                if finalRecurse
                  then xobj -- This was the TRUE global env, stop here and leave 'xobj' as is.
                  else doesNotBelongToAnInterface True globalEnv
            [(_, Binder _ foundOne@(XObj (Lst (XObj (External (Just overrideWithName)) _ _ : _)) _ _))] ->
              XObj (Sym (getPath foundOne) (LookupGlobalOverride overrideWithName)) i t
            [(e, Binder _ (XObj (Mod modEnv) _ _))] ->
              -- Lookup of a "naked" module name means that the Carp code is trying to
              -- instantiate a (nested) module with an implicit .init, e.g. (Pair 1 2)
              case envModuleName modEnv of
                Nothing -> error ("Can't get name from unqualified module path: " ++ show path)
                Just name ->
                  let pathHere = pathToEnv e
                   in XObj (Sym (SymPath (pathHere ++ [name]) "init") (LookupGlobal CarpLand AFunction)) i t
            [(e, Binder _ foundOne)] ->
              case envMode e of
                ExternalEnv ->
                  XObj
                    ( Sym
                        (getPath foundOne)
                        (LookupGlobal (if isExternalFunction foundOne then ExternalCode else CarpLand) (definitionMode foundOne))
                    )
                    i
                    t
                RecursionEnv -> XObj (Sym (getPath foundOne) LookupRecursive) i t
                _ ->
                  --trace ("\nLOCAL variable " ++ show (getPath foundOne) ++ ":\n" ++ prettyEnvironmentChain e) $
                  XObj (Sym (getPath foundOne) (LookupLocal (captureOrNot e))) i t
            multiple ->
              case filter (not . envIsExternal . fst) multiple of
                -- There is at least one local binding, use the path of that one:
                (e, Binder _ local) : _ -> XObj (Sym (getPath local) (LookupLocal (captureOrNot e))) i t
                -- There are no local bindings, this is allowed to become a multi lookup symbol:
                [] ->
                  -- (trace $ "Turned " ++ show path ++ " into multisym: " ++ joinWithComma (map (show . (\(e, b) -> (getPath (binderXObj b), safeEnvModuleName e, envMode e))) multiple)) $
                  case path of
                    (SymPath [] name) ->
                      -- Create a MultiSym!
                      XObj (MultiSym name (map (getPath . binderXObj . snd) multiple)) i t
                    pathWithQualifiers ->
                      -- The symbol IS qualified but can't be found, should produce an error later during compilation.
                      trace ("PROBLEMATIC: " ++ show path) (XObj (Sym pathWithQualifiers (LookupGlobal CarpLand AFunction)) i t)
    removeThoseShadowedByRecursiveSymbol :: [(Env, Binder)] -> [(Env, Binder)]
    removeThoseShadowedByRecursiveSymbol allBinders = visit allBinders allBinders
      where
        visit bs res =
          foldl'
            ( \result b ->
                case b of
                  (Env {envMode = RecursionEnv}, Binder _ xobj') ->
                    remove (\(_, Binder _ x) -> xobj' /= x && getName xobj' == getName x) result
                  _ -> result
            )
            res
            bs
qualifySym _ _ _ xobj = Left $ FailedToQualifySymbols xobj

-- | Qualify an Arr form.
qualifyArr :: Qualifier
qualifyArr typeEnv globalEnv env (XObj (Arr array) i t) =
  mapM (setFullyQualifiedSymbols typeEnv globalEnv env) array
    >>= pure . map unQualified
    >>= \array' -> pure $ Qualified $ XObj (Arr array') i t
qualifyArr _ _ _ xobj = Left $ FailedToQualifySymbols xobj

-- | Qualify a StaticArr form.
qualifyStaticArr :: Qualifier
qualifyStaticArr typeEnv globalEnv env (XObj (StaticArr array) i t) =
  mapM (setFullyQualifiedSymbols typeEnv globalEnv env) array
    >>= pure . map unQualified
    >>= \array' -> pure $ Qualified $ XObj (StaticArr array') i t
qualifyStaticArr _ _ _ xobj = Left $ FailedToQualifySymbols xobj

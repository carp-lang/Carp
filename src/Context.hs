module Context
  ( replaceGlobalEnv,
    replaceInternalEnv,
    replaceTypeEnv,
    replaceHistory,
    replaceProject,
    replacePath,
    replaceGlobalEnv',
    replaceInternalEnv',
    replaceTypeEnv',
    replaceHistory',
    insertInGlobalEnv,
    insertInGlobalEnv',
    insertInTypeEnv,
    insertInTypeEnv',
    insertInInternalEnv,
    innermostModuleEnv,
    bindLetDeclaration,
    lookupInterface,
    lookupBinderInGlobalEnv,
    lookupBinderInTypeEnv,
    lookupBinderInContextEnv,
    contextualize,
  )
where

import Env
import Lookup
import Obj
import Project
import Qualify (QualifiedPath, qualifyPath, unqualify)
import SymPath

--------------------------------------------------------------------------------
-- Contextual Class

-- | Class of symbol paths (identifiers) that can be made relative to a
-- context.
--
-- This class factors heavily in performing lookups in a given context
-- flexibly; certain portions of the codebase deliver fully qualified symbols
-- for lookup while others deliver an unqualified symbol that must be
-- contextualized before lookups are performed.
class Contextual a where
  contextualize :: a -> Context -> SymPath

-- | Unqualified paths are contextualized according to the current context.
instance Contextual SymPath where
  contextualize spath ctx = unqualify (qualifyPath ctx spath)

-- | Fully qualified paths require no further contextualization.
instance Contextual QualifiedPath where
  contextualize qpath _ = unqualify qpath

--------------------------------------------------------------------------------
-- Environment Replacement Functions

-- | Replace a context's internal environment with a new environment.
--
-- The previous environment is completely replaced and will not be recoverable.
replaceInternalEnv :: Context -> Env -> Context
replaceInternalEnv ctx env =
  ctx {contextInternalEnv = Just env}

-- | Replace a context's global environment with a new environment.
--
-- The previous environment is completely replaced and will not be recoverable.
replaceGlobalEnv :: Context -> Env -> Context
replaceGlobalEnv ctx env =
  ctx {contextGlobalEnv = env}

-- | Replace a context's type environment with a new environment.
--
-- The previous environment is completely replaced and will not be recoverable.
replaceTypeEnv :: Context -> TypeEnv -> Context
replaceTypeEnv ctx env =
  ctx {contextTypeEnv = env}

-- | Replace a context's history with a new history.
--
-- The previous history is completely replaced and will not be recoverable.
replaceHistory :: Context -> [XObj] -> Context
replaceHistory ctx hist =
  ctx {contextHistory = hist}

-- | Replace a context's project with a new project.
--
-- The previous project is completely replaced and will not be recoverable.
replaceProject :: Context -> Project -> Context
replaceProject ctx proj =
  ctx {contextProj = proj}

-- | Replace a context's path with a new path.
--
-- The previous path is completely replaced and will not be recoverable.
replacePath :: Context -> [String] -> Context
replacePath ctx paths =
  ctx {contextPath = paths}

-- | replaceInternalEnv with arguments flipped.
replaceInternalEnv' :: Env -> Context -> Context
replaceInternalEnv' = flip replaceInternalEnv

-- | replaceGlobalEnv with arguments flipped.
replaceGlobalEnv' :: Env -> Context -> Context
replaceGlobalEnv' = flip replaceGlobalEnv

-- | replaceTypeEnv with arguments flipped.
replaceTypeEnv' :: TypeEnv -> Context -> Context
replaceTypeEnv' = flip replaceTypeEnv

-- | replaceHistory with arguments flipped.
replaceHistory' :: [XObj] -> Context -> Context
replaceHistory' = flip replaceHistory

--------------------------------------------------------------------------------
-- Binding Insertion Functions

-- | Adds a binder to a context's global environment at a qualified path.
--
-- In most cases the qualified path will have been qualified under the same
-- context, but this constraint is *not* enforced by the definition of this
-- function.
insertInGlobalEnv :: Context -> QualifiedPath -> Binder -> Context
insertInGlobalEnv ctx qpath binder =
  let globalEnv = contextGlobalEnv ctx
   in ctx {contextGlobalEnv = envInsertAt globalEnv (unqualify qpath) binder}

-- | Adds a binder to a context's type environment at a qualified path.
--
-- In most cases the qualified path will have been qualified under the same
-- context, but this constraint is *not* enforced by the definition of this
-- function.
insertInTypeEnv :: Context -> QualifiedPath -> Binder -> Context
insertInTypeEnv ctx qpath binder =
  let typeEnv = getTypeEnv (contextTypeEnv ctx)
   in ctx {contextTypeEnv = TypeEnv (envInsertAt typeEnv (unqualify qpath) binder)}

-- | Adds a binder to a context's internal environment at an unqualified path.
--
-- If the context does not have an internal environment, this function does nothing.
insertInInternalEnv :: Context -> SymPath -> Binder -> Context
insertInInternalEnv ctx path@(SymPath [] _) binder =
  ctx {contextInternalEnv = fmap insert (contextInternalEnv ctx)}
  where
    insert :: Env -> Env
    insert e = envInsertAt e path binder
insertInInternalEnv _ _ _ =
  error "attempted to insert a qualified symbol into an internal environment"

-- | insertInGlobalEnv with arguments flipped.
insertInGlobalEnv' :: QualifiedPath -> Binder -> Context -> Context
insertInGlobalEnv' path binder ctx = insertInGlobalEnv ctx path binder

-- | insertInTypeEnv with arguments flipped.
insertInTypeEnv' :: QualifiedPath -> Binder -> Context -> Context
insertInTypeEnv' path binder ctx = insertInTypeEnv ctx path binder

-- | Inserts a let binding into the appropriate environment in a context.
bindLetDeclaration :: Context -> String -> XObj -> Context
bindLetDeclaration ctx name xobj =
  let binder = Binder emptyMeta (toLocalDef name xobj)
   in insertInInternalEnv ctx (SymPath [] name) binder

--------------------------------------------------------------------------------
-- Environment Retrieval Functions

-- | Retrieves the innermost (deepest) module environment in a context
-- according to the context's contextPath.
--
-- Returns Nothing if the Context path is empty.
innermostModuleEnv :: Context -> Maybe Env
innermostModuleEnv ctx = go (contextPath ctx)
  where
    go :: [String] -> Maybe Env
    go [] = Nothing
    go xs = Just $ getEnv (contextGlobalEnv ctx) xs

--------------------------------------------------------------------------------
-- Binder Lookup Functions

-- | Lookup a binder with a fully determined location in a context.
decontextualizedLookup :: (Context -> SymPath -> Maybe Binder) -> Context -> SymPath -> Maybe Binder
decontextualizedLookup f ctx path =
  f (replacePath ctx []) path

lookupInterface :: Context -> SymPath -> Maybe Binder
lookupInterface ctx path =
  decontextualizedLookup lookupBinderInTypeEnv ctx path

-- | Lookup a binder in a context's type environment.
--
-- Depending on the type of path passed to this function, further
-- contextualization of the path may be performed before the lookup is
-- performed.
lookupBinderInTypeEnv :: Contextual a => Context -> a -> Maybe Binder
lookupBinderInTypeEnv ctx path =
  let typeEnv = getTypeEnv (contextTypeEnv ctx)
      fullPath = contextualize path ctx
   in lookupBinder fullPath typeEnv

-- | Lookup a binder in a context's global environment.
--
-- Depending on the type of path passed to this function, further
-- contextualization of the path may be performed before the lookup is
-- performed.
lookupBinderInGlobalEnv :: Contextual a => Context -> a -> Maybe Binder
lookupBinderInGlobalEnv ctx path =
  let global = contextGlobalEnv ctx
      fullPath = contextualize path ctx
   in lookupBinder fullPath global

-- | Lookup a binder in a context's context environment.
--
-- Depending on the type of path passed to this function, further
-- contextualization of the path may be performed before the lookup is
-- performed.
lookupBinderInContextEnv :: Context -> SymPath -> Maybe Binder
lookupBinderInContextEnv ctx path =
  let ctxEnv = contextEnv ctx
      fullPath = contextualize path ctx
   in lookupBinder fullPath ctxEnv

module Context
  ( replaceGlobalEnv,
    replaceInternalEnv,
    replaceTypeEnv,
    replaceHistory,
    replaceProject,
    replaceGlobalEnv',
    replaceInternalEnv',
    replaceTypeEnv',
    replaceHistory',
    insertInGlobalEnv,
    insertInTypeEnv,
    insertIntoInternalEnv,
    innermostModuleEnv,
    bindLetDeclaration,
  )
where

import Env
import Obj
import SymPath
import Project

-- Environment replacement functions

-- | Replace a context's internal environment with a new environment.
-- The previous environment is completely replaced and will not be recoverable.
replaceInternalEnv :: Context -> Env -> Context
replaceInternalEnv ctx env =
  ctx {contextInternalEnv = Just env}

-- | Replace a context's global environment with a new environment.
-- The previous environment is completely replaced and will not be recoverable.
replaceGlobalEnv :: Context -> Env -> Context
replaceGlobalEnv ctx env =
  ctx {contextGlobalEnv = env}

-- | Replace a context's type environment with a new environment.
-- The previous environment is completely replaced and will not be recoverable.
replaceTypeEnv :: Context -> TypeEnv -> Context
replaceTypeEnv ctx env =
  ctx {contextTypeEnv = env}

-- | Replace a context's history with a new history.
-- The previous history is completely replaced and will not be recoverable.
replaceHistory :: Context -> [XObj] -> Context
replaceHistory ctx hist =
  ctx {contextHistory = hist}

-- | Replace a context's project with a new project.
-- The previous project is completely replaced and will not be recoverable.
replaceProject :: Context -> Project -> Context
replaceProject ctx proj =
  ctx {contextProj = proj}

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

-- Binding insertion functions

-- | Adds a binder to a context's global environment at the specified path.
insertInGlobalEnv :: Context -> SymPath -> Binder -> Context
insertInGlobalEnv ctx path binder =
  let globalEnv = contextGlobalEnv ctx
   in ctx {contextGlobalEnv = envInsertAt globalEnv path binder}

-- | Adds a binder to a context's type environment at the specified path.
insertInTypeEnv :: Context -> SymPath -> Binder -> Context
insertInTypeEnv ctx path binder =
  let typeEnv = getTypeEnv (contextTypeEnv ctx)
   in ctx {contextTypeEnv = TypeEnv (envInsertAt typeEnv path binder)}

-- | Adds a binder to a context's internal environment at the specified path.
-- If the context does not have an internal environment, this function does nothing.
insertIntoInternalEnv :: Context -> SymPath -> Binder -> Context
insertIntoInternalEnv ctx path binder =
  ctx {contextInternalEnv = fmap insert (contextInternalEnv ctx)}
  where insert :: Env -> Env
        insert e = envInsertAt e path binder

-- Specialized binding insertion functions

-- | Inserts a let binding into the appropriate environment in a context.
bindLetDeclaration :: Context -> String -> XObj -> Context
bindLetDeclaration ctx name xobj =
  let binder = Binder emptyMeta (toLocalDef name xobj)
   in insertIntoInternalEnv ctx (SymPath [] name) binder

-- Specialized environment retrieval functions

-- | Retrieves the innermost (deepest) module environment in a context
-- according to the context's contextPath.
-- Returns Nothing if the Context path is empty.
innermostModuleEnv :: Context -> Maybe Env
innermostModuleEnv ctx = go (contextPath ctx)
  where go :: [String] -> Maybe Env
        go [] = Nothing
        go xs = Just $ getEnv (contextGlobalEnv ctx) xs

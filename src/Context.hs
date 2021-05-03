module Context
  ( ContextError(..),
    replaceGlobalEnv,
    replaceInternalEnv,
    replaceTypeEnv,
    replaceHistory,
    replaceProject,
    replacePath,
    replaceGlobalEnv',
    replaceInternalEnv',
    replaceTypeEnv',
    replaceHistory',
    replacePath',
    insertInGlobalEnv,
    insertInGlobalEnv',
    insertTypeBinder,
    insertTypeBinder',
    insertInInternalEnv,
    insertType,
    replaceTypeBinder,
    innermostModuleEnv,
    bindLetDeclaration,
    lookupInterface,
    lookupBinderInGlobalEnv,
    lookupBinderInInternalEnv,
    lookupBinderInTypeEnv,
    lookupBinderInContextEnv,
    contextualize,
  )
where

import qualified Env as E
import Obj
import Project
import Qualify (QualifiedPath, qualifyPath, unqualify)
import SymPath
import Util (replaceLeft, joinWithPeriod)
import Debug.Trace
import Data.Bifunctor

--------------------------------------------------------------------------------
-- Errors

data ContextError = FailedToInsertInGlobalEnv SymPath Binder
                  | FailedToInsertInTypeEnv SymPath Binder
                  | FailedToInsertInInternalEnv SymPath Binder
                  | AttemptedToInsertQualifiedInternalBinder SymPath
                  | NoModuleEnvs String
                  | NotFoundGlobal SymPath
                  | NotFoundType SymPath
                  | NotFoundContext SymPath
                  | NotFoundInternal SymPath

insertFailure :: SymPath -> Binder -> String
insertFailure path binder =
  "Failed to insert the binder: " ++ show binder
    ++ " at path: " ++ show path

instance Show ContextError where
  show (FailedToInsertInGlobalEnv path binder) =
    insertFailure path binder
      ++ "in the context's global environment."
  show (FailedToInsertInTypeEnv path binder) =
    insertFailure path binder
      ++ "in the context's type environment."
  show (FailedToInsertInInternalEnv path binder) =
    insertFailure path binder
      ++ "in the context's internal environment."
  show (AttemptedToInsertQualifiedInternalBinder path) =
    "Attempted to insert a qualified binder: " ++ show path
      ++ " into a context's internal environment."
  show (NoModuleEnvs pathstring) =
    "Couldn't find any modules in the given context at path: "
      ++ pathstring
  show (NotFoundGlobal path) =
    "Couldn't find the symbol: " ++ show path
      ++ "in the context's global environment."
  show (NotFoundType path) =
    "Couldn't find the symbol: " ++ show path
      ++ "in the context's type environment."
  show (NotFoundContext path) =
    "Couldn't find the symbol: " ++ show path
      ++ "in the context's context environment."
  show (NotFoundInternal path) =
    "Couldn't find the symbol: " ++ show path
      ++ "in the context's internal environment."

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

-- | replacePath with arguments flipped.
replacePath' :: [String] -> Context -> Context
replacePath' = flip replacePath

--------------------------------------------------------------------------------
-- Binding Insertion Functions

-- | Adds a binder to a context's global environment at a qualified path.
--
-- In most cases the qualified path will have been qualified under the same
-- context, but this constraint is *not* enforced by the definition of this
-- function.
insertInGlobalEnv :: Context -> QualifiedPath -> Binder -> Either ContextError Context
insertInGlobalEnv ctx qpath binder =
  replaceLeft
    (FailedToInsertInGlobalEnv (unqualify qpath) binder)
    (E.insert (contextGlobalEnv ctx) (unqualify qpath) binder
       >>= \e -> pure $! (ctx {contextGlobalEnv = e}))

-- | Adds a binder to a context's type environment at a qualified path.
--
-- In most cases the qualified path will have been qualified under the same
-- context, but this constraint is *not* enforced by the definition of this
-- function.
insertTypeBinder :: Context -> QualifiedPath -> Binder -> Either ContextError Context
insertTypeBinder ctx qpath binder =
  let (SymPath path name) = unqualify qpath
   in first
        (\_ -> trace (show path) (FailedToInsertInTypeEnv (unqualify qpath) binder))
        (case path of
          [] ->
            (E.insert (contextTypeEnv ctx) (SymPath [] name) binder)
              >>= pure . (replaceTypeEnv ctx)
          -- TODO: We need to 'view' the global environment as a type
          -- environment here to ensure types are added to a module's type
          -- environment and not its value environment (the modality is
          -- correct)
          -- Find a more elegant API here.
          _ ->
            (E.insert (TypeEnv (contextGlobalEnv ctx)) (SymPath path name) binder)
              >>= pure . (replaceGlobalEnv ctx) . getTypeEnv)

replaceTypeBinder :: Context -> QualifiedPath -> Binder -> Either ContextError Context
replaceTypeBinder ctx qpath binder =
 let (SymPath path name) = unqualify qpath
  in first
       (\_ -> trace (show path) (FailedToInsertInTypeEnv (unqualify qpath) binder))
       ((E.replaceInPlace (contextTypeEnv ctx) name binder)
         >>= pure . (replaceTypeEnv ctx))
     <> insertTypeBinder ctx qpath binder

-- | Adds a binder to a context's internal environment at an unqualified path.
--
-- If the context does not have an internal environment, this function does nothing.
insertInInternalEnv :: Context -> SymPath -> Binder -> Either ContextError Context
insertInInternalEnv ctx path@(SymPath [] _) binder =
  maybe
    (Left (FailedToInsertInInternalEnv path binder))
    insert'
    (contextInternalEnv ctx)
  where
    insert' :: Env -> Either ContextError Context
    insert' e =
      replaceLeft
        (FailedToInsertInInternalEnv path binder)
        (E.insert e path binder >>= \e' -> pure (ctx {contextInternalEnv = pure e'}))
insertInInternalEnv _ path _ = Left (AttemptedToInsertQualifiedInternalBinder path)

-- | insertInGlobalEnv with arguments flipped.
insertInGlobalEnv' :: QualifiedPath -> Binder -> Context -> Either ContextError Context
insertInGlobalEnv' path binder ctx = insertInGlobalEnv ctx path binder

-- | insertTypeBinder with arguments flipped.
insertTypeBinder' :: QualifiedPath -> Binder -> Context -> Either ContextError Context
insertTypeBinder' path binder ctx = insertTypeBinder ctx path binder

-- | Inserts a let binding into the appropriate environment in a context.
bindLetDeclaration :: Context -> String -> XObj -> Either ContextError Context
bindLetDeclaration ctx name xobj =
  let binder = Binder emptyMeta (toLocalDef name xobj)
   in insertInInternalEnv ctx (SymPath [] name) binder

-- | Inserts a new type into a given context, adding a binding to the type
-- environment and a module to to value environment.
insertType :: Context -> QualifiedPath -> Binder -> Binder -> Either ContextError Context
insertType ctx qpath typeBinder modBinder =
  (insertInGlobalEnv ctx qpath modBinder)
     >>= \c -> (insertTypeBinder c qpath typeBinder)

--------------------------------------------------------------------------------
-- Environment Retrieval Functions

-- | Retrieves the innermost (deepest) module environment in a context
-- according to the context's contextPath.
--
-- Returns an error if the Context path is empty.
innermostModuleEnv :: Context -> Either ContextError Env
innermostModuleEnv ctx = go (contextPath ctx)
  where
    go :: [String] -> Either ContextError Env
    go [] = Left (NoModuleEnvs "")
    go xs = replaceLeft (NoModuleEnvs (joinWithPeriod xs)) (E.getInnerEnv (contextGlobalEnv ctx) xs)

--------------------------------------------------------------------------------
-- Binder Lookup Functions

-- | Lookup a binder with a fully determined location in a context.
decontextualizedLookup :: (Context -> SymPath -> Either ContextError Binder) -> Context -> SymPath -> Either ContextError Binder
decontextualizedLookup f ctx path =
  f (replacePath ctx []) path

-- | Lookup an interface in the given context.
lookupInterface :: Context -> SymPath -> Either ContextError Binder
lookupInterface ctx path =
  decontextualizedLookup lookupBinderInTypeEnv ctx path

-- | Lookup a binder in a context's type environment.
--
-- Depending on the type of path passed to this function, further
-- contextualization of the path may be performed before the lookup is
-- performed.
lookupBinderInTypeEnv :: Contextual a => Context -> a -> Either ContextError Binder
lookupBinderInTypeEnv ctx path =
  let typeEnv  = contextTypeEnv ctx
      global   = contextGlobalEnv ctx
      fullPath@(SymPath qualification name) = contextualize path ctx
      theType = (case qualification of 
                   [] -> E.getTypeBinder typeEnv name
                   _  -> E.searchTypeBinder global fullPath)
   in replaceLeft (NotFoundType fullPath) theType

-- | Lookup a binder in a context's global environment.
--
-- Depending on the type of path passed to this function, further
-- contextualization of the path may be performed before the lookup is
-- performed.
lookupBinderInGlobalEnv :: Contextual a => Context -> a -> Either ContextError Binder
lookupBinderInGlobalEnv ctx path =
  let global = contextGlobalEnv ctx
      fullPath = contextualize path ctx
   in replaceLeft (NotFoundGlobal fullPath) (E.searchValueBinder global fullPath)

-- | Lookup a binder in a context's internal environment.
lookupBinderInInternalEnv :: Contextual a => Context -> a -> Either ContextError Binder
lookupBinderInInternalEnv ctx path =
  let internal = contextInternalEnv ctx
      fullPath = contextualize path ctx
   in maybe (Left (NotFoundInternal fullPath))
            (\e -> replaceLeft (NotFoundInternal fullPath) (E.searchValueBinder e fullPath))
            internal

-- | Lookup a binder in a context's context environment.
--
-- Depending on the type of path passed to this function, further
-- contextualization of the path may be performed before the lookup is
-- performed.
lookupBinderInContextEnv :: Context -> SymPath -> Either ContextError Binder
lookupBinderInContextEnv ctx path =
  let ctxEnv = (E.contextEnv ctx)
      fullPath = contextualize path ctx
   in replaceLeft (NotFoundContext fullPath) (E.searchValueBinder ctxEnv fullPath)

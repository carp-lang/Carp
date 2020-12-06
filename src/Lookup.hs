module Lookup where

import qualified Data.Map as Map
import Data.Maybe (catMaybes, mapMaybe)
import qualified Meta
import Obj
import Types

-- | The type of generic lookup functions.
type LookupFunc a b = a -> Env -> [b]

-- | Find the Binder at a specified path.
lookupInEnv :: SymPath -> Env -> Maybe (Env, Binder)
lookupInEnv (SymPath [] name) env =
  case Map.lookup name (envBindings env) of
    Just found -> Just (env, found)
    Nothing -> case envParent env of
      Just parent -> lookupInEnv (SymPath [] name) parent
      Nothing -> Nothing
lookupInEnv path@(SymPath (p : ps) name) env =
  case Map.lookup p (envBindings env) of
    Just (Binder _ xobj) ->
      case xobj of
        (XObj (Mod modEnv) _ _) -> lookupInEnv (SymPath ps name) modEnv
        _ -> Nothing
    Nothing ->
      case envParent env of
        Just parent -> lookupInEnv path parent
        Nothing -> Nothing

-- | Like 'lookupInEnv' but only returns the Binder (no Env)
lookupBinder :: SymPath -> Env -> Maybe Binder
lookupBinder path env = snd <$> lookupInEnv path env

-- | Like 'lookupBinder' but return the Meta for the binder, or a default empty meta.
lookupMeta :: SymPath -> Env -> MetaData
lookupMeta path globalEnv =
  case lookupBinder path globalEnv of
    Just (Binder meta _) -> meta
    Nothing -> emptyMeta

-- | Get the Env stored in a binder, if any.
envFromBinder :: Binder -> Maybe Env
envFromBinder (Binder _ (XObj (Mod e) _ _)) = Just e
envFromBinder _ = Nothing

-- | Given an environment, returns the list of all environments of binders from
-- imported modules.
importedEnvs :: Env -> [Env]
importedEnvs env =
  catMaybes $ mapMaybe (\path -> fmap envFromBinder (lookupBinder path env)) (envUseModules env)

-- | Given an environment, returns the list of all environments of its binders.
allEnvs :: Env -> [Env]
allEnvs env =
  let envs = mapMaybe (envFromBinder . snd) (Map.toList (envBindings env))
   in envs ++ concatMap allEnvs envs

data LookWhere = Everywhere | OnlyImports

getEnvs :: LookWhere -> Env -> [Env]
getEnvs Everywhere = allEnvs
getEnvs OnlyImports = importedEnvs

-- | Given an environment, use a lookup function to recursively find all binders
-- in the environment that satisfy the lookup.
lookupMany :: LookWhere -> LookupFunc a b -> a -> Env -> [b]
lookupMany lookWhere lookf input env =
  let spine = lookf input env
      leaves = concatMap (lookf input) (getEnvs lookWhere env)
      above = case envParent env of
        Just parent -> lookupMany lookWhere lookf input parent
        Nothing -> []
   in spine ++ leaves ++ above

-- | Lookup binders by name in a single Env (no recursion),
lookupByName :: String -> Env -> [(Env, Binder)]
lookupByName name env =
  let filtered = Map.filterWithKey (\k _ -> k == name) (envBindings env)
   in map ((,) env . snd) (Map.toList filtered)

-- | Lookup binders that have specified metadata.
lookupByMeta :: String -> Env -> [Binder]
lookupByMeta key env =
  let filtered = Map.filter hasMeta (envBindings env)
   in map snd $ Map.toList filtered
  where
    hasMeta b = Meta.binderMember key b

-- | Given an interface, lookup all binders that implement the interface.
lookupImplementations :: SymPath -> Env -> [Binder]
lookupImplementations interface env =
  let binders = lookupByMeta "implements" env
   in filter isImpl binders
  where
    isImpl (Binder meta _) =
      case Meta.get "implements" meta of
        Just (XObj (Lst interfaces) _ _) -> interface `elem` map getPath interfaces
        _ -> False

-- | Find the possible (imported) symbols that could be referred to by a name.
multiLookupImports :: String -> Env -> [(Env, Binder)]
multiLookupImports = lookupMany OnlyImports lookupByName

-- | Find all symbols with a certain name, in *all* environments.
multiLookupEverywhere :: String -> Env -> [(Env, Binder)]
multiLookupEverywhere = lookupMany Everywhere lookupByName

-- | Enables look up "semi qualified" (and fully qualified) symbols.
-- | i.e. if there are nested environments with a function A.B.f
-- | you can find it by doing "(use A)" and then "(B.f)".
multiLookupQualified :: SymPath -> Env -> [(Env, Binder)]
multiLookupQualified (SymPath [] name) rootEnv =
  -- This case is just like normal multiLookup, we have a name but no qualifyers:
  multiLookupImports name rootEnv
multiLookupQualified path@(SymPath (p : _) _) rootEnv =
  case lookupInEnv (SymPath [] p) rootEnv of
    Just (_, Binder _ (XObj (Mod _) _ _)) ->
      -- Found a module with the correct name, that means we should not look at anything else:
      case lookupInEnv path rootEnv of
        Just found -> [found]
        Nothing -> []
    Just _ -> inexactMatch
    Nothing -> inexactMatch
  where
    inexactMatch =
      -- No exact match on the first qualifier, will look in various places for a match:
      let fromParent = case envParent rootEnv of
            Just parent -> multiLookupQualified path parent
            Nothing -> []
          fromUsedModules =
            let usedModules = envUseModules rootEnv
                envs = catMaybes $ mapMaybe (\path' -> fmap envFromBinder (lookupBinder path' rootEnv)) usedModules
             in concatMap (multiLookupQualified path) envs
       in fromParent ++ fromUsedModules

module Lookup where

import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import qualified Meta
import Obj
import Types

-- | The type of generic lookup functions.
type LookupFunc a = a -> Env -> [Binder]

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

-- | Recursively look through all environments for (def ...) forms.
findAllGlobalVariables :: Env -> [Binder]
findAllGlobalVariables env =
  concatMap finder (envBindings env)
  where
    finder :: Binder -> [Binder]
    finder def@(Binder _ (XObj (Lst (XObj Def _ _ : _)) _ _)) =
      [def]
    finder (Binder _ (XObj (Mod innerEnv) _ _)) =
      findAllGlobalVariables innerEnv
    finder _ =
      []

-- | Find all the possible (imported) symbols that could be referred to
multiLookup :: String -> Env -> [(Env, Binder)]
multiLookup = multiLookupInternal False

multiLookupALL :: String -> Env -> [(Env, Binder)]
multiLookupALL = multiLookupInternal True

-- TODO: Many of the local functions defined in the body of multiLookupInternal have been extracted.
-- Remove the duplication and define this in terms of the more generic/extracted functions.
{-# ANN multiLookupInternal "HLint: ignore Eta reduce" #-}

-- | The advanced version of multiLookup that allows for looking into modules that are NOT imported.
-- | Perhaps this function will become unnecessary when all functions can be found through Interfaces? (even 'delete', etc.)
multiLookupInternal :: Bool -> String -> Env -> [(Env, Binder)]
multiLookupInternal allowLookupInAllModules name rootEnv = recursiveLookup rootEnv
  where
    lookupInLocalEnv :: String -> Env -> Maybe (Env, Binder)
    lookupInLocalEnv n localEnv = case Map.lookup n (envBindings localEnv) of -- No recurse!
      Just b -> Just (localEnv, b)
      Nothing -> Nothing
    importsAll :: Env -> [Env]
    importsAll env =
      let envs = mapMaybe (envFromBinder . snd) (Map.toList (envBindings env))
       in envs ++ concatMap importsAll envs
    -- Only lookup in imported modules (nonrecursively!)
    importsNormal :: Env -> [Env]
    importsNormal env =
      mapMaybe (\path -> fmap getEnvFromBinder (lookupInEnv path env)) (envUseModules env)
    importsLookup :: Env -> [(Env, Binder)]
    importsLookup env =
      let envs = (if allowLookupInAllModules then importsAll else importsNormal) env
       in mapMaybe (lookupInLocalEnv name) envs
    recursiveLookup :: Env -> [(Env, Binder)]
    recursiveLookup env =
      let spine = case Map.lookup name (envBindings env) of
            Just found -> [(env, found)]
            Nothing -> []
          leaves = importsLookup env
          above = case envParent env of
            Just parent -> recursiveLookup parent
            Nothing -> []
       in --(trace $ "multiLookupInternal '" ++ name ++ "' " ++ show (envModuleName env) ++ ", spine: " ++ show (fmap snd spine) ++ ", leaves: " ++ show (fmap snd leaves) ++ ", above: " ++ show (fmap snd above))
          spine ++ leaves ++ above

envFromBinder :: Binder -> Maybe Env
envFromBinder (Binder _ (XObj (Mod e) _ _)) = Just e
envFromBinder _ = Nothing

-- | Given an environment, returns the list of all environments of binders from
-- imported modules.
importedEnvs :: Env -> [Env]
importedEnvs env =
  let envs = mapMaybe (envFromBinder . snd) (Map.toList (envBindings env))
   in envs ++ concatMap importedEnvs envs

-- | Given an environment, use a lookup function to recursively find all binders
-- in the environment that satisfy the lookup.
recursiveLookupAll :: a -> LookupFunc a -> Env -> [Binder]
recursiveLookupAll input lookf env =
  let spine = lookf input env
      leaves = concatMap (lookf input) (importedEnvs env)
      above = case envParent env of
        Just parent -> recursiveLookupAll input lookf parent
        Nothing -> []
   in spine ++ leaves ++ above

-- | Lookup binders by name.
lookupByName :: String -> Env -> [Binder]
lookupByName name env =
  let filtered = Map.filterWithKey (\k _ -> k == name) (envBindings env)
   in map snd $ Map.toList filtered

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
        Just (XObj (Lst interfaces) _ _) -> interface `elem` (map getPath interfaces)
        _ -> False

getEnvFromBinder :: (a, Binder) -> Env
getEnvFromBinder (_, Binder _ (XObj (Mod foundEnv) _ _)) = foundEnv
getEnvFromBinder (_, Binder _ err) = error ("Can't handle imports of non modules yet: " ++ show err)

-- | Enables look up "semi qualified" (and fully qualified) symbols.
-- | i.e. if there are nested environments with a function A.B.f
-- | you can find it by doing "(use A)" and then "(B.f)".
multiLookupQualified :: SymPath -> Env -> [(Env, Binder)]
multiLookupQualified (SymPath [] name) rootEnv =
  -- This case is just like normal multiLookup, we have a name but no qualifyers:
  multiLookup name rootEnv
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
                envs = mapMaybe (\path' -> fmap getEnvFromBinder (lookupInEnv path' rootEnv)) usedModules
             in concatMap (multiLookupQualified path) envs
       in fromParent ++ fromUsedModules

existingMeta :: Env -> XObj -> MetaData
existingMeta globalEnv xobj =
  case lookupInEnv (getPath xobj) globalEnv of
    Just (_, Binder meta _) -> meta
    Nothing -> emptyMeta

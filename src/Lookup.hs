module Lookup where

import Data.List (foldl')
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import qualified Meta
import Obj
import Text.EditDistance (defaultEditCosts, levenshteinDistance)
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

-- |
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
      let envs = mapMaybe (binderToEnv . snd) (Map.toList (envBindings env))
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

binderToEnv :: Binder -> Maybe Env
binderToEnv (Binder _ (XObj (Mod e) _ _)) = Just e
binderToEnv _ = Nothing

-- | Given an environment, returns the list of all environments of binders from
-- imported modules `(load "module-file.carp")`
importedEnvs :: Env -> [Env]
importedEnvs env =
  let envs = mapMaybe (binderToEnv . snd) (Map.toList (envBindings env))
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

-- | Add an XObj to a specific environment. TODO: rename to envInsert
extendEnv :: Env -> String -> XObj -> Env
extendEnv env name xobj = envAddBinding env name (Binder emptyMeta xobj)

-- | Add a Binder to an environment at a specific path location.
envInsertAt :: Env -> SymPath -> Binder -> Env
envInsertAt env (SymPath [] name) binder =
  envAddBinding env name binder
envInsertAt env (SymPath (p : ps) name) xobj =
  case Map.lookup p (envBindings env) of
    Just (Binder meta (XObj (Mod innerEnv) i t)) ->
      let newInnerEnv = Binder meta (XObj (Mod (envInsertAt innerEnv (SymPath ps name) xobj)) i t)
       in env {envBindings = Map.insert p newInnerEnv (envBindings env)}
    Just _ -> error ("Can't insert into non-module: " ++ p)
    Nothing -> error ("Can't insert into non-existing module: " ++ p)

envReplaceEnvAt :: Env -> [String] -> Env -> Env
envReplaceEnvAt _ [] replacement = replacement
envReplaceEnvAt env (p : ps) replacement =
  case Map.lookup p (envBindings env) of
    Just (Binder _ (XObj (Mod innerEnv) i t)) ->
      let newInnerEnv = Binder emptyMeta (XObj (Mod (envReplaceEnvAt innerEnv ps replacement)) i t)
       in env {envBindings = Map.insert p newInnerEnv (envBindings env)}
    Just _ -> error ("Can't replace non-module: " ++ p)
    Nothing -> error ("Can't replace non-existing module: " ++ p)

-- | Add a Binder to a specific environment.
envAddBinding :: Env -> String -> Binder -> Env
envAddBinding env name binder = env {envBindings = Map.insert name binder (envBindings env)}

{-# ANN addListOfBindings "HLint: ignore Eta reduce" #-}

-- | Add a list of bindings to an environment
addListOfBindings :: Env -> [(String, Binder)] -> Env
addListOfBindings env bindingsToAdd = foldl' (\e (n, b) -> envAddBinding e n b) env bindingsToAdd

-- | Get an inner environment.
getEnv :: Env -> [String] -> Env
getEnv env [] = env
getEnv env (p : ps) = case Map.lookup p (envBindings env) of
  Just (Binder _ (XObj (Mod innerEnv) _ _)) -> getEnv innerEnv ps
  Just _ -> error "Can't get non-env."
  Nothing -> error "Can't get env."

contextEnv :: Context -> Env
contextEnv Context {contextInternalEnv = Just e} = e
contextEnv Context {contextGlobalEnv = e, contextPath = p} = getEnv e p

-- | Checks if an environment is "external", meaning it's either the global scope or a module scope.
envIsExternal :: Env -> Bool
envIsExternal env =
  case envMode env of
    ExternalEnv -> True
    InternalEnv -> False
    RecursionEnv -> True

-- | Find out if a type is "external", meaning it is not defined by the user
--   in this program but instead imported from another C library or similar.
isExternalType :: TypeEnv -> Ty -> Bool
isExternalType typeEnv (PointerTy p) =
  isExternalType typeEnv p
isExternalType typeEnv (StructTy (ConcreteNameTy name) _) =
  case lookupInEnv (SymPath [] name) (getTypeEnv typeEnv) of
    Just (_, Binder _ (XObj (Lst (XObj (ExternalType _) _ _ : _)) _ _)) -> True
    Just _ -> False
    Nothing -> False
isExternalType _ _ =
  False

-- | Is this type managed - does it need to be freed?
isManaged :: TypeEnv -> Ty -> Bool
isManaged typeEnv (StructTy (ConcreteNameTy name) _) =
  (name == "Array") || (name == "StaticArray") || (name == "Dictionary")
    || ( case lookupInEnv (SymPath [] name) (getTypeEnv typeEnv) of
           Just (_, Binder _ (XObj (Lst (XObj (ExternalType _) _ _ : _)) _ _)) -> False
           Just (_, Binder _ (XObj (Lst (XObj (Deftype _) _ _ : _)) _ _)) -> True
           Just (_, Binder _ (XObj (Lst (XObj (DefSumtype _) _ _ : _)) _ _)) -> True
           Just (_, Binder _ (XObj wrong _ _)) -> error ("Invalid XObj in type env: " ++ show wrong)
           Nothing -> error ("Can't find " ++ name ++ " in type env.") -- TODO: Please don't crash here!
       )
isManaged _ StringTy = True
isManaged _ PatternTy = True
isManaged _ FuncTy {} = True
isManaged _ _ = False

-- | Is this type a function type?
isFunctionType :: Ty -> Bool
isFunctionType FuncTy {} = True
isFunctionType _ = False

-- | Is this type a struct type?
isStructType :: Ty -> Bool
isStructType (StructTy _ _) = True
isStructType _ = False

keysInEnvEditDistance :: SymPath -> Env -> Int -> [String]
keysInEnvEditDistance (SymPath [] name) env distance =
  let candidates = Map.filterWithKey (\k _ -> levenshteinDistance defaultEditCosts k name < distance) (envBindings env)
   in Map.keys candidates
keysInEnvEditDistance path@(SymPath (p : ps) name) env distance =
  case Map.lookup p (envBindings env) of
    Just (Binder _ xobj) ->
      case xobj of
        (XObj (Mod modEnv) _ _) -> keysInEnvEditDistance (SymPath ps name) modEnv distance
        _ -> []
    Nothing ->
      case envParent env of
        Just parent -> keysInEnvEditDistance path parent distance
        Nothing -> []

envReplaceBinding :: SymPath -> Binder -> Env -> Env
envReplaceBinding s@(SymPath [] name) binder env =
  case Map.lookup name (envBindings env) of
    Just _ ->
      envAddBinding env name binder
    Nothing ->
      case envParent env of
        Just parent -> env {envParent = Just (envReplaceBinding s binder parent)}
        Nothing -> env
envReplaceBinding (SymPath _ _) _ _ = error "TODO: cannot replace qualified bindings"

bindingNames :: Env -> [String]
bindingNames = concatMap select . envBindings
  where
    select :: Binder -> [String]
    select (Binder _ (XObj (Mod i) _ _)) = bindingNames i
    select (Binder _ obj) = [getName obj]

existingMeta :: Env -> XObj -> MetaData
existingMeta globalEnv xobj =
  case lookupInEnv (getPath xobj) globalEnv of
    Just (_, Binder meta _) -> meta
    Nothing -> emptyMeta

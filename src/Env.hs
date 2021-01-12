module Env where

import Data.List (foldl')
import Data.Maybe (fromMaybe)
import qualified Map
import Obj
import Types

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

envReplaceBinding :: SymPath -> Binder -> Env -> Env
envReplaceBinding s@(SymPath [] name) binder env =
  case Map.lookup name (envBindings env) of
    Just _ ->
      envAddBinding env name binder
    Nothing ->
      case envParent env of
        Just parent -> env {envParent = Just (envReplaceBinding s binder parent)}
        Nothing -> env
envReplaceBinding s@(SymPath (p : ps) name) binder env =
  case Map.lookup p (envBindings env) of
    Just b@(Binder _ (XObj (Mod innerEnv) i t)) ->
      envReplaceBinding (SymPath [] p) b {binderXObj = (XObj (Mod (envReplaceBinding (SymPath ps name) binder innerEnv)) i t)} env
    _ ->
      fromMaybe env (envParent env >>= \parent -> Just (env {envParent = Just (envReplaceBinding s binder parent)}))

envBindingNames :: Env -> [String]
envBindingNames = concatMap select . envBindings
  where
    select :: Binder -> [String]
    select (Binder _ (XObj (Mod m) _ _)) = envBindingNames m
    select (Binder _ obj) = [getName obj]

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

{-# LANGUAGE TupleSections #-}

module Env
  (EnvironmentError,
   Environment,
   next,
   prj,
   -- creation
   empty,
   new,
   parent,
   setParent,
   nested,
   recursive,
   binders,
   update,
   children,
   ancestors,
   -- getter
   getInnerEnv,
   contextEnv,
   -- utility
   envIsExternal,
   envBindingNames,
   envPublicBindingNames,
   -- mutation
   insert,
   insertX,
   insertNestedType,
   replace,
   addBinding,
   addListOfBindings,
   replaceEnvironment,
   addUsePath,
   -- finds
   find,
   findPoly,
   findAllByName,
   findAllByMeta,
   findChildren,
   findImplementations,
   findAllGlobalVariables,
   findModules,
   findUnifiable,
   allImportedEnvs,
   -- lookups
   Env.lookup,
   lookupBinder,
   lookupType,
   lookupContextually,
   lookupMeta,
   lookupChildren,
   lookupInUsed,
   lookupEverywhere,
   lookupBinderEverywhere,
   progenitor,
   replaceInPlace,
   insertInPlace,
  ) where

import Data.List (foldl', unfoldr)
import Data.Maybe (fromMaybe)
import qualified Map
import Obj
import Types
import qualified Set
import Data.Either (fromRight, rights)
import qualified Meta

--------------------------------------------------------------------------------
-- Data

data EnvironmentError = NoEnvInNonModule
                      | NoReplaceInNonModule
                      | BindingNotFound String Env
                      | NoMatchingBindingFound String
                      | NestedTypeError String

instance Show EnvironmentError where
  show NoEnvInNonModule = "Can't get an environment from a non-module."
  show NoReplaceInNonModule = "Can't replace an environment in a non-module."
  show (BindingNotFound name e) = "Failed to find " ++ name ++ "in the given environment: " ++ show e
  show (NoMatchingBindingFound predicate) = "Couldn't find any bindings with " ++ predicate ++ "in the given environment."
  show (NestedTypeError name) =
    "Couldn't insert the top-level type " ++ name
      ++ " in a module environment."

--------------------------------------------------------------------------------
-- The Environment Class and Utilities

-- | Class for generically handling type and value environments.
class Environment e where
  inj        :: Env -> e
  prj        :: e -> Env
  update     :: e -> Binder -> Either EnvironmentError Binder

-- | The value environment
instance Environment Env where
  inj = id
  prj = id
  update e (Binder meta (XObj (Mod _ et) i t)) = Right (Binder meta (XObj (Mod e et) i t))
  update _ _ = Left NoReplaceInNonModule

-- | The type environment
instance Environment TypeEnv where
  inj = TypeEnv
  prj = getTypeEnv
  update e (Binder meta (XObj (Mod ev _) i t)) = Right (Binder meta (XObj (Mod ev e) i t))
  update _ _ = Left NoReplaceInNonModule

-- | Returns an unnamed empty environment with no parent.
empty :: Environment e => e
empty = inj $ Env (Map.fromList []) Nothing Nothing Set.empty ExternalEnv 0

-- | Returns a new environment with a given parent and name.
new :: Environment e => Maybe e -> Maybe String -> e
new p name =
  let e' = Env (Map.fromList []) (fmap prj p) name Set.empty ExternalEnv 0
   in inj e'

-- | Returns a new environment with a designated nesting level.
nested :: Environment e => Maybe e -> Maybe String -> Int -> e
nested p name lvl = inj ((prj (new p name)) { envMode = InternalEnv, envFunctionNestingLevel = lvl })

-- | Returns a new recursive environment with a designated nesting level.
recursive :: Environment e => Maybe e -> Maybe String -> Int -> e
recursive p name lvl = inj ((prj (new p name)) { envMode = RecursionEnv, envFunctionNestingLevel = lvl })

-- | Returns the binders stored in an environment.
binders :: Environment e => e -> Map.Map String Binder
binders = envBindings . prj

-- | Get the parent of an environment.
parent :: Environment e => e -> Maybe e
parent = fmap inj . envParent . prj

-- | Set the parent of an environment.
setParent :: Environment e => e -> e -> e
setParent e p = inj ((prj e) {envParent = Just (prj p)})

-- | Get the next environment in a path chain.
--
-- The first argument is ignored. It's used to fix the type.
next :: Environment e => e -> String -> Either EnvironmentError (e, Binder)
next e n =
  (find e n)
  >>= \b -> (case b of
               (Binder _ (XObj (Mod ev _) _ _)) -> Right (inj ev, b)
               _ -> Left NoEnvInNonModule)

data Mode = Types | Values

get :: Mode -> Binder -> Either EnvironmentError Env
get Types (Binder _ (XObj (Mod _ et) _ _)) = Right $ prj et
get Values (Binder _ (XObj (Mod ev _) _ _)) = Right $ prj ev
get _ _ = Left NoEnvInNonModule

updateEnv :: Mode -> Env -> Binder -> Either EnvironmentError Binder
updateEnv Values e (Binder meta (XObj (Mod _ et) i t)) = Right (Binder meta (XObj (Mod e et) i t))
updateEnv Types e (Binder meta (XObj (Mod ev _) i t)) = Right (Binder meta (XObj (Mod ev (TypeEnv e)) i t))
updateEnv _ _ _ = Left NoEnvInNonModule

--------------------------------------------------------------------------------
-- Environment traversal

type EnvironmentRetriever e = (e -> String -> Either EnvironmentError (e, Binder))
type EnvironmentProducer e = (e -> String -> Binder -> Either EnvironmentError e)
type EnvironmentMutator   e = (e -> Binder -> Either EnvironmentError Binder)

-- |
walk :: Environment e => (Maybe (EnvironmentRetriever e)) -> e -> SymPath -> Either EnvironmentError e
walk _ e (SymPath [] _) = pure e
walk f e (SymPath (p : ps) name) =
  retriever e p
    >>= \(nextEnv, _) -> walk f nextEnv (SymPath ps name)
  where retriever = fromMaybe next f

-- | Given an environment and a complete identifier path, traverse a chain of
-- environments until the path is exhausted, if requested, mutating the
-- environments along the way:
--
-- f: 'env retriever' function for retrieving the next environment. next if not provided.
-- g: 'env producer' function for producing a new environment using the result of f. id if not provided.
-- h: 'mutator' function for producing a new module binder using the result of g during traversal. id if not provided
mutate :: Environment e => (Maybe (EnvironmentRetriever e)) -> (Maybe (EnvironmentProducer e)) -> (Maybe (EnvironmentMutator e)) -> e -> SymPath -> Binder -> Either EnvironmentError e
mutate f g h e path binder = go path
  where
        go (SymPath [] name)       = producer e name binder
        go (SymPath (p : ps) name) =
          retriever e p
            >>= \(oldEnv, modBinder) -> mutate f g h oldEnv (SymPath ps name) binder
            >>= \result -> mutator result modBinder
            >>= producer e p
        retriever = fromMaybe next f
        producer  = fromMaybe (\e' _ _ -> pure e') g
        mutator   = fromMaybe (\_ old -> pure old) h

--------------------------------------------------------------------------------
-- Binding lookup functions
--
-- find* functions perform lookup in a single environment, without recursion.
-- lookup* functions perform lookups in an environment chain, with recursion.

-- | Lookup a binder by name in the given environment.
--
-- Returns an error if not found.
find :: Environment e => e -> String -> Either EnvironmentError Binder
find e name =
  case Map.lookup name (binders e) of
    Nothing -> Left $ BindingNotFound name (prj e)
    Just b  -> Right b

-- | Lookup a binder at a given path in an environment.
-- If the binder is not found in the initial walk, performs a fallback walk in
-- the environment's parent.
--
-- Returns an error if not found.
lookup :: Environment e => e -> SymPath -> Either EnvironmentError (e, Binder)
lookup e path@(SymPath _ name) =
  case (walk Nothing e path) of
    Right e' ->
      (find e' name >>= \binder -> pure (e', binder))
        <> checkParent (Left (BindingNotFound name (prj e))) e'
    Left err -> checkParent (Left err) e
  where checkParent err env =
          fromMaybe
            err
            (parent env >>= \p -> pure (Env.lookup p path))

lookupType :: Env -> TypeEnv -> SymPath -> Either EnvironmentError (TypeEnv, Binder)
lookupType e t p@(SymPath [] _) =
 Env.lookup t p
 <> (case parent t of
        Nothing -> Env.lookup t p
        Just t' -> lookupType e t' p)
lookupType e _ (SymPath [p] name) =
  (find e p)
  >>= \(Binder _ (XObj (Mod _ et) _ _)) -> find et name >>= \b -> pure (et, b)
lookupType e _ (SymPath ps name) =
  let steps = init ps
      end   = last ps
   in (walk Nothing e (SymPath (init steps) (last steps)))
        >>= \e' -> find e' end >>= \(Binder _ (XObj (Mod _ et) _ _)) ->
          find et name >>= \b -> pure (et, b)

lookupBinder :: Environment e => e -> SymPath -> Either EnvironmentError Binder
lookupBinder e path = Env.lookup e path >>= pure . snd

-- | Get the metadata associated with the binder at the specified path in an environment.
lookupMeta :: Environment e => e -> SymPath -> Either EnvironmentError MetaData
lookupMeta e path = lookupBinder e path >>= pure . Meta.fromBinder

-- | Performs a lookup for a single binder, wrapping the result in a list.
findAllByName :: Environment e => e -> String -> Either EnvironmentError [(e, Binder)]
findAllByName e name = find e name >>= \b -> pure [(e, b)]

-- | Find all binders in an environment that have a specified meta key.
findAllByMeta :: Environment e => e -> String -> Either EnvironmentError [Binder]
findAllByMeta e metaKey =
  let candidates = Map.elems (Map.filter (Meta.binderMember metaKey) (binders e))
   in case candidates of
        [] -> Left (NoMatchingBindingFound ("metadata " ++ metaKey))
        _  -> Right $ candidates

findModules :: Environment e => e -> [XObj]
findModules e =
  map binderXObj (filter modsOnly (Map.elems (binders e)))
  where modsOnly binder =
          case binderXObj binder of
            XObj (Mod _ _) _ _ -> True
            _ -> False

-- | It's more efficient to specialize this function as it can take advantage
-- of laziness; once we found the candidate function for a polymorphic
-- function, there's no need to consume the rest of the environment.
findPoly :: Environment e => e -> String -> Ty -> Either EnvironmentError (e, Binder)
findPoly env name ty =
  case find env name of
    Right b -> if unify b
                 then Right (env, b)
                 else (foldl' go (Left (BindingNotFound name (prj env))) (findChildren env))
    Left  _ -> foldl' go (Left (BindingNotFound name (prj env))) (findChildren env)
  where go x e = x <> (findPoly e name ty)
        unify = areUnifiable ty . fromMaybe Universe . xobjTy . binderXObj

findUnifiable :: Environment e => [e] -> String -> Ty -> Either EnvironmentError (e, Binder)
findUnifiable es name ty =
  foldl' go (Left (NoMatchingBindingFound ("types unifiable with " ++ show ty))) es
  where go previous e =
          case find e name of
            Right b -> if unify b then previous <> (Right (e, b)) else previous
            Left err -> previous <> Left err
        unify = areUnifiable ty . fromMaybe Universe . xobjTy . binderXObj

-- | Find all environments that are *direct* children of an environment (one
-- level down).
--
-- N.B. Don't use find here. We access binders directly, so there's no need to
-- perform additional O(n) lookup calls.
findChildren :: Environment e => e -> [e]
findChildren e =
  foldl' getEnv [] (binders e)
  where getEnv acc (Binder _ (XObj (Mod me _) _ _)) = ((inj me):acc)
        getEnv acc _ = acc

children :: Environment e => e -> [e]
children e =
  foldl' getEnv [] (binders e)
  where getEnv acc (Binder _ (XObj (Mod me _) _ _)) = ((inj me):acc)
        getEnv acc _ = acc

-- | Find all the environments contained in the modules initial environment,
-- plus any module environments contained in *those* modules.
lookupChildren :: Environment e => e -> [e]
lookupChildren e =
  foldl' go [] (findChildren e)
  where go acc e' = case findChildren e' of
                     [] -> (e':acc)
                     xs -> (foldl' go [] xs ++ acc)

-- | Find all the modules designated by the use paths in an environment.
findImportedEnvs :: Environment e => e -> [e]
findImportedEnvs e =
  let usePaths = Set.toList (envUseModules (prj e))
      used = fmap (\p -> walk Nothing e p >>= \e' -> next e' (getName' p) >>= pure . fst) usePaths -- fmap (\p -> lookupBinder e p >>= next e) usePaths
   in (rights used)
   where getName' (SymPath _ name) = name

-- | Given an environment, get its topmost parent up the environment chain.
--
-- For nearly all environments, this should be the global environment.
progenitor :: Environment e => e -> e
progenitor e = fromMaybe e (parent e >>= \p -> pure (progenitor p))

-- | Find all possible environments imported at some point *upwards* from e in a chain of environments.
allImportedEnvs :: Environment e => e -> Env -> [e]
allImportedEnvs e global =
  let paths = (Set.toList (foldl' og (envUseModules (prj e)) (unfoldr go (prj e))))
   in (rights (map get' paths))
  where go e' = parent e' >>= \p -> pure (p,p)
        og acc e' = (envUseModules e') <> acc
        get' path = (walk Nothing (inj global) path >>= \e' -> next e' (getName' path) >>= pure . fst)
        getName' (SymPath _ name) = name

ancestors :: Environment e => e -> [e]
ancestors e = unfoldr go e
  where go e' = (parent e') >>= \p -> pure (p,p)

-- | Find all binders the implement a given interface, designated by its path.
findImplementations :: Environment e => e -> SymPath -> Either EnvironmentError [Binder]
findImplementations e interface =
  ((findAllByMeta e "implements")
    >>= \is -> (pure (filter (isImpl . Meta.fromBinder) is)))
  <> Left (NoMatchingBindingFound ("implementation meta for " ++ show interface))
  where isImpl :: MetaData -> Bool
        isImpl meta =
          case Meta.get "implements" meta of
            Just (XObj (Lst interfaces) _ _) -> interface `elem` map getPath interfaces
            _ -> False

-- | Searches for binders exhaustively in the given environment, a list of
-- child environments it contains derived using a function and its parent, if
-- it has one.
--
-- The parent environment, when it exists, is also searched exhaustively
-- (derived children of the parent are searched, as well as the parent of the
-- parent, should it exist).
lookupExhuastive :: Environment e => (e -> [e]) -> e -> String -> [(e, Binder)]
lookupExhuastive f e name =
  let envs   = [e] ++ (f e)
   in (go (parent e) envs)
   where go _ [] = []
         go Nothing xs = foldl' accum [] xs
         go (Just p) xs = go (parent p) (xs ++ [p] ++ (f p))
         accum acc e' = case find e' name of
                          Right b -> ((e', b):acc)
                          _ -> acc

lookupBinderExhuastive :: Environment e => (e -> [e]) -> e -> String -> [Binder]
lookupBinderExhuastive f e name = fmap snd (lookupExhuastive f e name)

lookupEverywhere :: Environment e => e -> String -> [(e, Binder)]
lookupEverywhere = lookupExhuastive lookupChildren

lookupInImports :: Environment e => e -> String -> [(e, Binder)]
lookupInImports = lookupExhuastive findImportedEnvs

lookupInUsed :: Environment e => e -> Env -> SymPath -> [(e, Binder)]
lookupInUsed e global spath =
  foldl' go [] (allImportedEnvs e global)
  where go :: Environment e => [(e, Binder)] -> e -> [(e, Binder)]
        go acc e' = case (Env.lookup e' spath) of
                      Right (e'', b) -> ((e'', b):acc)
                      _ -> acc

-- | Lookup a binder in *all* possible environments in the chain of an initial
-- environment (parents and children, including Use modules).
lookupBinderEverywhere :: Environment e => e -> String -> [Binder]
lookupBinderEverywhere = lookupBinderExhuastive lookupChildren

lookupContextually :: Environment e => e -> SymPath -> Either EnvironmentError [(e, Binder)]
lookupContextually e (SymPath [] name) =
  case lookupInImports e name of
    [] -> Left (BindingNotFound name (prj e))
    xs -> Right xs
lookupContextually e path@(SymPath (p : ps) name) =
  lookupDirectly <> lookupInUsedAndParent
  where lookupDirectly = (next e p)
                           >>= \(e', _) -> Env.lookup e' (SymPath ps name)
                           >>= pure . (:[])
        lookupInUsedAndParent = case rights (fmap ((flip Env.lookup) path) (findImportedEnvs e)) of
                                  [] -> Left (BindingNotFound name (prj e))
                                  xs ->
                                    case parent e of
                                      Nothing -> Right xs
                                      Just e' -> (Env.lookup e' path >>= \found -> Right $ xs ++ [found]) <> Right xs

--------------------------------------------------------------------------------
-- Environment mutation functions

-- | Add a new binding to an environment.
--
-- N.B. This function returns an Either for compatibility with other functions
-- in this module. It is a constant function in the codomain of Either, as it
-- always returns Right.
addBinding :: Environment e => e -> String -> Binder -> Either EnvironmentError e
addBinding e name b = Right $ inj ((prj e) {envBindings = Map.insert name b (binders e)})

-- | Replace the value of a binding in an environment, but only if it already
-- exists, otherwise, return an error.
replaceBinding :: Environment e => e -> String -> Binder -> Either EnvironmentError e
replaceBinding e name b =
  pure (inj ((prj e) {envBindings = Map.adjust (const b) name (binders e)}))

-- | Insert a binding into an environment at the given path.
insert :: Environment e => e -> SymPath -> Binder -> Either EnvironmentError e
insert = mutate Nothing (Just addBinding) (Just update)

-- | Insert an XObj into an environment at the specified path.
-- This function does not perform insertions into parents.
insertX :: Environment e => e -> SymPath -> XObj -> Either EnvironmentError e
insertX e path x = insert e path (toBinder x)

-- | Replaces a binding "in-place" in an environment chain.
--
-- This function *only* considers members of an environment chain, that is,
-- it's limited to the given input environment and all of its ancestors (it's
-- parent and the parent of its parent all the way up).
--
-- It does not look in any "external" environments (used environments or
-- "children" (environments stored in module bindings)).
replaceInPlace :: Environment e => e -> String -> Binder -> Either EnvironmentError e
replaceInPlace e name b =
  (find e name >>= \_ -> addBinding e name b)
  <> case parent e of
       Just p  -> replaceInPlace p name b >>= \p' -> pure (inj ((prj e) {envParent = Just (prj p')}))
       Nothing -> Left (BindingNotFound name (prj e))
-- |
insertInPlace :: Environment e => e -> String -> Binder -> Either EnvironmentError e
insertInPlace e name b =
  case parent e of
       Just p  -> insertInPlace p name b >>= \p' -> pure (inj ((prj e) {envParent = Just (prj p')}))
       Nothing -> insert e (SymPath [] name) b

-- | Insertion of module types.
--
-- TODO: Handle this case generically.
insertNestedType :: Env -> SymPath -> Binder -> Either EnvironmentError Binder
insertNestedType _ (SymPath [] name) _ = (Left (NestedTypeError name))
insertNestedType ve (SymPath [p] name) binder =
  (find ve p)
  >>= \binder' -> get Types binder'
  >>= \env     -> addBinding env name binder
  >>= \env'    -> updateEnv Types env' binder'
insertNestedType ve (SymPath ps name) binder =
  let target = last ps
      steps  = init ps
   in (walk Nothing ve (SymPath steps target))
        >>= \me -> insertNestedType me (SymPath [target] name) binder

-- | Replace a binding at the given path in an environment.
replace :: Environment e => e -> SymPath -> Binder -> Either EnvironmentError e
replace = mutate Nothing (Just replaceBinding) (Just update)

-- | Add a list of bindings to an environment.
addListOfBindings :: Environment e => e -> [(String, Binder)] -> e
addListOfBindings e bindings =
  foldl' (\e' (n, b) -> fromRight e (addBinding e' n b)) e bindings

-- | Replace an environment at a given path.
replaceEnvironment :: Environment e => e -> [String] -> e -> Either EnvironmentError e
replaceEnvironment _ [] r = Right r
replaceEnvironment e (p : ps) r =
  (next e p)
  >>= \(oldEnv, modBinder) -> ((flip update) modBinder =<< (replaceEnvironment oldEnv ps r))
  >>= replaceBinding e p

addUsePath :: Environment e => e -> SymPath -> e
addUsePath e path = inj ((prj e) {envUseModules = Set.insert path (envUseModules (prj e))})

--------------------------------------------------------------------------------
-- Environment retrieval functions

-- | Get the environment at a given path that corresponds to the type of an
-- initial environment.
--
-- Returns the initial environment when given an empty path.
getInnerEnv :: Environment e => e -> [String] -> Either EnvironmentError e
getInnerEnv e [] = Right e
getInnerEnv e (p : ps) =
  (next e p)
    >>= \(moduleEnv, _) -> getInnerEnv moduleEnv ps

-- | Get a context's internal environment if it exists, otherwise get the
-- innermost module's value environment based on the context path.
contextEnv :: Environment e => Context -> e
contextEnv Context {contextInternalEnv = Just e} = inj e
contextEnv Context {contextGlobalEnv = e, contextPath = p} = inj (fromRight e (getInnerEnv e p))

--------------------------------------------------------------------------------
-- Utility functions

-- | Checks if an environment is "external", meaning it's either the global
-- scope or a module scope.
envIsExternal :: Environment e => e -> Bool
envIsExternal e =
  case envMode (prj e) of
    ExternalEnv -> True
    InternalEnv -> False
    RecursionEnv -> True

--------------------------------------------------------------------------------
-- Binding Utilities

-- | Get a list of all the names of bindings in an environment.
envBindingNames :: Environment e => e -> [String]
envBindingNames e =
  concatMap select (Map.toList (binders e))
  where select :: (String, Binder) -> [String]
        select (name, _) =
          case (next e name) of
            Right (e', _) -> envBindingNames e'
            Left  _      -> [name]

-- | Get a list of all the names of bindings in an environment that aren't
-- hidden or private.
envPublicBindingNames :: Environment e => e -> [String]
envPublicBindingNames e = concatMap select (Map.toList (binders e))
  where
    select :: (String, Binder) -> [String]
    select (name, binder) =
      case (next e name) of
        Left _  -> if metaIsTrue (binderMeta binder) "private" || metaIsTrue (binderMeta binder) "hidden"
                     then []
                     else [name]
        Right (e', _) -> envPublicBindingNames e'

-- | Recursively look through all environments for (def ...) forms.
--
-- N.B. Don't use find here. We access binders directly, so there's no need to
-- perform additional O(n) lookup calls.
findAllGlobalVariables :: Env -> [Binder]
findAllGlobalVariables e =
  foldl' finder [] (Map.elems (binders e))
  where
    finder :: [Binder] -> Binder -> [Binder]
    finder acc (Binder _ (XObj (Mod ev _) _ _)) = acc ++ (findAllGlobalVariables (inj ev))
    finder acc def@(Binder _ (XObj (Lst (XObj Def _ _ : _)) _ _)) = (def:acc)
    finder acc _ = acc

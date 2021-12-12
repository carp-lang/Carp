{-# LANGUAGE TupleSections #-}

module Env
  ( EnvironmentError,
    Environment (..),
    Mode (..),
    -- utils
    empty,
    new,
    parent,
    setParent,
    nested,
    recursive,
    binders,
    ------------------------
    -- lookups
    getType,
    getTypeBinder,
    findType,
    findTypeBinder,
    searchType,
    searchTypeBinder,
    getValue,
    getValueBinder,
    findValue,
    findValueBinder,
    searchValue,
    searchValueBinder,
    -------------------------
    -- Environment getters
    getInnerEnv,
    contextEnv,
    envIsExternal,
    envPublicBindingNames,
    -------------------------
    -- mutation
    insert,
    insertX,
    replace,
    addBinding,
    deleteBinding,
    addListOfBindings,
    addUsePath,
    -------------------------
    -- finds
    findPoly,
    findAllByMeta,
    findChildren,
    findImplementations,
    findAllGlobalVariables,
    findModules,
    allImportedEnvs,
    -------------------------
    -- lookups
    lookupContextually,
    lookupMeta,
    lookupChildren,
    lookupInUsed,
    lookupEverywhere,
    lookupBinderEverywhere,
    progenitor,
    replaceInPlace,
  )
where

import Data.Either (fromRight, rights)
import Data.List (foldl', unfoldr)
import Data.Maybe (fromMaybe)
import qualified Map
import qualified Meta
import Obj
import qualified Set
import Types

--------------------------------------------------------------------------------
-- Data

data EnvironmentError
  = NoEnvInNonModule
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

data Mode = Types | Values

--------------------------------------------------------------------------------
-- The Environment class and implementations

-- | Class for generically handling type and value environments.
class Environment e where
  inj :: Env -> e
  prj :: e -> Env
  update :: e -> Binder -> Either EnvironmentError Binder
  modality :: e -> Mode

-- | The value environment
instance Environment Env where
  inj = id
  prj = id
  update e (Binder meta (XObj (Mod _ et) i t)) = Right (Binder meta (XObj (Mod e et) i t))
  update _ _ = Left NoReplaceInNonModule
  modality _ = Values

-- | The type environment
instance Environment TypeEnv where
  inj = TypeEnv
  prj = getTypeEnv
  update e (Binder meta (XObj (Mod ev _) i t)) = Right (Binder meta (XObj (Mod ev e) i t))
  update _ _ = Left NoReplaceInNonModule
  modality _ = Types

--------------------------------------------------------------------------------
-- Misc. Environment utilities

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
nested p name lvl = inj ((prj (new p name)) {envMode = InternalEnv, envFunctionNestingLevel = lvl})

-- | Returns a new recursive environment with a designated nesting level.
recursive :: Environment e => Maybe e -> Maybe String -> Int -> e
recursive p name lvl = inj ((prj (new p name)) {envMode = RecursionEnv, envFunctionNestingLevel = lvl})

-- | Returns the binders stored in an environment.
binders :: Environment e => e -> Map.Map String Binder
binders = envBindings . prj

-- | Get the parent of an environment.
parent :: Environment e => e -> Maybe e
parent = fmap inj . envParent . prj

-- | Set the parent of an environment.
setParent :: Environment e => e -> e -> e
setParent e p = inj ((prj e) {envParent = Just (prj p)})

-- | Get an environment stored in a module binder.
nextEnv :: Mode -> Binder -> Either EnvironmentError Env
nextEnv Types (Binder _ (XObj (Mod _ et) _ _)) = Right $ prj et
nextEnv Values (Binder _ (XObj (Mod ev _) _ _)) = Right $ prj ev
nextEnv _ _ = Left NoEnvInNonModule

-- | Replace an environment stored in a module binder.
updateEnv :: Mode -> Env -> Binder -> Either EnvironmentError Binder
updateEnv Values e (Binder meta (XObj (Mod _ et) i t)) = Right (Binder meta (XObj (Mod e et) i t))
updateEnv Types e (Binder meta (XObj (Mod ev _) i t)) = Right (Binder meta (XObj (Mod ev (TypeEnv e)) i t))
updateEnv _ _ _ = Left NoEnvInNonModule

--------------------------------------------------------------------------------
-- Environment traversal
--
-- Naming conventions:
--
--   get: Direct lookup. Try to get the designated binder directly from an
--   environment, without traversing into parents or children. If not found,
--   fail.
--
--   find: Preorder lookup. Try to get the designated binder by proceeding from
--   the root environment down to its children. If not found in a child, fail.
--
--   search: pre and post order lookup: Try to get the designated binder by
--   proceeding from the root to children. If not found, try to find the binder
--   by proceeding from the root's parent, if it exists, to its children. If
--   not found, fail.

-- | Walk down an environment chain.
walk' :: Mode -> Env -> SymPath -> Either EnvironmentError Env
walk' _ e (SymPath [] _) = pure e
walk' mode' e (SymPath (p : ps) name) =
  do
    (_, binder) <- get e p
    go (SymPath ps name) binder
  where
    go :: SymPath -> Binder -> Either EnvironmentError Env
    go (SymPath [] _) binder = nextEnv mode' binder
    go path binder =
      do
        env <- nextEnv Values binder
        walk' mode' env path

-- | Generic *unidirectional* retrieval of binders (does not check parents).
walkAndGet :: Environment e => e -> SymPath -> (Either EnvironmentError e, Either EnvironmentError Binder)
walkAndGet e path@(SymPath _ name) =
  let target = walk' (modality e) (prj e) path
      binder = target >>= \t -> get t name
   in (fmap inj target, fmap snd binder)

-- | Direct lookup for a binder in environment `e`.
-- The environment returned in the output will be the same as that given as input.
--
-- Returns an error if not found.
get :: Environment e => e -> String -> Either EnvironmentError (e, Binder)
get e name =
  case Map.lookup name (binders e) of
    Nothing -> Left $ BindingNotFound name (prj e)
    Just b -> Right (e, b)

-- | Same as `get` but only returns a binder.
getBinder :: Environment e => e -> String -> Either EnvironmentError Binder
getBinder e name = fmap snd (get e name)

-- | Generic unidirectional retrieval of binders.
-- Searches the children of `e` using a given path, stopping at the terminus.
--
-- Returns an error if not found.
find' :: Environment e => e -> SymPath -> Either EnvironmentError (e, Binder)
find' e path =
  case walkAndGet e path of
    (Right e', Right b) -> Right (e', b)
    (Left err, _) -> Left err
    (_, Left err) -> Left err

-- | Same as `find` but only returns a binder.
findBinder :: Environment e => e -> SymPath -> Either EnvironmentError Binder
findBinder e path = fmap snd (find' e path)

-- | Generic *multidirectional* retrieval of binders.
-- Searches the children and parents of `e` (or the parent of a sub-environment
-- found in `e` and given by `path`).
--
-- Returns an error if not found.
search :: Environment e => e -> SymPath -> Either EnvironmentError (e, Binder)
search e path =
  case walkAndGet e path of
    (Right e', Right b) -> Right (e', b)
    (Right e', Left err) -> (checkParent e' err)
    (Left err, Left _) -> (checkParent e err) <> Left err
    -- impossible case. Included to keep `walk` honest.
    (Left _, Right _) -> error "impossible"
  where
    checkParent env err = maybe (Left err) (`search` path) (parent env)

-- | Same as `search` but only returns a binder.
searchBinder :: Environment e => e -> SymPath -> Either EnvironmentError Binder
searchBinder e path = fmap snd (search e path)

--------------------------------------------------------------------------------
-- Specialized retrievals
--
-- These functions are all equivalent to the generic retrieval functions
-- defined above but they enforce further restrictions at type level. Thus,
-- they can be used to help enforce constraints at call sites.
--
-- For example, suppose we want to search for a binder that may name a type

-- * or* module, preferring types. One could cast to enforce a type search

-- starting from the global env:
--
--   search typeEnv path
--   <> search (TypeEnv global) path
--   <> search global path
--
--   But:
--
--   searchType typeEnv path
--   searchType global path
--   <> searchValue global path
--
--   Is arguably much clearer.

--------------------------------------------------------------------------------
-- Type retrievals

-- | Get a type from a type environment.
getType :: TypeEnv -> String -> Either EnvironmentError (TypeEnv, Binder)
getType = get

-- | Get a type binder from a type environment.
getTypeBinder :: TypeEnv -> String -> Either EnvironmentError Binder
getTypeBinder = getBinder

-- | Unidirectional binder retrieval specialized to types.
--
-- Restricts the final step of a search to binders in a module's *type* environment.
findType :: Environment e => e -> SymPath -> Either EnvironmentError (TypeEnv, Binder)
findType e path = go $ find' (inj (prj e)) path
  where
    -- Make sure the binder is actually a type.
    go :: Either EnvironmentError (TypeEnv, Binder) -> Either EnvironmentError (TypeEnv, Binder)
    go (Right (t, b)) =
      if isType (binderXObj b)
        then Right (t, b)
        else Left (BindingNotFound (show path) (prj e))
    go x = x

findTypeBinder :: Environment e => e -> SymPath -> Either EnvironmentError Binder
findTypeBinder e path = fmap snd (findType e path)

-- | Multidirectional binder retrieval specialized to types.
--
-- Restricts the final step of a search to binders in a module's *type* environment.
searchType :: Environment e => e -> SymPath -> Either EnvironmentError (TypeEnv, Binder)
searchType e path = search (inj (prj e)) path

searchTypeBinder :: Environment e => e -> SymPath -> Either EnvironmentError Binder
searchTypeBinder e path = fmap snd (searchType e path)

--------------------------------------------------------------------------------
-- Value retrievals

-- | Get a value from a value environment.
getValue :: Env -> String -> Either EnvironmentError (Env, Binder)
getValue = get

getValueBinder :: Env -> String -> Either EnvironmentError Binder
getValueBinder = getBinder

-- | Unidirectional binder retrieval specialized to values.
findValue :: Env -> SymPath -> Either EnvironmentError (Env, Binder)
findValue = find'

findValueBinder :: Env -> SymPath -> Either EnvironmentError Binder
findValueBinder = findBinder

-- | Multidirectional binder retrieval specialized to values.
searchValue :: Env -> SymPath -> Either EnvironmentError (Env, Binder)
searchValue = search

searchValueBinder :: Env -> SymPath -> Either EnvironmentError Binder
searchValueBinder = searchBinder

--------------------------------------------------------------------------------
-- Environment mutation

--------------------------------------------------------------------------------
-- Mutation primitives

-- N.B. The following functions returns an Either for compatibility with other
-- functions in this module. It is a constant function in the co-domain of
-- Either, as they always returns Right.

-- | Add a new binding to an environment.
addBinding :: Environment e => e -> String -> Binder -> Either EnvironmentError e
addBinding e name b = pure (inj ((prj e) {envBindings = Map.insert name b (binders e)}))

-- | Replace the value of a binding in an environment, but only if it already
-- exists.
replaceBinding :: Environment e => e -> String -> Binder -> Either EnvironmentError e
replaceBinding e name b =
  pure (inj ((prj e) {envBindings = Map.adjust (const b) name (binders e)}))

-- | Delete a binding in an environment.
deleteBinding :: Environment e => e -> String -> Either EnvironmentError e
deleteBinding e name = pure (inj ((prj e) {envBindings = Map.delete name (binders e)}))

--------------------------------------------------------------------------------
-- Generic environment mutation

type EnvironmentProducer e = (e -> String -> Binder -> Either EnvironmentError e)

-- | Given an environment and a complete identifier path, traverse a chain of
-- environments until the path is exhausted, if requested, mutating the
-- environments along the way:
mutate :: Environment e => (EnvironmentProducer e) -> e -> SymPath -> Binder -> Either EnvironmentError e
mutate f e path binder = go path
  where
    go (SymPath [] name) = f e name binder
    go (SymPath (p : []) name) =
      do mod' <- getBinder e p
         env' <- nextEnv (modality e) mod'
         res <- mutate f (inj env') (SymPath [] name) binder
         new' <- updateEnv (modality e) (prj res) mod'
         addBinding e p new'
    go (SymPath (p : ps) name) =
      do mod' <- getBinder e p
         old <- nextEnv Values mod'
         result <- mutate f (inj old) (SymPath ps name) binder
         new' <- updateEnv Values (prj result) mod'
         addBinding e p new'

-- | Insert a binding into an environment at the given path.
insert :: Environment e => e -> SymPath -> Binder -> Either EnvironmentError e
insert = mutate addBinding

-- | Insert an XObj into an environment at the specified path.
-- This function does not perform insertions into parents.
insertX :: Environment e => e -> SymPath -> XObj -> Either EnvironmentError e
insertX e path x = insert e path (toBinder x)

-- | Replace a binding at the given path in an environment.
replace :: Environment e => e -> SymPath -> Binder -> Either EnvironmentError e
replace = mutate replaceBinding

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
  (get e name >>= \_ -> addBinding e name b)
    <> case parent e of
      Just p -> replaceInPlace p name b >>= \p' -> pure (inj ((prj e) {envParent = Just (prj p')}))
      Nothing -> Left (BindingNotFound name (prj e))

-- | Add a list of bindings to an environment.
addListOfBindings :: Environment e => e -> [(String, Binder)] -> e
addListOfBindings e bindings =
  foldl' (\e' (n, b) -> fromRight e (addBinding e' n b)) e bindings

-- | Add a module path to an environment's list of used modules.
addUsePath :: Environment e => e -> SymPath -> e
addUsePath e path = inj ((prj e) {envUseModules = Set.insert path (envUseModules (prj e))})

--------------------------------------------------------------------------------
-- Additional binding lookup functions
--
-- find* functions perform lookup in a single environment, without recursion.
-- lookup* functions perform lookups in an environment chain, with recursion.

-- | Get the metadata associated with the binder at the specified path in an environment.
lookupMeta :: Environment e => e -> SymPath -> Either EnvironmentError MetaData
lookupMeta e path = searchBinder e path >>= pure . Meta.fromBinder

-- | Find all binders in an environment that have a specified meta key.
findAllByMeta :: Environment e => e -> String -> Either EnvironmentError [Binder]
findAllByMeta e metaKey =
  let candidates = Map.elems (Map.filter (Meta.binderMember metaKey) (binders e))
   in case candidates of
        [] -> Left (NoMatchingBindingFound ("metadata " ++ metaKey))
        _ -> Right $ candidates

-- | Find all modules directly stored in environment `e`.
findModules :: Environment e => e -> [XObj]
findModules e =
  map binderXObj (filter modsOnly (Map.elems (binders e)))
  where
    modsOnly :: Binder -> Bool
    modsOnly binder =
      case binderXObj binder of
        XObj (Mod _ _) _ _ -> True
        _ -> False

-- | It's more efficient to specialize this function as it can take advantage
-- of laziness; once we found the candidate function for a polymorphic
-- function, there's no need to consume the rest of the environment.
findPoly :: Environment e => e -> String -> Ty -> Either EnvironmentError (e, Binder)
findPoly env name ty =
  case getBinder env name of
    Right b ->
      if unify b
        then Right (env, b)
        else (foldl' go (Left (BindingNotFound name (prj env))) (findChildren env))
    Left _ -> foldl' go (Left (BindingNotFound name (prj env))) (findChildren env)
  where
    go x e = x <> (findPoly e name ty)
    unify = areUnifiable ty . fromMaybe Universe . xobjTy . binderXObj

-- | Find all environments that are *direct* children of an environment (one
-- level down).
--
-- The modality of the children is determined by the modality of the root.
--
-- N.B. Don't use find here. We access binders directly, so there's no need to
-- perform additional O(n) lookup calls.
findChildren :: Environment e => e -> [e]
findChildren e =
  foldl' getEnv [] (binders e)
  where
    getEnv acc binder =
      case (nextEnv (modality e) binder) of
        Left _ -> acc
        Right e' -> ((inj e') : acc)

-- | Find all the environments contained in the modules initial environment,
-- plus any module environments contained in *those* modules.
lookupChildren :: Environment e => e -> [e]
lookupChildren e =
  foldl' go [] (findChildren e)
  where
    go acc e' = case findChildren e' of
      [] -> (e' : acc)
      xs -> (foldl' go [] xs ++ acc)

-- | Find all the environments designated by the use paths in an environment.
findImportedEnvs :: Environment e => e -> [e]
findImportedEnvs e =
  let eMode = modality e
      usePaths = Set.toList (envUseModules (prj e))
      getter path =
        walk' eMode (prj e) path
          >>= \e' ->
            get e' (getName' path)
              >>= nextEnv eMode . snd
              >>= pure . inj
      used = fmap getter usePaths
   in (rights used)
  where
    getName' (SymPath _ name) = name

-- | Given an environment, get its topmost parent up the environment chain.
--
-- For nearly all environments, this should be the global environment.
progenitor :: Environment e => e -> e
progenitor e = fromMaybe e (parent e >>= \p -> pure (progenitor p))

-- | Find all possible environments imported at some point *upwards* from e in a chain of environments.
allImportedEnvs :: Environment e => e -> Env -> [e]
allImportedEnvs e global =
  let env = prj e
      paths = (Set.toList (foldl' og (envUseModules env) (unfoldr go env)))
   in (rights (map get' paths))
  where
    go e' = parent e' >>= \p -> pure (p, p)
    og acc e' = (envUseModules e') <> acc
    get' path =
      findBinder global path
        >>= nextEnv (modality e)
        >>= pure . inj

-- | Find all binders the implement a given interface, designated by its path.
findImplementations :: Environment e => e -> SymPath -> Either EnvironmentError [Binder]
findImplementations e interface =
  ( (findAllByMeta e "implements")
      >>= \is -> (pure (filter (isImpl . Meta.fromBinder) is))
  )
    <> Left (NoMatchingBindingFound ("implementation meta for " ++ show interface))
  where
    isImpl :: MetaData -> Bool
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
  let envs = [e] ++ (f e)
   in (go (parent e) envs)
  where
    go _ [] = []
    go Nothing xs = foldl' accum [] xs
    go (Just p) xs = go (parent p) (xs ++ [p] ++ (f p))
    accum acc e' = case getBinder e' name of
      Right b -> ((e', b) : acc)
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
  where
    go :: Environment e => [(e, Binder)] -> e -> [(e, Binder)]
    go acc e' = case (search e' spath) of
      Right (e'', b) -> ((e'', b) : acc)
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
  where
    lookupDirectly =
      (getBinder e p)
        >>= nextEnv (modality e)
        >>= \e' ->
          search (inj e') (SymPath ps name)
            >>= pure . (: [])
    lookupInUsedAndParent = case rights (fmap ((flip search) path) (findImportedEnvs e)) of
      [] -> Left (BindingNotFound name (prj e))
      xs ->
        case parent e of
          Nothing -> Right xs
          Just e' -> (Env.search e' path >>= \found -> Right $ xs ++ [found]) <> Right xs

--------------------------------------------------------------------------------
-- Environment retrieval functions

-- | Get the environment at a given path that corresponds to the type of an
-- initial environment.
--
-- Returns the initial environment when given an empty path.
getInnerEnv :: Environment e => e -> [String] -> Either EnvironmentError e
getInnerEnv e [] = Right e
getInnerEnv e (p : ps) =
  (getBinder e p)
    >>= nextEnv (modality e)
    >>= \moduleEnv -> getInnerEnv (inj moduleEnv) ps

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

-- | Get a list of all the names of bindings in an environment that aren't
-- hidden or private.
envPublicBindingNames :: Environment e => e -> [String]
envPublicBindingNames e = concatMap select (Map.toList (binders e))
  where
    select :: (String, Binder) -> [String]
    select (name, binder) =
      case (nextEnv (modality e) binder) of
        Left _ ->
          if metaIsTrue (binderMeta binder) "private" || metaIsTrue (binderMeta binder) "hidden"
            then []
            else [name]
        Right e' -> map (\n -> name ++ "." ++ n) (envPublicBindingNames e')

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
    finder acc def@(Binder _ (XObj (Lst (XObj Def _ _ : _)) _ _)) = (def : acc)
    finder acc _ = acc

module Obj where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (intercalate, foldl')
import Data.Maybe (mapMaybe, fromMaybe)
import Control.Monad.State
import Data.Char
import Types
import Util
import Debug.Trace

-- | The canonical Lisp object.
data Obj = Sym SymPath
         | MultiSym String [SymPath]
         | Num Ty Double
         | Str String
         | Chr Char
         | Bol Bool
         | Lst [XObj]
         | Arr [XObj]
         | Defn
         | Def
         | Do
         | Let
         | While
         | If
         | Mod Env
         | Typ
         | External
         | ExternalType
         | Deftemplate TemplateCreator
         | Instantiate Template
         | Defalias Ty
         | Address
         | SetBang
         | Macro
         | Dynamic
         | The
         | Ref
         | Interface Ty
         deriving (Show, Eq)

newtype TemplateCreator = TemplateCreator { getTemplateCreator :: TypeEnv -> Env -> Template }

instance Show TemplateCreator where
  show _ = "TemplateCreator"

-- | Note: This is to make comparisons of Environments possible, otherwise
-- | they are always different when they contain TemplateCreators.
instance Eq TemplateCreator where
  _ == _ = True

-- | Information about where the Obj originated from.
data Info = Info { infoLine :: Int
                 , infoColumn :: Int
                 , infoFile :: String
                 , infoDelete :: Set.Set Deleter
                 , infoIdentifier :: Int
                 } deriving (Show, Eq)

dummyInfo :: Info
dummyInfo = Info 0 0 "dummy-file" Set.empty (-1)

data Deleter = ProperDeleter { deleterPath :: SymPath
                             , deleterVariable :: String
                             }
             | FakeDeleter { deleterVariable :: String -- used for external types with no delete function
                           }
             deriving (Show, Eq, Ord)

prettyInfo :: Info -> String
prettyInfo i = "line " ++ show (infoLine i) ++ ", column " ++ show (infoColumn i) ++ " in '" ++ infoFile i ++ "'"

prettyInfoFromXObj :: XObj -> String
prettyInfoFromXObj xobj = case info xobj of
                            Just i -> prettyInfo i
                            Nothing -> "no info"

-- TODO: change name of this function
freshVar :: Info -> String
freshVar i = "_" ++ show (infoIdentifier i)

-- | Obj with eXtra information.
data XObj = XObj { obj :: Obj
                 , info :: Maybe Info
                 , ty :: Maybe Ty
                 } deriving (Show, Eq)

getBinderDescription :: XObj -> String
getBinderDescription (XObj (Lst (XObj Defn _ _ : XObj (Sym _) _ _ : _)) _ _) = "defn"
getBinderDescription (XObj (Lst (XObj Def _ _ : XObj (Sym _) _ _ : _)) _ _) = "def"
getBinderDescription (XObj (Lst (XObj Macro _ _ : XObj (Sym _) _ _ : _)) _ _) = "macro"
getBinderDescription (XObj (Lst (XObj (Deftemplate _) _ _ : XObj (Sym _) _ _ : _)) _ _) = "template"
getBinderDescription (XObj (Lst (XObj (Instantiate _) _ _ : XObj (Sym _) _ _ : _)) _ _) = "instantiate"
getBinderDescription (XObj (Lst (XObj (Defalias _) _ _ : XObj (Sym _) _ _ : _)) _ _) = "alias"
getBinderDescription (XObj (Lst (XObj External _ _ : XObj (Sym _) _ _ : _)) _ _) = "external"
getBinderDescription (XObj (Lst (XObj ExternalType _ _ : XObj (Sym _) _ _ : _)) _ _) = "external-type"
getBinderDescription (XObj (Lst (XObj Typ _ _ : XObj (Sym _) _ _ : _)) _ _) = "deftype"
getBinderDescription (XObj (Lst (XObj (Interface _) _ _ : XObj (Sym _) _ _ : _)) _ _) = "interface"
getBinderDescription _ = "?"

getName :: XObj -> String
getName xobj = show (getPath xobj)

-- | Extracts the second form (where the name of definitions are stored) from a list of XObj:s.
getPath :: XObj -> SymPath
getPath (XObj (Lst (XObj Defn _ _ : XObj (Sym path) _ _ : _)) _ _) = path
getPath (XObj (Lst (XObj Def _ _ : XObj (Sym path) _ _ : _)) _ _) = path
getPath (XObj (Lst (XObj Macro _ _ : XObj (Sym path) _ _ : _)) _ _) = path
getPath (XObj (Lst (XObj (Deftemplate _) _ _ : XObj (Sym path) _ _ : _)) _ _) = path
getPath (XObj (Lst (XObj (Instantiate _) _ _ : XObj (Sym path) _ _ : _)) _ _) = path
getPath (XObj (Lst (XObj (Defalias _) _ _ : XObj (Sym path) _ _ : _)) _ _) = path
getPath (XObj (Lst (XObj External _ _ : XObj (Sym path) _ _ : _)) _ _) = path
getPath (XObj (Lst (XObj ExternalType _ _ : XObj (Sym path) _ _ : _)) _ _) = path
getPath (XObj (Lst (XObj (Mod _) _ _ : XObj (Sym path) _ _ : _)) _ _) = path
getPath (XObj (Sym path) _ _) = path
getPath x = SymPath [] (pretty x)

-- | Changes the second form (where the name of definitions are stored) in a list of XObj:s.
setPath :: XObj -> SymPath -> XObj
setPath (XObj (Lst (defn@(XObj Defn _ _) : XObj (Sym _) si st : rest)) i t) newPath =
  XObj (Lst (defn : XObj (Sym newPath) si st : rest)) i t
setPath (XObj (Lst [extr@(XObj External _ _), XObj (Sym _) si st]) i t) newPath =
  XObj (Lst [extr, XObj (Sym newPath) si st]) i t
setPath x _ =
  compilerError ("Can't set path on " ++ show x)

-- | Convert an XObj to a pretty string representation.
pretty :: XObj -> String
pretty = visit 0
  where visit :: Int -> XObj -> String
        visit indent xobj =
          case obj xobj of
            Lst lst -> "(" ++ joinWithSpace (map (visit indent) lst) ++ ")"
            Arr arr -> "[" ++ joinWithSpace (map (visit indent) arr) ++ "]"
            Num IntTy num -> show (round num :: Int)
            Num LongTy num -> show num ++ "l"
            Num FloatTy num -> show num ++ "f"
            Num DoubleTy num -> show num
            Num _ _ -> compilerError "Invalid number type."
            Str str -> show str
            Chr c -> '\\' : c : ""
            Sym path -> show path
            MultiSym originalName paths -> originalName ++ "{" ++ joinWithComma (map show paths) ++ "}"
            Bol b -> if b then "true" else "false"
            Defn -> "defn"
            Def -> "def"
            If -> "if"
            While -> "while"
            Do -> "do"
            Let -> "let"
            Mod env -> fromMaybe "module" (envModuleName env)
            Typ -> "deftype"
            Deftemplate _ -> "deftemplate"
            Instantiate _ -> "instantiate"
            External -> "external"
            ExternalType -> "external-type"
            Defalias _ -> "defalias"
            Address -> "address"
            SetBang -> "set!"
            Macro -> "macro"
            Dynamic -> "dynamic"
            The -> "the"
            Ref -> "ref"
            Interface _ -> "interface"

-- | Get the type of an XObj as a string.
typeStr :: XObj -> String
typeStr xobj = case ty xobj of
                 Nothing -> " : _"
                 Just t -> " : " ++ show t

-- | Convert XObj to pretty string representation with type annotations.
prettyTyped :: XObj -> String
prettyTyped = visit 0
  where visit :: Int -> XObj -> String
        visit indent xobj =
          let suffix = typeStr xobj ++ ", id = " ++ show (fmap infoIdentifier (info xobj)) ++ "\n"
          in case obj xobj of
               Lst lst -> "(" ++ joinWithSpace (map (visit indent) lst) ++ ")" ++ suffix
               Arr arr -> "[" ++ joinWithSpace (map (visit indent) arr) ++ "]" ++ suffix
               _ -> pretty xobj ++ suffix

-- | Wraps and holds an XObj in an environment.
newtype Binder = Binder { binderXObj :: XObj } deriving Eq

instance Show Binder where
  show binder = showBinderIndented 0 (getName (binderXObj binder), binder)

showBinderIndented :: Int -> (String, Binder) -> String
showBinderIndented indent (name, Binder (XObj (Mod env) _ _)) =
  replicate indent ' ' ++ name ++ " : Module = {\n" ++
  prettyEnvironmentIndented (indent + 4) env ++
  "\n" ++ replicate indent ' ' ++ "}"
showBinderIndented indent (name, Binder xobj) =
  replicate indent ' ' ++ name ++ -- " (" ++ show (getPath xobj) ++ ")" ++
  " : " ++ showMaybeTy (ty xobj) -- ++ " " ++ getBinderDescription xobj

-- | The score is used for sorting the bindings before emitting them.
-- | A lower score means appearing earlier in the emitted file.
scoreBinder :: TypeEnv -> Binder -> (Int, Binder)
scoreBinder typeEnv b@(Binder (XObj (Lst (XObj x _ _ : XObj (Sym (SymPath _ name)) _ _ : _)) _ _)) =
  case x of
    Defalias aliasedType ->
      let selfName = ""
      in  (depthOfType typeEnv selfName (Just aliasedType), b)
    Typ ->
      case lookupInEnv (SymPath [] name) (getTypeEnv typeEnv) of
        Just (_, Binder typedef) -> let depth = (dependencyDepthOfTypedef typeEnv typedef, b)
                                    in  --trace ("depth of " ++ name ++ ": " ++ show depth)
                                        depth
        Nothing -> compilerError ("Can't find user defined type '" ++ name ++ "' in type env.")
    _ ->
      (100, b)
scoreBinder _ b@(Binder (XObj (Mod _) _ _)) =
  (200, b)
scoreBinder _ x = error ("Can't score: " ++ show x)

dependencyDepthOfTypedef :: TypeEnv -> XObj -> Int
dependencyDepthOfTypedef typeEnv (XObj (Lst (_ : XObj (Sym (SymPath _ selfName)) _ _ : rest)) _ _) =
  case concatMap expandCase rest of
    [] -> 0
    xs -> maximum xs
  where
    expandCase :: XObj -> [Int]
    expandCase (XObj (Arr arr) _ _) = map (depthOfType typeEnv selfName . xobjToTy . snd) (pairwise arr)
    expandCase _ = compilerError "Malformed case in typedef."
dependencyDepthOfTypedef _ xobj =
  compilerError ("Can't get dependency depth from " ++ show xobj)

depthOfType :: TypeEnv -> String -> Maybe Ty -> Int
depthOfType typeEnv selfName = visitType
  where
    visitType :: Maybe Ty -> Int
    visitType (Just (StructTy name _)) = depthOfStructType name
    visitType (Just (FuncTy argTys retTy)) =
      -- trace ("Depth of args of " ++ show argTys ++ ": " ++ show (map (visitType . Just) argTys))
      maximum (visitType (Just retTy) : map (visitType . Just) argTys) + 1
    visitType (Just (PointerTy p)) = visitType (Just p)
    visitType (Just (RefTy r)) = visitType (Just r)
    visitType (Just _) = 0
    visitType Nothing = -100 -- External / unknown type

    depthOfStructType :: String -> Int
    depthOfStructType name =
      case name of
        "Array" -> 20
        _ | name == selfName -> 0
          | otherwise ->
              case lookupInEnv (SymPath [] name) (getTypeEnv typeEnv) of
                Just (_, Binder typedef) -> dependencyDepthOfTypedef typeEnv typedef + 1
                Nothing -> -- trace ("Unknown type: " ++ name)
                           0 -- Refering to unknown type

-- | Helper function to create binding pairs for registering external functions.
register :: String -> Ty -> (String, Binder)
register name t = (name, Binder (XObj (Lst [XObj External Nothing Nothing,
                                            XObj (Sym (SymPath [] name)) Nothing Nothing])
                                 (Just dummyInfo) (Just t)))

data EnvMode = ExternalEnv | InternalEnv deriving (Show, Eq)

-- | Environment
data Env = Env { envBindings :: Map.Map String Binder
               , envParent :: Maybe Env
               , envModuleName :: Maybe String
               , envUseModules :: [SymPath]
               , envMode :: EnvMode
               } deriving (Show, Eq)

newtype TypeEnv = TypeEnv { getTypeEnv :: Env }

instance Show TypeEnv where
  show (TypeEnv env) = "(TypeEnv " ++ show env ++ ")"

safeEnvModuleName :: Env -> String
safeEnvModuleName env =
  case envModuleName env of
    Just name -> name ++ ", with parent " ++ parent
    Nothing -> "???, with parent " ++ parent
  where parent =
          case envParent env of
            Just p -> safeEnvModuleName p
            Nothing -> "Global"

-- | Used by the compiler command "(env)"
prettyEnvironment :: Env -> String
prettyEnvironment = prettyEnvironmentIndented 0

prettyEnvironmentIndented :: Int -> Env -> String
prettyEnvironmentIndented indent env =
  joinWith "\n" $ map (showBinderIndented indent) (Map.toList (envBindings env)) ++
                  let modules = envUseModules env
                  in  if null modules
                      then []
                      else ("\n" ++ replicate indent ' ' ++ "Used modules:") : map (showImportIndented indent) modules

showImportIndented :: Int -> SymPath -> String
showImportIndented indent path = replicate indent ' ' ++ " * " ++ show path

-- | Checks if an environment is "external", meaning it's either the global scope or a module scope.
envIsExternal :: Env -> Bool
envIsExternal env =
  case envMode env of
    ExternalEnv -> True
    InternalEnv -> False

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
    Just (Binder xobj) ->
      case xobj of
        (XObj (Mod modEnv) _ _) -> lookupInEnv (SymPath ps name) modEnv
        _ -> Nothing
    Nothing ->
      case envParent env of
        Just parent -> lookupInEnv path parent
        Nothing -> Nothing

-- | Find all the possible (imported) symbols that could be referred to
multiLookup :: String -> Env -> [(Env, Binder)]
multiLookup = multiLookupInternal False

multiLookupALL :: String -> Env -> [(Env, Binder)]
multiLookupALL = multiLookupInternal True

{-# ANN multiLookupInternal "HLint: ignore Eta reduce" #-}
-- | The advanced version of multiLookup that allows for looking into modules that are NOT imported.
multiLookupInternal :: Bool -> String -> Env -> [(Env, Binder)]
multiLookupInternal allowLookupInAllModules name rootEnv = recursiveLookup rootEnv

  where lookupInLocalEnv :: String -> Env -> Maybe (Env, Binder)
        lookupInLocalEnv n localEnv = case Map.lookup n (envBindings localEnv) of -- No recurse!
                                        Just b -> Just (localEnv, b)
                                        Nothing -> Nothing

        imports :: Env -> [Env]
        imports env = if allowLookupInAllModules
                      then let envs = mapMaybe (binderToEnv . snd) (Map.toList (envBindings env))
                           in  envs ++ concatMap imports envs
                      -- Only lookup in imported modules:
                      else let envs = mapMaybe (\path -> fmap getEnvFromBinder (lookupInEnv path env)) (envUseModules env)
                           in  envs ++ concatMap imports envs

        binderToEnv :: Binder -> Maybe Env
        binderToEnv (Binder (XObj (Mod e) _ _)) = Just e
        binderToEnv _ = Nothing

        importsLookup :: Env -> [(Env, Binder)]
        importsLookup env = mapMaybe (lookupInLocalEnv name) (imports env)

        recursiveLookup :: Env -> [(Env, Binder)]
        recursiveLookup env =
          let spine = case Map.lookup name (envBindings env) of
                        Just found -> [(env, found)]
                        Nothing -> []
              leafs = importsLookup env
              above = case envParent env of
                        Just parent -> recursiveLookup parent
                        Nothing -> []
          in  spine ++ leafs ++ above

getEnvFromBinder :: (a, Binder) -> Env
getEnvFromBinder (_, Binder (XObj (Mod foundEnv) _ _)) = foundEnv
getEnvFromBinder (_, Binder err) = error ("Can't handle imports of non modules yet: " ++ show err)

-- | Enables look up "semi qualified" (and fully qualified) symbols.
-- | i.e. if there are nested environments with a function A.B.f
-- | you can find it by doing "(use A)" and then "(B.f)".
multiLookupQualified :: SymPath -> Env -> [(Env, Binder)]
multiLookupQualified (SymPath [] name) rootEnv =
  -- This case is just like normal multiLookup, we have a name but no qualifyers:
  multiLookup name rootEnv
multiLookupQualified path@(SymPath (p:ps) name) rootEnv =
  case lookupInEnv (SymPath [] p) rootEnv of
    Just (_, Binder (XObj (Mod _) _ _)) ->
      -- Found a module with the correct name, that means we should not look at anything else:
      case lookupInEnv path rootEnv of
        Just found -> [found]
        Nothing -> []
    Just _ -> inexactMatch
    Nothing -> inexactMatch
  where inexactMatch =
          -- No exact match on the first qualifier, will look in various places for a match:
          let fromParent = case envParent rootEnv of
                                Just parent -> multiLookupQualified path parent
                                Nothing -> []
              fromUsedModules = let usedModules = envUseModules rootEnv
                                    envs = mapMaybe (\path -> fmap getEnvFromBinder (lookupInEnv path rootEnv)) usedModules
                                in  concatMap (multiLookupQualified path) envs
          in fromParent ++ fromUsedModules


-- | Add an XObj to a specific environment. TODO: rename to envInsert
extendEnv :: Env -> String -> XObj -> Env
extendEnv env name xobj = envAddBinding env name (Binder xobj)

-- | Add a Binder to an environment at a specific path location.
envInsertAt :: Env -> SymPath -> XObj -> Env
envInsertAt env (SymPath [] name) xobj = envAddBinding env name (Binder xobj)
envInsertAt env (SymPath (p:ps) name) xobj =
  case Map.lookup p (envBindings env) of
    Just (Binder (XObj (Mod innerEnv) i t)) ->
      let newInnerEnv = Binder (XObj (Mod (envInsertAt innerEnv (SymPath ps name) xobj)) i t)
      in  env { envBindings = Map.insert p newInnerEnv (envBindings env) }
    Just _ -> error ("Can't insert into non-module: " ++ p)
    Nothing -> error ("Can't insert into non-existing module: " ++ p)

envReplaceEnvAt :: Env -> [String] -> Env -> Env
envReplaceEnvAt _ [] replacement = replacement
envReplaceEnvAt env (p:ps) replacement =
  case Map.lookup p (envBindings env) of
    Just (Binder (XObj (Mod innerEnv) i t)) ->
      let newInnerEnv = Binder (XObj (Mod (envReplaceEnvAt innerEnv ps replacement)) i t)
      in  env { envBindings = Map.insert p newInnerEnv (envBindings env) }
    Just _ -> error ("Can't replace non-module: " ++ p)
    Nothing -> error ("Can't replace non-existing module: " ++ p)

-- | Add a Binder to a specific environment.
envAddBinding :: Env -> String -> Binder -> Env
envAddBinding env name binder = env { envBindings = Map.insert name binder (envBindings env) }

{-# ANN addListOfBindings "HLint: ignore Eta reduce" #-}
-- | Add a list of bindings to an environment
addListOfBindings :: Env -> [(String, Binder)] -> Env
addListOfBindings env bindingsToAdd = foldl' (\e (n, b) -> envAddBinding e n b) env bindingsToAdd

-- | Get an inner environment.
getEnv :: Env -> [String] -> Env
getEnv env [] = env
getEnv env (p:ps) = case Map.lookup p (envBindings env) of
                      Just (Binder (XObj (Mod innerEnv) _ _)) -> getEnv innerEnv ps
                      Just _ -> error "Can't get non-env."
                      Nothing -> error "Can't get env."

-- | Changes the symbol part of a defn (the name) to a new symbol path
-- | Example: (defn foo () 123) => (defn GreatModule.foo () 123)
setFullyQualifiedDefn :: XObj -> SymPath -> XObj
setFullyQualifiedDefn (XObj (Lst [defn, XObj _ symi symt, args, body]) i t) newPath =
  XObj (Lst [defn, XObj (Sym newPath) symi symt, args, body]) i t
setFullyQualifiedDefn (XObj (Lst [def, XObj _ symi symt, expr]) i t) newPath =
  XObj (Lst [def, XObj (Sym newPath) symi symt, expr]) i t
setFullyQualifiedDefn xobj _ = error ("Can't set new path on " ++ show xobj)

-- | Changes all symbols EXCEPT bound vars (defn names, variable names, etc) to their fully qualified paths.
-- | This must run after the 'setFullyQualifiedDefn' function has fixed the paths of all bindings in the environment.
-- | This function does NOT go into function-body scope environments and the like.
setFullyQualifiedSymbols :: Env -> XObj -> XObj
setFullyQualifiedSymbols env (XObj (Lst [defn@(XObj Defn _ _),
                                         sym@(XObj (Sym (SymPath _ functionName)) _ _),
                                         args@(XObj (Arr argsArr) _ _),
                                         body])
                               i t) =
  -- For self-recursion, there must be a binding to the function in the inner env.
  -- Note: This inner env is ephemeral since it is not stored in a module or global scope.
  let functionEnv = Env Map.empty (Just env) Nothing [] InternalEnv
      envWithSelf = extendEnv functionEnv functionName sym
      envWithArgs = foldl' (\e arg@(XObj (Sym (SymPath _ argSymName)) _ _) -> extendEnv e argSymName arg) envWithSelf argsArr
  in  XObj (Lst [defn, sym, args, setFullyQualifiedSymbols envWithArgs body]) i t
setFullyQualifiedSymbols env (XObj (Lst [the@(XObj The _ _), typeXObj, value]) i t) =
  let value' = setFullyQualifiedSymbols env value
  in  XObj (Lst [the, typeXObj, value']) i t
setFullyQualifiedSymbols env (XObj (Lst [def@(XObj Def _ _), sym, expr]) i t) =
  let expr' = setFullyQualifiedSymbols env expr
  in  XObj (Lst [def, sym, expr']) i t
setFullyQualifiedSymbols env (XObj (Lst [letExpr@(XObj Let _ _), bind@(XObj (Arr bindings) bindi bindt), body]) i t) =
  if even (length bindings)
  then let innerEnv = Env Map.empty (Just env) (Just "LET") [] InternalEnv
           envWithBindings = foldl' (\e (binderSym@(XObj (Sym (SymPath _ binderName)) _ _), _) ->
                                       extendEnv e binderName binderSym)
                                    innerEnv
                                    (pairwise bindings)
           newBinders = XObj (Arr (concatMap (\(s, o) -> [s, setFullyQualifiedSymbols envWithBindings o])
                                   (pairwise bindings))) bindi bindt
           newBody = setFullyQualifiedSymbols envWithBindings body
       in  XObj (Lst [letExpr, newBinders, newBody]) i t
  else XObj (Lst [letExpr, bind, body]) i t -- Leave it untouched for the compiler to find the error.
setFullyQualifiedSymbols env (XObj (Lst xobjs) i t) =
  let xobjs' = map (setFullyQualifiedSymbols env) xobjs
  in  XObj (Lst xobjs') i t
setFullyQualifiedSymbols env xobj@(XObj (Sym path) i t) =
  case multiLookupQualified path env of
    [] -> xobj
    [(_, Binder foundOne)] -> XObj (Sym (getPath foundOne)) i t
    multiple ->
      case filter (not . envIsExternal . fst) multiple of
      -- There is at least one local binding, use the path of that one:
        (_, Binder local) : _ -> XObj (Sym (getPath local)) i t
      -- There are no local bindings, this is allowed to become a multi lookup symbol:
        _ -> --(trace $ "Turned " ++ name ++ " into multisym: " ++ joinWithComma (map (show .getPath . binderXObj . snd) multiple))
          case path of
            (SymPath [] name) -> XObj (MultiSym name (map (getPath . binderXObj . snd) multiple)) i t -- Create a MultiSym!
            pathWithQualifiers -> trace ("PROBLEMATIC: " ++ show path) (XObj (Sym pathWithQualifiers) i t) -- The symbol IS qualified but can't be found, should produce an error later during compilation.
setFullyQualifiedSymbols env xobj@(XObj (Arr array) i t) =
  let array' = map (setFullyQualifiedSymbols env) array
  in  XObj (Arr array') i t
setFullyQualifiedSymbols _ xobj = xobj

-- | Project (represents a lot of useful information for working at the REPL and building executables)
data Project = Project { projectTitle :: String
                       , projectIncludes :: [Includer]
                       , projectCFlags :: [FilePath]
                       , projectLibFlags :: [FilePath]
                       , projectFiles :: [FilePath]
                       , projectEchoC :: Bool
                       , projectCarpDir :: FilePath
                       , projectOutDir :: FilePath
                       , projectPrompt :: String
                       , projectCarpSearchPaths :: [FilePath]
                       }

projectFlags :: Project -> String
projectFlags proj = joinWithSpace (projectCFlags proj ++ projectLibFlags proj)

instance Show Project where
  show (Project title incl cFlags libFlags srcFiles echoC carpDir outDir prompt searchPaths) =
    unlines [ "Title: " ++ title
            , "Includes:\n    " ++ joinWith "\n    " (map show incl)
            , "Cflags:\n    " ++ joinWith "\n    " cFlags
            , "Library flags:\n    " ++ joinWith "\n    " libFlags
            , "Carp source files:\n    " ++ joinWith "\n    " srcFiles
            , "Echo C: " ++ if echoC then "true" else "false"
            , "Output directory: " ++ outDir
            , "CARP_DIR: " ++ carpDir
            , "Prompt: " ++ prompt
            , "Search paths for 'load' command:\n    " ++ joinWith  "\n    " searchPaths
            ]

-- | Represent the inclusion of a C header file, either like <string.h> or "string.h"
data Includer = SystemInclude String
              | LocalInclude String
              deriving Eq

instance Show Includer where
  show (SystemInclude file) = "<" ++ file ++ ">"
  show (LocalInclude file) = "\"" ++ file ++ "\""

-- | Converts an S-expression to one of the Carp types.
xobjToTy :: XObj -> Maybe Ty
xobjToTy (XObj (Sym (SymPath _ "Int")) _ _) = Just IntTy
xobjToTy (XObj (Sym (SymPath _ "Float")) _ _) = Just FloatTy
xobjToTy (XObj (Sym (SymPath _ "Double")) _ _) = Just DoubleTy
xobjToTy (XObj (Sym (SymPath _ "Long")) _ _) = Just LongTy
xobjToTy (XObj (Sym (SymPath _ "String")) _ _) = Just StringTy
xobjToTy (XObj (Sym (SymPath _ "Char")) _ _) = Just CharTy
xobjToTy (XObj (Sym (SymPath _ "Bool")) _ _) = Just BoolTy
xobjToTy (XObj (Sym (SymPath _ s@(firstLetter:_))) _ _) | isLower firstLetter = Just (VarTy s)
                                                        | otherwise = Just (StructTy s [])
xobjToTy (XObj (Lst [XObj (Sym (SymPath _ "Ptr")) _ _, innerTy]) _ _) =
  do okInnerTy <- xobjToTy innerTy
     return (PointerTy okInnerTy)
xobjToTy (XObj (Lst (XObj (Sym (SymPath _ "Ptr")) _ _ : _)) _ _) =
  Nothing
xobjToTy (XObj (Lst [XObj (Sym (SymPath _ "Ref")) _ _, innerTy]) _ _) =
  do okInnerTy <- xobjToTy innerTy
     return (RefTy okInnerTy)
xobjToTy (XObj (Lst [XObj Ref i t, innerTy]) _ _) = -- This enables parsing of '&'
  do okInnerTy <- xobjToTy innerTy
     return (RefTy okInnerTy)
xobjToTy (XObj (Lst (XObj (Sym (SymPath _ "Ref")) _ _ : _)) _ _) =
  Nothing
xobjToTy (XObj (Lst [XObj (Sym (SymPath path "λ")) fi ft, XObj (Arr argTys) ai at, retTy]) i t) =
  xobjToTy (XObj (Lst [XObj (Sym (SymPath path "Fn")) fi ft, XObj (Arr argTys) ai at, retTy]) i t)
xobjToTy (XObj (Lst [XObj (Sym (SymPath _ "Fn")) _ _, XObj (Arr argTys) _ _, retTy]) _ _) =
  do okArgTys <- mapM xobjToTy argTys
     okRetTy <- xobjToTy retTy
     return (FuncTy okArgTys okRetTy)
xobjToTy (XObj (Lst []) _ _) = Just UnitTy
xobjToTy (XObj (Lst (x:xs)) _ _) =
  do okX <- xobjToTy x
     okXS <- mapM xobjToTy xs
     case okX of
       (StructTy n []) -> return (StructTy n okXS)
       _ -> Nothing
xobjToTy _ = Nothing

-- | Generates the suffix added to polymorphic functions when they are instantiated.
--   For example                (defn id [x] x) : t -> t
--   might be invoked like this (id 5)
--   which will generate        int id__Int(int x) { return x; }
--   The "__Int" is the suffix!
polymorphicSuffix :: Ty -> Ty -> String
polymorphicSuffix signature actualType =
  case evalState (visit signature actualType) [] of
    [] -> ""
    parts -> "__" ++ intercalate "_" parts
  where visit :: Ty -> Ty -> State VisitedTypes [String]
        visit sig actual =
          case (sig, actual) of
            (VarTy _, VarTy _) -> -- error $ "Unsolved variable in actual type: " ++ show sig ++ " => " ++ show actual ++
                                  --        " when calculating polymorphic suffix for " ++
                                  --        show signature ++ " => " ++ show actualType
                                  return ["UNSOLVED_VARIABLE"]
            (a@(VarTy _), b) -> do visitedTypeVariables <- get
                                   if a `elem` visitedTypeVariables
                                     then return []
                                     else do put (a : visitedTypeVariables) -- now it's visited
                                             return [tyToC b]
            (FuncTy argTysA retTyA, FuncTy argTysB retTyB) -> do visitedArgs <- fmap concat (zipWithM visit argTysA argTysB)
                                                                 visitedRets <- visit retTyA retTyB
                                                                 return (visitedArgs ++ visitedRets)
            (StructTy _ a, StructTy _ b) -> fmap concat (zipWithM visit a b)
            (PointerTy a, PointerTy b) -> visit a b
            (RefTy a, RefTy b) -> visit a b
            (_, _) -> return []

type VisitedTypes = [Ty]

-- | Templates are like macros, but defined inside the compiler and with access to the types they are instantiated with
data Template = Template { templateSignature :: Ty
                         , templateDeclaration :: Ty -> [Token] -- Will this parameterization ever be useful?
                         , templateDefinition :: Ty -> [Token]
                         , templateDependencies :: Ty -> [XObj]
                         }

instance Show Template where
  show _ = "Template"

-- | Note: This is to make comparisons of Environments possible, otherwise
-- | they are always different when they contain Templates.
instance Eq Template where
  a == b = templateSignature a == templateSignature b

-- | Tokens are used for emitting C code from templates.
data Token = TokTy Ty        -- | Some kind of type, will be looked up if it's a type variable.
           | TokC String     -- | Plain C code.
           | TokDecl         -- | Will emit the declaration (i.e. "foo(int x)"), this is useful
                             --   for avoiding repetition in the definition part of the template.
           | TokName         -- | Will emit the name of the instantiated function/variable.
           deriving (Eq, Ord)

instance Show Token where
  show (TokC s) = s
  show (TokTy t) = tyToC t
  show TokName = "<name>"
  show TokDecl = "<declaration>"

instantiateTemplate :: SymPath -> Ty -> Template -> (XObj, [XObj])
instantiateTemplate path actualType template =
    let defLst = [XObj (Instantiate template) Nothing Nothing, XObj (Sym path) Nothing Nothing]
        deps = templateDependencies template actualType
    in  (XObj (Lst defLst) (Just dummyInfo) (Just actualType), deps)

-- | Type aliases are used to create C-typedefs when those are needed.
defineTypeAlias :: String -> Ty -> XObj
defineTypeAlias name t = XObj (Lst [XObj (Defalias t) Nothing Nothing
                                   ,XObj (Sym (SymPath [] name)) Nothing Nothing
                                   ]) (Just dummyInfo) (Just TypeTy)

defineFunctionTypeAlias :: Ty -> XObj
defineFunctionTypeAlias aliasTy = defineTypeAlias (tyToC aliasTy) aliasTy

defineArrayTypeAlias :: Ty -> XObj
defineArrayTypeAlias t = defineTypeAlias (tyToC t) (StructTy "Array" [])

-- |
defineInterface :: String -> Ty -> XObj
defineInterface name t = XObj (Lst [XObj (Interface t) Nothing Nothing
                                   ,XObj (Sym (SymPath [] name)) Nothing Nothing
                                   ]) (Just dummyInfo) (Just InterfaceTy)

-- | Find out if a type is "external", meaning it is not defined by the user
--   in this program but instead imported from another C library or similar.
isExternalType :: TypeEnv -> Ty -> Bool
isExternalType typeEnv (PointerTy p) =
  isExternalType typeEnv p
isExternalType typeEnv (StructTy name _) =
  case lookupInEnv (SymPath [] name) (getTypeEnv typeEnv) of
    Just (_, Binder (XObj (Lst (XObj ExternalType _ _ : _)) _ _)) -> True
    Just _ -> False
    Nothing -> False
isExternalType _ _ =
  False

-- | Unsafe way of getting the type from an XObj
forceTy :: XObj -> Ty
forceTy xobj = fromMaybe (error ("No type in " ++ show xobj)) (ty xobj)

-- | Is this type managed - does it need to be freed?
isManaged :: TypeEnv -> Ty -> Bool
isManaged typeEnv (StructTy name _) =
  (name == "Array") || (
    case lookupInEnv (SymPath [] name) (getTypeEnv typeEnv) of
         Just (_, Binder (XObj (Lst (XObj ExternalType _ _ : _)) _ _)) -> False
         Just (_, Binder (XObj (Lst (XObj Typ _ _ : _)) _ _)) -> True
         Just (_, Binder (XObj wrong _ _)) -> error ("Invalid XObj in type env: " ++ show wrong)
         Nothing -> error ("Can't find " ++ name ++ " in type env."))
isManaged _ StringTy = True
isManaged _ _ = False

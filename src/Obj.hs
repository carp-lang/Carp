module Obj where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (intercalate, foldl')
import Data.Maybe (mapMaybe, fromMaybe, fromJust)
import Control.Monad.State
import Data.Char
import Types
import Util
import Debug.Trace

data SymbolMode = Symbol
                | LookupLocal
                | LookupGlobal
                | LookupGlobalOverride String -- Used to emit another name than the one used in the Carp program.
                deriving (Eq, Show)

-- | The canonical Lisp object.
data Obj = Sym SymPath SymbolMode
         | MultiSym String [SymPath] -- refering to multiple functions with the same name
         | InterfaceSym String -- refering to an interface. TODO: rename to InterfaceLookupSym?
         | Num Ty Double
         | Str String
         | Pattern String
         | Chr Char
         | Bol Bool
         | Lst [XObj]
         | Arr [XObj]
         | Defn
         | Def
         | Do
         | Let
         | While
         | Break
         | If
         | And
         | Or
         | Mod Env
         | Typ Ty
         | With
         | External (Maybe String)
         | ExternalType
         | Deftemplate TemplateCreator
         | Instantiate Template
         | Defalias Ty
         | Address
         | SetBang
         | Macro
         | Dynamic
         | Command CommandFunctionType
         | The
         | Ref
         | Interface Ty [SymPath]
         deriving (Show, Eq)


newtype CommandFunctionType = CommandFunction { getCommand :: ([XObj] -> StateT Context IO (Either EvalError XObj)) }

instance Eq CommandFunctionType where
  a == b = True

instance Show CommandFunctionType where
  show t = "CommandFunction { ... }"


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
             deriving (Eq, Ord)

instance Show Deleter where
  show (ProperDeleter path var) = "(ProperDel " ++ show path ++ " " ++ show var ++ ")"
  show (FakeDeleter var) = "(FakeDel " ++ show var ++ ")"

prettyInfo :: Info -> String
prettyInfo i =
  let line = infoLine i
      column = infoColumn i
      file = infoFile i
  in  (if line > -1 then "line " ++ show line else "unkown line") ++ ", " ++
      (if column > -1 then "column " ++ show column else "unknown column") ++
      " in '" ++ file ++ "'"

prettyInfoFromXObj :: XObj -> String
prettyInfoFromXObj xobj = case info xobj of
                            Just i -> prettyInfo i
                            Nothing -> "no info"

machineReadableInfo :: Info -> String
machineReadableInfo i =
  let line = infoLine i
      column = infoColumn i
      file = infoFile i
  in  file ++ ":" ++ show line ++ ":" ++ show column

machineReadableInfoFromXObj :: XObj -> String
machineReadableInfoFromXObj xobj = case info xobj of
                                     Just i -> machineReadableInfo i
                                     Nothing -> ""

-- TODO: change name of this function
freshVar :: Info -> String
freshVar i = "_" ++ show (infoIdentifier i)

-- | Obj with eXtra information.
data XObj = XObj { obj :: Obj
                 , info :: Maybe Info
                 , ty :: Maybe Ty
                 } deriving (Show, Eq)

getBinderDescription :: XObj -> String
getBinderDescription (XObj (Lst (XObj Defn _ _ : XObj (Sym _ _) _ _ : _)) _ _) = "defn"
getBinderDescription (XObj (Lst (XObj Def _ _ : XObj (Sym _ _) _ _ : _)) _ _) = "def"
getBinderDescription (XObj (Lst (XObj Macro _ _ : XObj (Sym _ _) _ _ : _)) _ _) = "macro"
getBinderDescription (XObj (Lst (XObj Dynamic _ _ : XObj (Sym _ _) _ _ : _)) _ _) = "dynamic"
getBinderDescription (XObj (Lst (XObj (Command _) _ _ : XObj (Sym _ _) _ _ : _)) _ _) = "command"
getBinderDescription (XObj (Lst (XObj (Deftemplate _) _ _ : XObj (Sym _ _) _ _ : _)) _ _) = "template"
getBinderDescription (XObj (Lst (XObj (Instantiate _) _ _ : XObj (Sym _ _) _ _ : _)) _ _) = "instantiate"
getBinderDescription (XObj (Lst (XObj (Defalias _) _ _ : XObj (Sym _ _) _ _ : _)) _ _) = "alias"
getBinderDescription (XObj (Lst (XObj (External _) _ _ : XObj (Sym _ _) _ _ : _)) _ _) = "external"
getBinderDescription (XObj (Lst (XObj ExternalType _ _ : XObj (Sym _ _) _ _ : _)) _ _) = "external-type"
getBinderDescription (XObj (Lst (XObj (Typ _) _ _ : XObj (Sym _ _) _ _ : _)) _ _) = "deftype"
getBinderDescription (XObj (Lst (XObj (Interface _ _) _ _ : XObj (Sym _ _) _ _ : _)) _ _) = "interface"
getBinderDescription b = error ("Unhandled binder: " ++ show b)

getName :: XObj -> String
getName xobj = show (getPath xobj)

-- | Extracts the second form (where the name of definitions are stored) from a list of XObj:s.
getPath :: XObj -> SymPath
getPath (XObj (Lst (XObj Defn _ _ : XObj (Sym path _) _ _ : _)) _ _) = path
getPath (XObj (Lst (XObj Def _ _ : XObj (Sym path _) _ _ : _)) _ _) = path
getPath (XObj (Lst (XObj Macro _ _ : XObj (Sym path _) _ _ : _)) _ _) = path
getPath (XObj (Lst (XObj (Deftemplate _) _ _ : XObj (Sym path _) _ _ : _)) _ _) = path
getPath (XObj (Lst (XObj (Instantiate _) _ _ : XObj (Sym path _) _ _ : _)) _ _) = path
getPath (XObj (Lst (XObj (Defalias _) _ _ : XObj (Sym path _) _ _ : _)) _ _) = path
getPath (XObj (Lst (XObj (External _) _ _ : XObj (Sym path _) _ _ : _)) _ _) = path
getPath (XObj (Lst (XObj ExternalType _ _ : XObj (Sym path _) _ _ : _)) _ _) = path
getPath (XObj (Lst (XObj (Typ _) _ _ : XObj (Sym path _) _ _ : _)) _ _) = path
getPath (XObj (Lst (XObj (Mod _) _ _ : XObj (Sym path _) _ _ : _)) _ _) = path
getPath (XObj (Lst (XObj (Interface _ _) _ _ : XObj (Sym path _) _ _ : _)) _ _) = path
getPath (XObj (Lst (XObj (Command _) _ _ : XObj (Sym path _) _ _ : _)) _ _) = path
getPath (XObj (Sym path _) _ _) = path
getPath x = SymPath [] (pretty x)

-- | Changes the second form (where the name of definitions are stored) in a list of XObj:s.
setPath :: XObj -> SymPath -> XObj
setPath (XObj (Lst (defn@(XObj Defn _ _) : XObj (Sym _ _) si st : rest)) i t) newPath =
  XObj (Lst (defn : XObj (Sym newPath Symbol) si st : rest)) i t
setPath (XObj (Lst [extr@(XObj (External _) _ _), XObj (Sym _ _) si st]) i t) newPath =
  XObj (Lst [extr, XObj (Sym newPath Symbol) si st]) i t
setPath x _ =
  error ("Can't set path on " ++ show x)

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
            Num _ _ -> error "Invalid number type."
            Str str -> show str
            Pattern str -> '#' : show str
            Chr c -> '\\' : c : ""
            Sym path _ -> show path
            MultiSym originalName paths -> originalName ++ "{" ++ joinWithComma (map show paths) ++ "}"
            InterfaceSym name -> name
            Bol b -> if b then "true" else "false"
            Defn -> "defn"
            Def -> "def"
            If -> "if"
            And -> "and"
            Or -> "or"
            While -> "while"
            Do -> "do"
            Let -> "let"
            Mod env -> fromMaybe "module" (envModuleName env)
            Typ _ -> "deftype"
            Deftemplate _ -> "deftemplate"
            Instantiate _ -> "instantiate"
            External Nothing -> "external"
            External (Just override) -> "external (override: " ++ show override ++ ")"
            ExternalType -> "external-type"
            Defalias _ -> "defalias"
            Address -> "address"
            SetBang -> "set!"
            Macro -> "macro"
            Dynamic -> "dynamic"
            Command _ -> "command"
            The -> "the"
            Ref -> "ref"
            Break -> "break"
            Interface _ _ -> "interface"
            With -> "with"

newtype EvalError = EvalError String deriving (Eq)

instance Show EvalError where
  show (EvalError msg) = msg

-- | Get the type of an XObj as a string.
typeStr :: XObj -> String
typeStr xobj = case ty xobj of
                 Nothing -> " : _"
                 Just t -> " : " ++ show t

-- | Get the identifier of an XObj as a string.
identifierStr :: XObj -> String
identifierStr xobj = case info xobj of
                       Just i -> "#" ++ show (infoIdentifier i)
                       Nothing -> "#?"

-- | Get the deleters of an XObj as a string.
deletersStr :: XObj -> String
deletersStr xobj = case info xobj of
                     Just i -> joinWithComma (map show (Set.toList (infoDelete i)))
                     Nothing -> ""

-- | Convert XObj to pretty string representation with type annotations.
prettyTyped :: XObj -> String
prettyTyped = visit 0
  where visit :: Int -> XObj -> String
        visit indent xobj =
          let suffix = typeStr xobj ++ " " ++
                       identifierStr xobj ++ " " ++
                       deletersStr xobj ++ " " ++
                       "\n"
          in case obj xobj of
               Lst lst -> "(" ++ joinWithSpace (map (visit indent) lst) ++ ")" ++ suffix
               Arr arr -> "[" ++ joinWithSpace (map (visit indent) arr) ++ "]" ++ suffix
               _ -> pretty xobj ++ suffix

-- | Datatype for holding meta data about a binder, like type annotation or docstring.
newtype MetaData = MetaData { getMeta :: Map.Map String XObj } deriving (Eq, Show)

emptyMeta :: MetaData
emptyMeta = (MetaData Map.empty)

-- | Wraps and holds an XObj in an environment.
data Binder = Binder { binderMeta :: MetaData, binderXObj :: XObj } deriving Eq

instance Show Binder where
  show binder = showBinderIndented 0 (getName (binderXObj binder), binder)

showBinderIndented :: Int -> (String, Binder) -> String
showBinderIndented indent (name, Binder _ (XObj (Mod env) _ _)) =
  replicate indent ' ' ++ name ++ " : Module = {\n" ++
  prettyEnvironmentIndented (indent + 4) env ++
  "\n" ++ replicate indent ' ' ++ "}"
showBinderIndented indent (name, Binder _ (XObj (Lst [XObj (Interface t paths) _ _, _]) _ _)) =
  replicate indent ' ' ++ name ++ " : " ++ show t ++ " = {\n    " ++
  joinWith "\n    " (map show paths) ++
  "\n" ++ replicate indent ' ' ++ "}"
showBinderIndented indent (name, Binder _ xobj) =
  replicate indent ' ' ++ name ++
  -- " (" ++ show (getPath xobj) ++ ")" ++
  " : " ++ showMaybeTy (ty xobj)
  -- ++ " <" ++ getBinderDescription xobj ++ ">"

-- | Get a list of pairs from a deftype declaration.
memberXObjsToPairs :: [XObj] -> [(String, Ty)]
memberXObjsToPairs xobjs = map (\(n, t) -> (mangle (getName n), fromJust (xobjToTy t))) (pairwise xobjs)

replaceGenericTypeSymbolsOnMembers :: Map.Map String Ty -> [XObj] -> [XObj]
replaceGenericTypeSymbolsOnMembers mappings memberXObjs =
  concatMap (\(v, t) -> [v, replaceGenericTypeSymbols mappings t]) (pairwise memberXObjs)

replaceGenericTypeSymbols :: Map.Map String Ty -> XObj -> XObj
replaceGenericTypeSymbols mappings xobj@(XObj (Sym (SymPath pathStrings name) _) i t) =
  let Just perhapsTyVar = xobjToTy xobj
  in if isFullyGenericType perhapsTyVar
     then case Map.lookup name mappings of
            Just found -> tyToXObj found
            Nothing -> xobj -- error ("Failed to concretize member '" ++ name ++ "' at " ++ prettyInfoFromXObj xobj ++ ", mappings: " ++ show mappings)
     else xobj
replaceGenericTypeSymbols mappings (XObj (Lst lst) i t) =
  (XObj (Lst (map (replaceGenericTypeSymbols mappings) lst)) i t)
replaceGenericTypeSymbols mappings (XObj (Arr arr) i t) =
  (XObj (Arr (map (replaceGenericTypeSymbols mappings) arr)) i t)
replaceGenericTypeSymbols _ xobj = xobj

-- | Convert a Ty to the s-expression that represents that type.
-- | TODO: Add more cases and write tests for this.
tyToXObj :: Ty -> XObj
tyToXObj (StructTy n []) = XObj (Sym (SymPath [] n) Symbol) Nothing Nothing
tyToXObj (StructTy n vs) = XObj (Lst ((XObj (Sym (SymPath [] n) Symbol) Nothing Nothing) : (map tyToXObj vs))) Nothing Nothing
tyToXObj (RefTy t) = XObj (Lst [(XObj (Sym (SymPath [] "Ref") Symbol) Nothing Nothing), tyToXObj t]) Nothing Nothing
tyToXObj (PointerTy t) = XObj (Lst [(XObj (Sym (SymPath [] "Ptr") Symbol) Nothing Nothing), tyToXObj t]) Nothing Nothing
tyToXObj (FuncTy argTys returnTy) = XObj (Lst [(XObj (Sym (SymPath [] "Fn") Symbol) Nothing Nothing), XObj (Arr (map tyToXObj argTys)) Nothing Nothing, tyToXObj returnTy]) Nothing Nothing
tyToXObj x = XObj (Sym (SymPath [] (show x)) Symbol) Nothing Nothing

-- | Helper function to create binding pairs for registering external functions.
register :: String -> Ty -> (String, Binder)
register name t = (name, Binder emptyMeta
                    (XObj (Lst [XObj (External Nothing) Nothing Nothing,
                                XObj (Sym (SymPath [] name) Symbol) Nothing Nothing])
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
                       , projectPrintTypedAST :: Bool
                       , projectCompiler :: String
                       , projectEchoCompilationCommand :: Bool
                       }

projectFlags :: Project -> String
projectFlags proj = joinWithSpace (projectCFlags proj ++ projectLibFlags proj)

instance Show Project where
  show (Project
        title
        incl
        cFlags
        libFlags
        srcFiles
        echoC
        carpDir
        outDir
        prompt
        searchPaths
        printTypedAST
        compiler
        echoCompilationCommand
       ) =
    unlines [ "Title: " ++ title
            , "Compiler: " ++ compiler
            , "Includes:\n    " ++ joinWith "\n    " (map show incl)
            , "Cflags:\n    " ++ joinWith "\n    " cFlags
            , "Library flags:\n    " ++ joinWith "\n    " libFlags
            , "Carp source files:\n    " ++ joinWith "\n    " srcFiles
            , "Echo C: " ++ if echoC then "true" else "false"
            , "Echo compilation command: " ++ if echoCompilationCommand then "true" else "false"
            , "Output directory: " ++ outDir
            , "CARP_DIR: " ++ carpDir
            , "Prompt: " ++ prompt
            , "Search paths for 'load' command:\n    " ++ joinWith  "\n    " searchPaths
            , "Print AST (with 'info' command): " ++ if printTypedAST then "true" else "false"
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
xobjToTy (XObj (Sym (SymPath _ "Int") _) _ _) = Just IntTy
xobjToTy (XObj (Sym (SymPath _ "Float") _) _ _) = Just FloatTy
xobjToTy (XObj (Sym (SymPath _ "Double") _) _ _) = Just DoubleTy
xobjToTy (XObj (Sym (SymPath _ "Long") _) _ _) = Just LongTy
xobjToTy (XObj (Sym (SymPath _ "String") _) _ _) = Just StringTy
xobjToTy (XObj (Sym (SymPath _ "Pattern") _) _ _) = Just PatternTy
xobjToTy (XObj (Sym (SymPath _ "Char") _) _ _) = Just CharTy
xobjToTy (XObj (Sym (SymPath _ "Bool") _) _ _) = Just BoolTy
xobjToTy (XObj (Sym (SymPath _ s@(firstLetter:_)) _) _ _) | isLower firstLetter = Just (VarTy s)
                                                          | otherwise = Just (StructTy s [])
xobjToTy (XObj (Lst [XObj (Sym (SymPath _ "Ptr") _) _ _, innerTy]) _ _) =
  do okInnerTy <- xobjToTy innerTy
     return (PointerTy okInnerTy)
xobjToTy (XObj (Lst (XObj (Sym (SymPath _ "Ptr") _) _ _ : _)) _ _) =
  Nothing
xobjToTy (XObj (Lst [XObj (Sym (SymPath _ "Ref") _) _ _, innerTy]) _ _) =
  do okInnerTy <- xobjToTy innerTy
     return (RefTy okInnerTy)
xobjToTy (XObj (Lst [XObj Ref i t, innerTy]) _ _) = -- This enables parsing of '&'
  do okInnerTy <- xobjToTy innerTy
     return (RefTy okInnerTy)
xobjToTy (XObj (Lst (XObj (Sym (SymPath _ "Ref") _) _ _ : _)) _ _) =
  Nothing
xobjToTy (XObj (Lst [XObj (Sym (SymPath path "╬╗") _) fi ft, XObj (Arr argTys) ai at, retTy]) i t) =
  xobjToTy (XObj (Lst [XObj (Sym (SymPath path "Fn") Symbol) fi ft, XObj (Arr argTys) ai at, retTy]) i t)
xobjToTy (XObj (Lst [XObj (Sym (SymPath path "λ") _) fi ft, XObj (Arr argTys) ai at, retTy]) i t) =
  xobjToTy (XObj (Lst [XObj (Sym (SymPath path "Fn") Symbol) fi ft, XObj (Arr argTys) ai at, retTy]) i t)
xobjToTy (XObj (Lst [XObj (Sym (SymPath _ "Fn") _) _ _, XObj (Arr argTys) _ _, retTy]) _ _) =
  do okArgTys <- mapM xobjToTy argTys
     okRetTy <- xobjToTy retTy
     return (FuncTy okArgTys okRetTy)
xobjToTy (XObj (Lst []) _ _) = Just UnitTy
xobjToTy (XObj (Lst (x:xs)) _ _) =
  do okX <- xobjToTy x
     okXS <- mapM xobjToTy xs
     case okX of
       (StructTy n []) -> return (StructTy n okXS)
       (VarTy n) -> return (StructTy n okXS) -- Struct type with type variable as a name, i.e. "(a b)"
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
                                  return ["?"]
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
    let defLst = [XObj (Instantiate template) Nothing Nothing, XObj (Sym path Symbol) Nothing Nothing]
        deps = templateDependencies template actualType
    in  (XObj (Lst defLst) (Just (Info (-1) (-1) (show path ++ " template") Set.empty (-1))) (Just actualType), deps)

-- | Type aliases are used to create C-typedefs when those are needed.
defineTypeAlias :: String -> Ty -> XObj
defineTypeAlias name t = XObj (Lst [XObj (Defalias t) Nothing Nothing
                                   ,XObj (Sym (SymPath [] name) Symbol) Nothing Nothing
                                   ]) (Just dummyInfo) (Just TypeTy)

defineFunctionTypeAlias :: Ty -> XObj
defineFunctionTypeAlias aliasTy = defineTypeAlias (tyToC aliasTy) aliasTy

defineArrayTypeAlias :: Ty -> XObj
defineArrayTypeAlias t = defineTypeAlias (tyToC t) (StructTy "Array" [])

-- |
defineInterface :: String -> Ty -> [SymPath] -> Maybe Info -> XObj
defineInterface name t paths info =
  XObj (Lst [XObj (Interface t paths) Nothing Nothing
            ,XObj (Sym (SymPath [] name) Symbol) Nothing Nothing
            ])
  info (Just InterfaceTy)

-- | Unsafe way of getting the type from an XObj
forceTy :: XObj -> Ty
forceTy xobj = fromMaybe (error ("No type in " ++ show xobj)) (ty xobj)

-- | How should the compiler be run? Interactively or just build / build & run and then quit?
data ExecutionMode = Repl | Build | BuildAndRun | Check deriving (Show, Eq)

-- | Information needed by the REPL
data Context = Context { contextGlobalEnv :: Env
                       , contextTypeEnv :: TypeEnv
                       , contextPath :: [String]
                       , contextProj :: Project
                       , contextLastInput :: String
                       , contextExecMode :: ExecutionMode
                       } deriving Show

popModulePath :: Context -> Context
popModulePath ctx = ctx { contextPath = init (contextPath ctx) }

-- | Unwrapping of XObj:s

-- | String
unwrapStringXObj :: XObj -> Either String String
unwrapStringXObj (XObj (Str s) _ _) = Right s
unwrapStringXObj x = Left ("The value '" ++ pretty x ++ "' at " ++ prettyInfoFromXObj x ++ " is not a String.")

-- | Bool
unwrapBoolXObj :: XObj -> Either String Bool
unwrapBoolXObj (XObj (Bol b) _ _) = Right b
unwrapBoolXObj x = Left ("The value '" ++ pretty x ++ "' at " ++ prettyInfoFromXObj x ++ " is not a Bool.")

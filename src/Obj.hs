{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Obj where

import Control.Applicative
import Control.Monad.State
import Data.Char
import Data.Hashable
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Info
import qualified Map
import Project
import qualified Set
import SymPath
import Types
import TypesToC
import Util

-- | Will the lookup look at other Carp code or at C code. This matters when calling functions, should they assume it's a lambda or a normal C function?
data GlobalMode
  = CarpLand
  | ExternalCode
  deriving (Eq, Show, Ord, Generic)

instance Hashable GlobalMode

-- | Will the lookup look at a global variable
data DefinitionMode
  = AVariable
  | AFunction
  deriving (Eq, Show, Ord, Generic)

instance Hashable DefinitionMode

-- | For local lookups, does the variable live in the current function or is it captured from outside it's body?
data CaptureMode
  = NoCapture
  | Capture Int
  deriving (Eq, Show, Ord, Generic)

instance Hashable CaptureMode

-- | A symbol knows a bit about what it refers to - is it a local scope or a global one? (the latter include modules).
-- | A symbol that is not used for looking up things will use the 'Symbol' case.
data SymbolMode
  = Symbol
  | LookupLocal CaptureMode
  | LookupRecursive
  | LookupGlobal GlobalMode DefinitionMode
  | LookupGlobalOverride String -- Used to emit another name than the one used in the Carp program.
  deriving (Eq, Show, Ord, Generic)

instance Hashable SymbolMode

isLookupGlobal :: SymbolMode -> Bool
isLookupGlobal (LookupGlobal _ _) = True
isLookupGlobal _ = False

isLookupLocal :: SymbolMode -> Bool
isLookupLocal (LookupLocal _) = True
isLookupLocal _ = False

data MatchMode = MatchValue | MatchRef deriving (Eq, Show, Generic)

instance Hashable MatchMode

data Number = Floating Double | Integral Int deriving (Generic)

instance Hashable Number

instance Eq Number where
  (Floating a) == (Floating b) = a == b
  (Integral a) == (Integral b) = a == b
  (Floating a) == (Integral b) = a == fromIntegral b
  (Integral a) == (Floating b) = fromIntegral a == b

instance Ord Number where
  (Floating a) <= (Floating b) = a <= b
  (Integral a) <= (Integral b) = a <= b
  (Floating a) <= (Integral b) = a <= fromIntegral b
  (Integral a) <= (Floating b) = fromIntegral a <= b

instance Num Number where
  (Floating a) + (Floating b) = Floating (a + b)
  (Integral a) + (Integral b) = Integral (a + b)
  (Floating a) + (Integral b) = Floating (a + fromIntegral b)
  (Integral a) + (Floating b) = Floating (fromIntegral a + b)
  (Integral a) * (Integral b) = Integral (a * b)
  (Floating a) * (Floating b) = Floating (a * b)
  (Integral a) * (Floating b) = Floating (fromIntegral a * b)
  (Floating a) * (Integral b) = Floating (a * fromIntegral b)
  negate (Floating a) = Floating (negate a)
  negate (Integral a) = Integral (negate a)
  abs (Floating a) = Floating (abs a)
  abs (Integral a) = Integral (abs a)
  signum (Floating a) = Floating (signum a)
  signum (Integral a) = Integral (signum a)
  fromInteger a = Integral (fromInteger a)

instance Real Number where
  toRational (Integral a) = toRational a
  toRational (Floating a) = toRational a

instance Enum Number where
  toEnum a = Integral (toEnum a)
  fromEnum (Integral a) = fromEnum a
  fromEnum (Floating _) = error "floating fromenum"

instance Fractional Number where
  fromRational a = Floating (fromRational a)
  recip (Floating a) = Floating (recip a)
  recip (Integral _) = error "integral recip"

instance Integral Number where
  quotRem (Integral a) (Integral b) = let (q, r) = quotRem a b in (Integral q, Integral r)
  quotRem _ _ = error "quotRem"
  toInteger (Integral a) = toInteger a
  toInteger _ = error "toInteger"

instance Show Number where
  show (Floating a) = show a
  show (Integral a) = show a

-- | The canonical Lisp object.
data Obj
  = Sym SymPath SymbolMode
  | MultiSym String [SymPath] -- refering to multiple functions with the same name
  | InterfaceSym String -- refering to an interface. TODO: rename to InterfaceLookupSym?
  | Num Ty Number
  | Str String
  | Pattern String
  | Chr Char
  | Bol Bool
  | Lst [XObj]
  | Arr [XObj]
  | StaticArr [XObj]
  | Dict (Map.Map XObj XObj)
  | Closure XObj ClosureContext
  | Defn (Maybe (Set.Set XObj)) -- if this is a lifted lambda it needs the set of captured variables
  | Def
  | Fn (Maybe SymPath) (Set.Set XObj) -- the name of the lifted function, the set of variables this lambda captures, and a dynamic environment
  | Do
  | Let
  | LocalDef
  | While
  | Break
  | If
  | Match MatchMode
  | Mod Env TypeEnv
  | Deftype Ty
  | DefSumtype Ty
  | With
  | External (Maybe String)
  | ExternalType (Maybe String)
  | MetaStub
  | Deftemplate TemplateCreator
  | Instantiate Template
  | Defalias Ty
  | SetBang
  | Macro
  | Dynamic -- DefnDynamic
  | DefDynamic
  | Command CommandFunctionType
  | Primitive PrimitiveFunctionType
  | The
  | Ref
  | Deref
  | Interface Ty [SymPath]
  | C String -- C literal
  deriving (Show, Eq, Generic)

instance Hashable Obj

isGlobalFunc :: XObj -> Bool
isGlobalFunc xobj =
  case xobj of
    XObj (InterfaceSym _) _ (Just FuncTy {}) -> True
    XObj (MultiSym _ _) _ (Just FuncTy {}) -> True
    XObj (Sym _ (LookupGlobal _ _)) _ (Just FuncTy {}) -> True
    XObj (Sym _ (LookupGlobalOverride _)) _ (Just FuncTy {}) -> True
    _ -> False

isNumericLiteral :: XObj -> Bool
isNumericLiteral (XObj (Num _ _) _ _) = True
isNumericLiteral (XObj (Bol _) _ _) = True
isNumericLiteral (XObj (Chr _) _ _) = True
isNumericLiteral _ = False

-- | Is the given XObj an unqualified symbol.
isUnqualifiedSym :: XObj -> Bool
isUnqualifiedSym (XObj (Sym (SymPath [] _) _) _ _) = True
isUnqualifiedSym _ = False

isSym :: XObj -> Bool
isSym (XObj (Sym (SymPath _ _) _) _ _) = True
isSym _ = False

isSpecialSym :: XObj -> Bool
isSpecialSym (XObj (Sym (SymPath [] s) _) _ _) =
  elem s ["defn", "def", "do", "while", "fn", "let", "break", "if", "match", "match-ref", "set!", "the", "ref", "deref", "with"]
isSpecialSym _ = False

isArray :: XObj -> Bool
isArray (XObj (Arr _) _ _) = True
isArray _ = False

isLiteral :: XObj -> Bool
isLiteral (XObj (Num _ _) _ _) = True
isLiteral (XObj (Chr _) _ _) = True
isLiteral (XObj (Bol _) _ _) = True
isLiteral _ = False

isExternalFunction :: XObj -> Bool
isExternalFunction (XObj (Lst (XObj (External _) _ _ : _)) _ _) = True
isExternalFunction _ = False

isTypeDef :: XObj -> Bool
isTypeDef (XObj (Lst (XObj (Defalias _) _ _ : _)) _ _) = True
isTypeDef (XObj (Lst (XObj (Deftype _) _ _ : _)) _ _) = True
isTypeDef (XObj (Lst (XObj (DefSumtype _) _ _ : _)) _ _) = True
isTypeDef _ = False

-- | This instance is needed for the dynamic Dictionary
instance Ord Obj where
  compare (Str a) (Str b) = compare a b
  compare (Num _ a) (Num _ b) = compare a b
  compare a b = compare (show a) (show b)

-- TODO: handle comparison of lists, arrays and dictionaries

type NullaryPrimitiveCallback = XObj -> Context -> IO (Context, Either EvalError XObj)

type UnaryPrimitiveCallback = XObj -> Context -> XObj -> IO (Context, Either EvalError XObj)

type BinaryPrimitiveCallback = XObj -> Context -> XObj -> XObj -> IO (Context, Either EvalError XObj)

type TernaryPrimitiveCallback = XObj -> Context -> XObj -> XObj -> XObj -> IO (Context, Either EvalError XObj)

type QuaternaryPrimitiveCallback = XObj -> Context -> XObj -> XObj -> XObj -> XObj -> IO (Context, Either EvalError XObj)

type VariadicPrimitiveCallback = XObj -> Context -> [XObj] -> IO (Context, Either EvalError XObj)

-- | The type of primitive functions. See Primitives.hs
data PrimitiveFunctionType
  = NullaryPrimitive NullaryPrimitiveCallback
  | UnaryPrimitive UnaryPrimitiveCallback
  | BinaryPrimitive BinaryPrimitiveCallback
  | TernaryPrimitive TernaryPrimitiveCallback
  | QuaternaryPrimitive QuaternaryPrimitiveCallback
  | VariadicPrimitive VariadicPrimitiveCallback

instance Hashable PrimitiveFunctionType where
  hashWithSalt s = const s

instance Eq PrimitiveFunctionType where
  _ == _ = True

instance Show PrimitiveFunctionType where
  show _ = "Primitive { ... }"

type NullaryCommandCallback = Context -> IO (Context, Either EvalError XObj)

type UnaryCommandCallback = Context -> XObj -> IO (Context, Either EvalError XObj)

type BinaryCommandCallback = Context -> XObj -> XObj -> IO (Context, Either EvalError XObj)

type TernaryCommandCallback = Context -> XObj -> XObj -> XObj -> IO (Context, Either EvalError XObj)

type VariadicCommandCallback = Context -> [XObj] -> IO (Context, Either EvalError XObj)

data CommandFunctionType
  = NullaryCommandFunction NullaryCommandCallback
  | UnaryCommandFunction UnaryCommandCallback
  | BinaryCommandFunction BinaryCommandCallback
  | TernaryCommandFunction TernaryCommandCallback
  | VariadicCommandFunction VariadicCommandCallback

instance Hashable CommandFunctionType where
  hashWithSalt s = const s

instance Eq CommandFunctionType where
  _ == _ = True

instance Show CommandFunctionType where
  show (NullaryCommandFunction _) = "NullaryCommandFunction { ... }"
  show (UnaryCommandFunction _) = "UnaryCommandFunction { ... }"
  show (BinaryCommandFunction _) = "BinaryCommandFunction { ... }"
  show (TernaryCommandFunction _) = "TernaryCommandFunction { ... }"
  show (VariadicCommandFunction _) = "VariadicCommandFunction { ... }"

newtype TemplateCreator = TemplateCreator {getTemplateCreator :: TypeEnv -> Env -> Template}

instance Hashable TemplateCreator where
  hashWithSalt s = const s

instance Show TemplateCreator where
  show _ = "TemplateCreator"

-- | Note: This is to make comparisons of Environments possible, otherwise
-- | they are always different when they contain TemplateCreators.
instance Eq TemplateCreator where
  _ == _ = True

prettyInfoFromXObj :: XObj -> String
prettyInfoFromXObj xobj = maybe "no info" prettyInfo (xobjInfo xobj)

machineReadableInfoFromXObj :: FilePathPrintLength -> XObj -> String
machineReadableInfoFromXObj fppl xobj =
  case xobjInfo xobj of
    Just i -> machineReadableInfo fppl i
    Nothing -> ""

-- | Obj with eXtra information.
data XObj = XObj
  { xobjObj :: Obj,
    xobjInfo :: Maybe Info,
    xobjTy :: Maybe Ty
  }
  deriving (Show, Eq, Ord)

instance Hashable XObj where
  hashWithSalt s XObj {..} = s `hashWithSalt` xobjObj

getBinderDescription :: XObj -> String
getBinderDescription (XObj (Lst (XObj (Defn _) _ _ : XObj (Sym _ _) _ _ : _)) _ _) = "defn"
getBinderDescription (XObj (Lst (XObj Def _ _ : XObj (Sym _ _) _ _ : _)) _ _) = "def"
getBinderDescription (XObj (Lst (XObj Macro _ _ : XObj (Sym _ _) _ _ : _)) _ _) = "macro"
getBinderDescription (XObj (Lst (XObj DefDynamic _ _ : XObj (Sym _ _) _ _ : _)) _ _) = "defdynamic"
getBinderDescription (XObj (Lst (XObj Dynamic _ _ : XObj (Sym _ _) _ _ : _)) _ _) = "dynamic"
getBinderDescription (XObj (Lst (XObj (Command _) _ _ : XObj (Sym _ _) _ _ : _)) _ _) = "command"
getBinderDescription (XObj (Lst (XObj (Primitive _) _ _ : XObj (Sym _ _) _ _ : _)) _ _) = "primitive"
getBinderDescription (XObj (Lst (XObj (Deftemplate _) _ _ : XObj (Sym _ _) _ _ : _)) _ _) = "template"
getBinderDescription (XObj (Lst (XObj (Instantiate _) _ _ : XObj (Sym _ _) _ _ : _)) _ _) = "instantiate"
getBinderDescription (XObj (Lst (XObj (Defalias _) _ _ : XObj (Sym _ _) _ _ : _)) _ _) = "alias"
getBinderDescription (XObj (Lst (XObj (External _) _ _ : XObj (Sym _ _) _ _ : _)) _ _) = "external"
getBinderDescription (XObj (Lst (XObj (ExternalType _) _ _ : XObj (Sym _ _) _ _ : _)) _ _) = "external-type"
getBinderDescription (XObj (Lst (XObj MetaStub _ _ : XObj (Sym _ _) _ _ : _)) _ _) = "meta-stub"
getBinderDescription (XObj (Lst (XObj (Deftype _) _ _ : XObj (Sym _ _) _ _ : _)) _ _) = "deftype"
getBinderDescription (XObj (Lst (XObj (DefSumtype _) _ _ : XObj (Sym _ _) _ _ : _)) _ _) = "deftype"
getBinderDescription (XObj (Lst (XObj (Interface _ _) _ _ : XObj (Sym _ _) _ _ : _)) _ _) = "interface"
getBinderDescription (XObj (Mod _ _) _ _) = "module"
getBinderDescription b = error ("Unhandled binder: " ++ show b)

getName :: XObj -> String
getName xobj = show (getPath xobj)

getSimpleName :: XObj -> String
getSimpleName xobj = let SymPath _ name = getPath xobj in name

getSimpleNameWithArgs :: XObj -> Maybe String
getSimpleNameWithArgs xobj@(XObj (Lst (XObj (Defn _) _ _ : _ : XObj (Arr args) _ _ : _)) _ _) =
  Just $
    "(" ++ getSimpleName xobj ++ (if not (null args) then " " else "")
      ++ unwords (map getSimpleName args)
      ++ ")"
getSimpleNameWithArgs xobj@(XObj (Lst (XObj Macro _ _ : _ : XObj (Arr args) _ _ : _)) _ _) =
  Just $
    "(" ++ getSimpleName xobj ++ (if not (null args) then " " else "")
      ++ unwords (map getSimpleName args)
      ++ ")"
getSimpleNameWithArgs xobj@(XObj (Lst (XObj Dynamic _ _ : _ : XObj (Arr args) _ _ : _)) _ _) =
  Just $
    "(" ++ getSimpleName xobj ++ (if not (null args) then " " else "")
      ++ unwords (map getSimpleName args)
      ++ ")"
getSimpleNameWithArgs _ = Nothing

-- | Extracts the second form (where the name of definitions are stored) from a list of XObj:s.
getPath :: XObj -> SymPath
getPath (XObj (Lst (XObj (Defn _) _ _ : XObj (Sym path _) _ _ : _)) _ _) = path
getPath (XObj (Lst (XObj Def _ _ : XObj (Sym path _) _ _ : _)) _ _) = path
getPath (XObj (Lst (XObj LocalDef _ _ : XObj (Sym path _) _ _ : _)) _ _) = path
getPath (XObj (Lst (XObj Macro _ _ : XObj (Sym path _) _ _ : _)) _ _) = path
getPath (XObj (Lst (XObj Dynamic _ _ : XObj (Sym path _) _ _ : _)) _ _) = path
getPath (XObj (Lst (XObj DefDynamic _ _ : XObj (Sym path _) _ _ : _)) _ _) = path
getPath (XObj (Lst (XObj (Deftemplate _) _ _ : XObj (Sym path _) _ _ : _)) _ _) = path
getPath (XObj (Lst (XObj (Instantiate _) _ _ : XObj (Sym path _) _ _ : _)) _ _) = path
getPath (XObj (Lst (XObj (Defalias _) _ _ : XObj (Sym path _) _ _ : _)) _ _) = path
getPath (XObj (Lst (XObj (External _) _ _ : XObj (Sym path _) _ _ : _)) _ _) = path
getPath (XObj (Lst (XObj (ExternalType _) _ _ : XObj (Sym path _) _ _ : _)) _ _) = path
getPath (XObj (Lst (XObj MetaStub _ _ : XObj (Sym path _) _ _ : _)) _ _) = path
getPath (XObj (Lst (XObj (Deftype _) _ _ : XObj (Sym path _) _ _ : _)) _ _) = path
getPath (XObj (Lst (XObj (Mod _ _) _ _ : XObj (Sym path _) _ _ : _)) _ _) = path
getPath (XObj (Lst (XObj (Interface _ _) _ _ : XObj (Sym path _) _ _ : _)) _ _) = path
getPath (XObj (Lst (XObj (Command _) _ _ : XObj (Sym path _) _ _ : _)) _ _) = path
getPath (XObj (Lst (XObj (Primitive _) _ _ : XObj (Sym path _) _ _ : _)) _ _) = path
getPath (XObj (Sym path _) _ _) = path
getPath x = SymPath [] (pretty x)

getBinderPath :: Binder -> SymPath
getBinderPath = getPath . binderXObj

-- | Changes the second form (where the name of definitions are stored) in a list of XObj:s.
setPath :: XObj -> SymPath -> XObj
setPath (XObj (Lst (defn@(XObj (Defn _) _ _) : XObj (Sym _ _) si st : rest)) i t) newPath =
  XObj (Lst (defn : XObj (Sym newPath Symbol) si st : rest)) i t
setPath (XObj (Lst [extr@(XObj (External _) _ _), XObj (Sym _ _) si st, ty]) i t) newPath =
  XObj (Lst [extr, XObj (Sym newPath Symbol) si st, ty]) i t
setPath (XObj (Lst (def@(XObj Def _ _) : XObj (Sym _ _) si st : rest)) i t) newPath =
  XObj (Lst (def : XObj (Sym newPath Symbol) si st : rest)) i t
setPath x _ =
  error ("Can't set path on " ++ show x)

-- | Convert an Obj to a pretty string representation.
-- | Reuses `pretty`.
prettyObj :: Obj -> String
prettyObj = pretty . buildXObj
  where
    buildXObj o = XObj o Nothing Nothing

-- | Convert an XObj to a pretty string representation.
pretty :: XObj -> String
pretty = visit 0
  where
    visit :: Int -> XObj -> String
    visit indent xobj =
      case xobjObj xobj of
        C c -> show c
        Lst lst -> "(" ++ joinWithSpace (map (visit indent) lst) ++ ")"
        Arr arr -> "[" ++ joinWithSpace (map (visit indent) arr) ++ "]"
        StaticArr arr -> "$[" ++ joinWithSpace (map (visit indent) arr) ++ "]"
        Dict dict -> "{" ++ joinWithSpace (map (visit indent) (concatMap (\(a, b) -> [a, b]) (Map.toList dict))) ++ "}"
        Num IntTy num -> show num
        Num LongTy num -> show num ++ "l"
        Num ByteTy num -> show num ++ "b"
        Num FloatTy num -> show num ++ "f"
        Num DoubleTy num -> show num
        Num _ _ -> error "Invalid number type."
        Str str -> show str
        Pattern str -> '#' : show str
        Chr c -> '\\' : c : ""
        Sym path _ -> show path -- ++ " <" ++ show mode ++ ">"
        MultiSym originalName paths -> originalName ++ "{" ++ joinWithComma (map show paths) ++ "}"
        InterfaceSym name -> name -- ++ "§"
        Bol b -> if b then "true" else "false"
        Defn maybeCaptures ->
          "defn" ++ case maybeCaptures of
            Just captures -> " <" ++ prettyCaptures captures ++ ">"
            Nothing -> ""
        Def -> "def"
        Fn _ captures -> "fn" ++ " <" ++ prettyCaptures captures ++ ">"
        Closure elt _ -> "closure<" ++ pretty elt ++ ">"
        If -> "if"
        Match MatchValue -> "match"
        Match MatchRef -> "match-ref"
        While -> "while"
        Do -> "do"
        Let -> "let"
        LocalDef -> "local-binding"
        Mod env _ -> fromMaybe "module" (envModuleName env)
        Deftype _ -> "deftype"
        DefSumtype _ -> "deftype"
        Deftemplate _ -> "deftemplate"
        Instantiate _ -> "instantiate"
        External Nothing -> "external"
        External (Just override) -> "external (override: " ++ show override ++ ")"
        ExternalType Nothing -> "external-type"
        ExternalType (Just override) -> "external-type (override: " ++ show override ++ ")"
        MetaStub -> "meta-stub"
        Defalias _ -> "defalias"
        SetBang -> "set!"
        Macro -> "macro"
        Dynamic -> "dynamic"
        DefDynamic -> "defdynamic"
        Command _ -> "command"
        Primitive _ -> "primitive"
        The -> "the"
        Ref -> "ref"
        Deref -> "deref"
        Break -> "break"
        Interface _ _ -> "interface"
        With -> "with"

prettyUpTo :: Int -> XObj -> String
prettyUpTo lim xobj =
  let prettied = pretty xobj
   in if length prettied > lim
        then take lim prettied ++ "..." ++ end
        else prettied
  where
    end =
      -- we match all of them explicitly to get errors if we forget one
      case xobjObj xobj of
        Lst _ -> ")"
        Arr _ -> "]"
        StaticArr _ -> "]"
        Dict _ -> "}"
        Num LongTy _ -> "l"
        Num IntTy _ -> ""
        Num ByteTy _ -> "b"
        Num FloatTy _ -> "f"
        Num DoubleTy _ -> ""
        Num _ _ -> error "Invalid number type."
        Str _ -> ""
        C _ -> ""
        Pattern _ -> ""
        Chr _ -> ""
        Sym _ _ -> ""
        MultiSym _ _ -> "}"
        InterfaceSym _ -> ""
        Bol _ -> ""
        Defn maybeCaptures ->
          case maybeCaptures of
            Just _ -> ">"
            Nothing -> ""
        Def -> ""
        Fn _ _ -> ">"
        Closure _ _ -> ">"
        If -> ""
        Match _ -> ""
        While -> ""
        Do -> ""
        Let -> ""
        LocalDef -> ""
        Mod _ _ -> ""
        Deftype _ -> ""
        DefSumtype _ -> ""
        Deftemplate _ -> ""
        Instantiate _ -> ""
        External Nothing -> ""
        External (Just _) -> ")"
        ExternalType Nothing -> ""
        ExternalType (Just _) -> ")"
        MetaStub -> ""
        Defalias _ -> ""
        SetBang -> ""
        Macro -> ""
        Dynamic -> ""
        DefDynamic -> ""
        Command _ -> ""
        Primitive _ -> ""
        The -> ""
        Ref -> ""
        Deref -> ""
        Break -> ""
        Interface _ _ -> ""
        With -> ""

prettyCaptures :: Set.Set XObj -> String
prettyCaptures captures =
  joinWithComma (map (\x -> getName x ++ " : " ++ maybe "" show (xobjTy x)) (Set.toList captures))

data EvalError
  = EvalError String [XObj] FilePathPrintLength (Maybe Info)
  | HasStaticCall XObj (Maybe Info)
  deriving (Eq)

instance Show EvalError where
  show (HasStaticCall xobj info) = "Expression " ++ pretty xobj ++ " has unexpected static call" ++ showInfo info
    where
      showInfo (Just i) = " at " ++ prettyInfo i ++ "."
      showInfo Nothing = ""
  show (EvalError msg t fppl info) = msg ++ showInfo info ++ getTrace
    where
      showInfo (Just i) = " at " ++ machineReadableInfo fppl i ++ "."
      showInfo Nothing = ""
      getTrace =
        if null t
          then ""
          else
            "\n\nTraceback:\n"
              ++ unlines (map (\x -> prettyUpTo 60 x ++ showInfo (xobjInfo x)) t)

-- | Get the type of an XObj as a string.
typeStr :: XObj -> String
typeStr xobj = case xobjTy xobj of
  Nothing -> "" --" : _"
  Just t -> " : " ++ show t

-- | Get the identifier of an XObj as a string.
identifierStr :: XObj -> String
identifierStr xobj = case xobjInfo xobj of
  Just i -> "#" ++ show (infoIdentifier i)
  Nothing -> "#?"

-- | Get the deleters of an XObj as a string.
deletersStr :: XObj -> String
deletersStr xobj = case xobjInfo xobj of
  Just i -> joinWithComma (map show (Set.toList (infoDelete i)))
  Nothing -> ""

-- | Convert XObj to pretty string representation with type annotations.
prettyTyped :: XObj -> String
prettyTyped = visit 0
  where
    visit :: Int -> XObj -> String
    visit indent xobj =
      let suffix =
            typeStr xobj ++ " "
              ++ identifierStr xobj
              ++ " "
              ++ deletersStr xobj
              ++ " "
              ++ "\n"
       in case xobjObj xobj of
            Lst lst ->
              listPrinter "(" ")" lst suffix indent
            Arr arr ->
              listPrinter "[" "]" arr suffix indent
            StaticArr arr ->
              listPrinter "$[" "]" arr suffix indent
            _ ->
              pretty xobj ++ suffix
    listPrinter :: String -> String -> [XObj] -> String -> Int -> String
    listPrinter opening closing xobjs suffix indent =
      opening ++ "   "
        ++ joinWith (spaces (indent + 4)) (map (visit (indent + 4)) xobjs)
        ++ spaces indent
        ++ closing
        ++ suffix

spaces :: Int -> String
spaces n = replicate n ' '

-- | Datatype for holding meta data about a binder, like type annotation or docstring.
newtype MetaData = MetaData {getMeta :: Map.Map String XObj} deriving (Eq, Show, Generic)

instance Hashable MetaData

emptyMeta :: MetaData
emptyMeta = MetaData Map.empty

metaIsTrue :: MetaData -> String -> Bool
metaIsTrue metaData key =
  case Map.lookup key (getMeta metaData) of
    Just (XObj (Bol True) _ _) -> True
    _ -> False

-- | Wraps and holds an XObj in an environment.
data Binder = Binder {binderMeta :: MetaData, binderXObj :: XObj} deriving (Eq, Generic)

instance Hashable Binder

instance Show Binder where
  show binder = showBinderIndented 0 False (getName (binderXObj binder), binder)

-- | Show a binder even if its hidden.
forceShowBinder :: Binder -> String
forceShowBinder binder = showBinderIndented 0 True (getName (binderXObj binder), binder)

showBinderIndented :: Int -> Bool -> (String, Binder) -> String
showBinderIndented indent _ (name, Binder _ (XObj (Mod env tenv) _ _)) =
  replicate indent ' ' ++ name ++ " : Module = {\n"
    ++ prettyEnvironmentIndented (indent + 4) env
    ++ "\n"
    ++ prettyEnvironmentIndented (indent + 4) (getTypeEnv tenv)
    ++ "\n"
    ++ replicate indent ' '
    ++ "}"
showBinderIndented indent _ (name, Binder _ (XObj (Lst [XObj (Interface t paths) _ _, _]) _ _)) =
  replicate indent ' ' ++ name ++ " : " ++ show t ++ " = {\n    "
    ++ joinWith "\n    " (map show paths)
    ++ "\n"
    ++ replicate indent ' '
    ++ "}"
showBinderIndented indent showHidden (name, Binder meta xobj) =
  if not showHidden && metaIsTrue meta "hidden"
    then ""
    else
      replicate indent ' ' ++ name
        ++
        -- " (" ++ show (getPath xobj) ++ ")" ++
        " : "
        ++ showMaybeTy (xobjTy xobj)

-- ++ " <" ++ getBinderDescription xobj ++ ">"

-- | Get a list of pairs from a deftype declaration.
memberXObjsToPairs :: [XObj] -> [(String, Ty)]
memberXObjsToPairs xobjs = map (\(n, t) -> (mangle (getName n), fromJustWithErrorMessage (xobjToTy t) ("Failed to convert " ++ show t ++ "\nPRETTY: " ++ pretty t ++ " from xobj to type."))) (pairwise xobjs)

fromJustWithErrorMessage :: Maybe Ty -> String -> Ty
fromJustWithErrorMessage (Just x) _ = x
fromJustWithErrorMessage Nothing msg = error msg

-- | Helper function to create binding pairs for registering external functions.
register :: String -> Ty -> (String, Binder)
register name t =
  ( name,
    Binder
      emptyMeta
      ( XObj
          ( Lst
              [ XObj (External Nothing) Nothing Nothing,
                XObj (Sym (SymPath [] name) Symbol) Nothing Nothing
              ]
          )
          (Just dummyInfo)
          (Just t)
      )
  )

data EnvMode = ExternalEnv | InternalEnv | RecursionEnv deriving (Show, Eq, Generic)

instance Hashable EnvMode

-- | Environment
data Env = Env
  { envBindings :: Map.Map String Binder,
    envParent :: Maybe Env,
    envModuleName :: Maybe String,
    envUseModules :: Set.Set SymPath,
    envMode :: EnvMode,
    envFunctionNestingLevel :: Int -- Normal defn:s have 0, lambdas get +1 for each level of nesting
  }
  deriving (Show, Eq, Generic)

instance Hashable Env

newtype ClosureContext = CCtx Context
  deriving (Show, Generic)

instance Hashable ClosureContext

instance Eq ClosureContext where
  _ == _ = True

newtype TypeEnv = TypeEnv {getTypeEnv :: Env} deriving (Generic, Eq)

instance Hashable TypeEnv

instance Show TypeEnv where
  show (TypeEnv env) = "(TypeEnv " ++ show env ++ ")"

safeEnvModuleName :: Env -> String
safeEnvModuleName env =
  case envModuleName env of
    Just name -> name ++ ", with parent " ++ parent
    Nothing -> "???, with parent " ++ parent
  where
    parent = maybe "Global" safeEnvModuleName (envParent env)

-- | Used by the compiler command "(env)"
prettyEnvironment :: Env -> String
prettyEnvironment = prettyEnvironmentIndented 0

prettyEnvironmentIndented :: Int -> Env -> String
prettyEnvironmentIndented indent env =
  joinLines $
    filter (/= "") (map (showBinderIndented indent False) (Map.toList (envBindings env)))
      ++ let modules = envUseModules env
          in if null modules
               then []
               else ("\n" ++ replicate indent ' ' ++ "Used modules:") : Set.toList (Set.map (showImportIndented indent) modules)

-- | For debugging nested environments
prettyEnvironmentChain :: Env -> String
prettyEnvironmentChain env =
  let bs = envBindings env
      name = fromMaybe "<env has no name>" (envModuleName env)
      otherInfo = "(" ++ show (envMode env) ++ ", lvl " ++ show (envFunctionNestingLevel env) ++ ")"
   in ( if length bs < 20
          then
            "'" ++ name ++ "' " ++ otherInfo ++ ":\n"
              ++ joinLines
                ( filter
                    (/= "")
                    (map (showBinderIndented 4 False) (Map.toList (envBindings env)))
                )
          else "'" ++ name ++ "' " ++ otherInfo ++ ":\n    Too big to show bindings."
      )
        ++ ( case envParent env of
               Just parent -> "\nWITH PARENT ENV " ++ prettyEnvironmentChain parent
               Nothing -> ""
           )

pathToEnv :: Env -> [String]
pathToEnv rootEnv = reverse (visit rootEnv)
  where
    visit env =
      case envModuleName env of
        Just name -> name : parent
        Nothing -> parent
      where
        parent = maybe [] visit (envParent env)

showImportIndented :: Int -> SymPath -> String
showImportIndented indent path = replicate indent ' ' ++ " * " ++ show path

incrementEnvNestLevel :: Env -> Env
incrementEnvNestLevel env =
  let current = envFunctionNestingLevel env
   in env {envFunctionNestingLevel = current + 1}

-- | Converts an S-expression to one of the Carp types.
xobjToTy :: XObj -> Maybe Ty
xobjToTy (XObj (Sym (SymPath _ "C") _) _ _) = Just CTy
xobjToTy (XObj (Sym (SymPath _ "Unit") _) _ _) = Just UnitTy
xobjToTy (XObj (Sym (SymPath _ "Int") _) _ _) = Just IntTy
xobjToTy (XObj (Sym (SymPath _ "Float") _) _ _) = Just FloatTy
xobjToTy (XObj (Sym (SymPath _ "Double") _) _ _) = Just DoubleTy
xobjToTy (XObj (Sym (SymPath _ "Long") _) _ _) = Just LongTy
xobjToTy (XObj (Sym (SymPath _ "Byte") _) _ _) = Just ByteTy
xobjToTy (XObj (Sym (SymPath _ "String") _) _ _) = Just StringTy
xobjToTy (XObj (Sym (SymPath _ "Pattern") _) _ _) = Just PatternTy
xobjToTy (XObj (Sym (SymPath _ "Char") _) _ _) = Just CharTy
xobjToTy (XObj (Sym (SymPath _ "Bool") _) _ _) = Just BoolTy
xobjToTy (XObj (Sym (SymPath _ "Static") _) _ _) = Just StaticLifetimeTy
xobjToTy (XObj (Sym spath@(SymPath _ s@(firstLetter : _)) _) _ _)
  | isLower firstLetter = Just (VarTy s)
  | otherwise = Just (StructTy (ConcreteNameTy spath) [])
xobjToTy (XObj (Lst [XObj (Sym (SymPath _ "Ptr") _) _ _, innerTy]) _ _) =
  do
    okInnerTy <- xobjToTy innerTy
    pure (PointerTy okInnerTy)
xobjToTy (XObj (Lst (XObj (Sym (SymPath _ "Ptr") _) _ _ : _)) _ _) =
  Nothing
xobjToTy (XObj (Lst [XObj (Sym (SymPath _ "Ref") _) _ _, innerTy]) i _) =
  do
    okInnerTy <- xobjToTy innerTy
    pure (RefTy okInnerTy (VarTy (makeTypeVariableNameFromInfo i)))
xobjToTy (XObj (Lst [XObj (Sym (SymPath _ "Ref") _) _ _, innerTy, lifetimeTy]) _ _) =
  do
    okInnerTy <- xobjToTy innerTy
    okLifetimeTy <- xobjToTy lifetimeTy
    pure (RefTy okInnerTy okLifetimeTy)
xobjToTy (XObj (Lst [XObj Ref _ _, innerTy]) i _) =
  -- This enables parsing of '&'
  do
    okInnerTy <- xobjToTy innerTy
    pure (RefTy okInnerTy (VarTy (makeTypeVariableNameFromInfo i)))
xobjToTy (XObj (Lst (XObj (Sym (SymPath _ "Ref") _) _ _ : _)) _ _) =
  Nothing
xobjToTy (XObj (Lst [XObj (Sym (SymPath path "╬╗") _) fi ft, XObj (Arr argTys) ai at, retTy]) i t) =
  xobjToTy (XObj (Lst [XObj (Sym (SymPath path "Fn") Symbol) fi ft, XObj (Arr argTys) ai at, retTy]) i t)
xobjToTy (XObj (Lst [XObj (Sym (SymPath path "λ") _) fi ft, XObj (Arr argTys) ai at, retTy]) i t) =
  xobjToTy (XObj (Lst [XObj (Sym (SymPath path "Fn") Symbol) fi ft, XObj (Arr argTys) ai at, retTy]) i t)
xobjToTy (XObj (Lst [XObj (Sym (SymPath _ "Fn") _) _ _, XObj (Arr argTys) _ _, retTy]) _ _) =
  do
    okArgTys <- mapM xobjToTy argTys
    okRetTy <- xobjToTy retTy
    pure (FuncTy okArgTys okRetTy StaticLifetimeTy)
xobjToTy (XObj (Lst [XObj (Sym (SymPath _ "Fn") _) _ _, XObj (Arr argTys) _ _, retTy, lifetime]) _ _) =
  do
    okArgTys <- mapM xobjToTy argTys
    okRetTy <- xobjToTy retTy
    _ <- xobjToTy lifetime
    pure (FuncTy okArgTys okRetTy StaticLifetimeTy)
xobjToTy (XObj (Lst []) _ _) = Just UnitTy
xobjToTy (XObj (Lst (x : xs)) _ _) =
  do
    okX <- xobjToTy x
    okXS <- mapM xobjToTy xs
    case okX of
      (StructTy n []) -> pure (StructTy n okXS)
      v@(VarTy _) -> pure (StructTy v okXS) -- Struct type with type variable as a name, i.e. "(a b)"
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
  where
    visit :: Ty -> Ty -> State VisitedTypes [String]
    visit sig actual =
      case (sig, actual) of
        (VarTy _, VarTy _) ->
          -- error $ "Unsolved variable in actual type: " ++ show sig ++ " => " ++ show actual ++
          --        " when calculating polymorphic suffix for " ++
          --        show signature ++ " => " ++ show actualType
          pure ["?"]
        (a@(VarTy _), b) -> do
          visitedTypeVariables <- get
          if a `elem` visitedTypeVariables
            then pure []
            else do
              put (a : visitedTypeVariables) -- now it's visited
              pure [tyToC b]
        (FuncTy argTysA retTyA _, FuncTy argTysB retTyB _) -> do
          visitedArgs <- fmap concat (zipWithM visit argTysA argTysB)
          visitedRets <- visit retTyA retTyB
          pure (visitedArgs ++ visitedRets)
        (StructTy _ a, StructTy _ b) -> fmap concat (zipWithM visit a b)
        (PointerTy a, PointerTy b) -> visit a b
        (RefTy a _, RefTy b _) -> visit a b
        (_, _) -> pure []

type VisitedTypes = [Ty]

-- | Templates are like macros, but defined inside the compiler and with access to the types they are instantiated with
data Template = Template
  { templateSignature :: Ty,
    templateDeclaration :: Ty -> [Token], -- Will this parameterization ever be useful?
    templateDefinition :: Ty -> [Token],
    templateDependencies :: Ty -> [XObj]
  }

instance Hashable Template where
  hashWithSalt s Template {..} = s `hashWithSalt` templateSignature

instance Show Template where
  show _ = "Template"

-- | Note: This is to make comparisons of Environments possible, otherwise
-- | they are always different when they contain Templates.
instance Eq Template where
  a == b = templateSignature a == templateSignature b

data TokTyMode = Normal | Raw deriving (Eq, Ord)

-- | Tokens are used for emitting C code from templates.
data Token
  = TokTy Ty TokTyMode -- Some kind of type, will be looked up if it's a type variable.
  | TokC String -- Plain C code.
  | TokDecl -- Will emit the declaration (i.e. "foo(int x)"), this is useful
  --   for avoiding repetition in the definition part of the template.
  | TokName -- Will emit the name of the instantiated function/variable.
  deriving (Eq, Ord)

instance Show Token where
  show (TokC s) = s
  show (TokTy t Normal) = tyToCLambdaFix t -- Any function type will be emitted as 'Lambda'
  show (TokTy t Raw) = tyToC t -- Function types will be emitted in typedef:able form
  show TokName = "<name>"
  show TokDecl = "<declaration>"

instantiateTemplate :: SymPath -> Ty -> Template -> (XObj, [XObj])
instantiateTemplate path actualType template =
  let defLst = [XObj (Instantiate template) Nothing Nothing, XObj (Sym path Symbol) Nothing Nothing]
      deps = templateDependencies template actualType
   in (XObj (Lst defLst) (Just (Info (-1) (-1) (show path ++ " template") Set.empty (-1))) (Just actualType), deps)

-- | Type aliases are used to create C-typedefs when those are needed.
defineTypeAlias :: String -> Ty -> XObj
defineTypeAlias name t =
  XObj
    ( Lst
        [ XObj (Defalias t) Nothing Nothing,
          XObj (Sym (SymPath [] name) Symbol) Nothing Nothing
        ]
    )
    (Just dummyInfo)
    (Just TypeTy)

defineFunctionTypeAlias :: Ty -> XObj
defineFunctionTypeAlias aliasTy = defineTypeAlias (tyToC aliasTy) aliasTy

defineArrayTypeAlias :: Ty -> XObj
defineArrayTypeAlias t = defineTypeAlias (tyToC t) (StructTy (ConcreteNameTy (SymPath [] "Array")) [])

defineStaticArrayTypeAlias :: Ty -> XObj
defineStaticArrayTypeAlias t = defineTypeAlias (tyToC t) (StructTy (ConcreteNameTy (SymPath [] "Array")) [])

-- |
defineInterface :: String -> Ty -> [SymPath] -> Maybe Info -> XObj
defineInterface name t paths info =
  XObj
    ( Lst
        [ XObj (Interface t paths) Nothing Nothing,
          XObj (Sym (SymPath [] name) Symbol) Nothing Nothing
        ]
    )
    info
    (Just InterfaceTy)

-- | Unsafe way of getting the type from an XObj
forceTy :: XObj -> Ty
forceTy xobj = fromMaybe (error ("No type in " ++ show xobj)) (xobjTy xobj)

-- | How should the compiler be run? Interactively or just build / build & run and then quit?
data ExecutionMode = Repl | Build | BuildAndRun | Install String | Check deriving (Show, Eq)

-- | Information needed by the REPL
data Context = Context
  { contextGlobalEnv :: Env,
    contextInternalEnv :: Maybe Env,
    contextTypeEnv :: TypeEnv,
    contextPath :: [String],
    contextProj :: Project,
    contextLastInput :: String,
    contextExecMode :: ExecutionMode,
    contextHistory :: ![XObj]
  }
  deriving (Show, Generic)

instance Hashable Context where
  hashWithSalt s Context {..} =
    s
      `hashWithSalt` contextGlobalEnv
      `hashWithSalt` contextInternalEnv
      `hashWithSalt` contextTypeEnv

popModulePath :: Context -> Context
popModulePath ctx = ctx {contextPath = init (contextPath ctx)}

pushFrame :: Context -> XObj -> Context
pushFrame ctx x = ctx {contextHistory = x : contextHistory ctx}

popFrame :: Context -> Context
popFrame ctx@Context {contextHistory = []} = ctx
popFrame ctx@Context {contextHistory = (_ : rest)} = ctx {contextHistory = rest}

-- | Unwrapping of XObj:s
-- | Unwrapping of XObj:s

-- | String
unwrapStringXObj :: XObj -> Either String String
unwrapStringXObj (XObj (Str s) _ _) = Right s
unwrapStringXObj x = Left ("The value '" ++ pretty x ++ "' at " ++ prettyInfoFromXObj x ++ " is not a String.")

-- | Bool
unwrapBoolXObj :: XObj -> Either String Bool
unwrapBoolXObj (XObj (Bol b) _ _) = Right b
unwrapBoolXObj x = Left ("The value '" ++ pretty x ++ "' at " ++ prettyInfoFromXObj x ++ " is not a Bool.")

-- | Symbol
unwrapSymPathXObj :: XObj -> Either String SymPath
unwrapSymPathXObj (XObj (Sym p _) _ _) = Right p
unwrapSymPathXObj x = Left ("The value '" ++ pretty x ++ "' at " ++ prettyInfoFromXObj x ++ " is not a Symbol.")

-- | Given a form, what definition mode will it generate?
definitionMode :: XObj -> DefinitionMode
definitionMode (XObj (Lst (XObj Def _ _ : _)) _ _) = AVariable
definitionMode _ = AFunction

isGlobalVariableLookup :: SymbolMode -> Bool
isGlobalVariableLookup (LookupGlobal _ AVariable) = True
isGlobalVariableLookup _ = False

anonMemberNames :: [String]
anonMemberNames = map (\i -> "member" ++ show i) ([0 ..] :: [Int])

anonMemberSymbols :: [XObj]
anonMemberSymbols = map (\n -> XObj (Sym (SymPath [] n) Symbol) Nothing Nothing) anonMemberNames

-- | Calculate the name of a Sumtype tag
tagName :: Ty -> String -> String
tagName sumTy caseName =
  tyToC sumTy ++ "_" ++ mangle caseName ++ "_tag"

wrapInParens :: XObj -> XObj
wrapInParens xobj@(XObj (Lst _) _ _) =
  xobj -- already in parens
wrapInParens xobj@(XObj _ i t) =
  XObj (Lst [xobj]) i t

-- | Is this symbol name appropriate for a normal variable (i.e. NOT a type name or sumtype tag)
isVarName :: String -> Bool
isVarName (firstLetter : _) =
  not (isUpper firstLetter) -- This allows names beginning with special chars etc. to be OK for vars
isVarName _ = False

-- construct an empty list xobj
emptyList :: XObj
emptyList = XObj (Lst []) Nothing Nothing

wrapInRefTyIfMatchRef :: MatchMode -> Ty -> Ty
wrapInRefTyIfMatchRef MatchRef t = RefTy t (VarTy "whatever") -- TODO: Better name for the lifetime variable.
wrapInRefTyIfMatchRef MatchValue t = t

-- | Check if the Obj is static and resolvable
isResolvableStaticObj :: Obj -> Bool
isResolvableStaticObj Def = True
isResolvableStaticObj (Defn _) = True
isResolvableStaticObj (External _) = True
isResolvableStaticObj (Deftemplate _) = True
isResolvableStaticObj (Instantiate _) = True
isResolvableStaticObj (Fn _ _) = True
isResolvableStaticObj (Interface _ _) = True
isResolvableStaticObj _ = False

-- | Left biased semigroup instance for Envs.
instance Semigroup Env where
  e <> e' =
    let bindings = envBindings e
        bindings' = envBindings e'
        joinedParents =
          (envParent e >>= \p -> (pure p <|> (envParent e' >>= \p' -> pure (p <> p'))))
            <|> envParent e'
        joinedUseModules = envUseModules e <> envUseModules e'
     in e
          { envBindings = Map.union bindings bindings',
            envParent = joinedParents,
            envUseModules = joinedUseModules
          }

-- | Semigroup instance for Contexts
--   - Left biased in internal env combination
--   - Right biased in global env combination
--   - Right biased in type env combination
--  The assumption here is that the context on the LHS is the *older* context
--  in the case of conflicts, we prefer the bindings on the RHS *except* for
--  the internal environment, since retaining some bindings from the internal
--  env is typically the reason you'd call this function.
instance Semigroup Context where
  c <> c' =
    let global = contextGlobalEnv c
        global' = contextGlobalEnv c'
        internal = contextInternalEnv c
        internal' = contextInternalEnv c'
        typeEnv = getTypeEnv (contextTypeEnv c)
        typeEnv' = getTypeEnv (contextTypeEnv c')
     in c
          { contextGlobalEnv = global' <> global,
            contextInternalEnv = internal <> internal',
            contextTypeEnv = TypeEnv (typeEnv' <> typeEnv)
          }

toLocalDef :: String -> XObj -> XObj
toLocalDef var value =
  (XObj (Lst [XObj LocalDef Nothing Nothing, XObj (Sym (SymPath [] var) Symbol) Nothing Nothing, value]) (xobjInfo value) (xobjTy value))

-- | Create a fresh binder for an XObj (a binder with empty Metadata).
toBinder :: XObj -> Binder
toBinder xobj = Binder emptyMeta xobj

-- | Dynamic 'true'.
trueXObj :: XObj
trueXObj = XObj (Bol True) Nothing Nothing

-- | Dynamic 'false'.
falseXObj :: XObj
falseXObj = XObj (Bol False) Nothing Nothing

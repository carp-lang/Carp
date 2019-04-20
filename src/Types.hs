module Types ( TypeMappings
             , Ty(..)
             , showMaybeTy
             , tyToC
             , tyToCLambdaFix
             , tyToCRawFunctionPtrFix
             , isTypeGeneric
             , SymPath(..)
             , unifySignatures
             , replaceTyVars
             , mangle
             , pathToC
             , areUnifiable
             , typesDeleterFunctionType
             , typesCopyFunctionType
             , isFullyGenericType
             , consPath
             , doesTypeContainTyVarWithName
             , lambdaEnvTy
             ) where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Util
--import Debug.Trace

-- | Carp types.
data Ty = IntTy
        | LongTy
        | BoolTy
        | FloatTy
        | DoubleTy
        | StringTy
        | PatternTy
        | CharTy
        | FuncTy [Ty] Ty
        | VarTy String
        | UnitTy
        | ModuleTy
        | PointerTy Ty
        | RefTy Ty
        | StructTy String [Ty] -- the name of the struct, and it's type parameters
        | TypeTy -- the type of types
        | MacroTy
        | DynamicTy -- the type of dynamic functions (used in REPL and macros)
        | InterfaceTy
        deriving (Eq, Ord)

data SumTyCase = SumTyCase { caseName :: String
                           , caseMembers :: [(String, Ty)]
                           } deriving (Show, Ord, Eq)

fnOrLambda =
  case platform of
    Windows -> "Fn"
    _ -> "λ"

instance Show Ty where
  show IntTy                 = "Int"
  show FloatTy               = "Float"
  show DoubleTy              = "Double"
  show LongTy                = "Long"
  show BoolTy                = "Bool"
  show StringTy              = "String"
  show PatternTy             = "Pattern"
  show CharTy                = "Char"
  show (FuncTy argTys retTy) = "(" ++ fnOrLambda ++ " [" ++ joinWithComma (map show argTys) ++ "] " ++ show retTy ++ ")"
  show (VarTy t)             = t
  show UnitTy                = "()"
  show ModuleTy              = "Module"
  show TypeTy                = "Type"
  show InterfaceTy           = "Interface"
  show (StructTy s [])       = s
  show (StructTy s typeArgs) = "(" ++ s ++ " " ++ joinWithSpace (map show typeArgs) ++ ")"
  show (PointerTy p)         = "(Ptr " ++ show p ++ ")"
  show (RefTy r)             =
    case r of
      PointerTy _ -> listView
      StructTy _ _ -> listView
      FuncTy _ _ -> listView
      _ -> "&" ++ show r
    where listView = "(Ref " ++ show r ++ ")"
  show MacroTy               = "Macro"
  show DynamicTy             = "Dynamic"

showMaybeTy :: Maybe Ty -> String
showMaybeTy (Just t) = show t
showMaybeTy Nothing  = "(missing-type)"

tyToC :: Ty -> String
tyToC = tyToCManglePtr False

tyToCLambdaFix :: Ty -> String
tyToCLambdaFix t@(FuncTy _ _) = "Lambda"
tyToCLambdaFix (RefTy (FuncTy _ _)) = "Lambda*"
tyToCLambdaFix (RefTy (RefTy (FuncTy _ _))) = "Lambda**"
tyToCLambdaFix (RefTy (RefTy (RefTy (FuncTy _ _)))) = "Lambda***" -- | TODO: More cases needed?! What's a better way to do it..?
tyToCLambdaFix t = tyToCManglePtr False t

tyToCRawFunctionPtrFix :: Ty -> String
tyToCRawFunctionPtrFix t@(FuncTy _ _) = "void*"
tyToCRawFunctionPtrFix t = tyToCManglePtr False t

tyToCManglePtr :: Bool -> Ty -> String
tyToCManglePtr _ IntTy                   = "int"
tyToCManglePtr _ BoolTy                  = "bool"
tyToCManglePtr _ FloatTy                 = "float"
tyToCManglePtr _ DoubleTy                = "double"
tyToCManglePtr _ LongTy                  = "long"
tyToCManglePtr _ StringTy                = "String"
tyToCManglePtr _ PatternTy               = "Pattern"
tyToCManglePtr _ CharTy                  = "char"
tyToCManglePtr _ UnitTy                  = "void"
tyToCManglePtr _ (VarTy x)               = x
tyToCManglePtr _ (FuncTy argTys retTy)   = "Fn__" ++ joinWithUnderscore (map (tyToCManglePtr True) argTys) ++ "_" ++ tyToCManglePtr True retTy
tyToCManglePtr _ ModuleTy                = error "Can't emit module type."
tyToCManglePtr b (PointerTy p)           = tyToCManglePtr b p ++ (if b then mangle "*" else "*")
tyToCManglePtr b (RefTy r)               = tyToCManglePtr b r ++ (if b then mangle "*" else "*")
tyToCManglePtr _ (StructTy s [])         = mangle s
tyToCManglePtr _ (StructTy s typeArgs)   = mangle s ++ "__" ++ joinWithUnderscore (map (tyToCManglePtr True) typeArgs)
tyToCManglePtr _ TypeTy                  = error "Can't emit the type of types."
tyToCManglePtr _ MacroTy                 = error "Can't emit the type of macros."
tyToCManglePtr _ DynamicTy               = error "Can't emit the type of dynamic functions."

isTypeGeneric :: Ty -> Bool
isTypeGeneric (VarTy _) = True
isTypeGeneric (FuncTy argTys retTy) = any isTypeGeneric argTys || isTypeGeneric retTy
isTypeGeneric (StructTy _ tyArgs) = any isTypeGeneric tyArgs
isTypeGeneric (PointerTy p) = isTypeGeneric p
isTypeGeneric (RefTy r) = isTypeGeneric r
isTypeGeneric _ = False

doesTypeContainTyVarWithName :: String -> Ty -> Bool
doesTypeContainTyVarWithName name (VarTy n) = name == n
doesTypeContainTyVarWithName name (FuncTy argTys retTy) = any (doesTypeContainTyVarWithName name) argTys || (doesTypeContainTyVarWithName name) retTy
doesTypeContainTyVarWithName name (StructTy _ tyArgs) = any (doesTypeContainTyVarWithName name) tyArgs
doesTypeContainTyVarWithName name (PointerTy p) = doesTypeContainTyVarWithName name p
doesTypeContainTyVarWithName name (RefTy r) = doesTypeContainTyVarWithName name r
doesTypeContainTyVarWithName _ _ = False

-- | Map type variable names to actual types, eg. t0 => Int, t1 => Float
type TypeMappings = Map.Map String Ty

-- | The path to a binding
data SymPath = SymPath [String] String deriving (Ord, Eq)

instance Show SymPath where
  show (SymPath modulePath symName) =
    if null modulePath
    then symName
    else joinWithPeriod modulePath ++ "." ++ symName

pathToC :: SymPath -> String
pathToC (SymPath modulePath name) =
  concatMap ((++ "_") . mangle) modulePath ++ mangle name

-- | Add qualifying strings to beginning of a path.
consPath :: [String] -> SymPath -> SymPath
consPath qualifyers (SymPath stringPaths name) =
  SymPath (qualifyers ++ stringPaths) name

-- | Replaces symbols not allowed in C-identifiers.
mangle :: String -> String
mangle = sreplace . creplace
  where creplace = replaceChars (Map.fromList [('+', "_PLUS_")
                                               ,('-', "_MINUS_")
                                               ,('*', "_MUL_")
                                               ,('/', "_DIV_")
                                               ,('<', "_LT_")
                                               ,('>', "_GT_")
                                               ,('?', "_QMARK_")
                                               ,('!', "_BANG_")
                                               ,('=', "_EQ_")])
        sreplace = replaceStrings (Map.fromList [("auto", "_AUTO_")
                                                 ,("break", "_BREAK_")
                                                 ,("case", "_CASE_")
                                                 ,("const", "_CONST_")
                                                 ,("char", "_CHAR_")
                                                 ,("continue", "_CONTINUE_")
                                                 ,("default", "_DEFAULT_")
                                                 ,("do", "_DO_")
                                                 ,("double", "_DOUBLE_")
                                                 ,("else", "_ELSE_")
                                                 ,("enum", "_ENUM_")
                                                 ,("extern", "_EXTERN")
                                                 ,("float", "_FLOAT_")
                                                 ,("for", "_FOR")
                                                 ,("goto", "_GOTO_")
                                                 ,("if", "_IF_")
                                                 ,("int", "_INT_")
                                                 ,("long", "_LONG_")
                                                 ,("register", "_REGISTER_")
                                                 ,("return", "_RETURN_")
                                                 ,("short", "_SHORT_")
                                                 ,("signed", "_SIGNED_")
                                                 ,("sizeof", "_SIZEOF_")
                                                 ,("static", "_STATIC_")
                                                 ,("struct", "_STRUCT_")
                                                 ,("switch", "_SWITCH_")
                                                 ,("typedef", "_TYPEDEF_")
                                                 ,("union", "_UNION_")
                                                 ,("unsigned", "_UNSIGNED_")
                                                 ,("volatile", "_VOLATILE_")
                                                 ,("void", "_VOID_")
                                                 ,("while", "_WHILE_")])

-- | From two types, one with type variables and one without (e.g. (Fn ["t0"] "t1") and (Fn [Int] Bool))
--   create mappings that translate from the type variables to concrete types, e.g. "t0" => Int, "t1" => Bool
unifySignatures :: Ty -> Ty -> TypeMappings
unifySignatures v t = Map.fromList (unify v t)
  where unify :: Ty -> Ty -> [(String, Ty)]
        unify a@(VarTy _) b@(VarTy _) = [] -- if a == b then [] else error ("Can't unify " ++ show a ++ " with " ++ show b)

        unify (VarTy a) value = [(a, value)]

        unify (StructTy a aArgs) (StructTy b bArgs) | a == b    = concat (zipWith unify aArgs bArgs)
                                                    | otherwise = [] -- error ("Can't unify " ++ a ++ " with " ++ b)
        unify a@(StructTy _ _) b = [] -- error ("Can't unify " ++ show a ++ " with " ++ show b)

        unify (PointerTy a) (PointerTy b) = unify a b
        unify a@(PointerTy _) b = [] -- error ("Can't unify " ++ show a ++ " with " ++ show b)

        unify (RefTy a) (RefTy b) = unify a b
        unify a@(RefTy _) b = [] -- error ("Can't unify " ++ show a ++ " with " ++ show b)

        unify (FuncTy argTysA retTyA) (FuncTy argTysB retTyB) = let argToks = concat (zipWith unify argTysA argTysB)
                                                                    retToks = unify retTyA retTyB
                                                                in  argToks ++ retToks
        unify a@(FuncTy _ _) b = [] -- error ("Can't unify " ++ show a ++ " with " ++ show b)
        unify a b | a == b    = []
                  | otherwise = [] -- error ("Can't unify " ++ show a ++ " with " ++ show b)

-- | Checks if two types will unify
areUnifiable :: Ty -> Ty -> Bool
areUnifiable (VarTy _) (VarTy _) = True
areUnifiable (VarTy _) _ = True
areUnifiable _ (VarTy _) = True
areUnifiable (StructTy a aArgs) (StructTy b bArgs)
  | length aArgs /= length bArgs = False
  | a == b = let argBools = zipWith areUnifiable aArgs bArgs
             in  all (== True) argBools
  | otherwise = False
areUnifiable (StructTy _ _) _ = False
areUnifiable (PointerTy a) (PointerTy b) = areUnifiable a b
areUnifiable (PointerTy _) _ = False
areUnifiable (RefTy a) (RefTy b) = areUnifiable a b
areUnifiable (RefTy _) _ = False
areUnifiable (FuncTy argTysA retTyA) (FuncTy argTysB retTyB)
  | length argTysA /= length argTysB = False
  | otherwise = let argBools = zipWith areUnifiable argTysA argTysB
                    retBool = areUnifiable retTyA retTyB
                in  all (== True) (retBool : argBools)
areUnifiable (FuncTy _ _) _ = False
areUnifiable a b | a == b    = True
          | otherwise = False

-- | Put concrete types into the places where there are type variables.
--   For example (Fn [a] b) => (Fn [Int] Bool)
--   NOTE: If a concrete type can't be found, the type variable will stay the same.
replaceTyVars :: TypeMappings -> Ty -> Ty
replaceTyVars mappings t =
  case t of
    (VarTy key) -> fromMaybe t (Map.lookup key mappings)
    (FuncTy argTys retTy) -> FuncTy (map (replaceTyVars mappings) argTys) (replaceTyVars mappings retTy)
    (StructTy name tyArgs) -> StructTy name (fmap (replaceTyVars mappings) tyArgs)
    (PointerTy x) -> PointerTy (replaceTyVars mappings x)
    (RefTy x) -> RefTy (replaceTyVars mappings x)
    _ -> t

-- | The type of a type's copying function.
typesCopyFunctionType :: Ty -> Ty
typesCopyFunctionType memberType = FuncTy [RefTy memberType] memberType

-- | The type of a type's deleter function.
typesDeleterFunctionType :: Ty -> Ty
typesDeleterFunctionType memberType = FuncTy [memberType] UnitTy

isFullyGenericType (VarTy _) = True
isFullyGenericType _ = False

-- | The type of environments sent to Lambdas (used in emitted C code)
lambdaEnvTy :: Ty
lambdaEnvTy = StructTy "LambdaEnv" []

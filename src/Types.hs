module Types
  ( TypeMappings,
    Ty (..),
    showMaybeTy,
    unifySignatures,
    replaceTyVars,
    areUnifiable,
    typesDeleterFunctionType,
    typesCopyFunctionType,
    doesTypeContainTyVarWithName,
    replaceConflicted,
    lambdaEnvTy,
    typeEqIgnoreLifetimes,
    checkKinds,
    -- SymPath imports
    SymPath (..),
    mangle,
    pathToC,
    consPath,
    Kind,
    tyToKind,
  )
where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import SymPath
import Util

--import Debug.Trace

-- | Carp types.
data Ty
  = IntTy
  | LongTy
  | ByteTy
  | BoolTy
  | FloatTy
  | DoubleTy
  | StringTy
  | PatternTy
  | CharTy
  | FuncTy [Ty] Ty Ty -- In order of appearance: (1) Argument types, (2) Return type, (3) Lifetime
  | VarTy String
  | UnitTy
  | ModuleTy
  | PointerTy Ty
  | RefTy Ty Ty -- second Ty is the lifetime
  | StaticLifetimeTy
  | StructTy Ty [Ty] -- the name (possibly a var) of the struct, and it's type parameters
  | ConcreteNameTy String -- the name of a struct
  | TypeTy -- the type of types
  | MacroTy
  | DynamicTy -- the type of dynamic functions (used in REPL and macros)
  | InterfaceTy
  | Universe -- the type of types of types (the type of TypeTy)
  deriving (Eq, Ord)

-- | Kinds checking
-- Carp's system is simple enough that we do not need to describe kinds by their airty.
-- After confirming two tys have either base or higher kind
-- unification checks are sufficient to determine whether their arities are compatible.
data Kind
  = Base
  | Higher
  deriving (Eq, Ord, Show)

tyToKind :: Ty -> Kind
tyToKind (StructTy _ _) = Higher
tyToKind (FuncTy _ _ _) = Higher -- the type of functions, consider the (->) constructor in Haskell
tyToKind (PointerTy _) = Higher
tyToKind (RefTy _ _) = Higher -- Refs may also be treated as a data constructor
tyToKind _ = Base

-- Exactly like '==' for Ty, but ignore lifetime parameter
typeEqIgnoreLifetimes :: Ty -> Ty -> Bool
typeEqIgnoreLifetimes (RefTy a _) (RefTy b _) = a == b
typeEqIgnoreLifetimes (FuncTy argsA retA _) (FuncTy argsB retB _) =
  all (== True) (zipWith typeEqIgnoreLifetimes argsA argsB)
    && typeEqIgnoreLifetimes retA retB
typeEqIgnoreLifetimes (StructTy a tyVarsA) (StructTy b tyVarsB) =
  a == b
    && all (== True) (zipWith typeEqIgnoreLifetimes tyVarsA tyVarsB)
typeEqIgnoreLifetimes a b = a == b

data SumTyCase = SumTyCase
  { caseName :: String,
    caseMembers :: [(String, Ty)]
  }
  deriving (Show, Ord, Eq)

fnOrLambda :: String
fnOrLambda =
  case platform of
    Windows -> "Fn"
    _ -> "Fn" -- "λ"

instance Show Ty where
  show IntTy = "Int"
  show FloatTy = "Float"
  show DoubleTy = "Double"
  show LongTy = "Long"
  show ByteTy = "Byte"
  show BoolTy = "Bool"
  show StringTy = "String"
  show PatternTy = "Pattern"
  show CharTy = "Char"
  show (FuncTy argTys retTy StaticLifetimeTy) = "(" ++ fnOrLambda ++ " [" ++ joinWithComma (map show argTys) ++ "] " ++ show retTy ++ ")"
  show (FuncTy argTys retTy lt) = "(" ++ fnOrLambda ++ " [" ++ joinWithComma (map show argTys) ++ "] " ++ show retTy ++ " " ++ show lt ++ ")"
  show (VarTy t) = t
  show UnitTy = "()"
  show ModuleTy = "Module"
  show TypeTy = "Type"
  show InterfaceTy = "Interface"
  show (StructTy s []) = (show s)
  show (StructTy s typeArgs) = "(" ++ (show s) ++ " " ++ joinWithSpace (map show typeArgs) ++ ")"
  show (ConcreteNameTy name) = name
  show (PointerTy p) = "(Ptr " ++ show p ++ ")"
  show (RefTy r lt) =
    -- case r of
    --   PointerTy _ -> listView
    --   StructTy _ _ -> listView
    --   FuncTy _ _ -> listView
    --   _ -> "&" ++ show r
    -- where listView = "(Ref " ++ show r ++ ")"
    "(Ref " ++ show r ++ " " ++ show lt ++ ")"
  show StaticLifetimeTy = "StaticLifetime"
  show MacroTy = "Macro"
  show DynamicTy = "Dynamic"

showMaybeTy :: Maybe Ty -> String
showMaybeTy (Just t) = show t
showMaybeTy Nothing = "(missing-type)"

doesTypeContainTyVarWithName :: String -> Ty -> Bool
doesTypeContainTyVarWithName name (VarTy n) = name == n
doesTypeContainTyVarWithName name (FuncTy argTys retTy lt) =
  doesTypeContainTyVarWithName name lt
    || any (doesTypeContainTyVarWithName name) argTys
    || doesTypeContainTyVarWithName name retTy
doesTypeContainTyVarWithName name (StructTy n tyArgs) = doesTypeContainTyVarWithName name n || any (doesTypeContainTyVarWithName name) tyArgs
doesTypeContainTyVarWithName name (PointerTy p) = doesTypeContainTyVarWithName name p
doesTypeContainTyVarWithName name (RefTy r lt) =
  doesTypeContainTyVarWithName name r
    || doesTypeContainTyVarWithName name lt
doesTypeContainTyVarWithName _ _ = False

replaceConflicted :: String -> Ty -> Ty
replaceConflicted name (VarTy n) =
  if n == name
    then (VarTy (n ++ "conflicted"))
    else (VarTy n)
replaceConflicted name (FuncTy argTys retTy lt) =
  FuncTy
    (map (replaceConflicted name) argTys)
    (replaceConflicted name retTy)
    (replaceConflicted name lt)
replaceConflicted name (StructTy n tyArgs) = StructTy (replaceConflicted name n) (map (replaceConflicted name) tyArgs)
replaceConflicted name (PointerTy p) = PointerTy (replaceConflicted name p)
replaceConflicted name (RefTy r lt) =
  RefTy
    (replaceConflicted name r)
    (replaceConflicted name lt)
replaceConflicted _ t = t

-- | Map type variable names to actual types, eg. t0 => Int, t1 => Float
type TypeMappings = Map.Map String Ty

-- | From two types, one with type variables and one without (e.g. (Fn ["t0"] "t1") and (Fn [Int] Bool))
--   create mappings that translate from the type variables to concrete types, e.g. "t0" => Int, "t1" => Bool
unifySignatures :: Ty -> Ty -> TypeMappings
unifySignatures at ct = Map.fromList (unify at ct)
  where
    unify :: Ty -> Ty -> [(String, Ty)]
    unify (VarTy _) (VarTy _) = [] -- if a == b then [] else error ("Can't unify " ++ show a ++ " with " ++ show b)
    unify (VarTy a) value = [(a, value)]
    unify (StructTy v'@(VarTy _) aArgs) (StructTy n bArgs) = unify v' n ++ concat (zipWith unify aArgs bArgs)
    unify (StructTy a@(ConcreteNameTy _) aArgs) (StructTy b bArgs)
      | a == b = concat (zipWith unify aArgs bArgs)
      | otherwise = [] -- error ("Can't unify " ++ a ++ " with " ++ b)
    unify (StructTy _ _) _ = [] -- error ("Can't unify " ++ show a ++ " with " ++ show b)
    unify (PointerTy a) (PointerTy b) = unify a b
    unify (PointerTy _) _ = [] -- error ("Can't unify " ++ show a ++ " with " ++ show b)
    unify (RefTy a ltA) (RefTy b ltB) = unify a b ++ unify ltA ltB
    unify (RefTy _ _) _ = [] -- error ("Can't unify " ++ show a ++ " with " ++ show b)
    unify (FuncTy argTysA retTyA ltA) (FuncTy argTysB retTyB ltB) =
      let argToks = concat (zipWith unify argTysA argTysB)
          retToks = unify retTyA retTyB
          ltToks = unify ltA ltB
       in ltToks ++ argToks ++ retToks
    unify FuncTy {} _ = [] -- error ("Can't unify " ++ show a ++ " with " ++ show b)
    unify a b
      | a == b = []
      | otherwise = [] -- error ("Can't unify " ++ show a ++ " with " ++ show b)

-- | Checks if two types will unify
areUnifiable :: Ty -> Ty -> Bool
areUnifiable (VarTy _) (VarTy _) = True
areUnifiable (VarTy _) _ = True
areUnifiable _ (VarTy _) = True
areUnifiable (StructTy a aArgs) (StructTy b bArgs)
  | length aArgs /= length bArgs = False
  | areUnifiable a b =
    let argBools = zipWith areUnifiable aArgs bArgs
     in all (== True) argBools
  | otherwise = False
areUnifiable (StructTy (VarTy _) aArgs) (FuncTy bArgs _ _)
  | length aArgs /= length bArgs = False
  | otherwise = all (== True) (zipWith areUnifiable aArgs bArgs)
areUnifiable (StructTy (VarTy _) args) (RefTy _ _)
  | length args == 2 = True
  | otherwise = False
areUnifiable (StructTy _ _) _ = False
areUnifiable (PointerTy a) (PointerTy b) = areUnifiable a b
areUnifiable (PointerTy _) _ = False
areUnifiable (RefTy a ltA) (RefTy b ltB) = areUnifiable a b && areUnifiable ltA ltB
areUnifiable RefTy {} _ = False
areUnifiable (FuncTy argTysA retTyA ltA) (FuncTy argTysB retTyB ltB)
  | length argTysA /= length argTysB = False
  | otherwise =
    let argBools = zipWith areUnifiable argTysA argTysB
        retBool = areUnifiable retTyA retTyB
        ltBool = areUnifiable ltA ltB
     in all (== True) (ltBool : retBool : argBools)
areUnifiable FuncTy {} _ = False
areUnifiable a b
  | a == b = True
  | otherwise = False

-- Checks whether or not the kindedness of types match
-- Kinds are polymorphic constructors such as (f a)
-- Note that this disagrees with the notion of unifiablitity in areUnifiable
checkKinds :: Ty -> Ty -> Bool
-- Base < Higher
checkKinds (FuncTy argTysA retTyA _) (FuncTy argTysB retTyB _) =
  let argKinds = zipWith checkKinds argTysA argTysB
      retKinds = tyToKind retTyA <= tyToKind retTyB
   in all (== True) (retKinds : argKinds)
checkKinds t t' = tyToKind t <= tyToKind t'

-- | Put concrete types into the places where there are type variables.
--   For example (Fn [a] b) => (Fn [Int] Bool)
--   NOTE: If a concrete type can't be found, the type variable will stay the same.
replaceTyVars :: TypeMappings -> Ty -> Ty
replaceTyVars mappings t =
  case t of
    (VarTy key) -> fromMaybe t (Map.lookup key mappings)
    (FuncTy argTys retTy lt) -> FuncTy (map (replaceTyVars mappings) argTys) (replaceTyVars mappings retTy) (replaceTyVars mappings lt)
    (StructTy name tyArgs) ->
      case (replaceTyVars mappings name) of
        -- special case, struct (f a b) mapped to (RefTy a lt)
        -- We f in such a case to the full (Ref a lt) in constraints; we also still map
        -- individual members a and b, as these need mappings since they may be
        -- referred to in other places (e.g. (Fn [(f a b)] a)--without a mapping,
        -- a would remain generic here.
        (RefTy a lt) -> (replaceTyVars mappings (RefTy a lt))
        _ -> StructTy (replaceTyVars mappings name) (fmap (replaceTyVars mappings) tyArgs)
    (PointerTy x) -> PointerTy (replaceTyVars mappings x)
    (RefTy x lt) -> RefTy (replaceTyVars mappings x) (replaceTyVars mappings lt)
    _ -> t

-- | The type of a type's copying function.
typesCopyFunctionType :: Ty -> Ty
typesCopyFunctionType memberType = FuncTy [RefTy memberType (VarTy "q")] memberType StaticLifetimeTy

-- | The type of a type's deleter function.
typesDeleterFunctionType :: Ty -> Ty
typesDeleterFunctionType memberType = FuncTy [memberType] UnitTy StaticLifetimeTy

-- | The type of environments sent to Lambdas (used in emitted C code)
lambdaEnvTy :: Ty
lambdaEnvTy = StructTy (ConcreteNameTy "LambdaEnv") []

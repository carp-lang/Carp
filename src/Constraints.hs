module Constraints (solve,
                    Constraint(..),
                    ConstraintOrder(..),
                    UnificationFailure(..),
                    recursiveLookup,
                    debugSolveOne,    -- exported to avoid warning about unused function (should be another way...)
                    debugResolveFully -- exported to avoid warning about unused function
                   ) where

import qualified Data.Map as Map
import Control.Monad
import Debug.Trace

import Obj
import Types

data ConstraintOrder = OrdNo
                     | OrdArrHead
                     | OrdArg
                     | OrdDefnBody
                     | OrdDefExpr
                     | OrdLetBind
                     | OrdLetBody
                     | OrdIfCondition
                     | OrdIfReturn
                     | OrdIfWhole
                     | OrdWhileBody
                     | OrdWhileCondition
                     | OrdDoReturn
                     | OrdDoStatement
                     | OrdSetBang
                     | OrdThe
                     | OrdFuncAppVarTy
                     | OrdArrBetween
                     | OrdMultiSym
                     | OrdFuncAppRet
                     | OrdFuncAppArg
                     deriving (Show, Ord, Eq)

data Constraint = Constraint Ty Ty XObj XObj ConstraintOrder deriving Eq

instance Ord Constraint where
  compare (Constraint _ _ _ _ a) (Constraint _ _ _ _ b) = compare a b

data UnificationFailure = UnificationFailure { unificationFailure ::Constraint
                                             , unificationMappings :: TypeMappings
                                             }
                        | Holes [(String, Ty)]
                        deriving (Eq, Show)

instance Show Constraint where
  show (Constraint a b _ _ ord) = "{" ++ show a ++ " == " ++ show b ++ " (ord " ++ show ord ++ ")}"

-- Finds the symbol with the "lowest name" (first in alphabetical order)
recursiveLookup :: TypeMappings -> String -> Maybe Ty
recursiveLookup mappings name = innerLookup name []
  where innerLookup :: String -> [Ty] -> Maybe Ty
        innerLookup k visited =
          case Map.lookup k mappings of
            Just exists -> case exists of
                             VarTy v -> if exists `elem` visited
                                        then stop
                                        else innerLookup v (exists : visited)
                             actualType -> Just actualType
                           where
                             stop = Just (minimum (exists : visited))
            Nothing -> Nothing

-- | This is the entry-point function that takes a list of constraints
--   (for example [t0 == Int, t1 == t0, t1 == t2])
--   and creates a dictionary of mappings for the type variables
--   (for example t0 => Int, t1 => Int, t2 => Int).
solve :: [Constraint] -> Either UnificationFailure TypeMappings
solve constraints = do naiveMappings <- foldM solveOne Map.empty constraints
                       fullyResolved <- foldM resolveFully naiveMappings (map fst (Map.toList naiveMappings))
                       checkForHoles fullyResolved

checkForHoles :: TypeMappings -> Either UnificationFailure TypeMappings
checkForHoles mappings = case filter isTypeHole (Map.toList mappings) of
  [] -> Right mappings
  holes -> Left (Holes holes)

isTypeHole :: (String, Ty) -> Bool
isTypeHole ('?' : _, _) = True
isTypeHole _ = False

solveOne :: TypeMappings -> Constraint -> Either UnificationFailure TypeMappings
solveOne = solveOneInternal

debugSolveOne :: TypeMappings -> Constraint -> Either UnificationFailure TypeMappings
debugSolveOne mappings constraint = let m' = solveOneInternal mappings constraint
                                    in trace ("" ++ show constraint ++ ", MAPPINGS: " ++ show m')
                                       m'

solveOneInternal :: TypeMappings -> Constraint -> Either UnificationFailure TypeMappings
solveOneInternal mappings constraint =
  case constraint of
    -- Two type variables
    Constraint aTy@(VarTy aName) bTy@(VarTy bName) _ _ _ ->
      if aTy == bTy
      then Right mappings
      else do m' <- checkForConflict mappings constraint aName bTy
              checkForConflict m' constraint bName aTy

    -- One type variable
    Constraint (VarTy aName) bTy           _ _ _ -> checkForConflict mappings constraint aName bTy
    Constraint aTy           (VarTy bName) _ _ _ -> checkForConflict mappings constraint bName aTy

    -- Struct types
    Constraint (StructTy nameA varsA) (StructTy nameB varsB) _ _ _ ->
      if nameA == nameB
      then let (Constraint _ _ i1 i2 ord) = constraint
           in  foldM (\m (aa, bb) -> solveOneInternal m (Constraint aa bb i1 i2 ord)) mappings (zip varsA varsB)
      else Left (UnificationFailure constraint mappings)

    -- Func types
    Constraint (FuncTy argsA retA) (FuncTy argsB retB) _ _ _ ->
      if length argsA == length argsB
      then let (Constraint _ _ i1 i2 ord) = constraint
           in  foldM (\m (aa, bb) -> solveOneInternal m (Constraint aa bb i1 i2 ord )) mappings (zip (retA : argsA)
                                                                                                     (retB : argsB))
      else Left (UnificationFailure constraint mappings)

    -- Pointer types
    Constraint (PointerTy a) (PointerTy b) _ _ _ ->
      let (Constraint _ _ i1 i2 ord) = constraint
      in  solveOneInternal mappings (Constraint a b i1 i2 ord)

    -- Ref types
    Constraint (RefTy a) (RefTy b) _ _ _ ->
      let (Constraint _ _ i1 i2 ord) = constraint
      in  solveOneInternal mappings (Constraint a b i1 i2 ord)

    -- Else
    Constraint aTy bTy _ _ _ ->
      if aTy == bTy
      then Right mappings
      else Left (UnificationFailure constraint mappings)

mkConstraint :: XObj -> XObj -> Ty -> Ty -> Constraint
mkConstraint xobj1 xobj2 t1 t2 = Constraint t1 t2 xobj1 xobj2 OrdNo

checkForConflict :: TypeMappings -> Constraint -> String -> Ty -> Either UnificationFailure TypeMappings
checkForConflict mappings constraint name otherTy =
  let (Constraint _ _ xobj1 xobj2 _) = constraint
  in
  case recursiveLookup mappings name of
    Just (VarTy _) -> ok
    Just (StructTy structName structTyVars) ->
      case otherTy of
        StructTy otherStructName otherTyVars | structName == otherStructName ->
                                               foldM solveOneInternal mappings (zipWith (mkConstraint xobj1 xobj2) structTyVars otherTyVars)
        VarTy _ -> Right mappings
        _ -> Left (UnificationFailure constraint mappings)
    Just (FuncTy argTys retTy) ->
      case otherTy of
        FuncTy otherArgTys otherRetTy -> do m <- foldM solveOneInternal mappings (zipWith (mkConstraint xobj1 xobj2) argTys otherArgTys)
                                            solveOneInternal m (mkConstraint xobj1 xobj2 retTy otherRetTy)
        VarTy _ -> Right mappings
        _ -> Left (UnificationFailure constraint mappings)
    Just (PointerTy innerTy) ->
      case otherTy of
        PointerTy otherInnerTy -> solveOneInternal mappings (mkConstraint xobj1 xobj2 innerTy otherInnerTy)
        VarTy _ -> Right mappings
        _ -> Left (UnificationFailure constraint mappings)
    Just (RefTy innerTy) ->
      case otherTy of
        RefTy otherInnerTy -> solveOneInternal mappings (mkConstraint xobj1 xobj2 innerTy otherInnerTy)
        VarTy _ -> Right mappings
        _ -> Left (UnificationFailure constraint mappings)
    Just foundNonVar -> case otherTy of
                          (VarTy v) -> case recursiveLookup mappings v of
                                         Just (VarTy _) -> Right mappings
                                         Just otherNonVar -> if foundNonVar == otherNonVar
                                                             then Right mappings
                                                             else Left (UnificationFailure constraint mappings)
                                         Nothing -> Right mappings
                          _ -> if otherTy == foundNonVar
                               then ok
                               else Left (UnificationFailure constraint mappings)
    -- Not found, no risk for conflict:
    Nothing -> ok
  where
    ok = Right (Map.insert name otherTy mappings)

debugResolveFully :: TypeMappings -> String -> Either UnificationFailure TypeMappings
debugResolveFully mappings var = trace ("Mappings: " ++ show mappings ++ ", will resolve " ++ show var) (resolveFully mappings var)

resolveFully :: TypeMappings -> String -> Either UnificationFailure TypeMappings
resolveFully mappings varName = Right (Map.insert varName (fullResolve (VarTy varName)) mappings)

  where fullResolve :: Ty -> Ty
        fullResolve x@(VarTy var) =
          case recursiveLookup mappings var of
            Just (StructTy name varTys) -> StructTy name (map fullLookup varTys)
            Just (FuncTy argTys retTy) -> FuncTy (map fullLookup argTys) (fullLookup retTy)
            Just found -> found
            Nothing -> x -- still not found, must be a generic variable
        fullResolve x = x

        fullLookup :: Ty -> Ty
        fullLookup vv@(VarTy v) = case recursiveLookup mappings v of
                                    --Just found -> fullLookup found
                                    Just found -> if found == vv
                                                  then found
                                                  else fullLookup found
                                    Nothing -> vv-- compilerError ("In full lookup: Can't find " ++ v ++ " in mappings: " ++ show mappings)
        fullLookup (StructTy name vs) = StructTy name (map fullLookup vs)
        fullLookup (FuncTy argTys retTy) = FuncTy (map fullLookup argTys) (fullLookup retTy)
        fullLookup x = x

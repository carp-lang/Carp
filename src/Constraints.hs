module Constraints
  ( solve,
    Constraint (..),
    ConstraintOrder (..),
    UnificationFailure (..),
    recursiveNameLookup,
    debugSolveOne, -- exported to avoid warning about unused function (should be another way...)
    debugResolveFully, -- exported to avoid warning about unused function
  )
where

import Control.Monad
import Debug.Trace
import qualified Map
import Obj
import qualified Set
import Types

data ConstraintOrder
  = OrdNo
  | OrdFunc
  | OrdStruct
  | OrdPtr
  | OrdRef
  | OrdDeref
  | OrdFuncAppRet
  | OrdArrHead
  | OrdArg
  | OrdCapture
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
  | OrdAnd
  | OrdOr
  | OrdFuncAppVarTy
  | OrdFuncAppArg
  | OrdArrBetween
  | OrdMultiSym
  | OrdInterfaceSym
  | OrdInterfaceImpl
  | OrdSignatureAnnotation
  deriving (Show, Ord, Eq)

data Constraint = Constraint Ty Ty XObj XObj XObj ConstraintOrder deriving (Eq)

instance Ord Constraint where
  compare (Constraint _ _ _ _ _ a) (Constraint _ _ _ _ _ b) = compare a b

data UnificationFailure
  = UnificationFailure
      { unificationFailure :: Constraint,
        unificationMappings :: TypeMappings
      }
  | Holes [(String, Ty)]
  deriving (Eq, Show)

instance Show Constraint where
  show (Constraint a b _ _ _ ord) = "{" ++ show a ++ " == " ++ show b ++ " (ord " ++ show ord ++ ")} " -- ++ show (fmap infoLine (info xa)) ++ ", " ++ show (fmap infoLine (info xb)) ++ " in " ++ show ctx

-- Finds the symbol with the "lowest name" (first in alphabetical order)
recursiveNameLookup :: TypeMappings -> String -> Maybe Ty
recursiveNameLookup mappings name = innerLookup name []
  where
    innerLookup :: String -> [Ty] -> Maybe Ty
    innerLookup k visited =
      case Map.lookup k mappings of
        Just exists -> case exists of
          VarTy v ->
            if exists `elem` visited
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
solve constraints = do
  naiveMappings <- foldM solveOne Map.empty constraints
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
debugSolveOne mappings constraint =
  let m' = solveOneInternal mappings constraint
   in trace
        ("" ++ show constraint ++ ", MAPPINGS: " ++ show m')
        m'

solveOneInternal :: TypeMappings -> Constraint -> Either UnificationFailure TypeMappings
solveOneInternal mappings constraint =
  case constraint of --trace ("SOLVE " ++ show constraint) constraint of
  -- Two type variables
    Constraint aTy@(VarTy aName) bTy@(VarTy bName) _ _ _ _ ->
      if aTy == bTy
        then Right mappings
        else do
          m' <- checkForConflict mappings constraint aName bTy
          checkForConflict m' constraint bName aTy
    -- One type variable
    Constraint (VarTy aName) bTy _ _ _ _ -> checkForConflict mappings constraint aName bTy
    Constraint aTy (VarTy bName) _ _ _ _ -> checkForConflict mappings constraint bName aTy
    -- Struct types
    Constraint (StructTy nameA varsA) (StructTy nameB varsB) _ _ _ _ ->
      let (Constraint _ _ i1 i2 ctx ord) = constraint
       in case solveOneInternal mappings (Constraint nameA nameB i1 i2 ctx ord) of
            Left err -> Left err
            Right ok -> foldM (\m (aa, bb) -> solveOneInternal m (Constraint aa bb i1 i2 ctx ord)) ok (zip varsA varsB)
    -- Func types
    Constraint (FuncTy argsA retA ltA) (FuncTy argsB retB ltB) _ _ _ _ ->
      if length argsA == length argsB
        then
          let (Constraint _ _ i1 i2 ctx ord) = constraint
              res =
                foldM
                  (\m (aa, bb) -> solveOneInternal m (Constraint aa bb i1 i2 ctx ord))
                  mappings
                  ( zip
                      (retA : argsA)
                      (retB : argsB)
                  )
           in case res of
                Right ok -> solveOneInternal ok (Constraint ltA ltB i1 i2 ctx ord)
                Left err -> Left err
        else Left (UnificationFailure constraint mappings)
    -- Pointer types
    Constraint (PointerTy a) (PointerTy b) _ _ _ _ ->
      let (Constraint _ _ i1 i2 ctx ord) = constraint
       in solveOneInternal mappings (Constraint a b i1 i2 ctx ord)
    -- Ref types
    -- TODO: This messes up the error message since the constraint is between non-reffed types so the refs don't show in the error message!!!
    Constraint (RefTy a ltA) (RefTy b ltB) _ _ _ _ ->
      let (Constraint _ _ i1 i2 ctx ord) = constraint
       in case solveOneInternal mappings (Constraint a b i1 i2 ctx ord) of
            Left err -> Left err
            Right ok -> solveOneInternal ok (Constraint ltA ltB i1 i2 ctx ord)
    -- As a special case, allow Refs to stand for higher-order polymorphic
    -- structs (f a b) ~ (Ref a b)
    Constraint (StructTy v@(VarTy _) args) (RefTy b ltB) _ _ _ _ ->
      let (Constraint _ _ i1 i2 ctx ord) = constraint
       in case solveOneInternal mappings (Constraint v (RefTy b ltB) i1 i2 ctx ord) of
            Left err -> Left err
            Right ok -> foldM (\m (aa, bb) -> solveOneInternal m (Constraint aa bb i1 i2 ctx ord)) ok (zip args [b, ltB])
    -- TODO: The reverse argument order is necessary here since interface code
    -- uses the opposite order of most other solving code (abstract, concrete
    -- vs. concrete, abstract)--we should bring the interface code into
    -- compliance with this to obviate this stanza
    Constraint (RefTy b ltB) (StructTy v@(VarTy _) args) _ _ _ _ ->
      let (Constraint _ _ i1 i2 ctx ord) = constraint
       in case solveOneInternal mappings (Constraint v (RefTy b ltB) i1 i2 ctx ord) of
            Left err -> Left err
            Right ok -> foldM (\m (aa, bb) -> solveOneInternal m (Constraint aa bb i1 i2 ctx ord)) ok (zip args [b, ltB])
    Constraint (ProtocolTy path _) (ProtocolTy path' _) _ _ _ _ ->
      if path == path'
        then Right mappings
        else Left (UnificationFailure constraint mappings)
    Constraint t (ProtocolTy (SymPath [] key) ts) _ _ _ _ ->
      if t `elem` ts
        then Right (Map.insert key t mappings)
        else Left (UnificationFailure constraint mappings)
    Constraint (ProtocolTy (SymPath [] key) ts) t _ _ _ _ ->
      if t `elem` ts
        then Right (Map.insert key t mappings)
        else Left (UnificationFailure constraint mappings)
    -- Else
    Constraint aTy bTy _ _ _ _ ->
      if aTy == bTy
        then Right mappings
        else Left (UnificationFailure constraint mappings)

mkConstraint :: ConstraintOrder -> XObj -> XObj -> XObj -> Ty -> Ty -> Constraint
mkConstraint order xobj1 xobj2 ctx t1 t2 = Constraint t1 t2 xobj1 xobj2 ctx order

checkForConflict :: TypeMappings -> Constraint -> String -> Ty -> Either UnificationFailure TypeMappings
-- For interface/implementation resolution, it's quite common to implement an interface using a function that's
-- generic, i.e. implementing `a -> a` as `(Ref a) -> (Ref a)` For such cases the doesTypeContainTyVarWithName check
-- is problematic, so we circumvent it as a special case.
-- Once issue [#521](https://github.com/carp-lang/Carp/issues/521) is solved we might be able to remove this.
checkForConflict mappings constraint@(Constraint _ _ _ _ _ OrdInterfaceImpl) name otherTy =
  checkConflictInternal mappings constraint name otherTy
checkForConflict mappings constraint name otherTy =
  if doesTypeContainTyVarWithName name otherTy
    then Left (UnificationFailure constraint mappings)
    else checkConflictInternal mappings constraint name otherTy

checkConflictInternal :: TypeMappings -> Constraint -> String -> Ty -> Either UnificationFailure TypeMappings
checkConflictInternal mappings constraint name otherTy =
  let (Constraint _ _ xobj1 xobj2 ctx _) = constraint
      found = recursiveNameLookup mappings name
   in case found of --trace ("CHECK CONFLICT " ++ show constraint ++ " with name " ++ name ++ ", otherTy: " ++ show otherTy ++ ", found: " ++ show found) found of
        Just (VarTy _) -> ok
        Just (StructTy (VarTy _) structTyVars) ->
          case otherTy of
            StructTy _ otherTyVars -> foldM solveOneInternal mappings (zipWith (mkConstraint OrdStruct xobj1 xobj2 ctx) structTyVars otherTyVars)
            VarTy _ -> Right mappings
            _ -> Left (UnificationFailure constraint mappings)
        Just (StructTy (ConcreteNameTy structName) structTyVars) ->
          case otherTy of
            StructTy (ConcreteNameTy otherStructName) otherTyVars
              | structName == otherStructName -> foldM solveOneInternal mappings (zipWith (mkConstraint OrdStruct xobj1 xobj2 ctx) structTyVars otherTyVars)
            StructTy (VarTy _) otherTyVars -> foldM solveOneInternal mappings (zipWith (mkConstraint OrdStruct xobj1 xobj2 ctx) structTyVars otherTyVars)
            VarTy _ -> Right mappings
            _ -> Left (UnificationFailure constraint mappings)
        Just (FuncTy argTys retTy lifetimeTy) ->
          case otherTy of
            FuncTy otherArgTys otherRetTy otherLifetimeTy ->
              do
                m <- foldM solveOneInternal mappings (zipWith (mkConstraint OrdFunc xobj1 xobj2 ctx) argTys otherArgTys)
                case solveOneInternal m (mkConstraint OrdFunc xobj1 xobj2 ctx retTy otherRetTy) of
                  Right _ -> solveOneInternal m (mkConstraint OrdFunc xobj1 xobj2 ctx lifetimeTy otherLifetimeTy)
                  Left err -> Left err
            VarTy _ -> Right mappings
            _ -> Left (UnificationFailure constraint mappings)
        Just (PointerTy innerTy) ->
          case otherTy of
            PointerTy otherInnerTy -> solveOneInternal mappings (mkConstraint OrdPtr xobj1 xobj2 ctx innerTy otherInnerTy)
            VarTy _ -> Right mappings
            _ -> Left (UnificationFailure constraint mappings)
        Just (RefTy innerTy lifetimeTy) ->
          case otherTy of
            RefTy otherInnerTy otherLifetimeTy ->
              case solveOneInternal mappings (mkConstraint OrdRef xobj1 xobj2 ctx innerTy otherInnerTy) of
                Left err -> Left err
                Right smappings -> solveOneInternal smappings (mkConstraint OrdRef xobj1 xobj2 ctx lifetimeTy otherLifetimeTy)
            VarTy _ -> Right mappings
            _ -> Left (UnificationFailure constraint mappings)
        Just foundNonVar -> case otherTy of
          (VarTy v) -> case recursiveNameLookup mappings v of
            Just (VarTy _) -> Right mappings
            Just otherNonVar ->
              if foundNonVar == otherNonVar
                then Right mappings
                else Left (UnificationFailure constraint mappings)
            Nothing -> Right mappings
          _ ->
            if otherTy == foundNonVar
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
  where
    fullResolve :: Ty -> Ty
    fullResolve x@(VarTy var) =
      case recursiveNameLookup mappings var of
        Just (StructTy name varTys) -> StructTy name (map (fullLookup Set.empty) varTys)
        Just (FuncTy argTys retTy ltTy) -> FuncTy (map (fullLookup Set.empty) argTys) (fullLookup Set.empty retTy) (fullLookup Set.empty ltTy)
        Just found -> found
        Nothing -> x -- still not found, must be a generic variable
    fullResolve x = x
    fullLookup :: Set.Set Ty -> Ty -> Ty
    fullLookup visited vv@(VarTy v) =
      case recursiveNameLookup mappings v of
        Just found ->
          if found == vv || Set.member found visited
            then found
            else fullLookup (Set.insert found visited) found
        Nothing -> vv -- compilerError ("In full lookup: Can't find " ++ v ++ " in mappings: " ++ show mappings)
    fullLookup visited structTy@(StructTy name vs) =
      let newVisited = Set.insert structTy visited
       in StructTy name (map (fullLookup newVisited) vs)
    fullLookup visited funcTy@(FuncTy argTys retTy ltTy) =
      let newVisited = Set.insert funcTy visited
       in FuncTy (map (fullLookup newVisited) argTys) (fullLookup newVisited retTy) (fullLookup newVisited ltTy)
    fullLookup _ x = x

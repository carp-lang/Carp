module Validate where

import Control.Monad (foldM)
import Data.List (nubBy, (\\))
import qualified Env as E
import Obj
import qualified Reify as R
import qualified TypeCandidate as TC
import TypeError
import TypePredicates
import Types

--------------------------------------------------------------------------------
-- Public

-- | Determine whether a given type candidate is a valid type.
validateType :: TC.TypeCandidate -> Either TypeError ()
validateType candidate =
  do
    checkDuplicateMembers candidate
    checkMembers candidate
    checkKindConsistency candidate

--------------------------------------------------------------------------------
-- Private

-- | Checks whether any field names in the type are used more than once.
checkDuplicateMembers :: TC.TypeCandidate -> Either TypeError ()
checkDuplicateMembers candidate =
  let allFields = fmap TC.fieldName (TC.getFields candidate)
      uniqueFields = nubBy (==) allFields
      duplicates = allFields \\ uniqueFields
   in if null duplicates
        then Right ()
        else Left (DuplicatedMembers (map R.symbol duplicates))

-- | Returns an error if one of the types fields can't be used as a member type.
checkMembers :: TC.TypeCandidate -> Either TypeError ()
checkMembers candidate =
  let tenv = TC.getTypeEnv candidate
      env = TC.getValueEnv candidate
      tys = concat (map TC.fieldTypes (TC.getFields candidate))
   in mapM_ (canBeUsedAsMemberType (TC.getName candidate) (TC.getRestriction candidate) tenv env (TC.getVariables candidate)) tys

-- | Returns an error if the type variables in the body of the type and variables in the head of the type are of incompatible kinds.
checkKindConsistency :: TC.TypeCandidate -> Either TypeError ()
checkKindConsistency candidate =
  let allFieldTypes = concat (map TC.fieldTypes (TC.getFields candidate))
      allGenerics = filter isTypeGeneric $ allFieldTypes
   in case areKindsConsistent allGenerics of
        Left var -> Left (InconsistentKinds var (map R.reify allFieldTypes))
        _ -> pure ()

-- | Can this type be used as a member for a deftype?
canBeUsedAsMemberType :: String -> TC.TypeVarRestriction -> TypeEnv -> Env -> [Ty] -> Ty -> Either TypeError ()
canBeUsedAsMemberType tname typeVarRestriction typeEnv globalEnv typeVariables ty =
  case ty of
    UnitTy -> pure ()
    IntTy -> pure ()
    FloatTy -> pure ()
    DoubleTy -> pure ()
    ByteTy -> pure ()
    LongTy -> pure ()
    BoolTy -> pure ()
    StringTy -> pure ()
    PatternTy -> pure ()
    CharTy -> pure ()
    FuncTy {} -> pure ()
    PointerTy UnitTy -> pure ()
    PointerTy inner ->
      canBeUsedAsMemberType tname typeVarRestriction typeEnv globalEnv typeVariables inner
    -- Struct variables may appear as complete applications or individual
    -- components in the head of a definition; that is the forms:
    --     ((Foo (f a b)) [x (f a b)])
    --     ((Foo f a b) [x f y a z b])
    -- are both valid, but restrict their types differently. In the former,
    -- `f` may only appear in complete applications over `a` and `b`, in
    -- other words, `f` is closed over `a` and `b`. In the latter, f may
    -- flexibly be used as a type variable of nullary kind, or as a type
    -- variable of unary kind `(Foo f a b) [x (f a) y (f b)])` so long as
    -- the kinds of each occasion of `f` are consistent.
    --
    -- Likewise, the types denoted by:
    --     ((Foo (f a) b) ...)
    -- and
    --     ((Foo (f a) (f b)) ...)
    -- differ.
    -- Attempt the first, more restrictive formulation first.
    struct@(StructTy name tyVars) ->
      checkVar struct <> checkStruct name tyVars
    v@(VarTy _) -> checkVar v
    _ -> Left (InvalidMemberType ty (R.reify ty))
  where
    checkStruct :: Ty -> [Ty] -> Either TypeError ()
    checkStruct (ConcreteNameTy (SymPath [] "Array")) [innerType] =
      canBeUsedAsMemberType tname typeVarRestriction typeEnv globalEnv typeVariables innerType
    checkStruct (ConcreteNameTy path@(SymPath _ name)) vars =
      case E.getTypeBinder typeEnv name <> E.findTypeBinder globalEnv path of
        Right (Binder _ (XObj (Lst (XObj (ExternalType _) _ _ : _)) _ _)) ->
          pure ()
        Right (Binder _ (XObj (Lst (XObj (Deftype t) _ _ : _)) _ _)) ->
          checkInhabitants t >> foldM (\_ typ -> canBeUsedAsMemberType tname typeVarRestriction typeEnv globalEnv typeVariables typ) () vars
        Right (Binder _ (XObj (Lst (XObj (DefSumtype t) _ _ : _)) _ _)) ->
          checkInhabitants t >> foldM (\_ typ -> canBeUsedAsMemberType tname typeVarRestriction typeEnv globalEnv typeVariables typ) () vars
        _ -> Left (NotAmongRegisteredTypes ty (R.reify ty))
      where
        checkInhabitants :: Ty -> Either TypeError ()
        checkInhabitants (StructTy _ vs) =
          if length vs == length vars
            then pure ()
            else Left (UninhabitedConstructor ty (R.reify ty) (length vs) (length vars))
        checkInhabitants _ = Left (InvalidMemberType ty (R.reify ty))
    checkStruct v@(VarTy _) vars =
      canBeUsedAsMemberType tname typeVarRestriction typeEnv globalEnv typeVariables v
        >> foldM (\_ typ -> canBeUsedAsMemberType tname typeVarRestriction typeEnv globalEnv typeVariables typ) () vars
    checkStruct _ _ = error "checkstruct"
    checkVar :: Ty -> Either TypeError ()
    checkVar variable =
      case typeVarRestriction of
        TC.AllowAny -> pure ()
        TC.OnlyNamesInScope ->
          if any (isCaptured variable) typeVariables
            then pure ()
            else Left (InvalidMemberType ty (R.reify ty))
      where
        -- If a variable `a` appears in a higher-order polymorphic form, such as `(f a)`
        -- `a` may be used as a member, sans `f`, but `f` may not appear
        -- without `a`.
        isCaptured :: Ty -> Ty -> Bool
        isCaptured v@(VarTy _) t@(VarTy _) = t == v
        isCaptured sa@(StructTy (VarTy _) _) sb@(StructTy (VarTy _) _) = sa == sb
        isCaptured v@(VarTy _) (StructTy _ vars) = v `elem` vars
        -- Not a variable.
        isCaptured _ _ = True

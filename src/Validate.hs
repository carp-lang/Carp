module Validate where

import Control.Monad (foldM)
import Data.Function (on)
import Data.List (nubBy, (\\))
import Data.Maybe (fromJust)
import qualified Env as E
import Obj
import TypeError
import TypePredicates
import Types
import Util

{-# ANN validateMemberCases "HLint: ignore Eta reduce" #-}

data TypeVarRestriction
  = AllowAnyTypeVariableNames
  | AllowOnlyCapturedNames
  deriving (Eq)

-- | Make sure that the member declarations in a type definition
-- | Follow the pattern [<name> <type>, <name> <type>, ...]
-- | TODO: This function is only called by the deftype parts of the codebase, which is more specific than the following check implies.
validateMemberCases :: TypeEnv -> [Ty] -> [XObj] -> Either TypeError ()
validateMemberCases typeEnv typeVariables rest = mapM_ visit rest
  where
    visit (XObj (Arr membersXObjs) _ _) =
      validateMembers AllowOnlyCapturedNames typeEnv typeVariables membersXObjs
    visit xobj =
      Left (InvalidSumtypeCase xobj)

validateMembers :: TypeVarRestriction -> TypeEnv -> [Ty] -> [XObj] -> Either TypeError ()
validateMembers typeVarRestriction typeEnv typeVariables membersXObjs =
  checkUnevenMembers >> checkDuplicateMembers >> checkMembers >> checkKindConsistency
  where
    pairs = pairwise membersXObjs
    -- Are the number of members even?
    checkUnevenMembers :: Either TypeError ()
    checkUnevenMembers =
      if even (length membersXObjs)
        then Right ()
        else Left (UnevenMembers membersXObjs)
    -- Are any members duplicated?
    checkDuplicateMembers :: Either TypeError ()
    checkDuplicateMembers =
      if length fields == length uniqueFields
        then Right ()
        else Left (DuplicatedMembers dups)
      where
        fields = fst <$> pairs
        uniqueFields = nubBy ((==) `on` xobjObj) fields
        dups = fields \\ uniqueFields
    -- Do all type variables have consistent kinds?
    checkKindConsistency :: Either TypeError ()
    checkKindConsistency =
      case areKindsConsistent varsOnly of
        Left var -> Left (InconsistentKinds var membersXObjs)
        _ -> pure ()
      where
        -- fromJust is safe here; invalid types will be caught in the prior check.
        -- todo? be safer anyway?
        varsOnly = filter isTypeGeneric (map (fromJust . xobjToTy . snd) pairs)
    checkMembers :: Either TypeError ()
    checkMembers = mapM_ (okXObjForType typeVarRestriction typeEnv typeVariables . snd) pairs

okXObjForType :: TypeVarRestriction -> TypeEnv -> [Ty] -> XObj -> Either TypeError ()
okXObjForType typeVarRestriction typeEnv typeVariables xobj =
  case xobjToTy xobj of
    Just t -> canBeUsedAsMemberType typeVarRestriction typeEnv typeVariables t xobj
    Nothing -> Left (NotAType xobj)

-- | Can this type be used as a member for a deftype?
canBeUsedAsMemberType :: TypeVarRestriction -> TypeEnv -> [Ty] -> Ty -> XObj -> Either TypeError ()
canBeUsedAsMemberType typeVarRestriction typeEnv typeVariables ty xobj =
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
      canBeUsedAsMemberType typeVarRestriction typeEnv typeVariables inner xobj
        >> pure ()
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
    _ -> Left (InvalidMemberType ty xobj)
  where
    checkStruct :: Ty -> [Ty] -> Either TypeError ()
    checkStruct (ConcreteNameTy (SymPath [] "Array")) [innerType] =
      canBeUsedAsMemberType typeVarRestriction typeEnv typeVariables innerType xobj
        >> pure ()
    checkStruct (ConcreteNameTy (SymPath _ name)) vars =
      case E.getTypeBinder typeEnv name of
        Right (Binder _ (XObj (Lst (XObj (ExternalType _) _ _ : _)) _ _)) ->
          pure ()
        Right (Binder _ (XObj (Lst (XObj (Deftype t) _ _ : _)) _ _)) ->
          checkInhabitants t >> foldM (\_ typ -> canBeUsedAsMemberType typeVarRestriction typeEnv typeVariables typ xobj) () vars
        Right (Binder _ (XObj (Lst (XObj (DefSumtype t) _ _ : _)) _ _)) ->
          checkInhabitants t >> foldM (\_ typ -> canBeUsedAsMemberType typeVarRestriction typeEnv typeVariables typ xobj) () vars
        _ -> Left (NotAmongRegisteredTypes ty xobj)
      where
        checkInhabitants :: Ty -> Either TypeError ()
        checkInhabitants (StructTy _ vs) =
          if length vs == length vars
            then pure ()
            else Left (UninhabitedConstructor ty xobj (length vs) (length vars))
        checkInhabitants _ = Left (InvalidMemberType ty xobj)
    checkStruct v@(VarTy _) vars =
      canBeUsedAsMemberType typeVarRestriction typeEnv typeVariables v xobj
        >> foldM (\_ typ -> canBeUsedAsMemberType typeVarRestriction typeEnv typeVariables typ xobj) () vars
    checkStruct _ _ = error "checkstruct"
    checkVar :: Ty -> Either TypeError ()
    checkVar variable =
      case typeVarRestriction of
        AllowAnyTypeVariableNames ->
          pure ()
        AllowOnlyCapturedNames ->
          if any (isCaptured variable) typeVariables
            then pure ()
            else Left (InvalidMemberType ty xobj)
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

module Validate where

import Data.Function (on)
import Data.List ((\\), nubBy)
import Lookup
import Managed
import Obj
import TypeError
import Types
import Util

{-# ANN validateMembers "HLint: ignore Eta reduce" #-}

-- | Make sure that the member declarations in a type definition
-- | Follow the pattern [<name> <type>, <name> <type>, ...]
-- | TODO: This function is only called by the deftype parts of the codebase, which is more specific than the following check implies.
validateMemberCases :: TypeEnv -> [Ty] -> [XObj] -> Either TypeError ()
validateMemberCases typeEnv typeVariables rest = mapM_ visit rest
  where
    visit (XObj (Arr membersXObjs) _ _) =
      validateMembers typeEnv typeVariables membersXObjs
    visit xobj =
      Left (InvalidSumtypeCase xobj)

validateMembers :: TypeEnv -> [Ty] -> [XObj] -> Either TypeError ()
validateMembers typeEnv typeVariables membersXObjs =
  checkUnevenMembers >> checkDuplicateMembers >> checkMembers
  where
    checkUnevenMembers =
      if length membersXObjs `mod` 2 == 0
        then Right ()
        else Left (UnevenMembers membersXObjs)
    pairs = pairwise membersXObjs
    fields = fst <$> pairs
    uniqueFields = nubBy ((==) `on` xobjObj) fields
    dups = fields \\ uniqueFields
    checkDuplicateMembers =
      if length fields == length uniqueFields
        then Right ()
        else Left (DuplicatedMembers dups)
    checkMembers = mapM_ (okXObjForType typeEnv typeVariables . snd) pairs

-- validateOneCase :: XObj -> a
-- validateOneCase XObj {} =
--   error "Type members must be defined using array syntax: [member1 type1 member2 type2 ...]" -- | TODO: How to reach this case?

okXObjForType :: TypeEnv -> [Ty] -> XObj -> Either TypeError ()
okXObjForType typeEnv typeVariables xobj =
  case xobjToTy xobj of
    Just t -> canBeUsedAsMemberType typeEnv typeVariables t xobj
    Nothing -> Left (NotAType xobj)

-- | Can this type be used as a member for a deftype?
canBeUsedAsMemberType :: TypeEnv -> [Ty] -> Ty -> XObj -> Either TypeError ()
canBeUsedAsMemberType typeEnv typeVariables ty xobj =
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
    PointerTy inner -> do
      _ <- canBeUsedAsMemberType typeEnv typeVariables inner xobj
      pure ()
    StructTy (ConcreteNameTy "Array") [inner] -> do
      _ <- canBeUsedAsMemberType typeEnv typeVariables inner xobj
      pure ()
    StructTy name [tyVars] ->
      case name of
        (ConcreteNameTy name') ->
          -- ensure structs are filled with values
          -- Prevents deftypes such as (deftype Player [pos Vector3])
          do
            _ <- canBeUsedAsMemberType typeEnv typeVariables tyVars xobj
            case lookupBinder (SymPath [] name') (getTypeEnv typeEnv) of
              Just _ -> pure ()
              Nothing -> Left (NotAmongRegisteredTypes ty xobj)
        -- e.g. (deftype (Higher (f a)) (Of [(f a)]))
        (VarTy _) -> pure ()
    s@(StructTy name tyvar) ->
      if isExternalType typeEnv s
        then pure ()
        else case name of
          (ConcreteNameTy n) ->
            case lookupBinder (SymPath [] n) (getTypeEnv typeEnv) of
              Just (Binder _ (XObj (Lst (XObj (Deftype t) _ _ : _)) _ _)) ->
                checkInhabitants t
              Just (Binder _ (XObj (Lst (XObj (DefSumtype t) _ _ : _)) _ _)) ->
                checkInhabitants t
              _ -> Left (InvalidMemberType ty xobj)
            where
              -- Make sure any struct types have arguments before they can be used as members.

              checkInhabitants t =
                case t of
                  (StructTy _ vars) ->
                    if length vars == length tyvar
                      then pure ()
                      else Left (UninhabitedConstructor ty xobj (length tyvar) (length vars))
                  _ -> Left (InvalidMemberType ty xobj)
          _ -> Left (InvalidMemberType ty xobj)
    VarTy _ ->
      if foldr (||) False (map (isCaptured ty) typeVariables)
        then pure ()
        else Left (InvalidMemberType ty xobj)
      where
        -- If a variable `a` appears in a higher-order polymorphic form, such as `(f a)`
        -- `a` may be used as a member, sans `f`.
        isCaptured t v@(VarTy _) = t == v
        isCaptured t (StructTy (VarTy _) vars) = any (== t) vars
    _ -> Left (InvalidMemberType ty xobj)

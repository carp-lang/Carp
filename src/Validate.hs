module Validate where

import Control.Monad (foldM)
import Data.List (nubBy, (\\))
import qualified Env as E
import Obj
import TypeError
import TypePredicates
import Types
import TypeCandidate
import Interfaces
import Reify

{-# ANN validateMemberCases "HLint: ignore Eta reduce" #-}

-- | Make sure that the member declarations in a type definition
-- | Follow the pattern [<name> <type>, <name> <type>, ...]
-- | TODO: This function is only called by the deftype parts of the codebase, which is more specific than the following check implies.
validateMemberCases :: TypeCandidate -> Either TypeError ()
validateMemberCases candidate =
  validateMembers (candidate {restriction = AllowOnlyNamesInScope})

-- | Validates whether or not all the members of a type candidate can be used as member types.
validateMembers :: TypeCandidate -> Either TypeError ()
validateMembers candidate =
  (checkDuplicateMembers candidate) >>
  (checkMembers (candidateTypeEnv candidate) (candidateEnv candidate) candidate) >>
  (checkKindConsistency candidate)

-- | Validates whether or not a candidate's types implement interfaces.
validateInterfaceConstraints :: TypeCandidate -> Either TypeError ()
validateInterfaceConstraints candidate =
  let impls = map go (interfaceConstraints candidate)
   in if all (==True) impls
        then Right ()
        else Left $ InterfaceNotImplemented  (map interfaceName (interfaceConstraints candidate))
  where go ic = all (interfaceImplementedForTy (candidateTypeEnv candidate) (candidateEnv candidate) (interfaceName ic)) (types ic)

--------------------------------------------------------------------------------
-- Private

-- | Returns an error if a type has more than one member with the same name.
checkDuplicateMembers :: TypeCandidate -> Either TypeError ()
checkDuplicateMembers candidate =
  if length fields == length uniqueFields
    then Right ()
    else Left (DuplicatedMembers (map symbol dups))
  where
    fields = fmap fst (typemembers candidate)
    uniqueFields = nubBy (==) fields
    dups = fields \\ uniqueFields

-- | Returns an error if the type variables in the body of the type and variables in the head of the type are of incompatible kinds.
checkKindConsistency :: TypeCandidate -> Either TypeError ()
checkKindConsistency candidate =
  case areKindsConsistent varsOnly of
    Left var -> Left (InconsistentKinds var (map reify (concat (map snd (typemembers candidate)))))
    _ -> pure ()
  where
    varsOnly = filter isTypeGeneric $ concat (map snd (typemembers candidate))

-- | Returns an error if one of the types members can't be used as a member.
checkMembers :: TypeEnv -> Env -> TypeCandidate -> Either TypeError ()
checkMembers typeEnv globalEnv candidate =
  let tys = concat $ map snd (typemembers candidate)
   in mapM_ (canBeUsedAsMemberType (typename candidate) (restriction candidate) typeEnv globalEnv (variables candidate)) tys

-- | Can this type be used as a member for a deftype?
canBeUsedAsMemberType :: String -> TypeVarRestriction -> TypeEnv -> Env -> [Ty] -> Ty -> Either TypeError ()
canBeUsedAsMemberType tyname typeVarRestriction typeEnv globalEnv typeVariables ty =
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
      canBeUsedAsMemberType tyname typeVarRestriction typeEnv globalEnv typeVariables inner
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
    struct@(StructTy sname tyVars) ->
      checkVar struct <> checkStruct sname tyVars
    v@(VarTy _) -> checkVar v
    (RecTy _) -> pure ()
    _ -> Left (InvalidMemberType ty (reify ty))
  where
    checkStruct :: Ty -> [Ty] -> Either TypeError ()
    checkStruct (ConcreteNameTy (SymPath [] "Array")) [innerType] =
      canBeUsedAsMemberType tyname typeVarRestriction typeEnv globalEnv typeVariables innerType 
        >> pure ()
    checkStruct (ConcreteNameTy (SymPath [] "Box")) [innerType] =
      canBeUsedAsMemberType tyname typeVarRestriction typeEnv globalEnv typeVariables innerType 
        >> pure ()
    checkStruct (ConcreteNameTy path@(SymPath _ pname)) vars =
      if pname == tyname && length vars == length typeVariables
        then foldM (\_ typ -> canBeUsedAsMemberType tyname typeVarRestriction typeEnv globalEnv typeVariables typ) () vars
        else
          case E.getTypeBinder typeEnv pname <> E.findTypeBinder globalEnv path of
            Right (Binder _ (XObj (Lst (XObj (ExternalType _) _ _ : _)) _ _)) ->
              pure ()
            Right (Binder _ (XObj (Lst (XObj (Deftype t) _ _ : _)) _ _)) ->
              checkInhabitants t >> foldM (\_ typ -> canBeUsedAsMemberType tyname typeVarRestriction typeEnv globalEnv typeVariables typ) () vars
            Right (Binder _ (XObj (Lst (XObj (DefSumtype t) _ _ : _)) _ _)) ->
              checkInhabitants t >> foldM (\_ typ -> canBeUsedAsMemberType tyname typeVarRestriction typeEnv globalEnv typeVariables typ) () vars
            _ -> Left (NotAmongRegisteredTypes ty (reify ty))
      where
        checkInhabitants :: Ty -> Either TypeError ()
        checkInhabitants (StructTy _ vs) =
          if length vs == length vars
            then pure ()
            else Left (UninhabitedConstructor ty (reify ty) (length vs) (length vars))
        checkInhabitants _ = Left (InvalidMemberType ty (reify ty))
    checkStruct v@(VarTy _) vars =
      canBeUsedAsMemberType tyname typeVarRestriction typeEnv globalEnv typeVariables v
        >> foldM (\_ typ -> canBeUsedAsMemberType tyname typeVarRestriction typeEnv globalEnv typeVariables typ) () vars
    checkStruct _ _ = error "checkstruct"
    checkVar :: Ty -> Either TypeError ()
    checkVar variable =
      case typeVarRestriction of
        AllowAnyTypeVariableNames ->
          pure ()
        AllowOnlyNamesInScope ->
          if any (isCaptured variable) typeVariables
            then pure ()
            else Left (InvalidMemberType ty (reify ty))
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

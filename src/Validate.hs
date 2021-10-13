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
  = AllowAnyTypeVariableNames -- Used when checking a type found in the code, e.g. (Foo a), any name is OK for 'a'
  | AllowOnlyNamesInScope -- Used when checking a type definition, e.g. (deftype (Foo a) [x a]), requires a to be in scope
  deriving (Eq)

-- | TypeCandidate represents a type that's possibly valid or invalid.
data TypeCandidate = TypeCandidate {
  -- the name of the type
  typename :: String,
  -- a list of all variables in the type head
  variables :: [Ty],
  -- all members of the type
  typemembers :: [XObj],
  -- what sort of type variables are permitted.
  restriction :: TypeVarRestriction
}

-- | Make sure that the member declarations in a type definition
-- | Follow the pattern [<name> <type>, <name> <type>, ...]
-- | TODO: This function is only called by the deftype parts of the codebase, which is more specific than the following check implies.
validateMemberCases :: TypeEnv -> Env -> TypeCandidate -> Either TypeError ()
validateMemberCases typeEnv globalEnv candidate = --mapM_ visit (members candidate)
  validateMembers typeEnv globalEnv (candidate {restriction = AllowOnlyNamesInScope})
  -- where
  --  visit (XObj (Arr membersXObjs) _ _) =
  --    validateMembers typeEnv globalEnv (candidate {restriction = AllowOnlyNamesInScope})
  --  visit xobj =
  --    Left (InvalidSumtypeCase xobj)

validateMembers :: TypeEnv -> Env -> TypeCandidate -> Either TypeError ()
validateMembers typeEnv globalEnv candidate =
  (checkUnevenMembers candidate) >>
  (checkDuplicateMembers candidate) >>
  (checkMembers typeEnv globalEnv candidate) >>
  (checkKindConsistency candidate)

-- | Returns an error if a type has an uneven number of members.
checkUnevenMembers :: TypeCandidate -> Either TypeError ()
checkUnevenMembers candidate =
  if even (length (typemembers candidate))
    then Right ()
    else Left (UnevenMembers (typemembers candidate))

-- | Returns an error if a type has more than one member with the same name.
checkDuplicateMembers :: TypeCandidate -> Either TypeError ()
checkDuplicateMembers candidate =
  if length fields == length uniqueFields
    then Right ()
    else Left (DuplicatedMembers dups)
  where
    fields = fst <$> (pairwise (typemembers candidate))
    uniqueFields = nubBy ((==) `on` xobjObj) fields
    dups = fields \\ uniqueFields

-- | Returns an error if the type variables in the body of the type and variables in the head of the type are of incompatible kinds.
checkKindConsistency :: TypeCandidate -> Either TypeError ()
checkKindConsistency candidate =
  case areKindsConsistent varsOnly of
    Left var -> Left (InconsistentKinds var (typemembers candidate))
    _ -> pure ()
  where
    -- fromJust is safe here; invalid types will be caught in a prior check.
    -- TODO: be safer.
    varsOnly = filter isTypeGeneric (map (fromJust . xobjToTy . snd) (pairwise (typemembers candidate)))

-- | Returns an error if one of the types members can't be used as a member.
checkMembers :: TypeEnv -> Env -> TypeCandidate -> Either TypeError ()
checkMembers typeEnv globalEnv candidate = mapM_ (okXObjForType (typename candidate) (restriction candidate) typeEnv globalEnv (variables candidate) . snd) (pairwise (typemembers candidate))

okXObjForType :: String -> TypeVarRestriction -> TypeEnv -> Env -> [Ty] -> XObj -> Either TypeError ()
okXObjForType tyname typeVarRestriction typeEnv globalEnv typeVariables xobj =
  case xobjToTy xobj of
    Just t -> canBeUsedAsMemberType tyname typeVarRestriction typeEnv globalEnv typeVariables t xobj
    Nothing -> Left (NotAType xobj)

-- | Can this type be used as a member for a deftype?
canBeUsedAsMemberType :: String -> TypeVarRestriction -> TypeEnv -> Env -> [Ty] -> Ty -> XObj -> Either TypeError ()
canBeUsedAsMemberType tyname typeVarRestriction typeEnv globalEnv typeVariables ty xobj =
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
      canBeUsedAsMemberType tyname typeVarRestriction typeEnv globalEnv typeVariables inner xobj
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
    _ -> Left (InvalidMemberType ty xobj)
  where
    checkStruct :: Ty -> [Ty] -> Either TypeError ()
    checkStruct (ConcreteNameTy (SymPath [] "Array")) [innerType] =
      canBeUsedAsMemberType tyname typeVarRestriction typeEnv globalEnv typeVariables innerType xobj
        >> pure ()
    checkStruct (ConcreteNameTy path@(SymPath _ pname)) vars =
      case E.getTypeBinder typeEnv pname <> E.findTypeBinder globalEnv path of
        Right (Binder _ (XObj (Lst (XObj (ExternalType _) _ _ : _)) _ _)) ->
          pure ()
        Right (Binder _ (XObj (Lst (XObj (Deftype t) _ _ : _)) _ _)) ->
          checkInhabitants t >> foldM (\_ typ -> canBeUsedAsMemberType tyname typeVarRestriction typeEnv globalEnv typeVariables typ xobj) () vars
        Right (Binder _ (XObj (Lst (XObj (DefSumtype t) _ _ : _)) _ _)) ->
          checkInhabitants t >> foldM (\_ typ -> canBeUsedAsMemberType tyname typeVarRestriction typeEnv globalEnv typeVariables typ xobj) () vars
        _ -> Left (NotAmongRegisteredTypes ty xobj)
      where
        checkInhabitants :: Ty -> Either TypeError ()
        checkInhabitants (StructTy _ vs) =
          if length vs == length vars
            then pure ()
            else Left (UninhabitedConstructor ty xobj (length vs) (length vars))
        checkInhabitants _ = Left (InvalidMemberType ty xobj)
    checkStruct v@(VarTy _) vars =
      canBeUsedAsMemberType tyname typeVarRestriction typeEnv globalEnv typeVariables v xobj
        >> foldM (\_ typ -> canBeUsedAsMemberType tyname typeVarRestriction typeEnv globalEnv typeVariables typ xobj) () vars
    checkStruct _ _ = error "checkstruct"
    checkVar :: Ty -> Either TypeError ()
    checkVar variable =
      case typeVarRestriction of
        AllowAnyTypeVariableNames ->
          pure ()
        AllowOnlyNamesInScope ->
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

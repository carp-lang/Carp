module Managed where

import Interfaces
import Lookup
import Obj
import Types

-- | Find out if a type is "external", meaning it is not defined by the user
--   in this program but instead imported from another C library or similar.
-- NOTE: Quite possibly this function should be removed and we should rely on 'isManaged' instead?
isExternalType :: TypeEnv -> Ty -> Bool
isExternalType typeEnv (PointerTy p) =
  isExternalType typeEnv p
isExternalType typeEnv (StructTy (ConcreteNameTy name) _) =
  case lookupBinder (SymPath [] name) (getTypeEnv typeEnv) of
    Just (Binder _ (XObj (Lst (XObj (ExternalType _) _ _ : _)) _ _)) -> True
    _ -> False
isExternalType _ _ =
  False

-- | Should this type be handled by the memory management system.
-- Implementation note: This top-level pattern match should be able to just
-- match on all types and see whether they implement 'delete', but for some
-- reson that doesn't work. Might need to handle generic types separately?
isManaged :: TypeEnv -> Env -> Ty -> Bool
isManaged typeEnv globalEnv structTy@StructTy{} =
  interfaceImplementedForTy typeEnv globalEnv "delete" (FuncTy [structTy] UnitTy StaticLifetimeTy)
isManaged typeEnv globalEnv funcTy@FuncTy{} =
  interfaceImplementedForTy typeEnv globalEnv "delete" (FuncTy [funcTy] UnitTy StaticLifetimeTy)
isManaged _ _ StringTy =
  True
isManaged _ _ PatternTy =
  True
isManaged _ _ _ =
  False

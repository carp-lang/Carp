module Managed where

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

-- | Is this type managed - does it need to be freed?
isManaged :: TypeEnv -> Env -> Ty -> Bool
isManaged typeEnv globalEnv structTy@(StructTy _ _) =
  implements typeEnv globalEnv "delete" (FuncTy [structTy] UnitTy StaticLifetimeTy)
isManaged _ _ _ = False

module Managed where

import Interfaces
import Obj
import Types

-- | Should this type be handled by the memory management system.
-- Implementation note: This top-level pattern match should be able to just
-- match on all types and see whether they implement 'delete', but for some
-- reason that doesn't work. Might need to handle generic types separately?
--
-- TODO: When blit and delete are both implemented, issue a warning.
isManaged :: TypeEnv -> Env -> Ty -> Bool
isManaged typeEnv globalEnv structTy@StructTy {} =
  not (isBlittable typeEnv globalEnv structTy)
    && interfaceImplementedForTy typeEnv globalEnv "delete" (FuncTy [structTy] UnitTy StaticLifetimeTy)
isManaged typeEnv globalEnv funcTy@FuncTy {} =
  not (isBlittable typeEnv globalEnv funcTy)
    && interfaceImplementedForTy typeEnv globalEnv "delete" (FuncTy [funcTy] UnitTy StaticLifetimeTy)
isManaged _ _ StringTy =
  True
isManaged _ _ PatternTy =
  True
isManaged _ _ _ =
  False

-- | Returns true if this type implements the "blit" interface and is thus
-- freely copyable.
isBlittable :: TypeEnv -> Env -> Ty -> Bool
isBlittable typeEnv globalEnv t =
  interfaceImplementedForTy typeEnv globalEnv "blit" (FuncTy [t] t StaticLifetimeTy)

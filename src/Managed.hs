module Managed where

import Interfaces
import Obj
import Types

-- | Should this type be handled by the memory management system.
-- Implementation note: This top-level pattern match should be able to just
-- match on all types and see whether they implement 'delete', but for some
-- reson that doesn't work. Might need to handle generic types separately?
isManaged :: TypeEnv -> Env -> Ty -> Bool
isManaged typeEnv globalEnv structTy@StructTy {} =
  interfaceImplementedForTy typeEnv globalEnv "delete" (FuncTy [structTy] UnitTy StaticLifetimeTy)
isManaged typeEnv globalEnv funcTy@FuncTy {} =
  interfaceImplementedForTy typeEnv globalEnv "delete" (FuncTy [funcTy] UnitTy StaticLifetimeTy)
isManaged _ _ StringTy =
  True
isManaged _ _ PatternTy =
  True
isManaged _ _ _ =
  False

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
isManaged typeEnv globalEnv = isManagedIn (findManagedTypes typeEnv globalEnv)

-- | Returns true if this type implements the "blit" interface and is thus
-- freely copyable.
isBlittable :: TypeEnv -> Env -> Ty -> Bool
isBlittable typeEnv globalEnv = isBlittableIn (findManagedTypes typeEnv globalEnv)

-- | The types of the 'blit' and 'delete' implementations, looked up once so
-- that many checks against the same environments can share the work.
data ManagedTypes = ManagedTypes
  { blitImplementationTypes :: [Ty],
    deleteImplementationTypes :: [Ty]
  }

findManagedTypes :: TypeEnv -> Env -> ManagedTypes
findManagedTypes typeEnv globalEnv =
  ManagedTypes
    (interfaceImplementationTypes typeEnv globalEnv "blit")
    (interfaceImplementationTypes typeEnv globalEnv "delete")

-- | Like 'isManaged', but checks against precomputed 'ManagedTypes'.
isManagedIn :: ManagedTypes -> Ty -> Bool
isManagedIn managed structTy@StructTy {} =
  not (isBlittableIn managed structTy)
    && any (areUnifiable (FuncTy [structTy] UnitTy StaticLifetimeTy)) (deleteImplementationTypes managed)
isManagedIn managed funcTy@FuncTy {} =
  not (isBlittableIn managed funcTy)
    && any (areUnifiable (FuncTy [funcTy] UnitTy StaticLifetimeTy)) (deleteImplementationTypes managed)
isManagedIn _ StringTy =
  True
isManagedIn _ PatternTy =
  True
isManagedIn _ _ =
  False

-- | Like 'isBlittable', but checks against precomputed 'ManagedTypes'.
isBlittableIn :: ManagedTypes -> Ty -> Bool
isBlittableIn managed t =
  any (areUnifiable (FuncTy [t] t StaticLifetimeTy)) (blitImplementationTypes managed)

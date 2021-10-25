module TypeCandidate where

import Types
import Obj

data TypeVarRestriction
  = AllowAnyTypeVariableNames -- Used when checking a type found in the code, e.g. (Foo a), any name is OK for 'a'
  | AllowOnlyNamesInScope -- Used when checking a type definition, e.g. (deftype (Foo a) [x a]), requires a to be in scope
  deriving (Eq)

data InterfaceConstraint = InterfaceConstraint {
  interfaceName :: String,
  types :: [Ty]
} deriving Show

-- | TypeCandidate represents a type that's possibly valid or invalid.
data TypeCandidate = TypeCandidate {
  -- the name of the type
  typename :: String,
  -- a list of all variables in the type head
  variables :: [Ty],
  -- all members of the type
  typemembers :: [XObj],
  -- what sort of type variables are permitted.
  restriction :: TypeVarRestriction,
  -- what interfaces should types satisfy 
  interfaceConstraints :: [InterfaceConstraint],
  candidateTypeEnv :: TypeEnv,
  candidateEnv :: Env
}

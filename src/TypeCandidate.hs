module TypeCandidate where

import Types
import TypeError
import Obj
import Util

--------------------------------------------------------------------------------
-- Data types

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
  typemembers :: [(String, [Ty])],
  -- what sort of type variables are permitted.
  restriction :: TypeVarRestriction,
  -- what interfaces should types satisfy 
  interfaceConstraints :: [InterfaceConstraint],
  candidateTypeEnv :: TypeEnv,
  candidateEnv :: Env
}

--------------------------------------------------------------------------------
-- Constructors

-- | Constructs a type candidate from the members of a product type definition.
fromDeftype :: String -> [Ty] -> TypeEnv -> Env -> [XObj] -> Either TypeError TypeCandidate
fromDeftype name vars tenv env members =
  let tMembers = mapM go (pairwise members)
      candidate = TypeCandidate {
                    typename    = name,
                    variables   = vars,
                    typemembers = [],
                    interfaceConstraints = [],
                    restriction = AllowOnlyNamesInScope,
                    candidateTypeEnv = tenv,
                    candidateEnv = env
                  }
   in if even (length members) 
        then fmap (\ms -> candidate {typemembers = ms}) tMembers
        else Left (UnevenMembers members)
   where go :: (XObj, XObj) -> Either TypeError (String, [Ty])
         go ((XObj (Sym (SymPath [] fieldname) _) _ _), tyx) =
           case xobjToTy tyx of
             Just t  -> Right (fieldname, [t]) 
             Nothing -> Left (NotAType tyx)
         go (x, _) = Left (InvalidProductField x)

-- | Constructs a type candidate from the members of a sum type definition.
fromSumtype :: String -> [Ty] -> TypeEnv -> Env -> [XObj] -> Either TypeError TypeCandidate 
fromSumtype name vars tenv env members = 
  let tMembers = mapM go members
      candidate = TypeCandidate {
                    typename = name,
                    variables = vars,
                    typemembers = [],
                    interfaceConstraints = [],
                    restriction = AllowOnlyNamesInScope,
                    candidateTypeEnv = tenv,
                    candidateEnv = env
                  }
   in fmap (\ms -> candidate {typemembers = ms}) tMembers  
  where go :: XObj -> Either TypeError (String, [Ty])
        go x@(XObj (Lst [XObj (Sym (SymPath [] pname) Symbol) _ _, XObj (Arr tyXObjs) _ _]) _ _) = 
          case mapM xobjToTy tyXObjs of 
            Just ts -> Right (pname, ts)
            Nothing -> Left (InvalidSumtypeCase x)  
        go (XObj (Sym (SymPath [] pname) Symbol) _ _) = Right (pname, [])
        go x = Left (InvalidSumtypeCase x)

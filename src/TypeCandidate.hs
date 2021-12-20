-- | Module type candidate defines a structure for type definitions that have not been validated.
--
-- Type candidates can either be valid or invalid. Invalid type candidates will be rejected by the type system.
module TypeCandidate
  ( mkStructCandidate,
    mkSumtypeCandidate,
    TypeVarRestriction (..),
    InterfaceConstraint (..),
    TypeField (..),
    TypeMode (..),
    getFields,
    TypeCandidate.getName,
    getRestriction,
    getVariables,
    TypeCandidate.getTypeEnv,
    getConstraints,
    getValueEnv,
    getMode,
    TypeCandidate.getPath,
    getFullPath,
    fieldName,
    fieldTypes,
    setRestriction,
    toType,
    TypeCandidate,
  )
where

import Obj
import TypeError
import Types
import Util

--------------------------------------------------------------------------------
-- Data

data TypeVarRestriction
  = AllowAny
  | OnlyNamesInScope
  deriving (Eq)

data InterfaceConstraint = InterfaceConstraint
  { name :: String,
    types :: Ty
  }
  deriving (Show)

data TypeField
  = StructField String Ty
  | SumField String [Ty]
  deriving (Eq, Show)

data TypeMode
  = Struct
  | Sum
  deriving (Eq, Show)

data TypeCandidate = TypeCandidate
  { typeName :: String,
    variables :: [Ty],
    members :: [TypeField],
    restriction :: TypeVarRestriction,
    constraints :: [InterfaceConstraint],
    typeEnv :: TypeEnv,
    valueEnv :: Env,
    mode :: TypeMode,
    path :: [String]
  }

--------------------------------------------------------------------------------
-- Private

-- | Set the member fields of a type candidate.
setMembers :: TypeCandidate -> [TypeField] -> TypeCandidate
setMembers candidate fields = candidate {members = fields}

-- | Given a pair of XObjs, construct a struct (product type) field.
mkStructField :: (XObj, XObj) -> Either TypeError TypeField
mkStructField ((XObj (Sym (SymPath [] fname) _) _ _), tx) =
  maybe (Left (NotAType tx)) (Right . StructField fname) (xobjToTy tx)
mkStructField (x, _) = Left (InvalidStructField x)

-- | Given an XObj, construct a sum type field.
mkSumField :: XObj -> Either TypeError TypeField
mkSumField x@(XObj (Lst [XObj (Sym (SymPath [] fname) Symbol) _ _, XObj (Arr txs) _ _]) _ _) =
  maybe (Left (InvalidSumtypeCase x)) (Right . SumField fname) (mapM xobjToTy txs)
mkSumField (XObj (Sym (SymPath [] fname) Symbol) _ _) = Right (SumField fname [])
mkSumField x = Left (InvalidSumtypeCase x)

--------------------------------------------------------------------------------
-- Public

-- | Returns the fields of a type candidate
getFields :: TypeCandidate -> [TypeField]
getFields = members

getName :: TypeCandidate -> String
getName = typeName

getVariables :: TypeCandidate -> [Ty]
getVariables = variables

getRestriction :: TypeCandidate -> TypeVarRestriction
getRestriction = restriction

setRestriction :: TypeCandidate -> TypeVarRestriction -> TypeCandidate
setRestriction candidate restrict = candidate {restriction = restrict}

getTypeEnv :: TypeCandidate -> TypeEnv
getTypeEnv = typeEnv

getValueEnv :: TypeCandidate -> Env
getValueEnv = valueEnv

getConstraints :: TypeCandidate -> [InterfaceConstraint]
getConstraints = constraints

getMode :: TypeCandidate -> TypeMode
getMode = mode

getPath :: TypeCandidate -> [String]
getPath = path

getFullPath :: TypeCandidate -> [String]
getFullPath candidate = TypeCandidate.getPath candidate ++ [TypeCandidate.getName candidate]

-- | Returns the name of a type field.
fieldName :: TypeField -> String
fieldName (StructField n _) = n
fieldName (SumField n _) = n

-- | Returns the types of a type field.
fieldTypes :: TypeField -> [Ty]
fieldTypes (StructField _ ty) = [ty]
fieldTypes (SumField _ ts) = ts

-- | Creates a struct type candidate.
mkStructCandidate :: String -> [Ty] -> TypeEnv -> Env -> [XObj] -> [String] -> Either TypeError TypeCandidate
mkStructCandidate tname vars tenv env memberxs ps =
  let typedMembers = mapM mkStructField (pairwise memberxs)
      candidate =
        TypeCandidate
          { typeName = tname,
            variables = vars,
            members = [],
            restriction = OnlyNamesInScope,
            constraints = [],
            typeEnv = tenv,
            valueEnv = env,
            mode = Struct,
            path = ps
          }
   in if even (length memberxs)
        then fmap (setMembers candidate) typedMembers
        else Left (UnevenMembers memberxs)

-- | Creates a sum type candidate.
mkSumtypeCandidate :: String -> [Ty] -> TypeEnv -> Env -> [XObj] -> [String] -> Either TypeError TypeCandidate
mkSumtypeCandidate tname vars tenv env memberxs ps =
  let typedMembers = mapM mkSumField memberxs
      candidate =
        TypeCandidate
          { typeName = tname,
            variables = vars,
            members = [],
            restriction = OnlyNamesInScope,
            constraints = [],
            typeEnv = tenv,
            valueEnv = env,
            mode = Sum,
            path = ps
          }
   in fmap (setMembers candidate) typedMembers

toType :: TypeCandidate -> Ty
toType candidate =
  StructTy (ConcreteNameTy (SymPath (TypeCandidate.getPath candidate) (TypeCandidate.getName candidate))) (getVariables candidate)

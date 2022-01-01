module TypePredicates where

import Types

isTypeGeneric :: Ty -> Bool
isTypeGeneric (VarTy _) = True
isTypeGeneric (FuncTy argTys retTy _) = any isTypeGeneric argTys || isTypeGeneric retTy
isTypeGeneric (StructTy n tyArgs) = isTypeGeneric n || any isTypeGeneric tyArgs
isTypeGeneric (PointerTy p) = isTypeGeneric p
isTypeGeneric (RefTy r _) = isTypeGeneric r
isTypeGeneric _ = False

isFullyGenericType :: Ty -> Bool
isFullyGenericType (VarTy _) = True
isFullyGenericType (StructTy name members) = isFullyGenericType name && all isFullyGenericType members
isFullyGenericType _ = False

isUnit :: Ty -> Bool
isUnit UnitTy = True
isUnit (RefTy UnitTy _) = True
isUnit _ = False

-- | Is this type a function type?
isFunctionType :: Ty -> Bool
isFunctionType FuncTy {} = True
isFunctionType _ = False

-- | Is this type a ref to a function type?
isRefToFunctionType :: Ty -> Bool
isRefToFunctionType (RefTy FuncTy {} _) = True
isRefToFunctionType _ = False

-- | Is this type a struct type?
isStructType :: Ty -> Bool
isStructType (StructTy _ _) = True
isStructType _ = False

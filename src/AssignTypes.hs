module AssignTypes where

import Types
import Obj
import Util
import TypeError
import Data.List (nub)
import qualified Data.Map as Map

import Debug.Trace

{-# ANN assignTypes "HLint: ignore Eta reduce" #-}
-- | Walk the whole expression tree and replace all occurences of VarTy with their corresponding actual type.
assignTypes :: TypeMappings -> XObj -> Either TypeError XObj
assignTypes mappings root = visit root
  where
    visit xobj =
      case obj xobj of
        (Lst _) -> visitList xobj
        (Arr _) -> visitArray xobj
        _ -> assignType xobj

    visitList :: XObj -> Either TypeError XObj
    visitList (XObj (Lst xobjs) i t) =
      do visited <- mapM (assignTypes mappings) xobjs
         let xobj' = XObj (Lst visited) i t
         assignType xobj'
    visitList _ = error "The function 'visitList' only accepts XObjs with lists in them."

    visitArray :: XObj -> Either TypeError XObj
    visitArray (XObj (Arr xobjs) i t) =
      do visited <- mapM (assignTypes mappings) xobjs
         let xobj' = XObj (Arr visited) i t
         assignType xobj'
    visitArray _ = error "The function 'visitArray' only accepts XObjs with arrays in them."

    assignType :: XObj -> Either TypeError XObj
    assignType xobj = case ty xobj of
      Just startingType ->
        let finalType = replaceTyVars mappings startingType
        in  if isArrayTypeOK finalType
            then Right (xobj { ty = Just finalType })
            else Left  (ArraysCannotContainRefs xobj)
      Nothing -> return xobj


isArrayTypeOK :: Ty -> Bool
isArrayTypeOK (StructTy "Array" [RefTy _ _]) = False -- An array containing refs!
isArrayTypeOK _ = True


-- | Change auto generated type names (i.e. 't0') to letters (i.e. 'a', 'b', 'c', etc...)
-- | TODO: Only change variables that are machine generated.
beautifyTypeVariables :: XObj -> Either TypeError XObj
beautifyTypeVariables root =
  let Just t = ty root
      tys = nub (typeVariablesInOrderOfAppearance t)
      mappings = Map.fromList (zip (map (\(VarTy name) -> name) tys)
                                   (map (VarTy . (:[])) ['a'..]))
  in  assignTypes mappings root

typeVariablesInOrderOfAppearance :: Ty -> [Ty]
typeVariablesInOrderOfAppearance (FuncTy ltTy argTys retTy) =
  typeVariablesInOrderOfAppearance ltTy ++ concatMap typeVariablesInOrderOfAppearance argTys ++ typeVariablesInOrderOfAppearance retTy
typeVariablesInOrderOfAppearance (StructTy _ typeArgs) =
  concatMap typeVariablesInOrderOfAppearance typeArgs
typeVariablesInOrderOfAppearance (RefTy innerTy lifetimeTy) =
  typeVariablesInOrderOfAppearance innerTy ++ typeVariablesInOrderOfAppearance lifetimeTy
typeVariablesInOrderOfAppearance (PointerTy innerTy) =
  typeVariablesInOrderOfAppearance innerTy
typeVariablesInOrderOfAppearance t@(VarTy _) =
  [t]
typeVariablesInOrderOfAppearance _ =
  []

module AssignTypes where

import Data.List (nub)
import qualified Map
import Obj
import TypeError
import Types

{-# ANN assignTypes "HLint: ignore Eta reduce" #-}

-- | Walk the whole expression tree and replace all occurences of VarTy with their corresponding actual type.
assignTypes :: TypeMappings -> XObj -> Either TypeError XObj
assignTypes mappings root = visit root
  where
    visit xobj =
      case xobjObj xobj of
        (Lst _) -> visitList xobj
        (Arr _) -> visitArray xobj
        (StaticArr _) -> visitStaticArray xobj
        _ -> assignType xobj
    visitList :: XObj -> Either TypeError XObj
    visitList (XObj (Lst xobjs) i t) =
      do
        visited <- mapM (assignTypes mappings) xobjs
        let xobj' = XObj (Lst visited) i t
        assignType xobj'
    visitList _ = error "The function 'visitList' only accepts XObjs with lists in them."
    visitArray :: XObj -> Either TypeError XObj
    visitArray (XObj (Arr xobjs) i t) =
      do
        visited <- mapM (assignTypes mappings) xobjs
        let xobj' = XObj (Arr visited) i t
        assignType xobj'
    visitArray _ = error "The function 'visitArray' only accepts XObjs with arrays in them."
    visitStaticArray :: XObj -> Either TypeError XObj
    visitStaticArray (XObj (StaticArr xobjs) i t) =
      do
        visited <- mapM (assignTypes mappings) xobjs
        let xobj' = XObj (StaticArr visited) i t
        assignType xobj'
    visitStaticArray _ = error "The function 'visitStaticArray' only accepts XObjs with arrays in them."
    assignType :: XObj -> Either TypeError XObj
    assignType xobj = case xobjTy xobj of
      Just startingType ->
        let finalType = replaceTyVars mappings startingType
         in if isArrayTypeOK finalType
              then Right (xobj {xobjTy = Just finalType})
              else Left (ArraysCannotContainRefs xobj)
      Nothing -> pure xobj

isArrayTypeOK :: Ty -> Bool
isArrayTypeOK (StructTy (ConcreteNameTy (SymPath [] "Array")) [RefTy _ _]) = False -- An array containing refs!
isArrayTypeOK _ = True

-- | Change auto generated type names (i.e. 't0') to letters (i.e. 'a', 'b', 'c', etc...)
-- | TODO: Only change variables that are machine generated.
beautifyTypeVariables :: XObj -> Either TypeError XObj
beautifyTypeVariables root =
  let Just t = xobjTy root
      tys = nub (typeVariablesInOrderOfAppearance t)
      mappings =
        Map.fromList
          ( zip
              (map (\(VarTy name) -> name) tys)
              (map (VarTy . (: [])) ['a' ..])
          )
   in assignTypes mappings root

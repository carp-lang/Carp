-- | Module AssignTypes defines routines for replacing type variables with
-- concrete types.
module AssignTypes
  ( assignTypes,
    beautifyTypeVariables,
  )
where

import Data.List (nub)
import Forms
import qualified Map
import Obj
import TypeError
import Types

--------------------------------------------------------------------------------
-- Public functions

{-# ANN assignTypes "HLint: ignore Eta reduce" #-}

-- | Walk the whole expression tree and replace all occurrences of VarTy with
-- their corresponding actual type.
assignTypes :: TypeMappings -> XObj -> Either TypeError XObj
assignTypes mappings x@(ListPat xs) =
  do
    visited <- mapM (assignTypes mappings) xs
    let xobj' = XObj (Lst visited) (xobjInfo x) (xobjTy x)
    assignType mappings xobj'
assignTypes mappings x@(ArrPat xs) =
  do
    visited <- mapM (assignTypes mappings) xs
    let xobj' = XObj (Arr visited) (xobjInfo x) (xobjTy x)
    assignType mappings xobj'
assignTypes mappings x@(StaticArrPat xs) =
  do
    visited <- mapM (assignTypes mappings) xs
    let xobj' = XObj (StaticArr visited) (xobjInfo x) (xobjTy x)
    assignType mappings xobj'
assignTypes mappings xobj = assignType mappings xobj

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

--------------------------------------------------------------------------------
-- Private functions

-- | Replace a type variable with a concrete type, ensuring refs aren't passed
-- as members of arrays.
assignType :: TypeMappings -> XObj -> Either TypeError XObj
assignType mappings xobj =
  case xobjTy xobj of
    Just startingType ->
      let finalType = replaceTyVars mappings startingType
       in if isArrayTypeOK finalType
            then Right (xobj {xobjTy = Just finalType})
            else Left (ArraysCannotContainRefs xobj)
    Nothing -> pure xobj

-- | Returns false if an array contains a Ref type as a member.
isArrayTypeOK :: Ty -> Bool
isArrayTypeOK (StructTy (ConcreteNameTy (SymPath [] "Array")) [RefTy _ _]) =
  -- An array containing refs!
  False
isArrayTypeOK _ = True

module AssignTypes where

import Types
import Obj
import Util
import TypeError

-- | Walk the whole expression tree and replace all occurences of VarTy with their corresponding actual type.
assignTypes :: TypeMappings -> XObj -> Either TypeError XObj
assignTypes mappings = visit
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
    visitList _ = compilerError "The function 'visitList' only accepts XObjs with lists in them."

    visitArray :: XObj -> Either TypeError XObj
    visitArray (XObj (Arr xobjs) i t) =
      do visited <- mapM (assignTypes mappings) xobjs
         let xobj' = XObj (Arr visited) i t
         assignType xobj'
    visitArray _ = compilerError "The function 'visitArray' only accepts XObjs with arrays in them."

    assignType :: XObj -> Either TypeError XObj
    assignType xobj = case ty xobj of
      Just startingType ->
        let finalType = replaceTyVars mappings startingType
        in  if isArrayTypeOK finalType
            then Right (xobj { ty = Just finalType })
            else Left  (ArraysCannotContainRefs xobj)
      Nothing -> return xobj


isArrayTypeOK :: Ty -> Bool
isArrayTypeOK (StructTy "Array" [RefTy _]) = False -- An array containing refs!
isArrayTypeOK _ = True

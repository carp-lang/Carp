module AssignTypes where

import Types
import Obj
import Util
import TypeError

-- | Walk the whole expression tree and replace all occurences of VarTy with their corresponding actual type.
assignTypes :: TypeMappings -> XObj -> XObj
assignTypes mappings root = visit root
  where
    visit xobj =
      case obj xobj of
        (Lst _) -> visitList xobj
        (Arr _) -> visitArray xobj
        _ -> assignType xobj

    visitList (XObj (Lst xobjs) i t) =
      let visited = map (assignTypes mappings) xobjs
          xobj' = XObj (Lst visited) i t
      in  assignType xobj'
    visitList _ = compilerError "The function 'visitList' only accepts XObjs with lists in them."

    visitArray (XObj (Arr xobjs) i t) =
      let visited = map (assignTypes mappings) xobjs
          xobj' = XObj (Arr visited) i t
      in  assignType xobj'
    visitArray _ = compilerError "The function 'visitArray' only accepts XObjs with arrays in them."

    assignType :: XObj -> XObj
    assignType xobj = case ty xobj of
      Just startingType -> xobj { ty = Just (replaceTyVars mappings startingType) }
      Nothing -> xobj

module SumtypeCase where

import Obj
import TypeError
import Types
import Validate

data SumtypeCase = SumtypeCase
  { caseName :: String,
    caseTys :: [Ty]
  }
  deriving (Show, Eq)

toCases :: TypeEnv -> TypeVarRestriction -> [Ty] -> [XObj] -> Either TypeError [SumtypeCase]
toCases typeEnv restriction typeVars = mapM (toCase typeEnv restriction typeVars)

toCase :: TypeEnv -> TypeVarRestriction -> [Ty] -> XObj -> Either TypeError SumtypeCase
toCase typeEnv restriction typeVars x@(XObj (Lst [XObj (Sym (SymPath [] name) Symbol) _ _, XObj (Arr tyXObjs) _ _]) _ _) =
  let tys = map xobjToTy tyXObjs
   in case sequence tys of
        Nothing ->
          Left (InvalidSumtypeCase x)
        Just okTys ->
          let validated = map (\t -> canBeUsedAsMemberType restriction typeEnv typeVars t x) okTys
           in case sequence validated of
                Left e ->
                  Left e
                Right _ ->
                  Right $
                    SumtypeCase
                      { caseName = name,
                        caseTys = okTys
                      }
toCase _ _ _ (XObj (Sym (SymPath [] name) Symbol) _ _) =
  Right $
    SumtypeCase
      { caseName = name,
        caseTys = []
      }
toCase _ _ _ x =
  Left (InvalidSumtypeCase x)

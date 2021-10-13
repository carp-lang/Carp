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

toCases :: TypeEnv -> Env -> TypeCandidate -> Either TypeError [SumtypeCase]
toCases typeEnv globalEnv candidate = mapM (toCase (typename candidate) typeEnv globalEnv (restriction candidate) (variables candidate)) (typemembers candidate)

toCase :: String -> TypeEnv -> Env -> TypeVarRestriction -> [Ty] -> XObj -> Either TypeError SumtypeCase
toCase tyname typeEnv globalEnv varrestriction typeVars x@(XObj (Lst [XObj (Sym (SymPath [] pname) Symbol) _ _, XObj (Arr tyXObjs) _ _]) _ _) =
  let tys = map xobjToTy tyXObjs
   in case sequence tys of
        Nothing ->
          Left (InvalidSumtypeCase x)
        Just okTys ->
          let validated = map (\t -> canBeUsedAsMemberType tyname varrestriction typeEnv globalEnv typeVars t x) okTys
           in case sequence validated of
                Left e ->
                  Left e
                Right _ ->
                  Right $
                    SumtypeCase
                      { caseName = pname,
                        caseTys = okTys
                      }
toCase _ _ _ _ _ (XObj (Sym (SymPath [] pname) Symbol) _ _) =
  Right $
    SumtypeCase
      { caseName = pname,
        caseTys = []
      }
toCase _ _ _ _ _ x =
  Left (InvalidSumtypeCase x)

module SumtypeCase where

import Obj
import TypeError
import Types
import Validate
import TypeCandidate

data SumtypeCase = SumtypeCase
  { caseName :: String,
    caseTys :: [Ty]
  }
  deriving (Show, Eq)

toCases :: TypeEnv -> Env -> TypeCandidate -> Either TypeError [SumtypeCase]
toCases typeEnv globalEnv candidate = mapM (toCase (typename candidate) typeEnv globalEnv (restriction candidate) (variables candidate)) (typemembers candidate)

toCase :: String -> TypeEnv -> Env -> TypeVarRestriction -> [Ty] -> (String, [Ty]) -> Either TypeError SumtypeCase
toCase tyname typeEnv globalEnv varrestriction typeVars member =
  let validated = mapM (\t -> canBeUsedAsMemberType tyname varrestriction typeEnv globalEnv typeVars t) (snd member)
   in case validated of
        Left e -> Left e
        Right _ ->
          Right $
            SumtypeCase
              { caseName = fst member,
                caseTys = snd member
              }

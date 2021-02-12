module Infer
  ( annotate,
    initialTypes,
    genConstraints,
    assignTypes,
    concretizeXObj,
    manageMemory,
    depsOfPolymorphicFunction,
  )
where

import AssignTypes
import Concretize
import Constraints
import GenerateConstraints
import InitialTypes
import Obj
import Qualify
import TypeError
import Types

-- | Performs all the steps of creating initial types, solving constraints and assigning the types.
-- | Returns a list of all the bindings that need to be added for the new form to work.
-- | The concretization of MultiSym:s (= ambiguous use of symbols, resolved by type usage)
-- | makes it possible to solve more types so let's do it several times.
annotate :: TypeEnv -> Env -> Qualified -> Maybe (Ty, XObj) -> Either TypeError (XObj, [XObj])
annotate typeEnv globalEnv qualifiedXObj rootSig =
  do
    initiated <- initialTypes typeEnv globalEnv (unQualified qualifiedXObj)
    (annotated, dependencies) <- annotateUntilDone typeEnv globalEnv initiated rootSig [] 100
    (final, deleteDeps) <- manageMemory typeEnv globalEnv annotated
    finalWithNiceTypes <- beautifyTypeVariables final
    pure (finalWithNiceTypes, dependencies ++ deleteDeps)

-- | Call the 'annotateOne' function until nothing changes
annotateUntilDone :: TypeEnv -> Env -> XObj -> Maybe (Ty, XObj) -> [XObj] -> Int -> Either TypeError (XObj, [XObj])
annotateUntilDone typeEnv globalEnv xobj rootSig deps limiter =
  if limiter <= 0
    then Left (TooManyAnnotateCalls xobj)
    else do
      (xobj', deps') <- annotateOne typeEnv globalEnv xobj rootSig True
      let newDeps = deps ++ deps'
      if xobj == xobj' -- Is it the same?
        then pure (xobj', newDeps)
        else annotateUntilDone typeEnv globalEnv xobj' rootSig newDeps (limiter - 1)

-- | Performs ONE step of annotation. The 'annotate' function will call this function several times.
-- | TODO: Remove the allowAmbiguity flag?
annotateOne :: TypeEnv -> Env -> XObj -> Maybe (Ty, XObj) -> Bool -> Either TypeError (XObj, [XObj])
annotateOne typeEnv env xobj rootSig allowAmbiguity = do
  constraints <- genConstraints env xobj rootSig
  mappings <- solveConstraintsAndConvertErrorIfNeeded constraints -- (trace ("Constraints for '" ++ getName xobj ++ "':\n" ++ joinLines (map show constraints)) constraints
  typed <- assignTypes mappings xobj -- (trace ("Mappings for '" ++ getName xobj ++ ": " ++ show mappings) mappings) xobj
  concretizeXObj allowAmbiguity typeEnv env [] typed

-- | Convert from the type 'UnificationFailure' to 'TypeError' (enables monadic chaining of Either).
solveConstraintsAndConvertErrorIfNeeded :: [Constraint] -> Either TypeError TypeMappings
solveConstraintsAndConvertErrorIfNeeded constraints =
  case solve constraints of
    Left failure@(UnificationFailure _ _) ->
      Left
        ( UnificationFailed
            (unificationFailure failure)
            (unificationMappings failure)
            constraints
        )
    Left (Holes holes) -> Left (HolesFound holes)
    Right ok -> Right ok

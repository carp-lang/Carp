module Infer (annotate
             ,initialTypes
             ,genConstraints
             ,assignTypes
             ,concretizeXObj
             ,manageMemory
             ,depsOfPolymorphicFunction
             ) where

import Control.Monad.State
import Control.Monad (replicateM)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (foldl', sort)
import Data.Maybe (mapMaybe)
import Debug.Trace

import Obj
import Constraints
import Types
import Util
import TypeError
import InitialTypes
import AssignTypes
import GenerateConstraints
import Concretize

-- | Performs all the steps of creating initial types, solving constraints and assigning the types.
-- | Returns a list of all the bindings that need to be added for the new form to work.
-- | The concretization of MultiSym:s (= ambiguous use of symbols, resolved by type usage)
-- | makes it possible to solve more types so let's do it several times.
annotate :: TypeEnv -> Env -> XObj -> Either TypeError (XObj, [XObj])
annotate typeEnv globalEnv xobj =
  do initiated <- initialTypes typeEnv globalEnv xobj
     (annotated, dependencies) <- annotateUntilDone typeEnv globalEnv initiated [] 100
     (final, deleteDeps) <- manageMemory typeEnv globalEnv annotated
     finalWithNiceTypes <- beautifyTypeVariables final
     return (finalWithNiceTypes, dependencies ++ deleteDeps)

-- | Call the 'annotateOne' function until nothing changes
annotateUntilDone :: TypeEnv -> Env -> XObj -> [XObj] -> Int -> Either TypeError (XObj, [XObj])
annotateUntilDone typeEnv globalEnv xobj deps limiter =
  if limiter <= 0
  then Left (TooManyAnnotateCalls xobj)
  else do (xobj', deps') <- annotateOne typeEnv globalEnv xobj True
          let newDeps = deps ++ deps'
          if xobj == xobj' -- Is it the same?
            then return (xobj', newDeps)
            else annotateUntilDone typeEnv globalEnv xobj' newDeps (limiter - 1)

-- | Performs ONE step of annotation. The 'annotate' function will call this function several times.
-- | TODO: Remove the allowAmbiguity flag?
annotateOne :: TypeEnv -> Env -> XObj -> Bool -> Either TypeError (XObj, [XObj])
annotateOne typeEnv env xobj allowAmbiguity = do
  constraints <- genConstraints typeEnv xobj
  mappings <- solveConstraintsAndConvertErrorIfNeeded constraints -- (trace ("Constraints for '" ++ getName xobj ++ "':\n" ++ joinWith "\n" (map show constraints)) constraints)
  typed <- assignTypes mappings xobj -- (trace ("Mappings for '" ++ getName xobj ++ ": " ++ show mappings) mappings) xobj
  concretizeXObj allowAmbiguity typeEnv env [] typed

-- | Convert from the type 'UnificationFailure' to 'TypeError' (enables monadic chaining of Either).
solveConstraintsAndConvertErrorIfNeeded :: [Constraint] -> Either TypeError TypeMappings
solveConstraintsAndConvertErrorIfNeeded constraints =
  case solve constraints of
    Left failure@(UnificationFailure _ _) -> Left (UnificationFailed
                                                   (unificationFailure failure)
                                                   (unificationMappings failure)
                                                   constraints)
    Left (Holes holes) -> Left (HolesFound holes)
    Right ok -> Right ok

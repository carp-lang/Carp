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
import ManageMemory
import Concretize

-- | Performs all the steps of creating initial types, solving constraints and assigning the types.
-- | Returns a list of all the bindings that need to be added for the new form to work.
-- | The concretization of MultiSym:s (= ambiguous use of symbols, resolved by type usage)
-- | makes it possible to solve more types so let's do it several times.
annotate :: TypeEnv -> Env -> XObj -> Either TypeError [XObj]
annotate typeEnv globalEnv xobj =
  do initiated <- initialTypes typeEnv globalEnv xobj
     (annotated, dependencies) <- foldM (\(x, deps) allowAmbiguity ->
                                           do (x', deps') <- annotateOne typeEnv globalEnv x allowAmbiguity
                                              return (x', deps ++ deps'))
                                  (initiated, [])
                                  [True, True]
     (final, deleteDeps) <- manageMemory typeEnv globalEnv annotated
     return (final : dependencies ++ deleteDeps)

-- | Performs ONE step of annotation. The 'annotate' function will call this function several times.
annotateOne :: TypeEnv -> Env -> XObj -> Bool -> Either TypeError (XObj, [XObj])
annotateOne typeEnv env xobj allowAmbiguity = do
  constraints <- genConstraints xobj
  mappings <- solveConstraintsAndConvertErrorIfNeeded constraints --(trace ("CONSTRAINTS:\n" ++ joinWith "\n" (map show constraints)) constraints)
  typed <- assignTypes mappings xobj
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

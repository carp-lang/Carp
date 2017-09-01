module Infer (annotate
             ,initialTypes
             ,genConstraints
             ,assignTypes
             ,concretizeXObj
             ,concretizeDefinition
             ,manageMemory
             ,depsOfPolymorphicFunction
             ,insideArrayDeleteDeps
             ,insideArrayCopyDeps
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
import Eval
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
annotate :: Env -> Env -> XObj -> Either TypeError [XObj]
annotate typeEnv globalEnv xobj =
  do initiated <- initialTypes globalEnv xobj
     (annotated, dependencies) <- foldM (\(x, deps) allowAmbiguity ->
                                           do (x', deps') <- annotateOne typeEnv globalEnv x allowAmbiguity
                                              return (x', deps ++ deps'))
                                  (initiated, [])
                                  [True, False]
     final <- manageMemory typeEnv globalEnv annotated
     check final
     _ <- mapM check dependencies
     return (final : dependencies)

-- | Performs ONE step of annotation. The 'annotate' function will call this function several times.
annotateOne :: Env -> Env -> XObj -> Bool -> Either TypeError (XObj, [XObj])
annotateOne typeEnv env xobj allowAmbiguity = do
  constraints <- genConstraints xobj
  mappings <- solveConstraintsAndConvertErrorIfNeeded constraints
  let typed = assignTypes mappings xobj
  concretizeXObj allowAmbiguity typeEnv env typed
       
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

-- | Will make sure that a form doesn't return a reference.
-- | TODO: This check is needed on nested forms like 'let' statements, etc.
check :: XObj -> Either TypeError ()
check xobj@(XObj (Lst (XObj Defn _ _ : _)) _ t) =
  case t of
    Just (FuncTy _ (RefTy _)) -> Left (CantReturnRefTy xobj)
    Just _ -> return ()
    Nothing -> Left (DefnMissingType xobj)
check _ = return ()
  

module EvalTypes
  ( LookupPreference (..),
    EvalExecMode (..),
    EvalPhase (..),
  )
where

import qualified Map
import Obj
import qualified Set

data EvalExecMode
  = ExecFunction
  | ExecDynamic
  | ExecMacro
  deriving (Show, Eq)

data EvalPhase
  = PhaseExecute
  | PhaseExpand
  deriving (Show, Eq)

data LookupPreference
  = PreferDynamic
  | PreferGlobal
  | PreferLocal (Set.Set String) (Map.Map Int XObj) (Map.Map String Int) EvalExecMode
  deriving (Show)

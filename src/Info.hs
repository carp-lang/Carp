-- | Module Info defines data types and functions for reporting details about
-- the Carp forms in a source file.
module Info
  ( Info (..),
    Deleter (..),
    FilePathPrintLength (..),
    dummyInfo,
    getInfo,
    prettyInfo,
    freshVar,
    machineReadableInfo,
    makeTypeVariableNameFromInfo,
  )
where

import Path (takeFileName)
import qualified Set
import SymPath

-- | Information about where the Obj originated from.
data Info = Info
  { infoLine :: Int,
    infoColumn :: Int,
    infoFile :: String,
    infoDelete :: Set.Set Deleter,
    infoIdentifier :: Int
  }
  deriving (Show, Eq, Ord)

-- TODO: The name 'deleter' for these things are really confusing!

-- | Designates the deleter function for a Carp object.
data Deleter
  = ProperDeleter
      { deleterPath :: SymPath,
        deleterVariable :: String
      }
  | -- used for external types with no delete function
    FakeDeleter
      { deleterVariable :: String
      }
  | -- used by primitive types (i.e. Int) to signify that the variable is alive
    PrimDeleter
      { aliveVariable :: String
      }
  | RefDeleter
      { refVariable :: String
      }
  deriving (Eq, Ord)

instance Show Deleter where
  show (ProperDeleter path var) = "(ProperDel " ++ show path ++ " " ++ show var ++ ")"
  show (FakeDeleter var) = "(FakeDel " ++ show var ++ ")"
  show (PrimDeleter var) = "(PrimDel " ++ show var ++ ")"
  show (RefDeleter var) = "(RefDel " ++ show var ++ ")"

-- | Whether or not the full path of a source file or a short path should be
-- printed.
data FilePathPrintLength
  = FullPath
  | ShortPath
  deriving (Eq)

instance Show FilePathPrintLength where
  show FullPath = "full"
  show ShortPath = "short"

-- | A "dummy" info object, used for bindings that do not have meaningful info,
-- such as compiler generated bindings.
dummyInfo :: Info
dummyInfo = Info 0 0 "dummy-file" Set.empty (-1)

-- | Returns the line number, column number, and filename associated with an
-- Info.
getInfo :: Info -> (Int, Int, String)
getInfo i = (infoLine i, infoColumn i, infoFile i)

-- | Pretty print Info. Used for REPL output
prettyInfo :: Info -> String
prettyInfo i =
  let (line, column, file) = getInfo i
   in (if line > -1 then "line " ++ show line else "unknown line") ++ ", "
        ++ (if column > -1 then "column " ++ show column else "unknown column")
        ++ " in '"
        ++ file
        ++ "'"

-- TODO: change name of this function
freshVar :: Info -> String
freshVar i = "_" ++ show (infoIdentifier i)

-- | Print Info in a machine readable format.
machineReadableInfo :: FilePathPrintLength -> Info -> String
machineReadableInfo filePathPrintLength i =
  let (line, column, file) = getInfo i
      file' = case filePathPrintLength of
        FullPath -> file
        ShortPath -> takeFileName file
   in file' ++ ":" ++ show line ++ ":" ++ show column

-- | Use an Info to generate a type variable name.
makeTypeVariableNameFromInfo :: Maybe Info -> String
makeTypeVariableNameFromInfo (Just i) =
  "tyvar-from-info-" ++ show (infoIdentifier i) ++ "_" ++ show (infoLine i) ++ "_" ++ show (infoColumn i)
makeTypeVariableNameFromInfo Nothing = error "unnamed-typevariable"

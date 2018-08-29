module Template where

import qualified Text.Parsec as Parsec
import Text.Parsec ((<|>))
import qualified Data.Set as Set
import Debug.Trace

import Util
import Types
import Obj
import Parsing
import Infer
import Concretize

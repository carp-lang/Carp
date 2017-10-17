module Workbench where

import ColorText
import Obj
import Types
import Commands
import Template
import Parsing
import Infer
import Constraints
import Emit
import qualified Data.Map as Map

-- | Not part of the cabal file, just for interactive repl sessions:

pt :: XObj -> IO ()
pt = putStrLn . prettyTyped 

f = case (parse "(Ref Int)" "") of
  Left e -> error (show e)
  Right [x] -> x

g = case (parse "&Int" "") of
  Left e -> error (show e)
  Right [x] -> x

h = case xobjToTy g of
      Just x -> x
      Nothing -> error ""

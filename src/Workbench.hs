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
import Eval
import qualified Data.Map as Map

-- | Not part of the cabal file, just for interactive repl sessions:


startingGlobalEnv :: Env
startingGlobalEnv = Env { envBindings = bs,
                          envParent = Nothing,
                          envModuleName = Nothing,
                          envUseModules = [SymPath [] "String"],
                          envMode = ExternalEnv
                        }
  where bs = Map.fromList [ templateNoop
                          --, ("Array", Binder (XObj (Mod arrayModule) Nothing Nothing))
                          , register "NULL" (VarTy "a")
                          ]

startingTypeEnv :: Env
startingTypeEnv = Env { envBindings = Map.empty, envParent = Nothing, envModuleName = Nothing, envUseModules = [], envMode = ExternalEnv }


pt :: XObj -> IO ()
pt = putStrLn . prettyTyped

Right [parsed] = parse "(defn f [x] (Int.+ x 10))" ""

p1 = pt parsed
p2 = case expandAll startingGlobalEnv parsed of
       Left e -> error (show e)
       Right ok -> pt ok

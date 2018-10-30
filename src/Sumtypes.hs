module Sumtypes where

import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace

import Obj
import Types
import Util
--import Template
--import ToTemplate
--import Infer
--import Concretize
--import Polymorphism
--import ArrayTemplates
import Lookup

moduleForSumtype :: TypeEnv -> Env -> [String] -> String -> [Ty] -> [XObj] -> Maybe Info -> Maybe Env -> Either String (String, XObj, [XObj])
moduleForSumtype typeEnv env pathStrings typeName typeVariables rest i existingEnv =
  let typeModuleName = typeName
      typeModuleEnv = case existingEnv of
                             Just env -> env
                             Nothing -> Env (Map.fromList []) (Just env) (Just typeModuleName) [] ExternalEnv 0
      --insidePath = pathStrings ++ [typeModuleName]
  in do -- TODO: validate members
        let sumTyCases = []
            sumTy = StructTy typeName typeVariables
            funcs = []
            moduleEnvWithBindings = addListOfBindings typeModuleEnv funcs
            typeModuleXObj = XObj (Mod moduleEnvWithBindings) i (Just ModuleTy)
            deps = []
        return (typeModuleName, typeModuleXObj, deps)

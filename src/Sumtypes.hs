module Sumtypes where

import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace

import Obj
import Types
import Util
--import Infer
import Concretize
--import Polymorphism
import Lookup
import Template
import ToTemplate
import Deftype

moduleForSumtype :: TypeEnv -> Env -> [String] -> String -> [Ty] -> [XObj] -> Maybe Info -> Maybe Env -> Either String (String, XObj, [XObj])
moduleForSumtype typeEnv env pathStrings typeName typeVariables rest i existingEnv =
  let typeModuleName = typeName
      typeModuleEnv = case existingEnv of
                             Just env -> env
                             Nothing -> Env (Map.fromList []) (Just env) (Just typeModuleName) [] ExternalEnv 0
      insidePath = pathStrings ++ [typeModuleName]
  in do -- TODO: validate members
        let structTy = StructTy typeName typeVariables
            cases = toCases rest
        funcs <- initers insidePath structTy cases
        let moduleEnvWithBindings = addListOfBindings typeModuleEnv funcs
            typeModuleXObj = XObj (Mod moduleEnvWithBindings) i (Just ModuleTy)
            deps = []
        return (typeModuleName, typeModuleXObj, deps)

data SumtypeCase = SumtypeCase { caseName :: String
                               , caseTys :: [Ty]
                               } deriving (Show, Eq)

toCases :: [XObj] -> [SumtypeCase]
toCases = map toCase

toCase :: XObj -> SumtypeCase
toCase (XObj (Lst [XObj (Sym (SymPath [] name) Symbol) _ _, XObj (Arr tyXObjs) _ _]) _ _) =
  SumtypeCase { caseName = name
              , caseTys = (map (fromJust . xobjToTy) tyXObjs)
              }

initers :: [String] -> Ty -> [SumtypeCase] -> Either String [(String, Binder)]
initers insidePath structTy cases = sequence (map (binderForCaseInit insidePath structTy) cases)

binderForCaseInit :: [String] -> Ty -> SumtypeCase -> Either String (String, Binder)
binderForCaseInit insidePath structTy@(StructTy typeName _) sumtypeCase =
  if isTypeGeneric structTy
  then Right (genericCaseInit StackAlloc insidePath structTy sumtypeCase)
  else Right (concreteCaseInit StackAlloc insidePath structTy sumtypeCase)

concreteCaseInit :: AllocationMode -> [String] -> Ty -> SumtypeCase -> (String, Binder)
concreteCaseInit allocationMode insidePath structTy@(StructTy typeName _) sumtypeCase =
  instanceBinder (SymPath insidePath (caseName sumtypeCase)) (FuncTy (caseTys sumtypeCase) structTy) template
  where template =
          Template
          (FuncTy (caseTys sumtypeCase) (VarTy "p"))
          (\(FuncTy _ concreteStructTy) ->
             let mappings = unifySignatures structTy concreteStructTy
                 --correctedMembers = replaceGenericTypeSymbolsOnMembers mappings membersXObjs
                 --memberPairs = memberXObjsToPairs correctedMembers
             in  (toTemplate $ "$p $NAME(" ++ joinWithComma (map memberArg (zip anonMemberNames (caseTys sumtypeCase))) ++ ")"))
          (const (tokensForCaseInit allocationMode typeName sumtypeCase))
          (\(FuncTy _ _) -> [])

genericCaseInit :: AllocationMode -> [String] -> Ty -> SumtypeCase -> (String, Binder)
genericCaseInit allocationMode pathStrings originalStructTy@(StructTy typeName typeVariables) sumtypeCase =
  defineTypeParameterizedTemplate templateCreator path t
  where path = SymPath pathStrings (caseName sumtypeCase)
        t = FuncTy (caseTys sumtypeCase) originalStructTy
        templateCreator = TemplateCreator $
          \typeEnv env ->
            Template
            (FuncTy (caseTys sumtypeCase) (VarTy "p"))
            (\(FuncTy _ concreteStructTy) ->
               (toTemplate $ "$p $NAME(" ++ joinWithComma (map memberArg (zip anonMemberNames (caseTys sumtypeCase))) ++ ")"))
            (const (tokensForCaseInit allocationMode typeName sumtypeCase))
            (\(FuncTy _ concreteStructTy) ->
               case concretizeType typeEnv concreteStructTy of
                 Left err -> error (err ++ ". This error should not crash the compiler - change return type to Either here.")
                 Right ok -> ok)


tokensForCaseInit :: AllocationMode -> String -> SumtypeCase -> [Token]
tokensForCaseInit allocationMode typeName sumtypeCase =
  toTemplate $ unlines [ "$DECL {"
                       , case allocationMode of
                           StackAlloc -> "    $p instance;"
                       --     HeapAlloc ->  "    $p instance = CARP_MALLOC(sizeof(" ++ typeName ++ "));"
                       -- , joinWith "\n" (map (caseMemberAssignment allocationMode) (memberXObjsToPairs membersXObjs))
                       , "    return instance;"
                       , "}"]

caseMemberAssignment :: AllocationMode -> (String, Ty) -> String
caseMemberAssignment allocationMode (memberName, _) = "    instance" ++ sep ++ memberName ++ " = " ++ memberName ++ ";"
  where sep = case allocationMode of
                StackAlloc -> "."
                HeapAlloc -> "->"

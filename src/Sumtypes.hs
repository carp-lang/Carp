module Sumtypes where

import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace

import Obj
import Types
import Util
--import Infer
import Concretize
import Polymorphism
import Lookup
import Template
import ToTemplate
import Deftype
import StructUtils

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
        okIniters <- initers insidePath structTy cases
        (okStr, strDeps) <- binderForStrOrPrn typeEnv env insidePath structTy cases "str"
        (okPrn, _) <- binderForStrOrPrn typeEnv env insidePath structTy cases "prn"
        (okDelete, deleteDeps) <- binderForDelete typeEnv env insidePath structTy cases
        let moduleEnvWithBindings = addListOfBindings typeModuleEnv (okIniters ++ [okStr, okPrn, okDelete])
            typeModuleXObj = XObj (Mod moduleEnvWithBindings) i (Just ModuleTy)
            deps = strDeps ++ deleteDeps
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

replaceGenericTypesOnCases :: TypeMappings -> [SumtypeCase] -> [SumtypeCase]
replaceGenericTypesOnCases mappings cases =
  map replaceOnCase cases
  where replaceOnCase theCase =
          let newTys = map (replaceTyVars mappings) (caseTys theCase)
          in  theCase { caseTys = newTys }

initers :: [String] -> Ty -> [SumtypeCase] -> Either String [(String, Binder)]
initers insidePath structTy cases = sequence (map (binderForCaseInit insidePath structTy) cases)

binderForCaseInit :: [String] -> Ty -> SumtypeCase -> Either String (String, Binder)
binderForCaseInit insidePath structTy@(StructTy typeName _) sumtypeCase =
  if isTypeGeneric structTy
  then Right (genericCaseInit StackAlloc insidePath structTy sumtypeCase)
  else Right (concreteCaseInit StackAlloc insidePath structTy sumtypeCase)

concreteCaseInit :: AllocationMode -> [String] -> Ty -> SumtypeCase -> (String, Binder)
concreteCaseInit allocationMode insidePath structTy sumtypeCase =
  instanceBinder (SymPath insidePath (caseName sumtypeCase)) (FuncTy (caseTys sumtypeCase) structTy) template
  where template =
          Template
          (FuncTy (caseTys sumtypeCase) (VarTy "p"))
          (\(FuncTy _ concreteStructTy) ->
             let mappings = unifySignatures structTy concreteStructTy
                 correctedTys = map (replaceTyVars mappings) (caseTys sumtypeCase)
             in  (toTemplate $ "$p $NAME(" ++ joinWithComma (map memberArg (zip anonMemberNames correctedTys)) ++ ")"))
          (const (tokensForCaseInit allocationMode structTy sumtypeCase))
          (\(FuncTy _ _) -> [])

genericCaseInit :: AllocationMode -> [String] -> Ty -> SumtypeCase -> (String, Binder)
genericCaseInit allocationMode pathStrings originalStructTy sumtypeCase =
  defineTypeParameterizedTemplate templateCreator path t
  where path = SymPath pathStrings (caseName sumtypeCase)
        t = FuncTy (caseTys sumtypeCase) originalStructTy
        templateCreator = TemplateCreator $
          \typeEnv env ->
            Template
            (FuncTy (caseTys sumtypeCase) (VarTy "p"))
            (\(FuncTy _ concreteStructTy) ->
               (toTemplate $ "$p $NAME(" ++ joinWithComma (map memberArg (zip anonMemberNames (caseTys sumtypeCase))) ++ ")"))
            (\(FuncTy _ concreteStructTy) ->
               (tokensForCaseInit allocationMode concreteStructTy sumtypeCase))
            (\(FuncTy _ concreteStructTy) ->
               case concretizeType typeEnv concreteStructTy of
                 Left err -> error (err ++ ". This error should not crash the compiler - change return type to Either here.")
                 Right ok -> ok)


tokensForCaseInit :: AllocationMode -> Ty -> SumtypeCase -> [Token]
tokensForCaseInit allocationMode sumTy@(StructTy typeName typeVariables) sumtypeCase =
  toTemplate $ unlines [ "$DECL {"
                       , case allocationMode of
                           StackAlloc -> "    $p instance;"
                           HeapAlloc ->  "    $p instance = CARP_MALLOC(sizeof(" ++ typeName ++ "));"
                       , joinWith "\n" (map (caseMemberAssignment allocationMode (caseName sumtypeCase))
                                        (zip anonMemberNames (caseTys sumtypeCase)))
                       , "    instance._tag = " ++ tagName sumTy (caseName sumtypeCase) ++ ";"
                       , "    return instance;"
                       , "}"]

caseMemberAssignment :: AllocationMode -> String -> (String, Ty) -> String
caseMemberAssignment allocationMode caseName (memberName, _) =
  "    instance." ++ caseName ++ sep ++ memberName ++ " = " ++ memberName ++ ";"
  where sep = case allocationMode of
                StackAlloc -> "."
                HeapAlloc -> "->"

-- | Helper function to create the binder for the 'str' template.
binderForStrOrPrn :: TypeEnv -> Env -> [String] -> Ty -> [SumtypeCase] -> String -> Either String ((String, Binder), [XObj])
binderForStrOrPrn typeEnv env insidePath structTy@(StructTy typeName _) cases strOrPrn =
  if isTypeGeneric structTy
  then Right (genericStr insidePath structTy cases strOrPrn, [])
  else Right (concreteStr typeEnv env insidePath structTy cases strOrPrn, [])

-- | The template for the 'str' function for a concrete deftype.
concreteStr :: TypeEnv -> Env -> [String] -> Ty -> [SumtypeCase] -> String -> (String, Binder)
concreteStr typeEnv env insidePath concreteStructTy@(StructTy typeName _) cases strOrPrn =
  instanceBinder (SymPath insidePath strOrPrn) (FuncTy [(RefTy concreteStructTy)] StringTy) template
  where template =
          Template
            (FuncTy [RefTy concreteStructTy] StringTy)
            (\(FuncTy [RefTy structTy] StringTy) -> (toTemplate $ "String $NAME(" ++ tyToCLambdaFix structTy ++ " *p)"))
            (\(FuncTy [RefTy structTy@(StructTy _ concreteMemberTys)] StringTy) ->
                (tokensForStr typeEnv env typeName cases concreteStructTy))
            (\(ft@(FuncTy [RefTy structTy@(StructTy _ concreteMemberTys)] StringTy)) ->
               concatMap (depsOfPolymorphicFunction typeEnv env [] "prn" . typesStrFunctionType typeEnv)
                          (filter (\t -> (not . isExternalType typeEnv) t && (not . isFullyGenericType) t) (concatMap caseTys cases))
            )

-- | The template for the 'str' function for a generic deftype.
genericStr :: [String] -> Ty -> [SumtypeCase] -> String -> (String, Binder)
genericStr insidePath originalStructTy@(StructTy typeName varTys) cases strOrPrn =
  defineTypeParameterizedTemplate templateCreator path t
  where path = SymPath insidePath strOrPrn
        t = FuncTy [(RefTy originalStructTy)] StringTy
        templateCreator = TemplateCreator $
          \typeEnv env ->
            Template
            t
            (\(FuncTy [RefTy concreteStructTy] StringTy) ->
               (toTemplate $ "String $NAME(" ++ tyToCLambdaFix concreteStructTy ++ " *p)"))
            (\(FuncTy [RefTy concreteStructTy@(StructTy _ concreteMemberTys)] StringTy) ->
               let mappings = unifySignatures originalStructTy concreteStructTy
                   correctedCases = replaceGenericTypesOnCases mappings cases
               in (tokensForStr typeEnv env typeName correctedCases concreteStructTy))
            (\(ft@(FuncTy [RefTy concreteStructTy@(StructTy _ concreteMemberTys)] StringTy)) ->
               let mappings = unifySignatures originalStructTy concreteStructTy
                   correctedCases = replaceGenericTypesOnCases mappings cases
                   tys = (filter (\t -> (not . isExternalType typeEnv) t && (not . isFullyGenericType) t) (concatMap caseTys correctedCases))
               in  concatMap (depsOfPolymorphicFunction typeEnv env [] "prn" . typesStrFunctionType typeEnv) tys
                   ++
                   (if isTypeGeneric concreteStructTy then [] else [defineFunctionTypeAlias ft]))

tokensForStr :: TypeEnv -> Env -> String -> [SumtypeCase] -> Ty -> [Token]
tokensForStr typeEnv env typeName cases concreteStructTy  =
  (toTemplate $ unlines [ "$DECL {"
                        , "  // convert members to String here:"
                        , "  String temp = NULL;"
                        , "  int tempsize = 0;"
                        , "  (void)tempsize; // that way we remove the occasional unused warning "
                        , calculateStructStrSize typeEnv env cases concreteStructTy
                        , "  String buffer = CARP_MALLOC(size);"
                        , "  String bufferPtr = buffer;"
                        , ""
                        , (concatMap (strCase typeEnv env concreteStructTy) cases)
                        , "  return buffer;"
                        , "}"])

strCase :: TypeEnv -> Env -> Ty -> SumtypeCase -> String
strCase typeEnv env concreteStructTy theCase =
  let name = caseName theCase
      tys  = caseTys  theCase
  in unlines $
     [ "  if(p->_tag == " ++ (tagName concreteStructTy name) ++ ") {"
     , "    snprintf(bufferPtr, size, \"(%s \", \"" ++ name ++ "\");"
     , "    bufferPtr += strlen(\"" ++ name ++ "\") + 2;\n"
     , joinWith "\n" (map (memberPrn typeEnv env) (zip (map (\anon -> name ++ "." ++ anon) anonMemberNames) tys))
     , "    bufferPtr--;"
     , "    snprintf(bufferPtr, size, \")\");"
     , "  }"
     ]

-- | Figure out how big the string needed for the string representation of the struct has to be.
calculateStructStrSize :: TypeEnv -> Env -> [SumtypeCase] -> Ty -> String
calculateStructStrSize typeEnv env cases structTy@(StructTy name _) =
  "  int size = 0;\n" ++
  concatMap (strSizeCase typeEnv env structTy) cases

strSizeCase :: TypeEnv -> Env -> Ty -> SumtypeCase -> String
strSizeCase typeEnv env concreteStructTy theCase =
  let name = caseName theCase
      tys  = caseTys  theCase
  in unlines $
     [ "  if(p->_tag == " ++ (tagName concreteStructTy name) ++ ") {"
     , "    size = snprintf(NULL, size, \"(%s \", \"" ++ name ++ "\");"
     , joinWith "\n" (map (memberPrnSize typeEnv env) (zip (map (\anon -> name ++ "." ++ anon) anonMemberNames) tys))
     , "  }"
     ]

-- | Helper function to create the binder for the 'delete' template.
binderForDelete :: TypeEnv -> Env -> [String] -> Ty -> [SumtypeCase] -> Either String ((String, Binder), [XObj])
binderForDelete typeEnv env insidePath structTy@(StructTy typeName _) cases =
  if isTypeGeneric structTy
  then Right (genericSumtypeDelete insidePath structTy cases, [])
  else Right (concreteSumtypeDelete insidePath typeEnv env structTy cases, [])

  --then Right (genericStr insidePath structTy cases strOrPrn, [])
 -- else Right (concreteStr typeEnv env insidePath structTy cases strOrPrn, [])

-- | The template for the 'delete' function of a generic sumtype.
genericSumtypeDelete :: [String] -> Ty -> [SumtypeCase] -> (String, Binder)
genericSumtypeDelete pathStrings originalStructTy cases =
  defineTypeParameterizedTemplate templateCreator path (FuncTy [originalStructTy] UnitTy)
  where path = SymPath pathStrings "delete"
        t = (FuncTy [VarTy "p"] UnitTy)
        templateCreator = TemplateCreator $
          \typeEnv env ->
            Template
            t
            (const (toTemplate "void $NAME($p p)"))
            (\(FuncTy [concreteStructTy] UnitTy) ->
               let mappings = unifySignatures originalStructTy concreteStructTy
                   correctedCases = replaceGenericTypesOnCases mappings cases
               in  (toTemplate $ unlines [ "$DECL {"
                                         , concatMap (deleteCase typeEnv env concreteStructTy) (zip correctedCases (True : repeat False))
                                         , "}"]))
            (\(FuncTy [concreteStructTy] UnitTy) ->
               let mappings = unifySignatures originalStructTy concreteStructTy
                   correctedCases = replaceGenericTypesOnCases mappings cases
               in  if isTypeGeneric concreteStructTy
                   then []
                   else concatMap (depsOfPolymorphicFunction typeEnv env [] "delete" . typesDeleterFunctionType)
                                  (filter (isManaged typeEnv) (concatMap caseTys correctedCases)))

-- | The template for the 'delete' function of a concrete sumtype
concreteSumtypeDelete :: [String] -> TypeEnv -> Env -> Ty -> [SumtypeCase] -> (String, Binder)
concreteSumtypeDelete insidePath typeEnv env structTy cases =
  instanceBinder (SymPath insidePath "delete") (FuncTy [structTy] UnitTy) template
  where template = Template
                    (FuncTy [VarTy "p"] UnitTy)
                    (const (toTemplate "void $NAME($p p)"))
                    (const (toTemplate $ unlines [ "$DECL {"
                                                 , concatMap (deleteCase typeEnv env structTy) (zip cases (True : repeat False))
                                                 , "}"]))
                    (\_ -> concatMap (depsOfPolymorphicFunction typeEnv env [] "delete" . typesDeleterFunctionType)
                                     (filter (isManaged typeEnv) (concatMap caseTys cases)))

deleteCase :: TypeEnv -> Env -> Ty -> (SumtypeCase, Bool) -> String
deleteCase typeEnv env concreteStructTy (theCase, isFirstCase) =
  let name = caseName theCase
      tys  = caseTys  theCase
  in unlines $
     [ "  " ++ (if isFirstCase then "" else "else ") ++ "if(p._tag == " ++ (tagName concreteStructTy name) ++ ") {"
     , joinWith "\n" (map (memberDeletion typeEnv env) (zip (map (\anon -> name ++ "." ++ anon) anonMemberNames) tys))
     , "  }"
     ]

module Sumtypes where

import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace

import Obj
import Types
import TypesToC
import Util
import Concretize
import Polymorphism
import Lookup
import Template
import ToTemplate
import Deftype
import StructUtils
import TypeError
import Validate
import SumtypeCase
import Info

getCase :: [SumtypeCase] -> String -> Maybe SumtypeCase
getCase cases caseNameToFind =
  case filter (\c -> caseName c == caseNameToFind) cases of
    found : _ -> Just found
    [] -> Nothing

moduleForSumtype :: Env -> TypeEnv -> Env -> [String] -> String -> [Ty] -> [XObj] -> Maybe Info -> Maybe Env -> Either TypeError (String, XObj, [XObj])
moduleForSumtype innerEnv typeEnv env pathStrings typeName typeVariables rest i existingEnv =
  let typeModuleName = typeName
      typeModuleEnv = fromMaybe (Env (Map.fromList []) (Just innerEnv) (Just typeModuleName) [] ExternalEnv 0) existingEnv
      insidePath = pathStrings ++ [typeModuleName]
  in do let structTy = StructTy (ConcreteNameTy typeName) typeVariables
        cases <- toCases typeEnv typeVariables rest
        okIniters <- initers insidePath structTy cases
        okTag <- binderForTag insidePath structTy
        (okStr, okStrDeps) <- binderForStrOrPrn typeEnv env insidePath structTy cases "str"
        (okPrn, _) <- binderForStrOrPrn typeEnv env insidePath structTy cases "prn"
        okDelete <- binderForDelete typeEnv env insidePath structTy cases
        (okCopy, okCopyDeps) <- binderForCopy typeEnv env insidePath structTy cases
        okMemberDeps <- memberDeps typeEnv cases
        let moduleEnvWithBindings = addListOfBindings typeModuleEnv (okIniters ++ [okStr, okPrn, okDelete, okCopy, okTag])
            typeModuleXObj = XObj (Mod moduleEnvWithBindings) i (Just ModuleTy)
        return (typeModuleName, typeModuleXObj, okMemberDeps ++ okCopyDeps ++ okStrDeps)

memberDeps :: TypeEnv -> [SumtypeCase] -> Either TypeError [XObj]
memberDeps typeEnv cases = fmap concat (mapM (concretizeType typeEnv) (concatMap caseTys cases))

replaceGenericTypesOnCases :: TypeMappings -> [SumtypeCase] -> [SumtypeCase]
replaceGenericTypesOnCases mappings cases =
  map replaceOnCase cases
  where replaceOnCase theCase =
          let newTys = (map (replaceTyVars mappings) (caseTys theCase))
          in  theCase { caseTys = newTys }

initers :: [String] -> Ty -> [SumtypeCase] -> Either TypeError [(String, Binder)]
initers insidePath structTy cases = mapM (binderForCaseInit insidePath structTy) cases

binderForCaseInit :: [String] -> Ty -> SumtypeCase -> Either TypeError (String, Binder)
binderForCaseInit insidePath structTy@(StructTy (ConcreteNameTy typeName) _) sumtypeCase =
  if isTypeGeneric structTy
  then Right (genericCaseInit StackAlloc insidePath structTy sumtypeCase)
  else Right (concreteCaseInit StackAlloc insidePath structTy sumtypeCase)

concreteCaseInit :: AllocationMode -> [String] -> Ty -> SumtypeCase -> (String, Binder)
concreteCaseInit allocationMode insidePath structTy sumtypeCase =
  instanceBinder (SymPath insidePath (caseName sumtypeCase)) (FuncTy (caseTys sumtypeCase) structTy StaticLifetimeTy) template doc
  where doc = "creates a `" ++ caseName sumtypeCase ++ "`."
        template =
          Template
          (FuncTy (caseTys sumtypeCase) (VarTy "p") StaticLifetimeTy)
          (\(FuncTy _ concreteStructTy _) ->
             let mappings = unifySignatures structTy concreteStructTy
                 correctedTys = map (replaceTyVars mappings) (caseTys sumtypeCase)
             in  (toTemplate $ "$p $NAME(" ++ joinWithComma (zipWith (curry memberArg) anonMemberNames (remove isUnit correctedTys)) ++ ")"))
          (const (tokensForCaseInit allocationMode structTy sumtypeCase))
          (\FuncTy{} -> [])

genericCaseInit :: AllocationMode -> [String] -> Ty -> SumtypeCase -> (String, Binder)
genericCaseInit allocationMode pathStrings originalStructTy sumtypeCase =
  defineTypeParameterizedTemplate templateCreator path t docs
  where path = SymPath pathStrings (caseName sumtypeCase)
        t = FuncTy (caseTys sumtypeCase) originalStructTy StaticLifetimeTy
        docs = "creates a `" ++ caseName sumtypeCase ++ "`."
        templateCreator = TemplateCreator $
          \typeEnv env ->
            Template
            (FuncTy (caseTys sumtypeCase) (VarTy "p") StaticLifetimeTy)
            (\(FuncTy _ concreteStructTy _) ->
               let mappings = unifySignatures originalStructTy concreteStructTy
                   correctedTys = map (replaceTyVars mappings) (caseTys sumtypeCase)
               in  toTemplate $ "$p $NAME(" ++ joinWithComma (zipWith (curry memberArg) anonMemberNames (remove isUnit correctedTys)) ++ ")")
            (\(FuncTy _ concreteStructTy _) ->
               let mappings = unifySignatures originalStructTy concreteStructTy
                   correctedTys = map (replaceTyVars mappings) (caseTys sumtypeCase)
               in tokensForCaseInit allocationMode concreteStructTy (sumtypeCase {caseTys = correctedTys}))
            (\(FuncTy _ concreteStructTy _) ->
               case concretizeType typeEnv concreteStructTy of
                 Left err -> error (show err ++ ". This error should not crash the compiler - change return type to Either here.")
                 Right ok -> ok)

tokensForCaseInit :: AllocationMode -> Ty -> SumtypeCase -> [Token]
tokensForCaseInit allocationMode sumTy@(StructTy (ConcreteNameTy typeName) typeVariables) sumtypeCase =
  toTemplate $ unlines [ "$DECL {"
                       , case allocationMode of
                           StackAlloc -> "    $p instance;"
                           HeapAlloc ->  "    $p instance = CARP_MALLOC(sizeof(" ++ typeName ++ "));"
                       , joinLines $ caseMemberAssignment allocationMode correctedName . fst <$> unitless
                       , "    instance._tag = " ++ tagName sumTy correctedName ++ ";"
                       , "    return instance;"
                       , "}"]
  where correctedName = caseName sumtypeCase
        unitless = zip anonMemberNames $ remove isUnit (caseTys sumtypeCase)

caseMemberAssignment :: AllocationMode -> String -> String -> String
caseMemberAssignment allocationMode caseName memberName =
  "    instance" ++ sep ++ caseName ++ "." ++ memberName ++ " = " ++ memberName ++ ";"
  where sep = case allocationMode of
                StackAlloc -> ".u."
                HeapAlloc -> "->u."

binderForTag :: [String] -> Ty -> Either TypeError (String, Binder)
binderForTag insidePath originalStructTy@(StructTy (ConcreteNameTy typeName) _) =
  Right $ instanceBinder path (FuncTy [RefTy originalStructTy (VarTy "q")] IntTy StaticLifetimeTy) template doc
  where path = SymPath insidePath "get-tag"
        template = Template
          (FuncTy [RefTy originalStructTy (VarTy "q")] IntTy StaticLifetimeTy)
          (\(FuncTy [RefTy structTy _] IntTy _) -> toTemplate $ proto structTy)
          (\(FuncTy [RefTy structTy _] IntTy _) -> toTemplate $ proto structTy ++ " { return p->_tag; }")
          (\_ -> [])
        proto structTy = "int $NAME(" ++ tyToCLambdaFix structTy ++ " *p)"
        doc = "Gets the tag from a `" ++ typeName ++ "`."


-- | Helper function to create the binder for the 'str' template.
binderForStrOrPrn :: TypeEnv -> Env -> [String] -> Ty -> [SumtypeCase] -> String -> Either TypeError ((String, Binder), [XObj])
binderForStrOrPrn typeEnv env insidePath structTy@(StructTy (ConcreteNameTy typeName) _) cases strOrPrn =
  Right $ if isTypeGeneric structTy
          then (genericStr insidePath structTy cases strOrPrn, [])
          else concreteStr typeEnv env insidePath structTy cases strOrPrn

-- | The template for the 'str' function for a concrete deftype.
concreteStr :: TypeEnv -> Env -> [String] -> Ty -> [SumtypeCase] -> String -> ((String, Binder), [XObj])
concreteStr typeEnv env insidePath concreteStructTy@(StructTy (ConcreteNameTy typeName) _) cases strOrPrn =
  instanceBinderWithDeps (SymPath insidePath strOrPrn) (FuncTy [RefTy concreteStructTy (VarTy "q")] StringTy StaticLifetimeTy) template doc
  where doc = "converts a `" ++ typeName ++ "` to a string."
        template =
          Template
            (FuncTy [RefTy concreteStructTy (VarTy "q")] StringTy StaticLifetimeTy)
            (\(FuncTy [RefTy structTy _] StringTy _) -> toTemplate $ "String $NAME(" ++ tyToCLambdaFix structTy ++ " *p)")
            (\(FuncTy [RefTy structTy@(StructTy _ concreteMemberTys) _] StringTy _) ->
                tokensForStr typeEnv env typeName cases concreteStructTy)
            (\ft@(FuncTy [RefTy structTy@(StructTy _ concreteMemberTys) _] StringTy _) ->
               concatMap (depsOfPolymorphicFunction typeEnv env [] "prn" . typesStrFunctionType typeEnv)
                          (filter (\t -> (not . isExternalType typeEnv) t && (not . isFullyGenericType) t) (concatMap caseTys cases))
            )

-- | The template for the 'str' function for a generic deftype.
genericStr :: [String] -> Ty -> [SumtypeCase] -> String -> (String, Binder)
genericStr insidePath originalStructTy@(StructTy (ConcreteNameTy typeName) varTys) cases strOrPrn =
  defineTypeParameterizedTemplate templateCreator path t docs
  where path = SymPath insidePath strOrPrn
        t = FuncTy [RefTy originalStructTy (VarTy "q")] StringTy StaticLifetimeTy
        docs = "stringifies a `" ++ show typeName ++ "`."
        templateCreator = TemplateCreator $
          \typeEnv env ->
            Template
            t
            (\(FuncTy [RefTy concreteStructTy _] StringTy _) ->
               toTemplate $ "String $NAME(" ++ tyToCLambdaFix concreteStructTy ++ " *p)")
            (\(FuncTy [RefTy concreteStructTy@(StructTy _ concreteMemberTys) _] StringTy _) ->
               let mappings = unifySignatures originalStructTy concreteStructTy
                   correctedCases = replaceGenericTypesOnCases mappings cases
               in tokensForStr typeEnv env typeName correctedCases concreteStructTy)
            (\ft@(FuncTy [RefTy concreteStructTy@(StructTy _ concreteMemberTys) _] StringTy _) ->
               let mappings = unifySignatures originalStructTy concreteStructTy
                   correctedCases = replaceGenericTypesOnCases mappings cases
                   tys = filter (\t -> (not . isExternalType typeEnv) t && (not . isFullyGenericType) t) (concatMap caseTys correctedCases)
               in  concatMap (depsOfPolymorphicFunction typeEnv env [] "prn" . typesStrFunctionType typeEnv) tys
                   ++
                   (if isTypeGeneric concreteStructTy then [] else [defineFunctionTypeAlias ft]))

tokensForStr :: TypeEnv -> Env -> String -> [SumtypeCase] -> Ty -> [Token]
tokensForStr typeEnv env typeName cases concreteStructTy  =
  toTemplate $ unlines [ "$DECL {"
                       , "  // convert members to String here:"
                       , "  String temp = NULL;"
                       , "  int tempsize = 0;"
                       , "  (void)tempsize; // that way we remove the occasional unused warning "
                       , calculateStructStrSize typeEnv env cases concreteStructTy
                       , "  String buffer = CARP_MALLOC(size);"
                       , "  String bufferPtr = buffer;"
                       , ""
                       , concatMap (strCase typeEnv env concreteStructTy) cases
                       , "  return buffer;"
                       , "}"]

namesFromCase :: SumtypeCase -> Ty -> (String, [Ty], String)
namesFromCase theCase concreteStructTy =
  let name = caseName theCase
  in (name, caseTys theCase {caseTys = (remove isUnit (caseTys theCase))}, tagName concreteStructTy name)

strCase :: TypeEnv -> Env -> Ty -> SumtypeCase -> String
strCase typeEnv env concreteStructTy@(StructTy _ typeVariables) theCase =
  let (name, tys, correctedTagName) = namesFromCase theCase concreteStructTy
  in unlines
     [ "  if(p->_tag == " ++ correctedTagName ++ ") {"
     , "    sprintf(bufferPtr, \"(%s \", \"" ++ name ++ "\");"
     , "    bufferPtr += strlen(\"" ++ name ++ "\") + 2;\n"
     , joinLines $ memberPrn typeEnv env <$> unionMembers name tys
     , "    bufferPtr--;"
     , "    sprintf(bufferPtr, \")\");"
     , "  }"
     ]

-- | Figure out how big the string needed for the string representation of the struct has to be.
calculateStructStrSize :: TypeEnv -> Env -> [SumtypeCase] -> Ty -> String
calculateStructStrSize typeEnv env cases structTy@(StructTy (ConcreteNameTy name) _) =
  "  int size = 1;\n" ++
  concatMap (strSizeCase typeEnv env structTy) cases

strSizeCase :: TypeEnv -> Env -> Ty -> SumtypeCase -> String
strSizeCase typeEnv env concreteStructTy@(StructTy _ typeVariables) theCase =
  let (name, tys, correctedTagName) = namesFromCase theCase concreteStructTy
  in unlines
     [ "  if(p->_tag == " ++ correctedTagName ++ ") {"
     , "    size += snprintf(NULL, 0, \"(%s \", \"" ++ name ++ "\");"
     , joinLines $ memberPrnSize typeEnv env <$> unionMembers name tys
     , "  }"
     ]

-- | Helper function to create the binder for the 'delete' template.
binderForDelete :: TypeEnv -> Env -> [String] -> Ty -> [SumtypeCase] -> Either TypeError (String, Binder)
binderForDelete typeEnv env insidePath structTy@(StructTy (ConcreteNameTy typeName) _) cases =
  Right $ if isTypeGeneric structTy
          then genericSumtypeDelete insidePath structTy cases
          else concreteSumtypeDelete insidePath typeEnv env structTy cases

-- | The template for the 'delete' function of a generic sumtype.
genericSumtypeDelete :: [String] -> Ty -> [SumtypeCase] -> (String, Binder)
genericSumtypeDelete pathStrings originalStructTy cases =
  defineTypeParameterizedTemplate templateCreator path (FuncTy [originalStructTy] UnitTy StaticLifetimeTy) docs
  where path = SymPath pathStrings "delete"
        t = FuncTy [VarTy "p"] UnitTy StaticLifetimeTy
        docs = "deletes a `" ++ show originalStructTy ++ "`. Should usually not be called manually."
        templateCreator = TemplateCreator $
          \typeEnv env ->
            Template
            t
            (const (toTemplate "void $NAME($p p)"))
            (\(FuncTy [concreteStructTy] UnitTy _) ->
               let mappings = unifySignatures originalStructTy concreteStructTy
                   correctedCases = replaceGenericTypesOnCases mappings cases
               in  (toTemplate $ unlines [ "$DECL {"
                                         , concatMap (deleteCase typeEnv env concreteStructTy) (zip correctedCases (True : repeat False))
                                         , "}"]))
            (\(FuncTy [concreteStructTy] UnitTy _) ->
               let mappings = unifySignatures originalStructTy concreteStructTy
                   correctedCases = replaceGenericTypesOnCases mappings cases
               in  if isTypeGeneric concreteStructTy
                   then []
                   else concatMap (depsOfPolymorphicFunction typeEnv env [] "delete" . typesDeleterFunctionType)
                                  (filter (isManaged typeEnv) (concatMap caseTys correctedCases)))

-- | The template for the 'delete' function of a concrete sumtype
concreteSumtypeDelete :: [String] -> TypeEnv -> Env -> Ty -> [SumtypeCase] -> (String, Binder)
concreteSumtypeDelete insidePath typeEnv env structTy@(StructTy (ConcreteNameTy typeName) _) cases =
  instanceBinder (SymPath insidePath "delete") (FuncTy [structTy] UnitTy StaticLifetimeTy) template doc
  where doc = "deletes a `" ++ typeName ++ "`. This should usually not be called manually."
        template = Template
                    (FuncTy [VarTy "p"] UnitTy StaticLifetimeTy)
                    (const (toTemplate "void $NAME($p p)"))
                    (const (toTemplate $ unlines [ "$DECL {"
                                                 , concatMap (deleteCase typeEnv env structTy) (zip cases (True : repeat False))
                                                 , "}"]))
                    (\_ -> concatMap (depsOfPolymorphicFunction typeEnv env [] "delete" . typesDeleterFunctionType)
                                     (filter (isManaged typeEnv) (concatMap caseTys cases)))

deleteCase :: TypeEnv -> Env -> Ty -> (SumtypeCase, Bool) -> String
deleteCase typeEnv env concreteStructTy@(StructTy _ typeVariables) (theCase, isFirstCase) =
  let (name, tys, correctedTagName) = namesFromCase theCase concreteStructTy
  in unlines
     [ "  " ++ (if isFirstCase then "" else "else ") ++ "if(p._tag == " ++ correctedTagName ++ ") {"
     , joinLines $ memberDeletion typeEnv env <$> unionMembers name tys
     , "  }"
     ]

-- | Helper function to create the binder for the 'copy' template.
binderForCopy :: TypeEnv -> Env -> [String] -> Ty -> [SumtypeCase] -> Either TypeError ((String, Binder), [XObj])
binderForCopy typeEnv env insidePath structTy@(StructTy (ConcreteNameTy typeName) _) cases =
  Right $ if isTypeGeneric structTy
          then (genericSumtypeCopy insidePath structTy cases, [])
          else concreteSumtypeCopy insidePath typeEnv env structTy cases

-- | The template for the 'copy' function of a generic sumtype.
genericSumtypeCopy :: [String] -> Ty -> [SumtypeCase] -> (String, Binder)
genericSumtypeCopy pathStrings originalStructTy cases =
  defineTypeParameterizedTemplate templateCreator path (FuncTy [RefTy originalStructTy (VarTy "q")] originalStructTy StaticLifetimeTy) docs
  where path = SymPath pathStrings "copy"
        t = FuncTy [RefTy (VarTy "p") (VarTy "q")] (VarTy "p") StaticLifetimeTy
        docs = "copies a `" ++ show originalStructTy ++ "`."
        templateCreator = TemplateCreator $
          \typeEnv env ->
            Template
            t
            (const (toTemplate "$p $NAME($p* pRef)"))
            (\(FuncTy [RefTy concreteStructTy _] _ _) ->
               let mappings = unifySignatures originalStructTy concreteStructTy
                   correctedCases = replaceGenericTypesOnCases mappings cases
               in  tokensForSumtypeCopy typeEnv env concreteStructTy correctedCases)
            (\(FuncTy [RefTy concreteStructTy _] _ _) ->
               let mappings = unifySignatures originalStructTy concreteStructTy
                   correctedCases = replaceGenericTypesOnCases mappings cases
               in  if isTypeGeneric concreteStructTy
                   then []
                   else concatMap (depsOfPolymorphicFunction typeEnv env [] "copy" . typesCopyFunctionType)
                                  (filter (isManaged typeEnv) (concatMap caseTys correctedCases)))

-- | The template for the 'copy' function of a concrete sumtype
concreteSumtypeCopy :: [String] -> TypeEnv -> Env -> Ty -> [SumtypeCase] -> ((String, Binder), [XObj])
concreteSumtypeCopy insidePath typeEnv env structTy@(StructTy (ConcreteNameTy typeName) _) cases =
  instanceBinderWithDeps (SymPath insidePath "copy") (FuncTy [RefTy structTy (VarTy "q")] structTy StaticLifetimeTy) template doc
  where doc = "copies a `" ++ typeName ++ "`."
        template = Template
                    (FuncTy [RefTy (VarTy "p") (VarTy "q")] (VarTy "p") StaticLifetimeTy)
                    (const (toTemplate "$p $NAME($p* pRef)"))
                    (const (tokensForSumtypeCopy typeEnv env structTy cases))
                    (\_ -> concatMap (depsOfPolymorphicFunction typeEnv env [] "copy" . typesCopyFunctionType)
                                     (filter (isManaged typeEnv) (concatMap caseTys cases)))

tokensForSumtypeCopy :: TypeEnv -> Env -> Ty -> [SumtypeCase] -> [Token]
tokensForSumtypeCopy typeEnv env concreteStructTy cases =
  toTemplate $ unlines [ "$DECL {"
                       , "    $p copy = *pRef;"
                       , joinLines $ map (copyCase typeEnv env concreteStructTy) (zip cases (True : repeat False))
                       , "    return copy;"
                       , "}"]

copyCase :: TypeEnv -> Env -> Ty -> (SumtypeCase, Bool) -> String
copyCase typeEnv env concreteStructTy@(StructTy _ typeVariables) (theCase, isFirstCase) =
  let (name, tys, correctedTagName) = namesFromCase theCase concreteStructTy
  in unlines
     [ "    " ++ (if isFirstCase then "" else "else ") ++ "if(pRef->_tag == " ++ correctedTagName ++ ") {"
     , joinLines $ memberCopy typeEnv env <$> unionMembers name tys
     , "    }"
     ]

anonMemberName :: String -> String -> String
anonMemberName name anon = "u." ++ name ++ "." ++ anon

infiniteUnionMembers :: String -> [String]
infiniteUnionMembers name = anonMemberName name <$> anonMemberNames

unionMembers :: String -> [Ty] -> [(String, Ty)]
unionMembers name = zip (infiniteUnionMembers name)

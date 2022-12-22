{-# LANGUAGE NamedFieldPuns #-}

module Sumtypes
  ( moduleForSumtypeInContext,
    moduleForSumtype,
  )
where

import Concretize
import Context
import Data.Maybe
import Deftype
import Env (addListOfBindings, new)
import Info
import Managed
import Obj
import StructUtils
import Template
import TemplateGenerator as TG
import ToTemplate
import qualified TypeCandidate as TC
import TypeError
import TypePredicates
import Types
import TypesToC
import Util
import Validate

--------------------------------------------------------------------------------
-- Public

-- | Creates a module and generates standard functions for a user defined sum type in the given context.
moduleForSumtypeInContext :: Context -> String -> [Ty] -> [XObj] -> Maybe Info -> Either TypeError (String, XObj, [XObj])
moduleForSumtypeInContext ctx name vars members info =
  let global = contextGlobalEnv ctx
      types = contextTypeEnv ctx
      path = contextPath ctx
      inner = either (const Nothing) Just (innermostModuleEnv ctx)
      previous =
        either
          (const Nothing)
          Just
          ( (lookupBinderInInternalEnv ctx (SymPath path name))
              <> (lookupBinderInGlobalEnv ctx (SymPath path name))
                >>= \b ->
                  replaceLeft
                    (NotFoundGlobal (SymPath path name))
                    ( case binderXObj b of
                        XObj (Mod ev et) _ _ -> Right (ev, et)
                        _ -> Left "Non module"
                    )
          )
   in moduleForSumtype inner types global path name vars members info previous

-- | Creates a module and generates standard functions for a user defined sum type.
moduleForSumtype :: Maybe Env -> TypeEnv -> Env -> [String] -> String -> [Ty] -> [XObj] -> Maybe Info -> Maybe (Env, TypeEnv) -> Either TypeError (String, XObj, [XObj])
moduleForSumtype innerEnv typeEnv env pathStrings typeName typeVariables rest i existingEnv =
  let moduleValueEnv = fromMaybe (new innerEnv (Just typeName)) (fmap fst existingEnv)
      moduleTypeEnv = fromMaybe (new (Just typeEnv) (Just typeName)) (fmap snd existingEnv)
   in do
        -- validate the definition
        candidate <- TC.mkSumtypeCandidate typeName typeVariables typeEnv env rest pathStrings
        validateType candidate
        -- produce standard function bindings
        (binders, deps) <- generateBinders candidate
        -- insert the module into the environment
        let moduleEnvWithBindings = addListOfBindings moduleValueEnv binders
            typeModuleXObj = XObj (Mod moduleEnvWithBindings moduleTypeEnv) i (Just ModuleTy)
        pure (typeName, typeModuleXObj, deps)

--------------------------------------------------------------------------------
-- Private

-- | Generate standard binders for the sumtype
generateBinders :: TC.TypeCandidate -> Either TypeError ([(String, Binder)], [XObj])
generateBinders candidate =
  do
    okIniters <- initers candidate
    okTag <- binderForTag candidate
    (okStr, okStrDeps) <- binderForStrOrPrn candidate "str"
    (okPrn, _) <- binderForStrOrPrn candidate "prn"
    okDelete <- binderForDelete candidate
    (okCopy, okCopyDeps) <- binderForCopy candidate
    okMemberDeps <- memberDeps (TC.getTypeEnv candidate) (TC.getValueEnv candidate) (TC.getFields candidate)
    let binders = okIniters ++ [okStr, okPrn, okDelete, okCopy, okTag]
        deps = okMemberDeps ++ okCopyDeps ++ okStrDeps
    pure (binders, deps)

-- | Gets concrete dependencies for sum type fields.
memberDeps :: TypeEnv -> Env -> [TC.TypeField] -> Either TypeError [XObj]
memberDeps typeEnv env cases = fmap concat (mapM (concretizeType typeEnv env) (concatMap TC.fieldTypes cases))

-- | Replace type variables in a sum type case
replaceGenericTypesOnCases :: TypeMappings -> [TC.TypeField] -> [TC.TypeField]
replaceGenericTypesOnCases mappings = map replaceOnCase
  where
    replaceOnCase :: TC.TypeField -> TC.TypeField
    replaceOnCase (TC.SumField name tys) =
      let newTys = map (replaceTyVars mappings) tys
       in (TC.SumField name newTys)
    replaceOnCase field = field

--------------------------------------------------------------------------------
-- Binding generators

type BinderGen = TC.TypeCandidate -> Either TypeError (String, Binder)

type BinderGenDeps = TC.TypeCandidate -> Either TypeError ((String, Binder), [XObj])

type MultiBinderGen = TC.TypeCandidate -> Either TypeError [(String, Binder)]

-- | Generate initializer bindings for each sum type case.
initers :: MultiBinderGen
initers candidate = mapM binderForCaseInit (TC.getFields candidate)
  where
    binderForCaseInit :: TC.TypeField -> Either TypeError (String, Binder)
    binderForCaseInit sumtypeCase =
      if isTypeGeneric (TC.toType candidate)
        then Right (genericCaseInit StackAlloc sumtypeCase)
        else Right (concreteCaseInit StackAlloc sumtypeCase)
    concreteCaseInit :: AllocationMode -> TC.TypeField -> (String, Binder)
    concreteCaseInit alloc field@(TC.SumField fieldname tys) =
      let concrete = (TC.toType candidate)
          doc = "creates a `" ++ fieldname ++ "`."
          t = (FuncTy tys (VarTy "p") StaticLifetimeTy)
          decl = (const (tokensForCaseInitDecl concrete concrete field))
          body = (const (tokensForCaseInit alloc concrete concrete field))
          deps = (const [])
          temp = Template t decl body deps
          binderPath = SymPath (TC.getFullPath candidate) fieldname
       in instanceBinder binderPath (FuncTy tys concrete StaticLifetimeTy) temp doc
    concreteCaseInit _ _ = error "concreteCaseInit"
    genericCaseInit :: AllocationMode -> TC.TypeField -> (String, Binder)
    genericCaseInit alloc field@(TC.SumField fieldname tys) =
      let generic = (TC.toType candidate)
          docs = "creates a `" ++ fieldname ++ "`."
          ft = FuncTy tys generic StaticLifetimeTy
          binderPath = SymPath (TC.getFullPath candidate) fieldname
          t = (FuncTy tys (VarTy "p") StaticLifetimeTy)
          decl (FuncTy _ concrete _) = tokensForCaseInitDecl generic concrete field
          decl _ = error "sumtypes: genericCaseInit called with non function type"
          body (FuncTy _ concrete _) = tokensForCaseInit alloc generic concrete field
          body _ = error "sumtypes: genericCaseInit called with non function type"
          deps tenv env = \typ -> case typ of
            (FuncTy _ concrete _) -> either (const []) id (concretizeType tenv env concrete)
            _ -> []
          temp = TemplateCreator $ \tenv env -> Template t decl body (deps tenv env)
       in defineTypeParameterizedTemplate temp binderPath ft docs
    genericCaseInit _ _ = error "genericCaseInit"

-- | Generates a binder for retrieving the tag of a sum type.
binderForTag :: BinderGen
binderForTag candidate =
  let t = FuncTy [RefTy (TC.toType candidate) (VarTy "q")] IntTy StaticLifetimeTy
      decl (FuncTy [RefTy struct _] _ _) = toTemplate $ proto struct
      decl _ = error "sumtypes: binderForTag called with non function type"
      body (FuncTy [RefTy struct _] _ _) = toTemplate $ proto struct ++ " { return p->_tag; }"
      body _ = error "sumtypes: binderForTag called with non function type"
      deps = const []
      path' = SymPath (TC.getFullPath candidate) "get-tag"
      temp = Template t decl body deps
      doc = "Gets the tag from a `" ++ (TC.getName candidate) ++ "`."
   in Right (instanceBinder path' t temp doc)
  where
    proto :: Ty -> String
    proto structTy = "int $NAME(" ++ tyToCLambdaFix structTy ++ " *p)"

-- | Helper function to create the binder for the 'str' template.
binderForStrOrPrn :: TC.TypeCandidate -> String -> Either TypeError ((String, Binder), [XObj])
binderForStrOrPrn candidate strOrPrn =
  let doc = "converts a `" ++ (getStructName (TC.toType candidate)) ++ "` to a string."
      binderP = SymPath (TC.getFullPath candidate) strOrPrn
      binderT = FuncTy [RefTy (TC.toType candidate) (VarTy "q")] StringTy StaticLifetimeTy
   in Right $
        if isTypeGeneric (TC.toType candidate)
          then (defineTypeParameterizedTemplate (TG.generateGenericTypeTemplate candidate strGenerator) binderP binderT doc, [])
          else instanceBinderWithDeps binderP binderT (TG.generateConcreteTypeTemplate candidate strGenerator) doc
  where
    strGenerator :: TG.TemplateGenerator TC.TypeCandidate
    strGenerator = TG.mkTemplateGenerator genT decl body deps

    genT :: TG.TypeGenerator TC.TypeCandidate
    genT GeneratorArg {value} =
      FuncTy [RefTy (TC.toType value) (VarTy "q")] StringTy StaticLifetimeTy

    decl :: TG.TokenGenerator TC.TypeCandidate
    decl GeneratorArg {instanceT = (FuncTy [RefTy ty _] _ _)} =
      toTemplate $ "String $NAME(" ++ tyToCLambdaFix ty ++ " *p)"
    decl _ = toTemplate "/* template error! */"

    body :: TG.TokenGenerator TC.TypeCandidate
    body GeneratorArg {tenv, env, originalT, instanceT = (FuncTy [RefTy ty _] _ _), value} =
      tokensForStr tenv env originalT ty (TC.getFields value)
    body _ = toTemplate "/* template error! */"

    deps :: TG.DepenGenerator TC.TypeCandidate
    deps GeneratorArg {tenv, env, originalT, instanceT = (FuncTy [RefTy ty _] _ _), value} =
      depsForStr tenv env originalT ty (TC.getFields value)
    deps _ = []

-- | Helper function to create the binder for the 'delete' template.
binderForDelete :: BinderGen
binderForDelete candidate =
  let t = (TC.toType candidate)
      doc = "deletes a `" ++ (getStructName t) ++ "`. This should usually not be called manually."
      binderT = FuncTy [t] UnitTy StaticLifetimeTy
      binderP = SymPath (TC.getFullPath candidate) "delete"
   in Right $
        if isTypeGeneric t
          then defineTypeParameterizedTemplate (TG.generateGenericTypeTemplate candidate generator) binderP binderT doc
          else instanceBinder binderP binderT (TG.generateConcreteTypeTemplate candidate generator) doc
  where
    generator :: TG.TemplateGenerator TC.TypeCandidate
    generator = TG.mkTemplateGenerator genT decl body deps

    genT :: TG.TypeGenerator TC.TypeCandidate
    genT _ = (FuncTy [VarTy "p"] UnitTy StaticLifetimeTy)

    decl :: TG.TokenGenerator TC.TypeCandidate
    decl _ = toTemplate "void $NAME($p p)"

    body :: TG.TokenGenerator TC.TypeCandidate
    body GeneratorArg {tenv, env, originalT, instanceT = (FuncTy [ty] _ _), value} =
      tokensForDeleteBody tenv env originalT ty (TC.getFields value)
    body _ = toTemplate "/* template error! */"

    deps :: TG.DepenGenerator TC.TypeCandidate
    deps GeneratorArg {tenv, env, originalT, instanceT = (FuncTy [ty] _ _), value} =
      depsForDelete tenv env originalT ty (TC.getFields value)
    deps _ = []

-- | Helper function to create the binder for the 'copy' template.
binderForCopy :: BinderGenDeps
binderForCopy candidate =
  let t = TC.toType candidate
      doc = "copies a `" ++ (TC.getName candidate) ++ "`."
      binderT = FuncTy [RefTy t (VarTy "q")] t StaticLifetimeTy
      binderP = SymPath (TC.getFullPath candidate) "copy"
   in Right $
        if isTypeGeneric (TC.toType candidate)
          then (defineTypeParameterizedTemplate (TG.generateGenericTypeTemplate candidate generator) binderP binderT doc, [])
          else instanceBinderWithDeps binderP binderT (TG.generateConcreteTypeTemplate candidate generator) doc
  where
    generator :: TG.TemplateGenerator TC.TypeCandidate
    generator = TG.mkTemplateGenerator genT decl body deps

    genT :: TG.TypeGenerator TC.TypeCandidate
    genT _ = FuncTy [RefTy (VarTy "p") (VarTy "q")] (VarTy "p") StaticLifetimeTy

    decl :: TG.TokenGenerator TC.TypeCandidate
    decl _ = toTemplate "$p $NAME($p* pRef)"

    body :: TG.TokenGenerator TC.TypeCandidate
    body GeneratorArg {tenv, env, originalT, instanceT = (FuncTy [RefTy ty _] _ _), value} =
      tokensForSumtypeCopy tenv env originalT ty (TC.getFields value)
    body _ = toTemplate "/* template error! */"

    deps :: TG.DepenGenerator TC.TypeCandidate
    deps GeneratorArg {tenv, env, originalT, instanceT = (FuncTy [RefTy ty _] _ _), value} =
      depsForCopy tenv env originalT ty (TC.getFields value)
    deps _ = []

-------------------------------------------------------------------------------
-- Token and dep generators

type TokenGen = TypeEnv -> Env -> Ty -> Ty -> [TC.TypeField] -> [Token]

type DepGen = TypeEnv -> Env -> Ty -> Ty -> [TC.TypeField] -> [XObj]

--------------------------------------------------------------------------------
-- Initializers

-- | Generate an init function declaration.
tokensForCaseInitDecl :: Ty -> Ty -> TC.TypeField -> [Token]
tokensForCaseInitDecl orig concrete@(StructTy (ConcreteNameTy _) _) (TC.SumField _ tys) =
  let mappings = unifySignatures orig concrete
      concreteTys = map (replaceTyVars mappings) tys
   in toTemplate ("$p $NAME(" ++ joinWithComma (zipWith (curry memberArg) anonMemberNames (remove isUnit concreteTys)) ++ ")")
tokensForCaseInitDecl _ _ _ =
  error "tokensForCaseInitDecl"

-- | Given an allocation mode, an original, possibly polymorphic type, a
-- concrete type and a sum type field, generate an init function body.
tokensForCaseInit :: AllocationMode -> Ty -> Ty -> TC.TypeField -> [Token]
tokensForCaseInit alloc orig concrete (TC.SumField fieldname tys) =
  let mappings = unifySignatures orig concrete
      concreteTys = map (replaceTyVars mappings) tys
      unitless = zip anonMemberNames $ remove isUnit concreteTys
   in multilineTemplate
        [ "$DECL {",
          allocate alloc,
          joinLines (assign alloc fieldname . fst <$> unitless),
          "    instance._tag = " ++ tagName concrete fieldname ++ ";",
          "    return instance;",
          "}"
        ]
  where
    allocate :: AllocationMode -> String
    allocate StackAlloc = "  $p instance;"
    allocate HeapAlloc = "  $p instance = CARP_MALLOC(sizeof(" ++ show concrete ++ "));"

    assign :: AllocationMode -> String -> String -> String
    assign alloc' name member =
      "    instance" ++ (accessor alloc') ++ "u." ++ name ++ "." ++ member ++ " = " ++ member ++ ";"
tokensForCaseInit _ _ _ _ = error "tokenForCaseInit"

accessor :: AllocationMode -> String
accessor StackAlloc = "."
accessor HeapAlloc = "->"

--------------------------------------------------------------------------------
-- Copy

-- | Generates dependencies for sum type copy functions.
depsForCopy :: DepGen
depsForCopy tenv env generic concrete fields =
  let mappings = unifySignatures generic concrete
      concreteFields = replaceGenericTypesOnCases mappings fields
   in if isTypeGeneric concrete
        then []
        else
          concatMap
            (depsOfPolymorphicFunction tenv env [] "copy" . typesCopyFunctionType)
            (filter (isManaged tenv env) (concatMap TC.fieldTypes concreteFields))

-- | Generates C function bodies for sum type copy functions.
tokensForSumtypeCopy :: TypeEnv -> Env -> Ty -> Ty -> [TC.TypeField] -> [Token]
tokensForSumtypeCopy typeEnv env generic concrete fields =
  let mappings = unifySignatures generic concrete
      concreteFields = replaceGenericTypesOnCases mappings fields
   in multilineTemplate
        [ "$DECL {",
          "    $p copy = *pRef;",
          joinLines $
            zipWith
              (curry copyCase)
              concreteFields
              (True : repeat False),
          "    return copy;",
          "}"
        ]
  where
    copyCase :: (TC.TypeField, Bool) -> String
    copyCase (theCase, isFirstCase) =
      let (name, tys, correctedTagName) = namesFromCase theCase concrete
       in unlines
            [ "    " ++ (if isFirstCase then "" else "else ") ++ "if(pRef->_tag == " ++ correctedTagName ++ ") {",
              joinLines $ memberCopy typeEnv env <$> unionMembers name tys,
              "    }"
            ]

--------------------------------------------------------------------------------
-- Delete

-- | Generates tokens for the C function body of sum type copy functions.
tokensForDeleteBody :: TokenGen
tokensForDeleteBody tenv env generic concrete fields =
  let mappings = unifySignatures generic concrete
      concreteFields = replaceGenericTypesOnCases mappings fields
   in multilineTemplate
        [ "$DECL {",
          concatMap deleteCase (zip concreteFields (True : repeat False)),
          "}"
        ]
  where
    deleteCase :: (TC.TypeField, Bool) -> String
    deleteCase (theCase, isFirstCase) =
      let (name, tys, correctedTagName) = namesFromCase theCase concrete
       in unlines
            [ "  " ++ (if isFirstCase then "" else "else ") ++ "if(p._tag == " ++ correctedTagName ++ ") {",
              joinLines $ memberDeletion tenv env <$> unionMembers name tys,
              "  }"
            ]

-- | Generates deps for the body of a delete function.
depsForDelete :: TypeEnv -> Env -> Ty -> Ty -> [TC.TypeField] -> [XObj]
depsForDelete tenv env generic concrete fields =
  let mappings = unifySignatures generic concrete
      concreteFields = replaceGenericTypesOnCases mappings fields
   in if isTypeGeneric concrete
        then []
        else
          concatMap
            (depsOfPolymorphicFunction tenv env [] "delete" . typesDeleterFunctionType)
            (filter (isManaged tenv env) (concatMap (TC.fieldTypes) concreteFields))

--------------------------------------------------------------------------------
-- Str and prn

-- | Fetches dependencies for str and prn functions.
depsForStr :: TypeEnv -> Env -> Ty -> Ty -> [TC.TypeField] -> [XObj]
depsForStr tenv env generic concrete fields =
  let ft = FuncTy [RefTy concrete (VarTy "q")] StringTy StaticLifetimeTy
      mappings = unifySignatures generic concrete
      concreteFields = replaceGenericTypesOnCases mappings fields
      tys = remove isFullyGenericType (concatMap TC.fieldTypes concreteFields)
   in (concatMap (depsOfPolymorphicFunction tenv env [] "prn" . typesStrFunctionType tenv env) tys)
        ++ [defineFunctionTypeAlias ft | not (isTypeGeneric concrete)]

-- | Generates C function body tokens for sum type str and prn functions.
tokensForStr :: TypeEnv -> Env -> Ty -> Ty -> [TC.TypeField] -> [Token]
tokensForStr typeEnv env generic concrete fields =
  let mappings = unifySignatures generic concrete
      concreteFields = replaceGenericTypesOnCases mappings fields
   in multilineTemplate
        [ "$DECL {",
          "  // convert members to String here:",
          "  String temp = NULL;",
          "  int tempsize = 0;",
          "  (void)tempsize; // that way we remove the occasional unused warning ",
          calculateStructStrSize concreteFields,
          "  String buffer = CARP_MALLOC(size);",
          "  String bufferPtr = buffer;",
          "",
          concatMap strCase concreteFields,
          "  return buffer;",
          "}"
        ]
  where
    strCase :: TC.TypeField -> String
    strCase theCase =
      let (name, tys, correctedTagName) = namesFromCase theCase concrete
       in unlines
            [ "  if(p->_tag == " ++ correctedTagName ++ ") {",
              "    sprintf(bufferPtr, \"(%s \", \"" ++ name ++ "\");",
              "    bufferPtr += strlen(\"" ++ name ++ "\") + 2;\n",
              joinLines $ memberPrn typeEnv env <$> unionMembers name tys,
              "    bufferPtr--;",
              "    sprintf(bufferPtr, \")\");",
              "  }"
            ]
    calculateStructStrSize :: [TC.TypeField] -> String
    calculateStructStrSize cases = "  int size = 1;\n" ++ concatMap strSizeCase cases

    strSizeCase :: TC.TypeField -> String
    strSizeCase theCase =
      let (name, tys, correctedTagName) = namesFromCase theCase concrete
       in unlines
            [ "  if(p->_tag == " ++ correctedTagName ++ ") {",
              "    size += snprintf(NULL, 0, \"(%s \", \"" ++ name ++ "\");",
              joinLines $ memberPrnSize typeEnv env <$> unionMembers name tys,
              "  }"
            ]

--------------------------------------------------------------------------------
-- Additional utilities

namesFromCase :: TC.TypeField -> Ty -> (String, [Ty], String)
namesFromCase theCase concreteStructTy =
  let name = TC.fieldName theCase
   in (name, TC.fieldTypes (TC.SumField (TC.fieldName theCase) (remove isUnit (TC.fieldTypes theCase))), tagName concreteStructTy name)

anonMemberName :: String -> String -> String
anonMemberName name anon = "u." ++ name ++ "." ++ anon

infiniteUnionMembers :: String -> [String]
infiniteUnionMembers name = anonMemberName name <$> anonMemberNames

unionMembers :: String -> [Ty] -> [(String, Ty)]
unionMembers name = zip (infiniteUnionMembers name)

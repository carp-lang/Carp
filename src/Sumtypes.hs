module Sumtypes
  (
   moduleForSumtypeInContext,
   moduleForSumtype
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
import ToTemplate
import TypeError
import TypePredicates
import Types
import TypesToC
import Util
import Validate
import qualified TypeCandidate as TC

--------------------------------------------------------------------------------
-- Public

-- |
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

-- |
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
  do okIniters <- initers candidate
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
    -- | Generate an initializer binding for a single sum type case, using the given candidate.
    binderForCaseInit :: TC.TypeField -> Either TypeError (String, Binder)
    binderForCaseInit sumtypeCase =
      if isTypeGeneric (TC.toType candidate)
        then Right (genericCaseInit StackAlloc sumtypeCase)
        else Right (concreteCaseInit StackAlloc sumtypeCase)

    -- | Generates a template for a concrete (no type variables) sum type case.
    concreteCaseInit :: AllocationMode -> TC.TypeField -> (String, Binder)
    concreteCaseInit alloc field@(TC.SumField fieldname tys) =
      let concrete = (TC.toType candidate)
          doc      = "creates a `" ++ fieldname ++ "`."
          t        = (FuncTy tys (VarTy "p") StaticLifetimeTy)
          decl     = (const (tokensForCaseInitDecl concrete concrete field))
          body     = (const (tokensForCaseInit alloc concrete concrete field))
          deps     = (const [])
          temp     = Template t decl body deps
          binderPath = SymPath (TC.getFullPath candidate) fieldname
       in instanceBinder binderPath (FuncTy tys concrete StaticLifetimeTy) temp doc
    concreteCaseInit _ _ = error "concreteCaseInit"

    -- | Generates a template for a generic (has type variables) sum type case.
    genericCaseInit :: AllocationMode -> TC.TypeField -> (String, Binder)
    genericCaseInit alloc field@(TC.SumField fieldname tys) =
      let generic = (TC.toType candidate)
          docs    = "creates a `" ++ fieldname ++ "`."
          ft      = FuncTy tys generic StaticLifetimeTy
          binderPath = SymPath (TC.getFullPath candidate) fieldname
          t       = (FuncTy tys (VarTy "p") StaticLifetimeTy)
          decl    = \(FuncTy _ concrete _) -> tokensForCaseInitDecl generic concrete field
          body    = \(FuncTy _ concrete _) -> tokensForCaseInit alloc generic concrete field
          deps tenv env = \(FuncTy _ concrete _) -> either (const []) id (concretizeType tenv env concrete)
          temp = TemplateCreator $ \tenv env -> Template t decl body (deps tenv env)
       in defineTypeParameterizedTemplate temp binderPath ft docs
    genericCaseInit _ _ = error "genericCaseInit"

-- | Generates a binder for retrieving the tag of a sum type.
binderForTag :: BinderGen
binderForTag candidate =
  let t = FuncTy [RefTy (TC.toType candidate) (VarTy "q")] IntTy StaticLifetimeTy
      decl = \(FuncTy [RefTy struct _] _ _) -> toTemplate $ proto struct
      body = \(FuncTy [RefTy struct _] _ _) -> toTemplate $ proto struct ++ " { return p->_tag; }"
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
  Right $
    if isTypeGeneric (TC.toType candidate)
      then (genericStr, [])
      else concreteStr
  where
    -- | The template for the 'str' function for a concrete deftype.
    concreteStr :: ((String, Binder), [XObj])
    concreteStr =
      let tenv = TC.getTypeEnv candidate
          env = TC.getValueEnv candidate
          concrete = TC.toType candidate
          fields = TC.getFields candidate
          doc  = "converts a `" ++ (TC.getName candidate) ++ "` to a string."
          binderT = FuncTy [RefTy concrete (VarTy "q")] StringTy StaticLifetimeTy
          decl = const (toTemplate ("String $NAME(" ++ tyToCLambdaFix concrete ++ " *p)"))
          body = const (tokensForStr tenv env concrete concrete fields)
          deps = const (depsForStr tenv env concrete concrete fields)
          temp = Template binderT decl body deps
          path' = SymPath (TC.getFullPath candidate) strOrPrn
       in instanceBinderWithDeps path' binderT temp doc

    -- | The template for the 'str' function for a generic deftype.
    genericStr :: (String, Binder)
    genericStr =
      let generic = TC.toType candidate
          fields = TC.getFields candidate
          binderPath = SymPath (TC.getFullPath candidate) strOrPrn
          binderT = FuncTy [RefTy generic (VarTy "q")] StringTy StaticLifetimeTy
          docs = "stringifies a `" ++ (TC.getName candidate) ++ "`."
          decl = \(FuncTy [RefTy concrete _] _ _ )-> toTemplate $ "String $NAME(" ++ tyToCLambdaFix concrete ++ " *p)"
          body tenv env = \(FuncTy [RefTy concrete _] _ _) -> tokensForStr tenv env generic concrete fields
          deps tenv env = \(FuncTy [RefTy concrete _] _ _) -> depsForStr tenv env generic concrete fields
          temp = TemplateCreator $ \tenv env -> Template binderT decl (body tenv env) (deps tenv env)
       in defineTypeParameterizedTemplate temp binderPath binderT docs

-- | Helper function to create the binder for the 'delete' template.
binderForDelete :: BinderGen
binderForDelete candidate =
  Right $
    if isTypeGeneric (TC.toType candidate)
      then genericSumtypeDelete
      else concreteSumtypeDelete
  where
    -- | The template for the 'delete' function of a concrete sumtype
    concreteSumtypeDelete :: (String, Binder)
    concreteSumtypeDelete =
      let concrete = TC.toType candidate
          fields = TC.getFields candidate
          tenv = TC.getTypeEnv candidate
          env = TC.getValueEnv candidate
          doc = "deletes a `" ++ TC.getName candidate ++ "`. This should usually not be called manually."
          t   = (FuncTy [VarTy "p"] UnitTy StaticLifetimeTy)
          binderPath = SymPath (TC.getFullPath candidate) "delete"
          decl = const (toTemplate "void $NAME($p p)")
          body = const (tokensForDeleteBody tenv env concrete concrete fields)
          deps = const (depsForDelete tenv env concrete concrete fields)
          temp = Template t decl body deps
       in instanceBinder binderPath (FuncTy [concrete] UnitTy StaticLifetimeTy) temp doc

    -- | The template for the 'delete' function of a generic sumtype.
    genericSumtypeDelete ::(String, Binder)
    genericSumtypeDelete =
      let generic = TC.toType candidate
          fields = TC.getFields candidate
          binderT = FuncTy [VarTy "p"] UnitTy StaticLifetimeTy
          doc = "deletes a `" ++ (TC.getName candidate) ++ "`. Should usually not be called manually."
          binderPath = SymPath (TC.getFullPath candidate) "delete"
          decl = (const (toTemplate "void $NAME($p p)"))
          body tenv env = \(FuncTy [concrete] _ _) -> tokensForDeleteBody tenv env generic concrete fields
          deps tenv env = \(FuncTy [concrete] _ _) -> depsForDelete tenv env generic concrete fields
          temp = TemplateCreator $ \tenv env -> (Template binderT decl (body tenv env) (deps tenv env))
       in defineTypeParameterizedTemplate temp binderPath (FuncTy [generic] UnitTy StaticLifetimeTy) doc

-- | Helper function to create the binder for the 'copy' template.
binderForCopy :: BinderGenDeps
binderForCopy candidate =
  Right $
    if isTypeGeneric (TC.toType candidate)
      then (genericSumtypeCopy, [])
      else concreteSumtypeCopy
  where
    -- | The template for the 'copy' function of a generic sumtype.
    genericSumtypeCopy :: (String, Binder)
    genericSumtypeCopy =
      let generic = (TC.toType candidate)
          binderPath = SymPath (TC.getFullPath candidate) "copy"
          t = FuncTy [RefTy (VarTy "p") (VarTy "q")] (VarTy "p") StaticLifetimeTy
          fields = TC.getFields candidate
          doc = "copies a `" ++ (TC.getName candidate) ++ "`."
          decl = (const (toTemplate "$p $NAME($p* pRef)"))
          body tenv env = \(FuncTy [RefTy concrete _] _ _) -> tokensForSumtypeCopy tenv env generic concrete fields
          deps tenv env = \(FuncTy [RefTy concrete _] _ _) -> depsForCopy tenv env generic concrete fields
          temp = TemplateCreator $ \tenv env -> Template t decl (body tenv env) (deps tenv env)
       in defineTypeParameterizedTemplate temp binderPath (FuncTy [RefTy generic (VarTy "q")] generic StaticLifetimeTy) doc

    -- | The template for the 'copy' function of a concrete sumtype
    concreteSumtypeCopy ::((String, Binder), [XObj])
    concreteSumtypeCopy =
      let tenv = TC.getTypeEnv candidate
          env = TC.getValueEnv candidate
          fields = TC.getFields candidate
          binderPath = SymPath (TC.getFullPath candidate) "copy"
          doc = "copies a `" ++ TC.getName candidate ++ "`."
          t = (FuncTy [RefTy (VarTy "p") (VarTy "q")] (VarTy "p") StaticLifetimeTy)
          concrete = TC.toType candidate
          decl = (const (toTemplate "$p $NAME($p* pRef)"))
          body = const (tokensForSumtypeCopy tenv env concrete concrete fields)
          deps = const (depsForCopy tenv env concrete concrete fields)
          temp = Template t decl body deps
       in instanceBinderWithDeps binderPath (FuncTy [RefTy concrete (VarTy "q")] concrete StaticLifetimeTy) temp doc

-------------------------------------------------------------------------------
-- Token and dep generators

type TokenGen = TypeEnv -> Env -> Ty -> Ty -> [TC.TypeField] -> [Token]
type DepGen   = TypeEnv -> Env -> Ty -> Ty -> [TC.TypeField] -> [XObj]

--------------------------------------------------------------------------------
-- Initializers

-- | Generate an init function declaration.
tokensForCaseInitDecl :: Ty -> Ty -> TC.TypeField -> [Token]
tokensForCaseInitDecl orig concrete@(StructTy (ConcreteNameTy _) _) (TC.SumField _ tys) =
  let mappings    = unifySignatures orig concrete
      concreteTys = map (replaceTyVars mappings) tys
   in toTemplate ("$p $NAME(" ++ joinWithComma (zipWith (curry memberArg) anonMemberNames (remove isUnit concreteTys)) ++ ")")
tokensForCaseInitDecl _ _ _ =
  error "tokensForCaseInitDecl"

-- | Given an allocation mode, an original, possibly polymorphic type, a
-- concrete type and a sum type field, generate an init function body.
tokensForCaseInit :: AllocationMode -> Ty -> Ty -> TC.TypeField -> [Token]
tokensForCaseInit alloc orig concrete (TC.SumField fieldname tys) =
  let mappings    = unifySignatures orig concrete
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
   where allocate :: AllocationMode -> String
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
   in multilineTemplate [
        "$DECL {",
        concatMap deleteCase (zip concreteFields (True : repeat False)),
        "}"
     ]
  where deleteCase :: (TC.TypeField, Bool) -> String
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
  let mappings       = unifySignatures generic concrete
      concreteFields = replaceGenericTypesOnCases mappings fields
   in if isTypeGeneric concrete
        then []
        else concatMap
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
  where strCase :: TC.TypeField -> String
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

        -- | Figure out how big the string needed for the string representation of the struct has to be.
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

{-# LANGUAGE LambdaCase #-}

module ArrayTemplates where

import Concretize
import Lookup
import Obj
import Template
import ToTemplate
import Types
import TypesToC

-- | "Endofunctor Map"
templateEMap :: (String, Binder)
templateEMap =
  let fTy = FuncTy [VarTy "a"] (VarTy "a") (VarTy "fq")
      aTy = StructTy (ConcreteNameTy "Array") [VarTy "a"]
      bTy = StructTy (ConcreteNameTy "Array") [VarTy "a"]
      elt = "((($a*)a.data)[i])"
   in defineTemplate
        (SymPath ["Array"] "endo-map")
        (FuncTy [RefTy fTy (VarTy "q"), aTy] bTy StaticLifetimeTy)
        "applies a function `f` to an array `a`. The type of the elements cannot change."
        (toTemplate "Array $NAME(Lambda *f, Array a)") -- Lambda used to be $(Fn [a] a)
        ( toTemplate $
            unlines
              [ "$DECL { ",
                "    for(int i = 0; i < a.len; ++i) {",
                "        (($a*)a.data)[i] = " ++ templateCodeForCallingLambda "(*f)" fTy [elt] ++ ";",
                "    }",
                "    return a;",
                "}"
              ]
        )
        ( \(FuncTy [RefTy t@(FuncTy fArgTys fRetTy _) _, _] _ _) ->
            [defineFunctionTypeAlias t, defineFunctionTypeAlias (FuncTy (lambdaEnvTy : fArgTys) fRetTy StaticLifetimeTy)]
        )

templateShrinkCheck :: String -> String
templateShrinkCheck var =
  unlines
    [ "    if(" ++ var ++ ".len < (" ++ var ++ ".capacity / 4)) {",
      "        " ++ var ++ ".capacity = " ++ var ++ ".len * 2;",
      "        " ++ var ++ ".data = CARP_REALLOC(" ++ var ++ ".data, sizeof($a) * " ++ var ++ " .capacity);",
      "    }"
    ]

-- | Endofunctor filter, misnomer for consistency with flavors of map
templateEFilter :: (String, Binder)
templateEFilter = defineTypeParameterizedTemplate templateCreator path t docs
  where
    fTy = FuncTy [RefTy (VarTy "a") (VarTy "q")] BoolTy (VarTy "fq")
    aTy = StructTy (ConcreteNameTy "Array") [VarTy "a"]
    path = SymPath ["Array"] "endo-filter"
    t = FuncTy [RefTy fTy (VarTy "w"), aTy] aTy StaticLifetimeTy
    docs = "filters array members using a function. This function takes ownership."
    elt = "&((($a*)a.data)[i])"
    templateCreator = TemplateCreator $
      \typeEnv env ->
        Template
          t
          (const (toTemplate "Array $NAME(Lambda *predicate, Array a)")) -- Lambda used to be $(Fn [(Ref a)] Bool)
          ( \(FuncTy [RefTy (FuncTy [RefTy insideTy _] BoolTy _) _, _] _ _) ->
              toTemplate $
                unlines $
                  let deleter = insideArrayDeletion typeEnv env insideTy
                   in [ "$DECL { ",
                        "    int insertIndex = 0;",
                        "    for(int i = 0; i < a.len; ++i) {",
                        "        if(" ++ templateCodeForCallingLambda "(*predicate)" fTy [elt] ++ ") {",
                        "            ((($a*)a.data)[insertIndex++]) = (($a*)a.data)[i];",
                        "        } else {",
                        "        " ++ deleter "i",
                        "        }",
                        "    }",
                        "    a.len = insertIndex;",
                        templateShrinkCheck "a",
                        "    return a;",
                        "}"
                      ]
          )
          ( \(FuncTy [RefTy ft@(FuncTy fArgTys@[RefTy insideType _] BoolTy _) _, _] _ _) ->
              [defineFunctionTypeAlias ft, defineFunctionTypeAlias (FuncTy (lambdaEnvTy : fArgTys) BoolTy StaticLifetimeTy)]
                ++ depsForDeleteFunc typeEnv env insideType
          )

templatePushBack :: (String, Binder)
templatePushBack =
  let aTy = StructTy (ConcreteNameTy "Array") [VarTy "a"]
      valTy = VarTy "a"
   in defineTemplate
        (SymPath ["Array"] "push-back")
        (FuncTy [aTy, valTy] aTy StaticLifetimeTy)
        "adds an element `value` to the end of an array `a`."
        (toTemplate "Array $NAME(Array a, $a value)")
        ( toTemplate $
            unlines
              [ "$DECL { ",
                "    a.len++;",
                "    if(a.len > a.capacity) {",
                "        a.capacity = a.len * 2;",
                "        a.data = CARP_REALLOC(a.data, sizeof($a) * a.capacity);",
                "    }",
                "    (($a*)a.data)[a.len - 1] = value;",
                "    return a;",
                "}"
              ]
        )
        (\(FuncTy [_, _] _ _) -> [])

templatePushBackBang :: (String, Binder)
templatePushBackBang =
  let aTy = RefTy (StructTy (ConcreteNameTy "Array") [VarTy "a"]) (VarTy "q")
      valTy = VarTy "a"
   in defineTemplate
        (SymPath ["Array"] "push-back!")
        (FuncTy [aTy, valTy] UnitTy StaticLifetimeTy)
        "adds an element `value` to the end of an array `a` in-place."
        (toTemplate "void $NAME(Array *aRef, $a value)")
        ( toTemplate $
            unlines
              [ "$DECL { ",
                "    aRef->len++;",
                "    if(aRef->len > aRef->capacity) {",
                "        aRef->capacity = aRef->len * 2;",
                "        aRef->data = CARP_REALLOC(aRef->data, sizeof($a) * aRef->capacity);",
                "    }",
                "    (($a*)aRef->data)[aRef->len - 1] = value;",
                "}"
              ]
        )
        (\(FuncTy [_, _] _ _) -> [])

templatePopBack :: (String, Binder)
templatePopBack = defineTypeParameterizedTemplate templateCreator path t docs
  where
    path = SymPath ["Array"] "pop-back"
    aTy = StructTy (ConcreteNameTy "Array") [VarTy "a"]
    t = FuncTy [aTy] aTy StaticLifetimeTy
    docs = "removes the last element of an array and returns the new array."
    templateCreator = TemplateCreator $
      \typeEnv env ->
        Template
          t
          (const (toTemplate "Array $NAME(Array a)"))
          ( \(FuncTy [(StructTy _ [insideTy])] _ _) ->
              let deleteElement = insideArrayDeletion typeEnv env insideTy
               in toTemplate
                    ( unlines
                        [ "$DECL { ",
                          "  assert(a.len > 0);",
                          "  a.len--;",
                          "  " ++ deleteElement "a.len",
                          templateShrinkCheck "a",
                          "  return a;",
                          "}"
                        ]
                    )
          )
          ( \(FuncTy [arrayType@(StructTy _ [insideTy])] _ _) ->
              depsForDeleteFunc typeEnv env arrayType
                ++ depsForCopyFunc typeEnv env insideTy
          )

templatePopBackBang :: (String, Binder)
templatePopBackBang =
  let aTy = RefTy (StructTy (ConcreteNameTy "Array") [VarTy "a"]) (VarTy "q")
   in defineTemplate
        (SymPath ["Array"] "pop-back!")
        (FuncTy [aTy] (VarTy "a") StaticLifetimeTy)
        "removes an element `value` from the end of an array `a` in-place and returns it."
        (toTemplate "$a $NAME(Array *aRef)")
        ( toTemplate $
            unlines
              [ "$DECL { ",
                "  $a ret;",
                "  assert(aRef->len > 0);",
                "  ret = (($a*)aRef->data)[aRef->len - 1];",
                "  aRef->len--;",
                "  return ret;",
                "}"
              ]
        )
        (\(FuncTy [_] _ _) -> [])

templateNth :: (String, Binder)
templateNth =
  let t = VarTy "t"
   in defineTemplate
        (SymPath ["Array"] "unsafe-nth")
        (FuncTy [RefTy (StructTy (ConcreteNameTy "Array") [t]) (VarTy "q"), IntTy] (RefTy t (VarTy "q")) StaticLifetimeTy)
        "gets a reference to the `n`th element from an array `a`."
        (toTemplate "$t* $NAME (Array *aRef, int n)")
        ( toTemplate $
            unlines
              [ "$DECL {",
                "    Array a = *aRef;",
                "    assert(n >= 0);",
                "    assert(n < a.len);",
                "    return &((($t*)a.data)[n]);",
                "}"
              ]
        )
        ( \(FuncTy [RefTy _ _, _] _ _) ->
            []
        )

templateRaw :: (String, Binder)
templateRaw =
  defineTemplate
    (SymPath ["Array"] "raw")
    (FuncTy [StructTy (ConcreteNameTy "Array") [VarTy "t"]] (PointerTy (VarTy "t")) StaticLifetimeTy)
    "returns an array `a` as a raw pointer—useful for interacting with C."
    (toTemplate "$t* $NAME (Array a)")
    (toTemplate "$DECL { return a.data; }")
    (\(FuncTy [_] _ _) -> [])

templateUnsafeRaw :: (String, Binder)
templateUnsafeRaw =
  defineTemplate
    (SymPath ["Array"] "unsafe-raw")
    (FuncTy [RefTy (VarTy "q") (StructTy (ConcreteNameTy "Array") [VarTy "t"])] (PointerTy (VarTy "t")) StaticLifetimeTy)
    "returns an array `a` as a raw pointer—useful for interacting with C."
    (toTemplate "$t* $NAME (Array* a)")
    (toTemplate "$DECL { return a->data; }")
    (\(FuncTy [RefTy _ _] _ _) -> [])

templateAset :: (String, Binder)
templateAset = defineTypeParameterizedTemplate templateCreator path t docs
  where
    path = SymPath ["Array"] "aset"
    t = FuncTy [StructTy (ConcreteNameTy "Array") [VarTy "t"], IntTy, VarTy "t"] (StructTy (ConcreteNameTy "Array") [VarTy "t"]) StaticLifetimeTy
    docs = "sets an array element at the index `n` to a new value."
    templateCreator = TemplateCreator $
      \typeEnv env ->
        Template
          t
          (\_ -> toTemplate "Array $NAME (Array a, int n, $t newValue)")
          ( \(FuncTy [_, _, insideTy] _ _) ->
              let deleter = insideArrayDeletion typeEnv env insideTy
               in toTemplate $
                    unlines
                      [ "$DECL {",
                        "    assert(n >= 0);",
                        "    assert(n < a.len);",
                        deleter "n",
                        "    (($t*)a.data)[n] = newValue;",
                        "    return a;",
                        "}"
                      ]
          )
          ( \(FuncTy [_, _, insideTy] _ _) ->
              depsForDeleteFunc typeEnv env insideTy
          )

templateAsetBang :: (String, Binder)
templateAsetBang = defineTypeParameterizedTemplate templateCreator path t docs
  where
    path = SymPath ["Array"] "aset!"
    t = FuncTy [RefTy (StructTy (ConcreteNameTy "Array") [VarTy "t"]) (VarTy "q"), IntTy, VarTy "t"] UnitTy StaticLifetimeTy
    docs = "sets an array element at the index `n` to a new value in place."
    templateCreator = TemplateCreator $
      \typeEnv env ->
        Template
          t
          (const (toTemplate "void $NAME (Array *aRef, int n, $t newValue)"))
          ( \(FuncTy [_, _, insideTy] _ _) ->
              let deleter = insideArrayDeletion typeEnv env insideTy
               in ( toTemplate $
                      unlines
                        [ "$DECL {",
                          "    Array a = *aRef;",
                          "    assert(n >= 0);",
                          "    assert(n < a.len);",
                          deleter "n",
                          "    (($t*)a.data)[n] = newValue;",
                          "}"
                        ]
                  )
          )
          ( \(FuncTy [RefTy arrayType _, _, _] _ _) ->
              depsForDeleteFunc typeEnv env arrayType
          )

-- | This function can set uninitialized memory in an array (used together with 'allocate').
-- | It will NOT try to free the value that is already at location 'n'.
templateAsetUninitializedBang :: (String, Binder)
templateAsetUninitializedBang = defineTypeParameterizedTemplate templateCreator path t docs
  where
    path = SymPath ["Array"] "aset-uninitialized!"
    t = FuncTy [RefTy (StructTy (ConcreteNameTy "Array") [VarTy "t"]) (VarTy "q"), IntTy, VarTy "t"] UnitTy StaticLifetimeTy
    docs = "sets an uninitialized array member. The old member will not be deleted."
    templateCreator = TemplateCreator $
      \_ _ ->
        Template
          t
          (const (toTemplate "void $NAME (Array *aRef, int n, $t newValue)"))
          ( const
              ( toTemplate $
                  unlines
                    [ "$DECL {",
                      "    Array a = *aRef;",
                      "    assert(n >= 0);",
                      "    assert(n < a.len);",
                      "    (($t*)a.data)[n] = newValue;",
                      "}"
                    ]
              )
          )
          (const [])

templateLength :: (String, Binder)
templateLength = defineTypeParameterizedTemplate templateCreator path t docs
  where
    path = SymPath ["Array"] "length"
    t = FuncTy [RefTy (StructTy (ConcreteNameTy "Array") [VarTy "t"]) (VarTy "q")] IntTy StaticLifetimeTy
    docs = "gets the length of the array."
    templateCreator = TemplateCreator $
      \typeEnv env ->
        Template
          t
          (const (toTemplate "int $NAME (Array *a)"))
          (const (toTemplate "$DECL { return (*a).len; }"))
          ( \(FuncTy [RefTy arrayType _] _ _) ->
              depsForDeleteFunc typeEnv env arrayType
          )

templateAllocate :: (String, Binder)
templateAllocate = defineTypeParameterizedTemplate templateCreator path t docs
  where
    path = SymPath ["Array"] "allocate"
    t = FuncTy [IntTy] (StructTy (ConcreteNameTy "Array") [VarTy "t"]) StaticLifetimeTy
    docs = "allocates an uninitialized array. You can initialize members using [`aset-uninitialized`](#aset-uninitialized)."
    templateCreator = TemplateCreator $
      \typeEnv env ->
        Template
          t
          (const (toTemplate "Array $NAME (int n)"))
          ( \(FuncTy [_] arrayType _) ->
              toTemplate $
                unlines
                  ( [ "$DECL {",
                      "    Array a;",
                      "    a.len = n;",
                      "    a.capacity = n;",
                      "    a.data = CARP_MALLOC(n*sizeof($t));"
                    ]
                      ++ initTy arrayType
                      ++ [ "    return a;",
                           "}"
                         ]
                  )
          )
          ( \(FuncTy [_] arrayType _) ->
              depsForDeleteFunc typeEnv env arrayType
          )

templateDeleteArray :: (String, Binder)
templateDeleteArray = defineTypeParameterizedTemplate templateCreator path t docs
  where
    path = SymPath ["Array"] "delete"
    t = FuncTy [StructTy (ConcreteNameTy "Array") [VarTy "a"]] UnitTy StaticLifetimeTy
    docs = "deletes an array. This function should usually not be called manually."
    templateCreator = TemplateCreator $
      \typeEnv env ->
        Template
          t
          (const (toTemplate "void $NAME (Array a)"))
          ( \(FuncTy [arrayType] UnitTy _) ->
              [TokDecl, TokC "{\n"]
                ++ deleteTy typeEnv env arrayType
                ++ [TokC "}\n"]
          )
          ( \(FuncTy [(StructTy (ConcreteNameTy "Array") [insideType])] UnitTy _) ->
              depsForDeleteFunc typeEnv env insideType
          )

deleteTy :: TypeEnv -> Env -> Ty -> [Token]
deleteTy typeEnv env (StructTy _ [innerType]) =
  [ TokC "    for(int i = 0; i < a.len; i++) {\n",
    TokC $ "    " ++ insideArrayDeletion typeEnv env innerType "i",
    TokC "    }\n",
    TokC "    CARP_FREE(a.data);\n"
  ]
deleteTy _ _ _ = []

initTy :: Ty -> [String]
initTy (StructTy (ConcreteNameTy "Array") [innerType@FuncTy {}]) =
  [ "    // initialize each Lambda struct ",
    "    for(int i = 0; i < a.len; i++) {",
    "    " ++ insideArrayInitLambda innerType "i",
    "    }"
  ]
initTy _ = []

insideArrayInitLambda :: Ty -> String -> String
insideArrayInitLambda t indexer =
  "    Lambda lambda = " ++ initLambda ++ "\n"
    ++ "        (("
    ++ tyToCLambdaFix t
    ++ "*)a.data)["
    ++ indexer
    ++ "] = lambda;"

initLambda :: String
initLambda = "{ .callback = NULL, .env = NULL, .delete = NULL, .copy = NULL };"

insideArrayDeletion :: TypeEnv -> Env -> Ty -> String -> String
insideArrayDeletion typeEnv env t indexer =
  case findFunctionForMember typeEnv env "delete" (typesDeleterFunctionType t) ("Inside array.", t) of
    FunctionFound functionFullName ->
      "    " ++ functionFullName ++ "(((" ++ tyToCLambdaFix t ++ "*)a.data)[" ++ indexer ++ "]);\n"
    FunctionNotFound msg -> error msg
    FunctionIgnored -> "    /* Ignore non-managed type inside Array: '" ++ show t ++ "' */\n"

templateCopyArray :: (String, Binder)
templateCopyArray = defineTypeParameterizedTemplate templateCreator path t docs
  where
    path = SymPath ["Array"] "copy"
    t = FuncTy [RefTy (StructTy (ConcreteNameTy "Array") [VarTy "a"]) (VarTy "q")] (StructTy (ConcreteNameTy "Array") [VarTy "a"]) StaticLifetimeTy
    docs = "copies an array."
    templateCreator = TemplateCreator $
      \typeEnv env ->
        Template
          t
          (const (toTemplate "Array $NAME (Array* a)"))
          ( \(FuncTy [RefTy arrayType _] _ _) ->
              [TokDecl, TokC "{\n"]
                ++ [TokC "    Array copy;\n"]
                ++ [TokC "    copy.len = a->len;\n"]
                ++ [TokC "    copy.capacity = a->capacity;\n"]
                ++ [TokC "    copy.data = CARP_MALLOC(sizeof(", TokTy (VarTy "a") Normal, TokC ") * a->capacity);\n"]
                ++ copyTy typeEnv env arrayType
                ++ [TokC "    return copy;\n"]
                ++ [TokC "}\n"]
          )
          ( \case
              (FuncTy [RefTy arrayType@(StructTy (ConcreteNameTy "Array") [insideType]) _] _ _) ->
                depsForCopyFunc typeEnv env insideType
                  ++ depsForDeleteFunc typeEnv env arrayType
              err ->
                error ("CAN'T MATCH: " ++ show err)
          )

copyTy :: TypeEnv -> Env -> Ty -> [Token]
copyTy typeEnv env (StructTy (ConcreteNameTy "Array") [innerType]) =
  if managed
    then
      [ TokC "    for(int i = 0; i < a->len; i++) {\n",
        TokC $ "    " ++ insideArrayCopying typeEnv env innerType,
        TokC "    }\n"
      ]
    else [TokC "    memcpy(copy.data, a->data, sizeof(", TokTy (VarTy "a") Normal, TokC ") * a->len);\n"]
  where
    managed =
      case findFunctionForMember
        typeEnv
        env
        "delete"
        (typesDeleterFunctionType innerType)
        ("Inside array.", innerType) of
        FunctionFound _ -> True
        FunctionNotFound _ -> False
        FunctionIgnored -> False
copyTy _ _ _ = []

-- | The "memberCopy" and "memberDeletion" functions in Deftype are very similar!
insideArrayCopying :: TypeEnv -> Env -> Ty -> String
insideArrayCopying typeEnv env t =
  case findFunctionForMemberIncludePrimitives typeEnv env "copy" (typesCopyFunctionType t) ("Inside array.", t) of
    FunctionFound functionFullName ->
      "    ((" ++ tyToC t ++ "*)(copy.data))[i] = " ++ functionFullName ++ "(&(((" ++ tyToC t ++ "*)a->data)[i]));\n"
    FunctionNotFound msg -> error msg
    FunctionIgnored ->
      "    /* Ignore type inside Array when copying: '" ++ show t ++ "' (no copy function known)*/\n"

templateStrArray :: (String, Binder)
templateStrArray = defineTypeParameterizedTemplate templateCreator path t docs
  where
    templateCreator = TemplateCreator $
      \typeEnv env ->
        Template
          t
          (const (toTemplate "String $NAME (Array* a)"))
          ( \(FuncTy [RefTy arrayType _] StringTy _) ->
              [TokDecl, TokC " {\n"]
                ++ strTy typeEnv env arrayType
                ++ [TokC "}\n"]
          )
          ( \(FuncTy [RefTy (StructTy (ConcreteNameTy "Array") [insideType]) _] StringTy _) ->
              depsForPrnFunc typeEnv env insideType
          )
    path = SymPath ["Array"] "str"
    t = FuncTy [RefTy (StructTy (ConcreteNameTy "Array") [VarTy "a"]) (VarTy "q")] StringTy StaticLifetimeTy
    docs = "converts an array to a string."

-- | TODO: move this into the templateStrArray function?
strTy :: TypeEnv -> Env -> Ty -> [Token]
strTy typeEnv env (StructTy _ [innerType]) =
  [ TokC "",
    TokC "  String temp = NULL;\n",
    TokC $ calculateStrSize typeEnv env innerType,
    TokC "  String buffer = CARP_MALLOC(size);\n",
    TokC "  String bufferPtr = buffer;\n",
    TokC "\n",
    TokC "  sprintf(buffer, \"[\");\n",
    TokC "  bufferPtr += 1;\n",
    TokC "\n",
    TokC "  for(int i = 0; i < a->len; i++) {\n",
    TokC $ "  " ++ insideArrayStr typeEnv env innerType,
    TokC "  }\n",
    TokC "\n",
    TokC "  if(a->len > 0) { bufferPtr -= 1; }\n",
    TokC "  sprintf(bufferPtr, \"]\");\n",
    TokC "  return buffer;\n"
  ]
strTy _ _ _ = []

calculateStrSize :: TypeEnv -> Env -> Ty -> String
calculateStrSize typeEnv env t =
  unlines
    [ "  int size = 3; // opening and closing brackets and terminator",
      "  for(int i = 0; i < a->len; i++) {",
      arrayMemberSizeCalc ++ "  }",
      ""
    ]
  where
    arrayMemberSizeCalc =
      case findFunctionForMemberIncludePrimitives typeEnv env "prn" (typesStrFunctionType typeEnv t) ("Inside array.", t) of
        FunctionFound functionFullName ->
          let takeAddressOrNot = if isManaged typeEnv t then "&" else ""
           in unlines
                [ "    temp = " ++ functionFullName ++ "(" ++ takeAddressOrNot ++ "((" ++ tyToC t ++ "*)a->data)[i]);",
                  "    size += snprintf(NULL, 0, \"%s \", temp);",
                  "    if(temp) {",
                  "      CARP_FREE(temp);",
                  "      temp = NULL;",
                  "    }"
                ]
        FunctionNotFound msg -> error msg
        FunctionIgnored -> "    /* Ignore type inside Array: '" ++ show t ++ "' ??? */\n"

insideArrayStr :: TypeEnv -> Env -> Ty -> String
insideArrayStr typeEnv env t =
  case findFunctionForMemberIncludePrimitives typeEnv env "prn" (typesStrFunctionType typeEnv t) ("Inside array.", t) of
    FunctionFound functionFullName ->
      let takeAddressOrNot = if isManaged typeEnv t then "&" else ""
       in unlines
            [ "  temp = " ++ functionFullName ++ "(" ++ takeAddressOrNot ++ "((" ++ tyToC t ++ "*)a->data)[i]);",
              "    sprintf(bufferPtr, \"%s \", temp);",
              "    bufferPtr += strlen(temp) + 1;",
              "    if(temp) {",
              "      CARP_FREE(temp);",
              "      temp = NULL;",
              "    }"
            ]
    FunctionNotFound msg -> error msg
    FunctionIgnored -> "    /* Ignore type inside Array: '" ++ show t ++ "' ??? */\n"

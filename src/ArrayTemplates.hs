{-# LANGUAGE LambdaCase #-}

module ArrayTemplates where

import Util
import Types
import Obj
import Parsing
import Template
import Polymorphism
import Concretize
import Debug.Trace

templateCopyingMap :: (String, Binder)
templateCopyingMap = defineTypeParameterizedTemplate templateCreator path t
  where fTy = FuncTy [RefTy (VarTy "a")] (VarTy "b")
        aTy = RefTy (StructTy "Array" [VarTy "a"])
        bTy = StructTy "Array" [VarTy "b"]
        path = SymPath ["Array"] "copy-map"
        t = FuncTy [fTy, aTy] bTy
        templateCreator = TemplateCreator $
          \typeEnv env ->
            Template
            t
            (const (toTemplate "Array $NAME($(Fn [(Ref a)] b) f, Array* a)"))
            (\(FuncTy [FuncTy [_] outputTy, _] _) ->
               (toTemplate $ unlines
                  [ "$DECL { "
                  , "    Array b;"
                  , "    b.len = a->len;"
                  , "    b.data = CARP_MALLOC(sizeof($b) * a->len);"
                  , "    for(int i = 0; i < a->len; ++i) {"
                  , if outputTy == UnitTy
                    then "        f(&(($a*)a->data)[i]); "
                    else "        (($b*)b.data)[i] = f(&(($a*)a->data)[i]);"
                  , "    }"
                  , "    return b;"
                  , "}"
                  ]))
            (\(FuncTy [ft@(FuncTy [_] _), RefTy arrayTypeA] arrayTypeB) ->
               [defineFunctionTypeAlias ft,
                defineArrayTypeAlias arrayTypeA,
                defineArrayTypeAlias arrayTypeB] ++
                depsForDeleteFunc typeEnv env arrayTypeA ++
                depsForDeleteFunc typeEnv env arrayTypeB)

-- | "Endofunctor Map"
templateEMap :: (String, Binder)
templateEMap =
  let fTy = FuncTy [VarTy "a"] (VarTy "a")
      aTy = StructTy "Array" [VarTy "a"]
      bTy = StructTy "Array" [VarTy "a"]
  in  defineTemplate
      (SymPath ["Array"] "endo-map")
      (FuncTy [fTy, aTy] bTy)
      (toTemplate "Array $NAME($(Fn [a] a) f, Array a)")
      (toTemplate $ unlines
        ["$DECL { "
        ,"    for(int i = 0; i < a.len; ++i) {"
        ,"        (($a*)a.data)[i] = f((($a*)a.data)[i]); "
        ,"    }"
        ,"    return a;"
        ,"}"
        ])
      (\(FuncTy [t, arrayType] _) ->
         [defineFunctionTypeAlias t,
          defineArrayTypeAlias arrayType])

templateFilter :: (String, Binder)
templateFilter = defineTypeParameterizedTemplate templateCreator path t
  where
    fTy = FuncTy [RefTy (VarTy "a")] BoolTy
    aTy = StructTy "Array" [VarTy "a"]
    path = SymPath ["Array"] "filter"
    t = FuncTy [fTy, aTy] aTy
    templateCreator = TemplateCreator $
      \typeEnv env ->
        Template
        t
        (const (toTemplate "Array $NAME($(Fn [(Ref a)] Bool) predicate, Array a)"))
        (\(FuncTy [FuncTy [RefTy insideTy] BoolTy, _] _) ->
           (toTemplate $ unlines $
            let deleter = insideArrayDeletion typeEnv env insideTy
            in ["$DECL { "
               , "    int insertIndex = 0;"
               , "    for(int i = 0; i < a.len; ++i) {"
               , "        if(predicate(&((($a*)a.data)[i]))) {"
               , "            ((($a*)a.data)[insertIndex++]) = (($a*)a.data)[i];"
               , "        } else {"
               , "        " ++ deleter "i"
               , "        }"
               , "    }"
               , "    a.len = insertIndex;"
               , "    // NOTE: the array isn't resized for now, it probably should be?"
               , "    return a;"
               , "}"
               ]))
        (\(FuncTy [ft@(FuncTy [RefTy insideType] BoolTy), arrayType] _) ->
           [defineFunctionTypeAlias ft, defineArrayTypeAlias arrayType] ++
            depsForDeleteFunc typeEnv env insideType)

templatePushBack :: (String, Binder)
templatePushBack =
  let aTy = StructTy "Array" [VarTy "a"]
      valTy = VarTy "a"
  in  defineTemplate
      (SymPath ["Array"] "push-back")
      (FuncTy [aTy, valTy] aTy)
      (toTemplate "Array $NAME(Array a, $a value)")
      (toTemplate $ unlines
        ["$DECL { "
        ,"    a.len++;"
        -- ,"    void *pre = a.data;"
        -- ,"    a.data = CARP_MALLOC(sizeof($a) * a.len);"
        -- ,"    CARP_FREE(pre);"
        ,"    a.data = realloc(a.data, sizeof($a) * a.len);"
        ,"    (($a*)a.data)[a.len - 1] = value;"
        ,"    return a;"
        ,"}"
        ])
      (\(FuncTy [arrayType, _] _) -> [defineArrayTypeAlias arrayType])

templatePopBack :: (String, Binder)
templatePopBack = defineTypeParameterizedTemplate templateCreator path t
  where path = SymPath ["Array"] "pop-back"
        aTy = StructTy "Array" [VarTy "a"]
        t = FuncTy [aTy] aTy
        templateCreator = TemplateCreator $
          \typeEnv env ->
            Template
            t
            (const (toTemplate "Array $NAME(Array a)"))
            (\(FuncTy [arrayType@(StructTy _ [insideTy])] _) ->
               let deleteElement = insideArrayDeletion typeEnv env insideTy
               in toTemplate (unlines
                               ["$DECL { "
                               ,"  a.len--;"
                               ,"  " ++ deleteElement "a.len"
                               ,"  a.data = realloc(a.data, sizeof($a) * a.len);"
                               ,"  void *pre = a.data;"
                               ,"  unsigned long s = sizeof($a) * a.len;"
                               ,"  a.data = CARP_MALLOC(s);"
                               ,"  memcpy(a.data, pre, s);"
                               ,"  CARP_FREE(pre);"
                               ,"  return a;"
                               ,"}"
                               ]))
            (\(FuncTy [arrayType@(StructTy _ [insideTy])] _) ->
               defineArrayTypeAlias arrayType :
               depsForDeleteFunc typeEnv env arrayType ++
               depsForCopyFunc typeEnv env insideTy
            )

templateNth :: (String, Binder)
templateNth =
  let t = VarTy "t"
  in defineTemplate
  (SymPath ["Array"] "nth")
  (FuncTy [RefTy (StructTy "Array" [t]), IntTy] (RefTy t))
  (toTemplate "$t* $NAME (Array *aRef, int n)")
  (toTemplate $ unlines ["$DECL {"
                        ,"    Array a = *aRef;"
                        ,"    assert(n >= 0);"
                        ,"    assert(n < a.len);"
                        ,"    return &((($t*)a.data)[n]);"
                        ,"}"])
  (\(FuncTy [(RefTy arrayType), _] _) ->
     [defineArrayTypeAlias arrayType])

templateSort :: (String, Binder)
templateSort = defineTypeParameterizedTemplate templateCreator path t
  where path = (SymPath ["Array"] "sort-with")
        vt = VarTy "t"
        t = (FuncTy [StructTy "Array" [vt], FuncTy [RefTy vt, RefTy vt] IntTy] (StructTy "Array" [vt]))
        templateCreator = TemplateCreator $
          \typeEnv env ->
            Template
            t
            (const (toTemplate "Array $NAME (Array a, $(Fn [(Ref t), (Ref t)] Int) f)"))
            (const (toTemplate $ unlines ["$DECL {"
                                         ,"    qsort(a.data, a.len, sizeof($t), (int(*)(const void*, const void*))f);"
                                         ,"    return a;"
                                         ,"}"]))
            (\(FuncTy [arrayType, sortType] _) ->
               [defineFunctionTypeAlias sortType
               ,defineArrayTypeAlias arrayType] ++
               depsForDeleteFunc typeEnv env arrayType)

templateReplicate :: (String, Binder)
templateReplicate = defineTypeParameterizedTemplate templateCreator path t
  where path = SymPath ["Array"] "replicate"
        t = FuncTy [IntTy, RefTy (VarTy "t")] (StructTy "Array" [VarTy "t"])
        templateCreator = TemplateCreator $
          \typeEnv env ->
             Template
             t
             (const (toTemplate "Array $NAME(int n, $t *elem)"))
             (\(FuncTy [_, _] arrayType) ->
                let StructTy _ [insideType] = arrayType
                    copierType = FuncTy [RefTy insideType] insideType
                    copierPath = if isManaged typeEnv insideType -- TODO: also check if it's an external function
                                 then case nameOfPolymorphicFunction typeEnv env copierType "copy" of
                                        Just p -> Just p
                                        Nothing -> error ("Can't find copy function for array type: " ++ show insideType)
                                 else Nothing
                in
                toTemplate $ unlines [ "$DECL {"
                        , "    Array a; a.len = n; a.data = CARP_MALLOC(sizeof($t) * n);"
                        , "    for(int i = 0; i < n; ++i) {"
                        , "      (($t*)a.data)[i] = " ++ case copierPath of
                                                           Just p -> pathToC p ++ "(elem);"
                                                           Nothing -> "*elem;"
                        , "    }"
                        , "    return a;"
                        , "}"])
             (\(FuncTy [_, _] arrayType) ->
                let StructTy _ [insideType] = arrayType
                in defineArrayTypeAlias arrayType :
                   depsForDeleteFunc typeEnv env arrayType ++
                   depsForCopyFunc typeEnv env insideType)

templateRepeat :: (String, Binder)
templateRepeat = defineTypeParameterizedTemplate templateCreator path t
  where path = SymPath ["Array"] "repeat"
        t = FuncTy [IntTy, FuncTy [] (VarTy "t")] (StructTy "Array" [VarTy "t"])
        templateCreator = TemplateCreator $
          \typeEnv env ->
             Template
             t
             (const (toTemplate "Array $NAME(int n, $(Fn [] t) f)"))
             (const
                (toTemplate $ unlines
                  [ "$DECL {"
                  , "    Array a; a.len = n; a.data = CARP_MALLOC(sizeof($t) * n);"
                  , "    for(int i = 0; i < n; ++i) {"
                  , "      (($t*)a.data)[i] = f();"
                  , "    }"
                  , "    return a;"
                  , "}"]))
             (\(FuncTy [_, ft] arrayType) ->
                let StructTy _ [insideType] = arrayType
                in  defineArrayTypeAlias arrayType : defineFunctionTypeAlias ft :
                    depsForDeleteFunc typeEnv env arrayType)


templateInitialize :: (String, Binder)
templateInitialize = defineTypeParameterizedTemplate templateCreator path t
  where path = SymPath ["Array"] "initialize"
        t = FuncTy [IntTy, FuncTy [IntTy] (VarTy "t")] (StructTy "Array" [VarTy "t"])
        templateCreator = TemplateCreator $
          \typeEnv env ->
             Template
             t
             (const (toTemplate "Array $NAME(int n, $(Fn [int] t) f)"))
             (const
                (toTemplate $ unlines
                  [ "$DECL {"
                  , "    Array a; a.len = n; a.data = CARP_MALLOC(sizeof($t) * n);"
                  , "    for(int i = 0; i < n; ++i) {"
                  , "      (($t*)a.data)[i] = f(i);"
                  , "    }"
                  , "    return a;"
                  , "}"]))
             (\(FuncTy [_, ft] arrayType) ->
                let StructTy _ [insideType] = arrayType
                in  defineArrayTypeAlias arrayType : defineFunctionTypeAlias ft :
                    depsForDeleteFunc typeEnv env arrayType)


templateRaw :: (String, Binder)
templateRaw = defineTemplate
  (SymPath ["Array"] "raw")
  (FuncTy [StructTy "Array" [VarTy "t"]] (PointerTy (VarTy "t")))
  (toTemplate "$t* $NAME (Array a)")
  (toTemplate "$DECL { return a.data; }")
  (\(FuncTy [arrayType] _) -> [defineArrayTypeAlias arrayType])

templateAset :: (String, Binder)
templateAset = defineTemplate
  (SymPath ["Array"] "aset")
  (FuncTy [StructTy "Array" [VarTy "t"], IntTy, VarTy "t"] (StructTy "Array" [VarTy "t"]))
  (toTemplate "Array $NAME (Array a, int n, $t newValue)")
  (toTemplate $ unlines ["$DECL {"
                        ,"    assert(n >= 0);"
                        ,"    assert(n < a.len);"
                        ,"    (($t*)a.data)[n] = newValue;"
                        ,"    return a;"
                        ,"}"])
  (\(FuncTy [arrayType, _, _] _) -> [defineArrayTypeAlias arrayType])

templateAsetBang :: (String, Binder)
templateAsetBang = defineTypeParameterizedTemplate templateCreator path t
  where path = (SymPath ["Array"] "aset!")
        t = (FuncTy [RefTy (StructTy "Array" [VarTy "t"]), IntTy, VarTy "t"] UnitTy)
        templateCreator = TemplateCreator $
          \typeEnv env ->
            Template
            t
            (const (toTemplate "void $NAME (Array *aRef, int n, $t newValue)"))
            (const (toTemplate $ unlines ["$DECL {"
                                         ,"    Array a = *aRef;"
                                         ,"    assert(n >= 0);"
                                         ,"    assert(n < a.len);"
                                         ,"    (($t*)a.data)[n] = newValue;"
                                         ,"}"]))
            (\(FuncTy [(RefTy arrayType), _, _] _) ->
               [defineArrayTypeAlias arrayType] ++
               depsForDeleteFunc typeEnv env arrayType)

templateCount :: (String, Binder)
templateCount = defineTypeParameterizedTemplate templateCreator path t
  where path = (SymPath ["Array"] "count")
        t = (FuncTy [RefTy (StructTy "Array" [VarTy "t"])] IntTy)
        templateCreator = TemplateCreator $
          \typeEnv env ->
            Template
            t
            (const (toTemplate "int $NAME (Array *a)"))
            (const (toTemplate "$DECL { return (*a).len; }"))
            (\(FuncTy [(RefTy arrayType)] _) ->
               [defineArrayTypeAlias arrayType] ++
              depsForDeleteFunc typeEnv env arrayType)

templateDeleteArray :: (String, Binder)
templateDeleteArray = defineTypeParameterizedTemplate templateCreator path t
  where templateCreator = TemplateCreator $
          \typeEnv env ->
             Template
             t
             (const (toTemplate "void $NAME (Array a)"))
             (\(FuncTy [arrayType] UnitTy) ->
                [TokDecl, TokC "{\n"] ++
                deleteTy typeEnv env arrayType ++
                [TokC "}\n"])
             (\(FuncTy [arrayType@(StructTy "Array" [insideType])] UnitTy) ->
                defineArrayTypeAlias arrayType : depsForDeleteFunc typeEnv env insideType)
        path = SymPath ["Array"] "delete"
        t = FuncTy [StructTy "Array" [VarTy "a"]] UnitTy

deleteTy :: TypeEnv -> Env -> Ty -> [Token]
deleteTy typeEnv env (StructTy "Array" [innerType]) =
  [ TokC   "    for(int i = 0; i < a.len; i++) {\n"
  , TokC $ "    " ++ insideArrayDeletion typeEnv env innerType "i"
  , TokC   "    }\n"
  , TokC   "    CARP_FREE(a.data);\n"
  ]
deleteTy _ _ _ = []

insideArrayDeletion :: TypeEnv -> Env -> Ty -> String -> String
insideArrayDeletion typeEnv env t indexer =
  case findFunctionForMember typeEnv env "delete" (typesDeleterFunctionType t) ("Inside array.", t) of
    FunctionFound functionFullName ->
      "    " ++ functionFullName ++ "(((" ++ tyToC t ++ "*)a.data)[" ++ indexer ++ "]);\n"
    FunctionNotFound msg -> error msg
    FunctionIgnored -> "    /* Ignore non-managed type inside Array: '" ++ show t ++ "' */\n"

templateCopyArray :: (String, Binder)
templateCopyArray = defineTypeParameterizedTemplate templateCreator path t
  where path = SymPath ["Array"] "copy"
        t = FuncTy [RefTy (StructTy "Array" [VarTy "a"])] (StructTy "Array" [VarTy "a"])
        templateCreator = TemplateCreator $
          \typeEnv env ->
             Template
             t
             (const (toTemplate "Array $NAME (Array* a)"))
             (\(FuncTy [RefTy arrayType] _) ->
                [TokDecl, TokC "{\n"] ++
                [TokC "    Array copy;\n"] ++
                [TokC "    copy.len = a->len;\n"] ++
                [TokC "    copy.data = CARP_MALLOC(sizeof(", TokTy (VarTy "a"), TokC ") * a->len);\n"] ++
                copyTy typeEnv env arrayType ++
                [TokC "    return copy;\n"] ++
                [TokC "}\n"])
             (\case
                 (FuncTy [RefTy arrayType@(StructTy "Array" [insideType])] _) ->
                   defineArrayTypeAlias arrayType :
                   depsForCopyFunc typeEnv env insideType ++
                   depsForDeleteFunc typeEnv env arrayType
                 err ->
                   error ("CAN'T MATCH: " ++ show err))

copyTy :: TypeEnv -> Env -> Ty -> [Token]
copyTy typeEnv env (StructTy "Array" [innerType]) =
  [ TokC   "    for(int i = 0; i < a->len; i++) {\n"
  , TokC $ "    " ++ insideArrayCopying typeEnv env innerType
  , TokC   "    }\n"
  ]
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
templateStrArray = defineTypeParameterizedTemplate templateCreator path t
  where templateCreator = TemplateCreator $
          \typeEnv env ->
             Template
             t
             (const (toTemplate "string $NAME (Array* a)"))
             (\(FuncTy [RefTy arrayType] StringTy) ->
                [TokDecl, TokC " {\n"] ++
                strTy typeEnv env arrayType ++
                [TokC "}\n"])
             (\(FuncTy [RefTy arrayType@(StructTy "Array" [insideType])] StringTy) ->
                let deps = depsForStrFunc typeEnv env insideType
                in  defineArrayTypeAlias arrayType : deps)
        path = SymPath ["Array"] "str"
        t = FuncTy [RefTy (StructTy "Array" [VarTy "a"])] StringTy

-- | TODO: move this into the templateStrArray function?
strTy :: TypeEnv -> Env -> Ty -> [Token]
strTy typeEnv env (StructTy "Array" [innerType]) =
  [ TokC   ""
  , TokC   "  string temp = NULL;\n"
  , TokC $ calculateStrSize typeEnv env innerType
  , TokC   "  string buffer = CARP_MALLOC(size);\n"
  , TokC   "  string bufferPtr = buffer;\n"
  , TokC   "\n"
  , TokC   "  snprintf(buffer, size, \"[\");\n"
  , TokC   "  bufferPtr += 1;\n"
  , TokC   "\n"
  , TokC   "  for(int i = 0; i < a->len; i++) {\n"
  , TokC $ "  " ++ insideArrayStr typeEnv env innerType
  , TokC   "  }\n"
  , TokC   "\n"
  , TokC   "  if(a->len > 0) { bufferPtr -= 1; }\n"
  , TokC   "  snprintf(bufferPtr, size, \"]\");\n"
  , TokC   "  return buffer;\n"
  ]
strTy _ _ _ = []

calculateStrSize :: TypeEnv -> Env -> Ty -> String
calculateStrSize typeEnv env t =
  unlines [ "  int size = 3; // opening and closing brackets and terminator"
          , "  for(int i = 0; i < a->len; i++) {"
          , arrayMemberSizeCalc ++ "  }"
          , ""
          ]
  where arrayMemberSizeCalc =
          case findFunctionForMemberIncludePrimitives typeEnv env "str" (typesStrFunctionType typeEnv t) ("Inside array.", t) of
              FunctionFound functionFullName ->
                let takeAddressOrNot = if isManaged typeEnv t then "&" else ""
                in  unlines [ "    temp = " ++ functionFullName ++ "(" ++ takeAddressOrNot ++ "((" ++ tyToC t ++ "*)a->data)[i]);"
                            , "    size += snprintf(NULL, 0, \"%s \", temp);"
                            , "    if(temp) {"
                            , "      CARP_FREE(temp);"
                            , "      temp = NULL;"
                            , "    }"
                            ]
              FunctionNotFound msg -> error msg
              FunctionIgnored -> "    /* Ignore type inside Array: '" ++ show t ++ "' ??? */\n"


insideArrayStr :: TypeEnv -> Env -> Ty -> String
insideArrayStr typeEnv env t =
  case findFunctionForMemberIncludePrimitives typeEnv env "str" (typesStrFunctionType typeEnv t) ("Inside array.", t) of
    FunctionFound functionFullName ->
      let takeAddressOrNot = if isManaged typeEnv t then "&" else ""
      in  unlines [ "  temp = " ++ functionFullName ++ "(" ++ takeAddressOrNot ++ "((" ++ tyToC t ++ "*)a->data)[i]);"
                  , "    snprintf(bufferPtr, size, \"%s \", temp);"
                  , "    bufferPtr += strlen(temp) + 1;"
                  , "    if(temp) {"
                  , "      CARP_FREE(temp);"
                  , "      temp = NULL;"
                  , "    }"
                  ]
    FunctionNotFound msg -> error msg
    FunctionIgnored -> "    /* Ignore type inside Array: '" ++ show t ++ "' ??? */\n"

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
  where fTy = FuncTy [VarTy "a"] (VarTy "b")
        aTy = StructTy "Array" [VarTy "a"]
        bTy = StructTy "Array" [VarTy "b"]
        path = SymPath ["Array"] "transform"
        t = FuncTy [fTy, aTy] bTy
        templateCreator = TemplateCreator $
          \typeEnv env -> 
            Template
            t
            (const (toTemplate "Array $NAME($(Fn [a] b) f, Array a)"))
            (\(FuncTy [(FuncTy [_] _), _] _) ->
               (toTemplate $ unlines $                
                  [ "$DECL { "
                  , "    Array b;"
                  , "    b.len = a.len;"
                  , "    b.data = CARP_MALLOC(sizeof($b) * a.len);"
                  , "    for(int i = 0; i < a.len; ++i) {"
                  , "        (($b*)b.data)[i] = f((($a*)a.data)[i]); "
                  , "    }"
                  , "    CARP_FREE(a.data);"
                  , "    return b;"
                  , "}"
                  ]))
            (\(FuncTy [ft@(FuncTy [insideTypeA] _), arrayTypeA] arrayTypeB) ->
               [defineFunctionTypeAlias ft, defineArrayTypeAlias arrayTypeA, defineArrayTypeAlias arrayTypeB] ++
                depsForDeleteFunc typeEnv env insideTypeA)

-- | "Endofunctor Map"
templateEMap :: (String, Binder)
templateEMap = 
  let fTy = FuncTy [VarTy "a"] (VarTy "a")
      aTy = StructTy "Array" [VarTy "a"]
      bTy = StructTy "Array" [VarTy "a"]
  in  defineTemplate
      (SymPath ["Array"] "map")
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
      (\(FuncTy [t, arrayType] _) -> [defineFunctionTypeAlias t, defineArrayTypeAlias arrayType])

templateFilter :: (String, Binder)
templateFilter = defineTypeParameterizedTemplate templateCreator path t
  where
    fTy = FuncTy [VarTy "a"] BoolTy
    aTy = StructTy "Array" [VarTy "a"]
    path = SymPath ["Array"] "filter"
    t = (FuncTy [fTy, aTy] aTy)
    templateCreator = TemplateCreator $
      \typeEnv env ->
        Template
        t
        (const (toTemplate "Array $NAME($(Fn [a] Bool) predicate, Array a)"))
        (\(FuncTy [(FuncTy [insideTy] BoolTy), _] _) ->
           (toTemplate $ unlines $
            let deleter = insideArrayDeletion typeEnv env insideTy
            in ["$DECL { "
               , "    int insertIndex = 0;"
               , "    for(int i = 0; i < a.len; ++i) {"
               , "        if(predicate((($a*)a.data)[i])) {"
               , "            ((($a*)a.data)[insertIndex++]) = (($a*)a.data)[i];"
               , "        } else {"
               , "        " ++ deleter
               , "        }"
               , "    }"
               , "    a.len = insertIndex;"
               , "    // NOTE: the array isn't resized for now, it probably should be?"
               , "    return a;"
               , "}"
               ]))
        (\(FuncTy [ft@(FuncTy [insideType] BoolTy), arrayType] _) ->
           [defineFunctionTypeAlias ft, defineArrayTypeAlias arrayType] ++
            depsForDeleteFunc typeEnv env insideType)

templateReduce :: (String, Binder)
templateReduce = defineTypeParameterizedTemplate templateCreator path t
  where
    fTy = FuncTy [bTy, aTy] bTy
    arrTy = StructTy "Array" [aTy]
    aTy = VarTy "a"
    bTy = VarTy "b"
    path = SymPath ["Array"] "reduce"
    t = (FuncTy [fTy, bTy, arrTy] bTy)
    templateCreator = TemplateCreator $
      \_ _ ->
        Template
        t
        (const (toTemplate "$b $NAME($(Fn [b a] a) f, $b initial_value, Array a)"))
        (\(FuncTy [(FuncTy [_, _] _), _, _] _) ->
           (toTemplate $ unlines $
               [ "$DECL { "
               , "    $b b = initial_value;"
               , "    for(int i = 0; i < a.len; ++i) {"
               , "        b = f(b, (($a*)a.data)[i]);"
               , "    }"
               , "    CARP_FREE(a.data); // Can't call Array_delete since it will destroy the items that have been handed off to f()."
               , "    return b;"
               , "}"
               ]))
        (\(FuncTy [ft@(FuncTy [_, _] _), _, arrayType] _) ->
           [defineFunctionTypeAlias ft, defineArrayTypeAlias arrayType])

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
        ,"    void *pre = a.data;"
        ,"    a.data = CARP_MALLOC(sizeof($a) * a.len);"
        ,"    CARP_FREE(pre);"
         --a.data = realloc(a.data, sizeof($a) * a.len);"
        ,"    (($a*)a.data)[a.len - 1] = value;"
        ,"    return a;"
        ,"}"
        ])
      (\(FuncTy [arrayType, _] _) -> [defineArrayTypeAlias arrayType])

templatePopBack :: (String, Binder)
templatePopBack = 
  let aTy = StructTy "Array" [VarTy "a"]
  in  defineTemplate
      (SymPath ["Array"] "pop-back")
      (FuncTy [aTy] aTy)
      (toTemplate "Array $NAME(Array a)")
      (toTemplate $ unlines
        ["$DECL { "
        ,"    a.len--;"
        ,"    if(a.len > 0) {"
        --,"        a.data = realloc(a.data, sizeof($a) * a.len);"
        ,"        void *pre = a.data;"
        ,"        a.data = CARP_MALLOC(sizeof($a) * a.len);"
        ,"        CARP_FREE(pre);"
        ,"    }"
        ,"    return a;"
        ,"}"
        ])
      (\(FuncTy [arrayType] _) -> [defineArrayTypeAlias arrayType])

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
  (\(FuncTy [arrayType, _] _) -> [defineArrayTypeAlias arrayType])
  
templateReplicate :: (String, Binder)
templateReplicate = defineTypeParameterizedTemplate templateCreator path t
  where path = SymPath ["Array"] "replicate"
        t = FuncTy [IntTy, (RefTy (VarTy "t"))] (StructTy "Array" [VarTy "t"])
        templateCreator = TemplateCreator $
          \typeEnv env ->
             Template
             t
             (const (toTemplate "Array $NAME(int n, $t *elem)"))
             (\(FuncTy [_, _] arrayType) ->
                let StructTy _ [insideType] = arrayType
                    copierType = (FuncTy [(RefTy insideType)] insideType)
                    copierPath = if isManaged typeEnv insideType -- TODO: also check if it's an external function
                                 then case nameOfPolymorphicFunction typeEnv env copierType "copy" of
                                        Just p -> Just p
                                        Nothing -> error ("Can't find copy function for array type: " ++ show insideType)
                                 else Nothing
                in
                (toTemplate $ unlines [ "$DECL {"
                        , "    Array a; a.len = n; a.data = CARP_MALLOC(sizeof($t) * n);"
                        , "    for(int i = 0; i < n; ++i) {"
                        , "      (($t*)a.data)[i] = " ++ case copierPath of
                                                           Just p -> pathToC p ++ "(elem);"
                                                           Nothing -> "*elem;"
                        , "    }"
                        , "    return a;"
                        , "}"]))
             (\(FuncTy [_, _] arrayType) ->
                let StructTy _ [insideType] = arrayType
                in defineArrayTypeAlias arrayType :
                   depsForDeleteFunc typeEnv env arrayType ++
                   depsForCopyFunc typeEnv env insideType)

templateRepeat :: (String, Binder)
templateRepeat = defineTypeParameterizedTemplate templateCreator path t
  where path = SymPath ["Array"] "repeat"
        t = (FuncTy [IntTy, (FuncTy [] (VarTy "t"))] (StructTy "Array" [VarTy "t"]))
        templateCreator = TemplateCreator $
          \typeEnv env ->
             Template
             t
             (const (toTemplate "Array $NAME(int n, $(Fn [] t) f)"))
             (\_ ->
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
templateAsetBang = defineTemplate
  (SymPath ["Array"] "aset!")
  (FuncTy [RefTy (StructTy "Array" [VarTy "t"]), IntTy, VarTy "t"] UnitTy)
  (toTemplate "void $NAME (Array *aRef, int n, $t newValue)")
  (toTemplate $ unlines ["$DECL {"
                        ,"    Array a = *aRef;"
                        ,"    assert(n >= 0);"
                        ,"    assert(n < a.len);"
                        ,"    (($t*)a.data)[n] = newValue;"
                        ,"}"])
  (\(FuncTy [arrayType, _, _] _) -> [defineArrayTypeAlias arrayType])
  
templateCount :: (String, Binder)
templateCount = defineTemplate
  (SymPath ["Array"] "count")
  (FuncTy [RefTy (StructTy "Array" [VarTy "t"])] IntTy)
  (toTemplate "int $NAME (Array *a)")
  (toTemplate "$DECL { return (*a).len; }")
  (\(FuncTy [arrayType] _) -> [defineArrayTypeAlias arrayType])

-- templateRange :: (String, Binder)
-- templateRange = defineTemplate
--   (SymPath ["Array"] "range")
--   (FuncTy [(VarTy "t"), (VarTy "t")] (StructTy "Array" [(VarTy "t")]))
--   (toTemplate "Array $NAME ($t start, $t end)")
--   (toTemplate (unlines [ "$DECL { "
--                        , "    assert(end > start);"
--                        , "    int length = end - start;"
--                        , "    Array a = { .len = length, .data = malloc(sizeof($t) * length) };"
--                        , "    for(int i = 0; i < length; i++) {"
--                        , "        (($t*)a.data)[i] = start + ($t)i;"
--                        , "    }"
--                        , "    return a;"
--                        , "}"]))
--   (\(FuncTy [t, _] _) -> [defineArrayTypeAlias (StructTy "Array" [t])] ++ )
  
templateDeleteArray :: (String, Binder)
templateDeleteArray = defineTypeParameterizedTemplate templateCreator path t
  where templateCreator = TemplateCreator $
          \typeEnv env ->
             Template
             t
             (const (toTemplate "void $NAME (Array a)"))
             (\(FuncTy [arrayType] UnitTy) ->
                [TokDecl, TokC "{\n"] ++
                (deleteTy typeEnv env arrayType) ++
                [TokC "}\n"])
             (\(FuncTy [arrayType@(StructTy "Array" [insideType])] UnitTy) ->
                defineArrayTypeAlias arrayType : depsForDeleteFunc typeEnv env insideType)
        path = SymPath ["Array"] "delete"
        t = (FuncTy [(StructTy "Array" [VarTy "a"])] UnitTy)
        
deleteTy :: TypeEnv -> Env -> Ty -> [Token]
deleteTy typeEnv env (StructTy "Array" [innerType]) =
  [ TokC   "    for(int i = 0; i < a.len; i++) {\n"
  , TokC $ "    " ++ insideArrayDeletion typeEnv env innerType
  , TokC   "    }\n"
  , TokC   "    CARP_FREE(a.data);\n"
  ]
deleteTy _ _ _ = []

insideArrayDeletion :: TypeEnv -> Env -> Ty -> String
insideArrayDeletion typeEnv env t =
  case findFunctionForMember typeEnv env "delete" (typesDeleterFunctionType t) ("Inside array.", t) of
    FunctionFound functionFullName ->
      "    " ++ functionFullName ++ "(((" ++ tyToC t ++ "*)a.data)[i]);\n"
    FunctionNotFound msg -> error msg
    FunctionIgnored -> "    /* Ignore non-managed type inside Array: '" ++ show t ++ "' */\n"
  
templateCopyArray :: (String, Binder)
templateCopyArray = defineTypeParameterizedTemplate templateCreator path t
  where templateCreator = TemplateCreator $
          \typeEnv env ->
             Template
             t
             (const (toTemplate "Array $NAME (Array* a)"))
             (\(FuncTy [(RefTy arrayType)] _) ->
                [TokDecl, TokC "{\n"] ++
                [TokC "    Array copy;\n"] ++
                [TokC "    copy.len = a->len;\n"] ++
                [TokC "    copy.data = CARP_MALLOC(sizeof(", TokTy (VarTy "a"), TokC ") * a->len);\n"] ++
                (copyTy typeEnv env arrayType) ++
                [TokC "    return copy;\n"] ++
                [TokC "}\n"])
             (\case
                 (FuncTy [(RefTy arrayType@(StructTy "Array" [insideType]))] _) ->
                   defineArrayTypeAlias arrayType :
                   depsForCopyFunc typeEnv env insideType
                 err ->
                   error ("CAN'T MATCH: " ++ (show err))
             )
        path = SymPath ["Array"] "copy"
        t = (FuncTy [(RefTy (StructTy "Array" [VarTy "a"]))] (StructTy "Array" [VarTy "a"]))
        
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
  case findFunctionForMember typeEnv env "copy" (typesCopyFunctionType t) ("Inside array.", t) of
    FunctionFound functionFullName ->
      "    ((" ++ tyToC t ++ "*)(copy.data))[i] = " ++ functionFullName ++ "(&(((" ++ tyToC t ++ "*)a->data)[i]));\n"
    FunctionNotFound msg -> error msg
    FunctionIgnored -> "    /* Ignore non-managed type inside Array: '" ++ show t ++ "' */\n"
  
templateStrArray :: (String, Binder)
templateStrArray = defineTypeParameterizedTemplate templateCreator path t
  where templateCreator = TemplateCreator $
          \typeEnv env ->
             Template
             t
             (const (toTemplate "string $NAME (Array* a)"))
             (\(FuncTy [(RefTy arrayType)] StringTy) ->
                [TokDecl, TokC "{\n"] ++
                (strTy typeEnv env arrayType) ++
                [TokC "}\n"])
             (\(FuncTy [(RefTy arrayType@(StructTy "Array" [insideType]))] StringTy) ->
                let deps = depsForStrFunc typeEnv env insideType
                in  defineArrayTypeAlias arrayType : deps)
        path = SymPath ["Array"] "str"
        t = (FuncTy [(RefTy (StructTy "Array" [VarTy "a"]))] StringTy)

-- | TODO: move this into the templateStrArray function?
strTy :: TypeEnv -> Env -> Ty -> [Token]
strTy typeEnv env (StructTy "Array" [innerType]) =
  [ TokC   ""
  , TokC   "  string buffer = CARP_MALLOC(1024);\n"
  , TokC   "  string bufferPtr = buffer;\n"
  , TokC   "  string temp = NULL;\n"
  , TokC   "\n"
  , TokC   "  snprintf(buffer, 1024, \"[\");\n"
  , TokC   "  bufferPtr += 1;\n"
  , TokC   "\n"
  , TokC   "  for(int i = 0; i < a->len; i++) {\n"
  , TokC $ "  " ++ insideArrayStr typeEnv env innerType
  , TokC   "  }\n"
  , TokC   "\n"
  , TokC   "  if(a->len > 0) { bufferPtr -= 1; }\n"
  , TokC   "  snprintf(bufferPtr, 1024, \"]\");\n"
  , TokC   "  return buffer;\n"
  ]
strTy _ _ _ = []

insideArrayStr :: TypeEnv -> Env -> Ty -> String
insideArrayStr typeEnv env t =
  case findFunctionForMemberIncludePrimitives typeEnv env "str" (typesStrFunctionType typeEnv t) ("Inside array.", t) of
    FunctionFound functionFullName ->
      let takeAddressOrNot = if isManaged typeEnv t then "&" else ""
      in  unlines [ "  temp = " ++ functionFullName ++ "(" ++ takeAddressOrNot ++ "((" ++ tyToC t ++ "*)a->data)[i]);"
                  , "  snprintf(bufferPtr, 1024, \"%s \", temp);"
                  , "  bufferPtr += strlen(temp) + 1;"
                  , "  if(temp) { CARP_FREE(temp); temp = NULL; }"
                  ]
    FunctionNotFound msg -> error msg
    FunctionIgnored -> "    /* Ignore type inside Array: '" ++ show t ++ "' ??? */\n"

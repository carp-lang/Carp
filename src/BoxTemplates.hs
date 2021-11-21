-- | Module BoxTemplates defines Carp's Box type, a container for managed,
-- heap allocated objects.
module BoxTemplates
  ( delete,
    nil,
    str,
    prn,
    BoxTemplates.init,
    getter,
    copy,
    unbox,
  )
where

import Concretize
import Obj
import Polymorphism
import Template
import ToTemplate
import Types
import TypesToC

boxTy :: Ty
boxTy = StructTy (ConcreteNameTy (SymPath [] "Box")) [(VarTy "t")]

nil :: (String, Binder)
nil = defineTypeParameterizedTemplate templateCreator path t docs
  where
    path = SymPath ["Box"] "nil"
    t = FuncTy [] boxTy StaticLifetimeTy
    docs = "Initializes a box pointing to nothing."
    templateCreator = TemplateCreator $
      \typeEnv env ->
        Template
          t
          (const (toTemplate "Box__$t $NAME ()"))
          ( \(FuncTy _ _ _) ->
              toTemplate $
                unlines
                  [ "$DECL {",
                    "  Box__$t box;",
                    "  box.data = NULL;",
                    "  return box;",
                    "}"
                  ]
          )
          ( \(FuncTy _ boxT _) ->
              depsForDeleteFunc typeEnv env boxT
          )

init :: (String, Binder)
init = defineTypeParameterizedTemplate templateCreator path t docs
  where
    path = SymPath ["Box"] "init"
    t = FuncTy [(VarTy "t")] boxTy StaticLifetimeTy
    docs = "Initializes a box pointing to value t."
    templateCreator = TemplateCreator $
      \_ _ ->
        Template
          t
          (templateLiteral "Box__$t $NAME ($t t)")
          ( \_ ->
              multilineTemplate
                [ "$DECL {",
                  "  Box__$t instance;",
                  "  instance.data = CARP_MALLOC(sizeof($t));",
                  "  *instance.data = t;",
                  "  return instance;",
                  "}"
                ]
          )
          (\_ -> [])

getter :: (String, Binder)
getter = defineTypeParameterizedTemplate templateCreator path t docs
  where
    path = SymPath ["Box"] "deref"
    t = FuncTy [(StructTy (ConcreteNameTy (SymPath [] "Box")) [(VarTy "t")])] (VarTy "t") StaticLifetimeTy
    docs = "Gets the value from a box and deletes the box."
    templateCreator = TemplateCreator $
      \_ _ ->
        Template
          t
          (templateLiteral "$t $NAME (Box__$t box)")
          ( \_ ->
              multilineTemplate
                [ "$DECL {",
                  "  return *box.data;",
                  "}"
                ]
          )
          (\_ -> [])

unbox :: (String, Binder)
unbox = defineTypeParameterizedTemplate templateCreator path t docs
  where
    path = SymPath ["Box"] "unbox"
    t = FuncTy [(RefTy (StructTy (ConcreteNameTy (SymPath [] "Box")) [(VarTy "t")]) (VarTy "q"))] (RefTy (VarTy "t") (VarTy "q")) StaticLifetimeTy
    docs = "Convert a box to a ref and delete the box."
    templateCreator = TemplateCreator $
      \_ _ ->
        Template
          t
          (templateLiteral "$t* $NAME(Box__$t* box)")
          ( \_ ->
              multilineTemplate
                [ "$DECL {",
                  "  return box->data;",
                  "}"
                ]
          )
          (\_ -> [])

copy :: (String, Binder)
copy = defineTypeParameterizedTemplate templateCreator path t docs
  where
    path = SymPath ["Box"] "copy"
    t = FuncTy [(RefTy (StructTy (ConcreteNameTy (SymPath [] "Box")) [(VarTy "t")]) (VarTy "q"))] (StructTy (ConcreteNameTy (SymPath [] "Box")) [(VarTy "t")]) StaticLifetimeTy
    docs = "copies a box."
    templateCreator = TemplateCreator $
      \tenv env ->
        Template
          t
          (templateLiteral "Box__$t $NAME (Box__$t* box)")
          ( \(FuncTy [RefTy (StructTy (ConcreteNameTy (SymPath [] "Box")) [inner]) _] _ _) ->
              innerCopy tenv env inner
          )
          ( \(FuncTy [RefTy boxType@(StructTy (ConcreteNameTy (SymPath [] "Box")) [inner]) _] _ _) ->
              depsForCopyFunc tenv env inner
                ++ depsForDeleteFunc tenv env boxType
          )
    innerCopy typeEnv valEnv innerTy =
      case findFunctionForMemberIncludePrimitives typeEnv valEnv "copy" (typesCopyFunctionType innerTy) ("Inside box.", innerTy) of
        FunctionFound functionFullName ->
          multilineTemplate
            [ "$DECL {",
              "  Box__$t copy;",
              "  copy.data = CARP_MALLOC(sizeof($t));",
              "  if (box->data) {",
              "    *copy.data = " ++ functionFullName ++ "(box->data);\n",
              "  } else {",
              "    copy.data = NULL;",
              "  }",
              "  return copy;",
              "}"
            ]
        _ ->
          multilineTemplate
            [ "$DECL {",
              "  Box__$t copy;",
              "  copy.data = CARP_MALLOC(sizeof($t));",
              "  if (box->data) { ",
              "    *copy.data = *box->data;",
              "  } else {",
              "    copy.data = NULL;",
              "  }",
              "  return copy;",
              "}"
            ]

--FunctionIgnored ->
--  [ "$DECL {",
--      "  Box__$t copy;",
--      "  copy.data = CARP_MALLOC(sizeof($t));",
--      "  *copy.data = box->data;",
--      "  return copy;"
--  ]
--  "    /* Ignore type inside Array when copying: '" ++ show t ++ "' (no copy function known)*/\n"

prn :: (String, Binder)
prn = defineTypeParameterizedTemplate templateCreator path t docs
  where
    path = SymPath ["Box"] "prn"
    t = FuncTy [boxTy] StringTy StaticLifetimeTy
    docs = "Returns a string representation of a Box."
    templateCreator =
      TemplateCreator $
        ( \tenv env ->
            Template
              t
              (templateLiteral "String $NAME (Box__$t* box)")
              ( \(FuncTy [boxT] StringTy _) ->
                  multilineTemplate
                    [ "$DECL {",
                      "  if(!box->data){",
                      "    String buffer = CARP_MALLOC(4);",
                      "    sprintf(buffer, \"Nil\");",
                      "    return buffer;",
                      "  }",
                      innerStr tenv env boxT,
                      "  return buffer;",
                      "}"
                    ]
              )
              ( \(FuncTy [(StructTy (ConcreteNameTy (SymPath [] "Box")) [inner])] StringTy _) ->
                  depsForPrnFunc tenv env inner
              )
        )

str :: (String, Binder)
str = defineTypeParameterizedTemplate templateCreator path t docs
  where
    path = SymPath ["Box"] "str"
    t = FuncTy [(RefTy boxTy (VarTy "q"))] StringTy StaticLifetimeTy
    docs = "Returns a string representation of a Box."
    templateCreator =
      TemplateCreator $
        ( \tenv env ->
            Template
              t
              (templateLiteral "String $NAME (Box__$t* box)")
              ( \(FuncTy [RefTy boxT _] StringTy _) ->
                  multilineTemplate
                    [ "$DECL {",
                      "  if(!box->data){",
                      "    String buffer = CARP_MALLOC(4);",
                      "    sprintf(buffer, \"Nil\");",
                      "    return buffer;",
                      "  }",
                      innerStr tenv env boxT,
                      "  return buffer;",
                      "}"
                    ]
              )
              ( \(FuncTy [RefTy (StructTy (ConcreteNameTy (SymPath [] "Box")) [inner]) _] StringTy _) ->
                  depsForPrnFunc tenv env inner
              )
        )

innerStr :: TypeEnv -> Env -> Ty -> String
innerStr tenv env (StructTy _ [t]) =
  case findFunctionForMemberIncludePrimitives tenv env "prn" (typesStrFunctionType tenv env (RefTy t StaticLifetimeTy)) ("Inside box.", t) of
    FunctionFound functionFullName ->
      unlines
        [ "  char* temp = " ++ functionFullName ++ "(box->data);",
          "  int size = snprintf(NULL, 0, \"(Box %s)\", temp);",
          "  String buffer = CARP_MALLOC(size);",
          "  sprintf(buffer, \"(Box %s)\", temp);",
          "  if(temp) {",
          "    CARP_FREE(temp);",
          "    temp = NULL;",
          "  }"
        ]
    FunctionNotFound _ ->
      unlines
        [ "  String buffer = CARP_MALLOC(14);",
          "  sprintf(buffer, \"(Box unknown)\");"
        ]
    FunctionIgnored -> "    /* Ignore type inside Box: '" ++ show t ++ "' ??? */\n"
innerStr _ _ _ = ""

delete :: (String, Binder)
delete = defineTypeParameterizedTemplate templateCreator path t docs
  where
    path = SymPath ["Box"] "delete"
    t = FuncTy [boxTy] UnitTy StaticLifetimeTy
    docs = "Deletes a box, freeing its associated memory."
    templateCreator = TemplateCreator $
      \tenv env ->
        Template
          t
          (const (toTemplate "void $NAME (Box__$t box)"))
          ( \(FuncTy [bTy] UnitTy _) ->
              toTemplate $
                unlines
                  [ "$DECL {",
                    innerDelete tenv env bTy,
                    "}"
                  ]
          )
          ( \(FuncTy [StructTy (ConcreteNameTy (SymPath [] "Box")) [insideType]] UnitTy _) ->
              depsForDeleteFunc tenv env insideType
          )

innerDelete :: TypeEnv -> Env -> Ty -> String
innerDelete tenv env (StructTy (ConcreteNameTy (SymPath [] "Box")) [inner]) =
  case findFunctionForMember tenv env "delete" (typesDeleterFunctionType inner) ("Inside box.", inner) of
    FunctionFound functionFullName ->
      "    if(box.data){\n"
        ++ "      "
        ++ functionFullName
        ++ "((("
        ++ tyToCLambdaFix inner
        ++ "*)box.data));\n"
        ++ "      CARP_FREE(box.data);"
        ++ "    }\n"
    FunctionNotFound msg -> error msg
    FunctionIgnored ->
      "    /* Ignore non-managed type inside Box: '" ++ show inner ++ "' */\n"
        ++ "    if(box.data){\n"
        ++ "      CARP_FREE(box.data);"
        ++ "    }\n"
innerDelete _ _ _ = ""

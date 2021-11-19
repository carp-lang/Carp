-- | Module BoxTemplates defines Carp's Box type, a container for managed,
-- heap allocated objects.
module BoxTemplates
  ( delete,
    str,
    prn,
    BoxTemplates.init,
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

boxTy :: Ty
boxTy = StructTy (ConcreteNameTy (SymPath [] "Box")) [(VarTy "t")]

-- | Defines a template for initializing Boxes.
init :: (String, Binder)
init = let path = SymPath ["Box"] "init"
           t = FuncTy [(VarTy "t")] boxTy StaticLifetimeTy
           docs = "Initializes a box pointing to value t."
           decl = templateLiteral "$t* $NAME ($t t)"
           body = const (multilineTemplate
                                  [ "$DECL {",
                                    "  $t* instance;",
                                    "  instance = CARP_MALLOC(sizeof($t));",
                                    "  *instance = t;",
                                    "  return instance;",
                                    "}"
                                  ])
           deps = const []
           template = TemplateCreator $ \_ _ -> Template t decl body deps
        in defineTypeParameterizedTemplate template path t docs

-- | Defines a template for converting a boxed value to a local value.
unbox :: (String, Binder)
unbox = let path = SymPath ["Box"] "unbox"
            t = FuncTy [(StructTy (ConcreteNameTy (SymPath [] "Box")) [(VarTy "t")])] (VarTy "t") StaticLifetimeTy
            docs = "Converts a boxed value to a reference to the value and delete the box."
            decl = templateLiteral "$t $NAME($t* box)"
            body = const (multilineTemplate
                           [ "$DECL {",
                             "  $t local;",
                             "  local = *box;",
                             "  CARP_FREE(box);",
                             "  return local;",
                             "}"
                           ])
            deps = const []
            template = TemplateCreator $ \_ _ -> Template t decl body deps
         in defineTypeParameterizedTemplate template path t docs

-- | Defines a template for copying a box. The copy will also be heap allocated.
copy :: (String, Binder)
copy =
  let path = SymPath ["Box"] "copy"
      t = FuncTy [(RefTy (StructTy (ConcreteNameTy (SymPath [] "Box")) [(VarTy "t")]) (VarTy "q"))] (StructTy (ConcreteNameTy (SymPath [] "Box")) [(VarTy "t")]) StaticLifetimeTy
      docs = "Copies a box."
      decl = (templateLiteral "$t* $NAME ($t** box)")
      template = TemplateCreator $
        \tenv env ->
          Template
            t
            decl
            ( \(FuncTy [RefTy (StructTy (ConcreteNameTy (SymPath [] "Box")) [inner]) _] _ _) ->
                innerCopy tenv env inner
            )
            ( \(FuncTy [RefTy boxType@(StructTy (ConcreteNameTy (SymPath [] "Box")) [inner]) _] _ _) ->
                depsForCopyFunc tenv env inner
                  ++ depsForDeleteFunc tenv env boxType
            )
   in defineTypeParameterizedTemplate template path t docs
  where
    innerCopy typeEnv valEnv innerTy =
      case findFunctionForMemberIncludePrimitives typeEnv valEnv "copy" (typesCopyFunctionType innerTy) ("Inside box.", innerTy) of
        FunctionFound functionFullName ->
          multilineTemplate
            [ "$DECL {",
              "  $t* copy;",
              "  copy = CARP_MALLOC(sizeof($t));",
              "  *copy = " ++ functionFullName ++ "(*box);",
              "  return copy;",
              "}"
            ]
        _ ->
          multilineTemplate
            [ "$DECL {",
              "  $t* copy;",
              "  copy = CARP_MALLOC(sizeof($t));",
              "  *copy = *box;",
              "  return copy;",
              "}"
            ]

-- | Defines a template for deleting a box.
delete :: (String, Binder)
delete =
  let path = SymPath ["Box"] "delete"
      t = FuncTy [boxTy] UnitTy StaticLifetimeTy
      docs = "Deletes a box, freeing its associated memory."
      decl = (templateLiteral "void $NAME ($t* box)")
      templateCreator = TemplateCreator $
        \tenv env ->
          Template
            t
            decl
            ( \(FuncTy [bTy] UnitTy _) ->
                multilineTemplate
                  [ "$DECL {",
                    "  " ++ innerDelete tenv env bTy,
                    "}"
                  ]
            )
            ( \(FuncTy [StructTy (ConcreteNameTy (SymPath [] "Box")) [insideType]] UnitTy _) ->
                depsForDeleteFunc tenv env insideType
            )
   in defineTypeParameterizedTemplate templateCreator path t docs
  where
    innerDelete :: TypeEnv -> Env -> Ty -> String
    innerDelete tenv env (StructTy (ConcreteNameTy (SymPath [] "Box")) [inner]) =
      case findFunctionForMember tenv env "delete" (typesDeleterFunctionType inner) ("Inside box.", inner) of
        FunctionFound functionFullName ->
            "  " ++ functionFullName ++ "(*box);\n"
            ++ "  CARP_FREE(box);"
        FunctionNotFound msg -> error msg
        FunctionIgnored ->
          "  /* Ignore non-managed type inside Box: '" ++ show inner ++ "' */\n"
            ++ "  CARP_FREE(box);"
    innerDelete _ _ _ = ""

-- | Defines a template for printing a box as a string.
prn :: (String, Binder)
prn =
  let path = SymPath ["Box"] "prn"
      t = FuncTy [boxTy] StringTy StaticLifetimeTy
      docs = "Returns a string representation of a Box."
      decl = templateLiteral "String $NAME ($t* box)"
      templateCreator =
        TemplateCreator $
          ( \tenv env ->
              Template
                t
                decl
                ( \(FuncTy [boxT] StringTy _) ->
                    multilineTemplate
                      [ "$DECL {",
                        "  if(!box){",
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
   in defineTypeParameterizedTemplate templateCreator path t docs

-- | Defines a template for printing a reference to a box as a string.
str :: (String, Binder)
str =
  let path = SymPath ["Box"] "str"
      t = FuncTy [(RefTy boxTy (VarTy "q"))] StringTy StaticLifetimeTy
      docs = "Returns a string representation of a Box."
      templateCreator =
        TemplateCreator $
          ( \tenv env ->
              Template
                t
                (templateLiteral "String $NAME ($t** box)")
                ( \(FuncTy [RefTy boxT _] StringTy _) ->
                    multilineTemplate
                      [ "$DECL {",
                        "  if(!box){",
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
   in defineTypeParameterizedTemplate templateCreator path t docs

innerStr :: TypeEnv -> Env -> Ty -> String
innerStr tenv env (StructTy _ [t]) =
  case findFunctionForMemberIncludePrimitives tenv env "prn" (typesStrFunctionType tenv env (RefTy t StaticLifetimeTy)) ("Inside box.", t) of
    FunctionFound functionFullName ->
      unlines
        [ "  char* temp = " ++ functionFullName ++ "(*box);",
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


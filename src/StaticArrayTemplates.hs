{-# LANGUAGE LambdaCase #-}

module StaticArrayTemplates where

import qualified ArrayTemplates
import Concretize
import Obj
import Template
import ToTemplate
import Types

-- | NOTE: The code for these templates is copied from ArrayTemplates.hs but
-- since there are some small differences here and there I'v decided to not
-- try to abstract over them and just duplicate the templates instead.
concreteArray :: Ty
concreteArray = ConcreteNameTy (SymPath [] "StaticArray")

templateUnsafeNth :: (String, Binder)
templateUnsafeNth =
  let t = VarTy "t"
   in defineTemplate
        (SymPath ["StaticArray"] "unsafe-nth")
        (FuncTy [RefTy (StructTy concreteArray [t]) (VarTy "q"), IntTy] (RefTy t (VarTy "q")) StaticLifetimeTy)
        "gets a reference to the `n`th element from a static array `a`."
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
        ( \case
            (FuncTy [RefTy _ _, _] _ _) -> []
            _ -> error "static array templates: nth called on non array"
        )

templateLength :: (String, Binder)
templateLength = defineTypeParameterizedTemplate templateCreator path t docs
  where
    path = SymPath ["StaticArray"] "length"
    t = FuncTy [RefTy (StructTy concreteArray [VarTy "t"]) (VarTy "q")] IntTy StaticLifetimeTy
    docs = "gets the length of the static array."
    templateCreator = TemplateCreator $
      \typeEnv env ->
        Template
          t
          (const (toTemplate "int $NAME (Array *a)"))
          (const (toTemplate "$DECL { return (*a).len; }"))
          ( \case
              (FuncTy [RefTy arrayType _] _ _) ->
                depsForDeleteFunc typeEnv env arrayType
              _ -> error "static array templates: length called on non array"
          )

templateDeleteArray :: (String, Binder)
templateDeleteArray = defineTypeParameterizedTemplate templateCreator path t docs
  where
    path = SymPath ["StaticArray"] "delete"
    t = FuncTy [StructTy concreteArray [VarTy "a"]] UnitTy StaticLifetimeTy
    docs = "deletes a static array. This function should not be called manually (there shouldn't be a way to create value types of type StaticArray)."
    templateCreator = TemplateCreator $
      \typeEnv env ->
        Template
          t
          (const (toTemplate "void $NAME (Array a)"))
          ( \case
              (FuncTy [arrayType] UnitTy _) ->
                [TokDecl, TokC "{\n"]
                  ++ deleteTy typeEnv env arrayType
                  ++ [TokC "}\n"]
              _ -> error "static array templates: delete called on non array"
          )
          ( \case
              (FuncTy [StructTy _ [insideType]] UnitTy _) ->
                depsForDeleteFunc typeEnv env insideType
              _ -> error "static array templates: delete called on non array"
          )

deleteTy :: TypeEnv -> Env -> Ty -> [Token]
deleteTy typeEnv env (StructTy _ [innerType]) =
  [ TokC "    for(int i = 0; i < a.len; i++) {\n",
    TokC $ "    " ++ ArrayTemplates.insideArrayDeletion typeEnv env innerType "i",
    TokC "    }\n"
  ]
deleteTy _ _ _ = []

templateAsetBang :: (String, Binder)
templateAsetBang = defineTypeParameterizedTemplate templateCreator path t docs
  where
    path = SymPath ["StaticArray"] "aset!"
    t = FuncTy [RefTy (StructTy concreteArray [VarTy "t"]) (VarTy "q"), IntTy, VarTy "t"] UnitTy StaticLifetimeTy
    docs = "sets a static array element at the index `n` to a new value in place."
    templateCreator = TemplateCreator $
      \typeEnv env ->
        Template
          t
          (const (toTemplate "void $NAME (Array *aRef, int n, $t newValue)"))
          ( \case
              (FuncTy [_, _, insideTy] _ _) ->
                let deleter = ArrayTemplates.insideArrayDeletion typeEnv env insideTy
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
              _ -> error "static array templates: aset bang called on non array"
          )
          ( \case
              (FuncTy [RefTy arrayType _, _, _] _ _) ->
                depsForDeleteFunc typeEnv env arrayType
              _ -> error "static array templates: aset bang called on non array"
          )

templateAsetUninitializedBang :: (String, Binder)
templateAsetUninitializedBang = defineTypeParameterizedTemplate templateCreator path t docs
  where
    path = SymPath ["StaticArray"] "aset-uninitialized!"
    t = FuncTy [RefTy (StructTy (ConcreteNameTy (SymPath [] "StaticArray")) [VarTy "t"]) (VarTy "q"), IntTy, VarTy "t"] UnitTy StaticLifetimeTy
    docs = "sets an uninitialized static array member. The old member will not be deleted."
    templateCreator = TemplateCreator $
      \_ _ ->
        Template
          t
          ( \case
              (FuncTy [_, _, valueType] _ _) ->
                case valueType of
                  UnitTy -> toTemplate "void $NAME (Array *aRef, int n)"
                  _ -> toTemplate "void $NAME (Array *aRef, int n, $t newValue)"
              _ -> error "static array templates: aset called on non array"
          )
          ( \case
              (FuncTy [_, _, valueType] _ _) ->
                case valueType of
                  UnitTy -> ArrayTemplates.unitSetterTemplate
                  _ ->
                    multilineTemplate
                      [ "$DECL {",
                        "    Array a = *aRef;",
                        "    assert(n >= 0);",
                        "    assert(n < a.len);",
                        "    (($t*)a.data)[n] = newValue;",
                        "}"
                      ]
              _ -> error "static array templates: aset called on non array"
          )
          (const [])

templateStrArray :: (String, Binder)
templateStrArray = defineTypeParameterizedTemplate templateCreator path t docs
  where
    templateCreator = TemplateCreator $
      \typeEnv env ->
        Template
          t
          (const (toTemplate "String $NAME (Array* a)"))
          ( \case
              (FuncTy [RefTy arrayType _] StringTy _) ->
                [TokDecl, TokC " {\n"]
                  ++ ArrayTemplates.strTy typeEnv env arrayType
                  ++ [TokC "}\n"]
              _ -> error "static array templates: str called on non array"
          )
          ( \case
              (FuncTy [RefTy (StructTy _ [insideType]) _] StringTy _) ->
                depsForPrnFunc typeEnv env insideType
              _ -> error "static array templates: str called on non array"
          )
    path = SymPath ["StaticArray"] "str"
    t = FuncTy [RefTy (StructTy concreteArray [VarTy "a"]) (VarTy "q")] StringTy StaticLifetimeTy
    docs = "converts a static array to a string."

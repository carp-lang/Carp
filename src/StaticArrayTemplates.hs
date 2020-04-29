module StaticArrayTemplates where

import Util
import Types
import Obj
import Parsing
import Template
import ToTemplate
import Polymorphism
import Concretize
import Lookup
import qualified ArrayTemplates



-- | NOTE: The code for these templates is copied from ArrayTemplates.hs but
-- since there are some small differences here and there I'v decided to not
-- try to abstract over them and just duplicate the templates instead.



templateUnsafeNth :: (String, Binder)
templateUnsafeNth =
  let t = VarTy "t"
  in defineTemplate
  (SymPath ["StaticArray"] "unsafe-nth")
  (FuncTy [RefTy (StructTy "StaticArray" [t]) (VarTy "q"), IntTy] (RefTy t (VarTy "q")) StaticLifetimeTy)
  "gets a reference to the `n`th element from a static array `a`."
  (toTemplate "$t* $NAME (Array *aRef, int n)")
  (toTemplate $ unlines ["$DECL {"
                        ,"    Array a = *aRef;"
                        ,"    assert(n >= 0);"
                        ,"    assert(n < a.len);"
                        ,"    return &((($t*)a.data)[n]);"
                        ,"}"])
  (\(FuncTy [RefTy arrayType _, _] _ _) ->
     [])

templateLength :: (String, Binder)
templateLength = defineTypeParameterizedTemplate templateCreator path t docs
  where path = SymPath ["StaticArray"] "length"
        t = FuncTy [RefTy (StructTy "StaticArray" [VarTy "t"]) (VarTy "q")] IntTy StaticLifetimeTy
        docs = "gets the length of the static array."
        templateCreator = TemplateCreator $
          \typeEnv env ->
            Template
            t
            (const (toTemplate "int $NAME (Array *a)"))
            (const (toTemplate "$DECL { return (*a).len; }"))
            (\(FuncTy [RefTy arrayType _] _ _) ->
              depsForDeleteFunc typeEnv env arrayType)

templateDeleteArray :: (String, Binder)
templateDeleteArray = defineTypeParameterizedTemplate templateCreator path t docs
  where path = SymPath ["StaticArray"] "delete"
        t = FuncTy [StructTy "StaticArray" [VarTy "a"]] UnitTy StaticLifetimeTy
        docs = "deletes a static array. This function should not be called manually (there shouldn't be a way to create value types of type StaticArray)."
        templateCreator = TemplateCreator $
          \typeEnv env ->
             Template
             t
             (const (toTemplate "void $NAME (Array a)"))
             (\(FuncTy [arrayType] UnitTy _) ->
                [TokDecl, TokC "{\n"] ++
                deleteTy typeEnv env arrayType ++
                [TokC "}\n"])
             (\(FuncTy [arrayType@(StructTy "StaticArray" [insideType])] UnitTy _) ->
                depsForDeleteFunc typeEnv env insideType)

deleteTy :: TypeEnv -> Env -> Ty -> [Token]
deleteTy typeEnv env (StructTy _ [innerType]) =
  [ TokC   "    for(int i = 0; i < a.len; i++) {\n"
  , TokC $ "    " ++ ArrayTemplates.insideArrayDeletion typeEnv env innerType "i"
  , TokC   "    }\n"
  ]
deleteTy _ _ _ = []

templateAsetBang :: (String, Binder)
templateAsetBang = defineTypeParameterizedTemplate templateCreator path t docs
  where path = SymPath ["StaticArray"] "aset!"
        t = FuncTy [RefTy (StructTy "StaticArray" [VarTy "t"]) (VarTy "q"), IntTy, VarTy "t"] UnitTy StaticLifetimeTy
        docs = "sets a static array element at the index `n` to a new value in place."
        templateCreator = TemplateCreator $
          \typeEnv env ->
            Template
            t
            (const (toTemplate "void $NAME (Array *aRef, int n, $t newValue)"))
            (\(FuncTy [_, _, insideTy] _ _) ->
               let deleter = ArrayTemplates.insideArrayDeletion typeEnv env insideTy
               in  (toTemplate $ unlines ["$DECL {"
                                         ,"    Array a = *aRef;"
                                         ,"    assert(n >= 0);"
                                         ,"    assert(n < a.len);"
                                         ,     deleter "n"
                                         ,"    (($t*)a.data)[n] = newValue;"
                                         ,"}"]))
            (\(FuncTy [RefTy arrayType _, _, _] _ _) ->
               depsForDeleteFunc typeEnv env arrayType)

templateStrArray :: (String, Binder)
templateStrArray = defineTypeParameterizedTemplate templateCreator path t docs
  where templateCreator = TemplateCreator $
          \typeEnv env ->
             Template
             t
             (const (toTemplate "String $NAME (Array* a)"))
             (\(FuncTy [RefTy arrayType _] StringTy _) ->
                [TokDecl, TokC " {\n"] ++
                ArrayTemplates.strTy typeEnv env arrayType ++
                [TokC "}\n"])
             (\(FuncTy [RefTy arrayType@(StructTy "StaticArray" [insideType]) _] StringTy _) ->
                depsForPrnFunc typeEnv env insideType)
        path = SymPath ["StaticArray"] "str"
        t = FuncTy [RefTy (StructTy "StaticArray" [VarTy "a"]) (VarTy "q")] StringTy StaticLifetimeTy
        docs = "converts a static array to a string."

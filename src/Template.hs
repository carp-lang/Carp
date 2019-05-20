module Template where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Debug.Trace

import Util
import Types
import Obj
import Parsing
import Infer
import Concretize
import ToTemplate

-- | Create a binding pair used for adding a template instantiation to an environment.
instanceBinder :: SymPath -> Ty -> Template -> String -> (String, Binder)
instanceBinder path@(SymPath _ name) actualType template docs =
  let (x, _) = instantiateTemplate path actualType template
      docObj = (XObj (Str docs) (Just dummyInfo) Nothing)
      meta = MetaData (Map.insert "doc" docObj Map.empty)
  in  (name, Binder meta x)

-- | Create a binding pair and don't discard the dependencies
instanceBinderWithDeps :: SymPath -> Ty -> Template -> String -> ((String, Binder), [XObj])
instanceBinderWithDeps path@(SymPath _ name) actualType template docs =
  let (x, deps) = instantiateTemplate path actualType template
      docObj = (XObj (Str docs) (Just dummyInfo) Nothing)
      meta = MetaData (Map.insert "doc" docObj Map.empty)
  in  ((name, Binder meta x), deps)

-- | Templates are instructions for the compiler to generate some C-code
-- | based on some template and the names and types to fill into the template.
-- | Templates are generic and need to be given an explicit type to generate the
-- | correct code.

-- | Example:
-- | template1 : ((Array T) -> Int) = "int length__T(<T> xs) { return xs->len; }"
-- | Given the type ((Array Float) -> Int) the following code is produced:
-- | "int length__Float(Array__Float xs) { return xs->len; }"

-- | Create a binding pair used for adding a template definition to an environment.
defineTemplate :: SymPath -> Ty -> String -> [Token] -> [Token] -> (Ty -> [XObj]) -> (String, Binder)
defineTemplate path t docs declaration definition depsFunc =
  let (SymPath _ name) = path
      template = Template t (const declaration) (const definition) depsFunc
      i = Info 0 0 (show path ++ ".template") Set.empty 0
      defLst = [XObj (Deftemplate (TemplateCreator (\_ _ -> template))) Nothing Nothing, XObj (Sym path Symbol) Nothing Nothing]
      docObj = (XObj (Str docs) (Just dummyInfo) Nothing)
      meta = MetaData (Map.insert "doc" docObj Map.empty)
  in  (name, Binder meta (XObj (Lst defLst) (Just i) (Just t)))

-- | The more advanced version of a template, where the code can vary depending on the type.
defineTypeParameterizedTemplate :: TemplateCreator -> SymPath -> Ty -> String -> (String, Binder)
defineTypeParameterizedTemplate templateCreator path t docs =
  let (SymPath _ name) = path
      i = Info 0 0 (show path ++ ".parameterizedTemplate") Set.empty 0
      defLst = [XObj (Deftemplate templateCreator) Nothing Nothing, XObj (Sym path Symbol) Nothing Nothing]
      docObj = (XObj (Str docs) (Just dummyInfo) Nothing)
      meta = MetaData (Map.insert "doc" docObj Map.empty)
  in  (name, Binder meta (XObj (Lst defLst) (Just i) (Just t)))

-- | Concretizes the types used in @token
--   @cName is the name of the definition, i.e. the "foo" in "void foo() { ... }"
concretizeTypesInToken :: TypeMappings -> String -> [Token] -> Token -> [Token]
concretizeTypesInToken mappings cName decl token =
  case token of
    TokDecl -> concatMap (concretizeTypesInToken mappings cName (error "Nope.")) decl
    TokName -> [TokC cName]
    TokTy t mode -> [TokTy (replaceTyVars mappings t) mode]
    _ -> [token]

-- | The code needed to correctly call a lambda from C.
templateCodeForCallingLambda :: String -> Ty -> [String] -> String
templateCodeForCallingLambda functionName t args =
  let FuncTy argTys retTy = t
      castToFnWithEnv = tyToCast (FuncTy (lambdaEnvTy : argTys) retTy)
      castToFn = tyToCast t
  in
    functionName ++ ".env ? " ++
    "((" ++ castToFnWithEnv ++ ")" ++ functionName ++ ".callback)(" ++ functionName ++ ".env" ++ (if null args then "" else ", ") ++ joinWithComma args ++ ")" ++
    " : " ++
    "((" ++ castToFn ++ ")" ++ functionName ++ ".callback)(" ++  joinWithComma args ++ ")"

-- | Must cast a lambda:s .callback member to the correct type to be able to call it.
tyToCast :: Ty -> String
tyToCast t =
  let FuncTy argTys retTy = t
  in  "§(Fn [" ++ joinWithSpace (map show argTys) ++ "] " ++ show retTy ++ ")" -- Note! The '§' means that the emitted type will be "raw" and not converted to 'Lambda'.

----------------------------------------------------------------------------------------------------------
-- ACTUAL TEMPLATES

-- | This function accepts a pointer and will do nothing with it.
templateNoop :: (String, Binder)
templateNoop = defineTemplate
  (SymPath [] "noop")
  (FuncTy [PointerTy (VarTy "a")] UnitTy)
  "accepts a pointer and will do nothing with it."
  (toTemplate "void $NAME ($a* a)")
  (toTemplate "$DECL { }")
  (const [])

module Concretize where

import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.List (foldl')
import Debug.Trace

import Obj
import Constraints
import Types
import Util
import TypeError
import AssignTypes
import ManageMemory

-- | This function performs two things:
--   1. Finds out which polymorphic functions that needs to be added to the environment for the calls in the function to work.
--   2. Changes the name of symbols at call sites so they use the polymorphic name
--   Both of these results are returned in a tuple: (<new xobj>, <dependencies>)
concretizeXObj :: Bool -> TypeEnv -> Env -> [SymPath] -> XObj -> Either TypeError (XObj, [XObj])
concretizeXObj allowAmbiguity typeEnv rootEnv visitedDefinitions root =
  case runState (visit rootEnv root) [] of
    (Left err, _) -> Left err
    (Right xobj, deps) -> Right (xobj, deps)
  where
    visit :: Env -> XObj -> State [XObj] (Either TypeError XObj)
    visit env xobj@(XObj (Sym _) _ _) = visitSymbol env xobj
    visit env xobj@(XObj (MultiSym _ _) _ _) = visitMultiSym env xobj
    visit env (XObj (Lst lst) i t) = do visited <- visitList env lst
                                        return $ do okVisited <- visited
                                                    Right (XObj (Lst okVisited) i t)
    visit env (XObj (Arr arr) i (Just t)) = do visited <- fmap sequence (mapM (visit env) arr)
                                               modify (depsForDeleteFunc typeEnv env t ++)
                                               modify (defineArrayTypeAlias t : )
                                               return $ do okVisited <- visited
                                                           Right (XObj (Arr okVisited) i (Just t))
    visit _ x = return (Right x)

    visitList :: Env -> [XObj] -> State [XObj] (Either TypeError [XObj])
    visitList _ [] = return (Right [])

    visitList env [defn@(XObj Defn _ _), nameSymbol@(XObj (Sym (SymPath [] "main")) _ _), args@(XObj (Arr argsArr) _ _), body] =
      if not (null argsArr)
      then return $ Left (MainCannotHaveArguments (length argsArr))
      else do visitedBody <- visit env body
              return $ do okBody <- visitedBody
                          let t = fromMaybe UnitTy (ty okBody)
                          if t /= UnitTy && t /= IntTy
                          then Left (MainCanOnlyReturnUnitOrInt t)
                          else return [defn, nameSymbol, args, okBody]

    visitList env [defn@(XObj Defn _ _), nameSymbol, args@(XObj (Arr argsArr) _ _), body] =
      do mapM_ checkForNeedOfTypedefs argsArr
         let functionEnv = Env Map.empty (Just env) Nothing [] InternalEnv
             envWithArgs = foldl' (\e arg@(XObj (Sym (SymPath _ argSymName)) _ _) ->
                                     extendEnv e argSymName arg)
                                  functionEnv argsArr
         visitedBody <- visit envWithArgs body
         return $ do okBody <- visitedBody
                     return [defn, nameSymbol, args, okBody]

    visitList env [letExpr@(XObj Let _ _), XObj (Arr bindings) bindi bindt, body] =
      do visitedBindings <- fmap sequence (mapM (visit env) bindings)
         visitedBody <- visit env body
         return $ do okVisitedBindings <- visitedBindings
                     okVisitedBody <- visitedBody
                     return [letExpr, XObj (Arr okVisitedBindings) bindi bindt, okVisitedBody]

    visitList env [theExpr@(XObj The _ _), typeXObj, value] =
      do visitedValue <- visit env value
         return $ do okVisitedValue <- visitedValue
                     return [theExpr, typeXObj, okVisitedValue]

    visitList env (func : args) =
      do f <- visit env func
         a <- fmap sequence (mapM (visit env) args)
         return $ do okF <- f
                     okA <- a
                     return (okF : okA)

    checkForNeedOfTypedefs :: XObj -> State [XObj] (Either TypeError ())
    checkForNeedOfTypedefs (XObj _ _ (Just t)) =
      case t of
        (FuncTy _ _) | typeIsGeneric t -> return (Right ())
                     | otherwise -> do modify (defineFunctionTypeAlias t :)
                                       return (Right ())
        _ -> return (Right ())
    checkForNeedOfTypedefs _ = error "Missing type."

    visitSymbol :: Env -> XObj -> State [XObj] (Either TypeError XObj)
    visitSymbol env xobj@(XObj (Sym path) i t) =
      case lookupInEnv path env of
        Just (foundEnv, binder)
          | envIsExternal foundEnv ->
            let theXObj = binderXObj binder
                Just theType = ty theXObj
                Just typeOfVisited = t
            in if --(trace $ "CHECKING " ++ getName xobj ++ " : " ++ show theType ++ " with visited type " ++ show typeOfVisited ++ " and visited definitions: " ++ show visitedDefinitions) $
                  typeIsGeneric theType && not (typeIsGeneric typeOfVisited)
                  then case concretizeDefinition allowAmbiguity typeEnv env visitedDefinitions theXObj typeOfVisited of
                         Left err -> return (Left err)
                         Right (concrete, deps) ->
                           do modify (concrete :)
                              modify (deps ++)
                              return (Right (XObj (Sym (getPath concrete)) i t))
                  else return (Right xobj)
          | otherwise -> return (Right xobj)
        Nothing -> return (Right xobj)
    visitSymbol _ _ = error "Not a symbol."

    visitMultiSym :: Env -> XObj -> State [XObj] (Either TypeError XObj)
    visitMultiSym env xobj@(XObj (MultiSym originalSymbolName paths_not_needed_anymore) i t) =
      let Just actualType = t
          paths = case lookupInEnv (SymPath [] originalSymbolName) (getTypeEnv typeEnv) of
                    Just (_, Binder (XObj (Lst [XObj (Interface interfaceSignature interfacePaths) _ _, _]) _ _)) ->
                        interfacePaths
                    Nothing ->
                        paths_not_needed_anymore -- TODO: Remove?!
          tys = map (typeFromPath env) paths
          tysToPathsDict = zip tys paths
      in  case filter (matchingSignature actualType) tysToPathsDict of
            [] ->
              if allowAmbiguity
              then return (Right xobj)
              else return (Left (NoMatchingSignature xobj originalSymbolName actualType tysToPathsDict))
            [(theType, singlePath)] -> let Just t' = t
                                           fake1 = XObj (Sym (SymPath [] "theType")) Nothing Nothing
                                           fake2 = XObj (Sym (SymPath [] "xobjType")) Nothing Nothing
                                       in  case solve [Constraint theType t' fake1 fake2 OrdMultiSym] of
                                             Right mappings ->
                                               let replaced = replaceTyVars mappings t'
                                                   normalSymbol = XObj (Sym singlePath) i (Just replaced)
                                               in visitSymbol env --- $ (trace ("Disambiguated " ++ pretty xobj ++
                                                                  ---   " to " ++ show singlePath ++ " : " ++ show replaced))
                                                                    normalSymbol
                                             Left failure@(UnificationFailure _ _) ->
                                               return $ Left (UnificationFailed
                                                              (unificationFailure failure)
                                                              (unificationMappings failure)
                                                              [])
                                             Left (Holes holes) ->
                                               return $ Left (HolesFound holes)
            severalPaths -> if allowAmbiguity
                            then return (Right xobj)
                            else return (Left (CantDisambiguate xobj originalSymbolName actualType severalPaths))
      where matchingSignature :: Ty -> (Ty, SymPath) -> Bool
            matchingSignature tA (tB, _) = areUnifiable tA tB

    visitMultiSym _ _ = error "Not a multi symbol."

-- | Get the type of a symbol at a given path.
typeFromPath :: Env -> SymPath -> Ty
typeFromPath env p =
  case lookupInEnv p env of
    Just (e, Binder found)
      | envIsExternal e -> forceTy found
      | otherwise -> error "Local bindings shouldn't be ambiguous."
    Nothing -> error ("Couldn't find " ++ show p ++ " in env " ++ safeEnvModuleName env)

-- | Given a definition (def, defn, template, external) and
--   a concrete type (a type without any type variables)
--   this function returns a new definition with the concrete
--   types assigned, and a list of dependencies.
concretizeDefinition :: Bool -> TypeEnv -> Env -> [SymPath] -> XObj -> Ty -> Either TypeError (XObj, [XObj])
concretizeDefinition allowAmbiguity typeEnv globalEnv visitedDefinitions definition concreteType =
  let SymPath pathStrings name = getPath definition
      Just polyType = ty definition
      suffix = polymorphicSuffix polyType concreteType
      newPath = SymPath pathStrings (name ++ suffix)
  in
    case definition of
      XObj (Lst (XObj Defn _ _ : _)) _ _ ->
        let withNewPath = setPath definition newPath
            mappings = unifySignatures polyType concreteType
        in case assignTypes mappings withNewPath of
          Right typed ->
            if newPath `elem` visitedDefinitions
            then return (withNewPath, [])
            else do (concrete, deps) <- concretizeXObj allowAmbiguity typeEnv globalEnv (newPath : visitedDefinitions) typed
                    managed <- manageMemory typeEnv globalEnv concrete
                    return (managed, deps)
          Left e -> Left e
      XObj (Lst (XObj (Deftemplate (TemplateCreator templateCreator)) _ _ : _)) _ _ ->
        let template = templateCreator typeEnv globalEnv
        in  Right (instantiateTemplate newPath concreteType template)
      XObj (Lst [XObj External _ _, _]) _ _ ->
        if name == "NULL"
        then Right (definition, []) -- A hack to make all versions of NULL have the same name
        else let withNewPath = setPath definition newPath
                 withNewType = withNewPath { ty = Just concreteType }
             in  Right (withNewType, [])
      err ->
        compilerError ("Can't concretize " ++ show err ++ ": " ++ pretty definition)

-- | Find ALL functions with a certain name, matching a type signature.
allFunctionsWithNameAndSignature env functionName functionType =
  filter (predicate . ty . binderXObj . snd) (multiLookupALL functionName env)
  where
    predicate (Just t) = areUnifiable functionType t

-- | Find all the dependencies of a polymorphic function with a name and a desired concrete type.
depsOfPolymorphicFunction :: TypeEnv -> Env -> [SymPath] -> String -> Ty -> [XObj]
depsOfPolymorphicFunction typeEnv env visitedDefinitions functionName functionType =
  case allFunctionsWithNameAndSignature env functionName functionType of
    [] ->
      (trace $ "No '" ++ functionName ++ "' function found with type " ++ show functionType ++ ".")
      []
    [(_, Binder (XObj (Lst (XObj (Instantiate _) _ _ : _)) _ _))] ->
      []
    [(_, Binder single)] ->
      case concretizeDefinition False typeEnv env visitedDefinitions single functionType of
        Left err -> error (show err)
        Right (ok, deps) -> ok : deps
    _ ->
      (trace $ "Too many '" ++ functionName ++ "' functions found with type " ++ show functionType ++ ", can't figure out dependencies.")
      []

-- | Helper for finding the 'delete' function for a type.
depsForDeleteFunc :: TypeEnv -> Env -> Ty -> [XObj]
depsForDeleteFunc typeEnv env t =
  if isManaged typeEnv t
  then depsOfPolymorphicFunction typeEnv env [] "delete" (FuncTy [t] UnitTy)
  else []

-- | Helper for finding the 'copy' function for a type.
depsForCopyFunc :: TypeEnv -> Env -> Ty -> [XObj]
depsForCopyFunc typeEnv env t =
  if isManaged typeEnv t
  then depsOfPolymorphicFunction typeEnv env [] "copy" (FuncTy [RefTy t] t)
  else []

-- | Helper for finding the 'str' function for a type.
depsForStrFunc :: TypeEnv -> Env -> Ty -> [XObj]
depsForStrFunc typeEnv env t =
  if isManaged typeEnv t
  then depsOfPolymorphicFunction typeEnv env [] "str" (FuncTy [RefTy t] StringTy)
  else depsOfPolymorphicFunction typeEnv env [] "str" (FuncTy [t] StringTy)

-- | The type of a type's str function.
typesStrFunctionType :: TypeEnv -> Ty -> Ty
typesStrFunctionType typeEnv memberType =
  if isManaged typeEnv memberType
  then FuncTy [RefTy memberType] StringTy
  else FuncTy [memberType] StringTy

-- | The various results when trying to find a function using 'findFunctionForMember'.
data FunctionFinderResult = FunctionFound String
                          | FunctionNotFound String
                          | FunctionIgnored
                          deriving (Show)

getConcretizedPath :: XObj -> Ty -> SymPath
getConcretizedPath single functionType =
  let Just t' = ty single
      (SymPath pathStrings name) = getPath single
      suffix = polymorphicSuffix t' functionType
  in SymPath pathStrings (name ++ suffix)

-- | Used for finding functions like 'delete' or 'copy' for members of a Deftype (or Array).
findFunctionForMember :: TypeEnv -> Env -> String -> Ty -> (String, Ty) -> FunctionFinderResult
findFunctionForMember typeEnv env functionName functionType (memberName, memberType)
  | isManaged typeEnv memberType =
    case allFunctionsWithNameAndSignature env functionName functionType of
      [] -> FunctionNotFound ("Can't find any '" ++ functionName ++ "' function for member '" ++
                              memberName ++ "' of type " ++ show functionType)
      [(_, Binder single)] ->
        let concretizedPath = getConcretizedPath single functionType
        in  FunctionFound (pathToC concretizedPath)
      _ -> FunctionNotFound ("Can't find a single '" ++ functionName ++ "' function for member '" ++
                             memberName ++ "' of type " ++ show functionType)
  | otherwise = FunctionIgnored

-- | TODO: should this be the default and 'findFunctionForMember' be the specific one
findFunctionForMemberIncludePrimitives :: TypeEnv -> Env -> String -> Ty -> (String, Ty) -> FunctionFinderResult
findFunctionForMemberIncludePrimitives typeEnv env functionName functionType (memberName, memberType) =
  case allFunctionsWithNameAndSignature env functionName functionType of
    [] -> FunctionNotFound ("Can't find any '" ++ functionName ++ "' function for member '" ++
                            memberName ++ "' of type " ++ show functionType)
    [(_, Binder single)] ->
      let concretizedPath = getConcretizedPath single functionType
      in  FunctionFound (pathToC concretizedPath)
    _ -> FunctionNotFound ("Can't find a single '" ++ functionName ++ "' function for member '" ++
                           memberName ++ "' of type " ++ show functionType)

{-# LANGUAGE MultiWayIf #-}

module Concretize where

import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, fromJust)
import qualified Data.Set as Set
import Data.Set ((\\))
import Data.List (foldl')
import Debug.Trace
import qualified Text.Parsec as Parsec
import Text.Parsec ((<|>))

import Obj
import Constraints
import Types
import Util
import TypeError
import AssignTypes
import Polymorphism
import InitialTypes
import Lookup
import Parsing

--import Template
--import ArrayTemplates

-- | This function performs two things:
-- |  1. Finds out which polymorphic functions that needs to be added to the environment for the calls in the function to work.
-- |  2. Changes the name of symbols at call sites so they use the polymorphic name
-- |  Both of these results are returned in a tuple: (<new xobj>, <dependencies>)
concretizeXObj :: Bool -> TypeEnv -> Env -> [SymPath] -> XObj -> Either TypeError (XObj, [XObj])
concretizeXObj allowAmbiguityRoot typeEnv rootEnv visitedDefinitions root =
  case runState (visit allowAmbiguityRoot rootEnv root) [] of
    (Left err, _) -> Left err
    (Right xobj, deps) -> Right (xobj, deps)
  where
    rootDefinitionName :: String
    rootDefinitionName = getName root

    visit :: Bool -> Env -> XObj -> State [XObj] (Either TypeError XObj)
    visit allowAmbig env xobj@(XObj (Sym _ _) _ _) = visitSymbol allowAmbig env xobj
    visit allowAmbig env xobj@(XObj (MultiSym _ _) _ _) = visitMultiSym allowAmbig env xobj
    visit allowAmbig env xobj@(XObj (InterfaceSym _) _ _) = visitInterfaceSym allowAmbig env xobj
    visit allowAmbig env xobj@(XObj (Lst _) i t) =
      do visited <- visitList allowAmbig env xobj
         return $ do okVisited <- visited
                     Right (XObj (Lst okVisited) i t)
    visit allowAmbig env xobj@(XObj (Arr arr) i (Just t)) =
      do visited <- fmap sequence (mapM (visit allowAmbig env) arr)
         concretizeTypeOfXObj typeEnv xobj
         return $ do okVisited <- visited
                     Right (XObj (Arr okVisited) i (Just t))
    visit _ _ x = return (Right x)

    visitList :: Bool -> Env -> XObj -> State [XObj] (Either TypeError [XObj])
    visitList _ _ (XObj (Lst []) _ _) = return (Right [])

    visitList _ env (XObj (Lst [defn@(XObj Defn _ _), nameSymbol@(XObj (Sym (SymPath [] "main") _) _ _), args@(XObj (Arr argsArr) _ _), body]) _ _) =
      if not (null argsArr)
      then return $ Left (MainCannotHaveArguments nameSymbol (length argsArr))
      else do visitedBody <- visit False env body -- allowAmbig == 'False'
              return $ do okBody <- visitedBody
                          let t = fromMaybe UnitTy (ty okBody)
                          if not (isTypeGeneric t) && t /= UnitTy && t /= IntTy
                            then Left (MainCanOnlyReturnUnitOrInt nameSymbol t)
                            else return [defn, nameSymbol, args, okBody]

    visitList _ env (XObj (Lst [defn@(XObj Defn _ _), nameSymbol, args@(XObj (Arr argsArr) _ _), body]) _ t) =
      do mapM_ (concretizeTypeOfXObj typeEnv) argsArr
         let functionEnv = Env Map.empty (Just env) Nothing [] InternalEnv 0
             envWithArgs = foldl' (\e arg@(XObj (Sym (SymPath _ argSymName) _) _ _) ->
                                     extendEnv e argSymName arg)
                                  functionEnv argsArr
             Just funcTy = t
             allowAmbig = isTypeGeneric funcTy
         visitedBody <- visit allowAmbig envWithArgs body
         return $ do okBody <- visitedBody
                     return [defn, nameSymbol, args, okBody]

    -- | Fn / λ
    visitList allowAmbig env (XObj (Lst [(XObj (Fn _ _) fni fnt), args@(XObj (Arr argsArr) ai at), body]) i t) =
      -- The basic idea of this function is to first visit the body of the lambda ("in place"),
      -- then take the resulting body and put into a separate function 'defn' with a new name
      -- in the global scope. That function definition will be set as the lambdas '.callback' in
      -- the C code.
      do let Just ii = i
             Just funcTy = t
              -- | TODO: This code is a copy of the one above in Defn, remove duplication:
             functionEnv = Env Map.empty (Just env) Nothing [] InternalEnv (envFunctionNestingLevel env + 1)
             envWithArgs = foldl' (\e arg@(XObj (Sym (SymPath _ argSymName) _) _ _) ->
                                     extendEnv e argSymName arg)
                                  functionEnv argsArr
         visitedBody <- visit allowAmbig envWithArgs body
         case visitedBody of
           Right (okBody) ->
             let -- Analyse the body of the lambda to find what variables it captures
                 capturedVars = collectCapturedVars okBody

                 -- Create a new (top-level) function that will be used when the lambda is called.
                 -- Its name will contain the name of the (normal, non-lambda) function it's contained within,
                 -- plus the identifier of the particular s-expression that defines the lambda.
                 lambdaName = "_Lambda_" ++ rootDefinitionName ++ "_" ++ show (infoIdentifier ii)
                 lambdaNameSymbol = XObj (Sym (SymPath [] lambdaName) Symbol) (Just dummyInfo) Nothing
                 extendedArgs = if null capturedVars
                                then args
                                     -- If the lambda captures anything it need an extra arg for its env:
                                else XObj (Arr (XObj (Sym (SymPath [] "_env") Symbol)
                                                (Just dummyInfo)
                                                (Just (PointerTy (StructTy environmentTypeName []))) :
                                                argsArr)) ai at
                 lambdaCallback = XObj (Lst [XObj Defn (Just dummyInfo) Nothing, lambdaNameSymbol, extendedArgs, okBody]) i t

                 -- The lambda will also carry with it a special made struct containing the variables it captures
                 -- (if it captures at least one variable)
                 structMemberPairs = concatMap (\(XObj (Sym path _) _ (Just symTy)) ->
                                                  [XObj (Sym path Symbol) Nothing Nothing, tyToXObj symTy])
                                     capturedVars
                 environmentTypeName = lambdaName ++ "_env"
                 environmentStructTy = StructTy environmentTypeName []
                 environmentStruct = XObj (Lst (XObj (Typ environmentStructTy) Nothing Nothing :
                                                XObj (Sym (SymPath [] environmentTypeName) Symbol) Nothing Nothing :
                                                XObj (Arr structMemberPairs) Nothing Nothing :
                                                [])) i (Just TypeTy)

                 deleteFnTy = typesDeleterFunctionType environmentStructTy
                 deleteFnTemplate = concreteDelete typeEnv env (memberXObjsToPairs structMemberPairs)
                 (deleteFn, deleterDeps) = instantiateTemplate (SymPath [] (environmentTypeName ++ "_delete")) deleteFnTy deleteFnTemplate

                 copyFnTy = typesCopyFunctionType environmentStructTy
                 copyFnTemplate = concreteCopy typeEnv env (memberXObjsToPairs structMemberPairs)
                 (copyFn, copyDeps) = instantiateTemplate (SymPath [] (environmentTypeName ++ "_copy")) copyFnTy copyFnTemplate

                 -- The type env has to contain the lambdas environment struct for 'concretizeDefinition' to work:
                 extendedTypeEnv = TypeEnv (extendEnv (getTypeEnv typeEnv) environmentTypeName environmentStruct)

             in case concretizeDefinition allowAmbig extendedTypeEnv env visitedDefinitions lambdaCallback funcTy of
                  Left err -> return (Left err)
                  Right (concreteLiftedLambda, deps) ->
                    do when (not (null capturedVars)) $
                         do modify (environmentStruct :)
                            modify (deleteFn :)
                            modify (deleterDeps ++)
                            modify (copyFn :)
                            modify (copyDeps ++)
                       modify (concreteLiftedLambda :)
                       modify (deps ++)
                       return (Right [XObj (Fn (Just lambdaName) (Set.fromList capturedVars)) fni fnt, args, okBody])
           _ ->
             error "Visited body isn't a defn."

    visitList _ env (XObj (Lst [def@(XObj Def _ _), nameSymbol, body]) _ t) =
      do let Just defTy = t
             allowAmbig = isTypeGeneric defTy
         visitedBody <- visit allowAmbig env body
         return $ do okBody <- visitedBody
                     return [def, nameSymbol, okBody]

    visitList allowAmbig env (XObj (Lst [letExpr@(XObj Let _ _), XObj (Arr bindings) bindi bindt, body]) _ _) =
      do visitedBindings <- fmap sequence (mapM (visit allowAmbig env) bindings)
         visitedBody <- visit allowAmbig env body
         mapM_ (concretizeTypeOfXObj typeEnv) (map fst (pairwise bindings))
         return $ do okVisitedBindings <- visitedBindings
                     okVisitedBody <- visitedBody
                     return [letExpr, XObj (Arr okVisitedBindings) bindi bindt, okVisitedBody]

    visitList allowAmbig env (XObj (Lst [theExpr@(XObj The _ _), typeXObj, value]) _ _) =
      do visitedValue <- visit allowAmbig env value
         return $ do okVisitedValue <- visitedValue
                     return [theExpr, typeXObj, okVisitedValue]

    visitList allowAmbig env (XObj (Lst [andExpr@(XObj And _ _), expr1, expr2]) _ _) =
      do visitedExpr1 <- visit allowAmbig env expr1
         visitedExpr2 <- visit allowAmbig env expr2
         return $ do okVisitedExpr1 <- visitedExpr1
                     okVisitedExpr2 <- visitedExpr2
                     return [andExpr, okVisitedExpr1, okVisitedExpr2]

    visitList allowAmbig env (XObj (Lst [orExpr@(XObj Or _ _), expr1, expr2]) _ _) =
      do visitedExpr1 <- visit allowAmbig env expr1
         visitedExpr2 <- visit allowAmbig env expr2
         return $ do okVisitedExpr1 <- visitedExpr1
                     okVisitedExpr2 <- visitedExpr2
                     return [orExpr, okVisitedExpr1, okVisitedExpr2]

    visitList allowAmbig env (XObj (Lst (func : args)) _ _) =
      do concretizeTypeOfXObj typeEnv func
         mapM_ (concretizeTypeOfXObj typeEnv) args
         f <- visit allowAmbig env func
         a <- fmap sequence (mapM (visit allowAmbig env) args)
         return $ do okF <- f
                     okA <- a
                     return (okF : okA)

    visitSymbol :: Bool -> Env -> XObj -> State [XObj] (Either TypeError XObj)
    visitSymbol allowAmbig env xobj@(XObj (Sym path lookupMode) i t) =
      case lookupInEnv path env of
        Just (foundEnv, binder)
          | envIsExternal foundEnv ->
            let theXObj = binderXObj binder
                Just theType = ty theXObj
                typeOfVisited = case t of
                                  Just something -> something
                                  Nothing -> error ("Missing type on " ++ show xobj ++ " at " ++ prettyInfoFromXObj xobj)
            in if --(trace $ "CHECKING " ++ getName xobj ++ " : " ++ show theType ++ " with visited type " ++ show typeOfVisited ++ " and visited definitions: " ++ show visitedDefinitions) $
                  isTypeGeneric theType && not (isTypeGeneric typeOfVisited)
                  then case concretizeDefinition allowAmbig typeEnv env visitedDefinitions theXObj typeOfVisited of
                         Left err -> return (Left err)
                         Right (concrete, deps) ->
                           do modify (concrete :)
                              modify (deps ++)
                              return (Right (XObj (Sym (getPath concrete) lookupMode) i t))
                  else return (Right xobj)
          | otherwise -> return (Right xobj)
        Nothing -> return (Right xobj)
    visitSymbol _ _ _ = error "Not a symbol."

    visitMultiSym :: Bool -> Env -> XObj -> State [XObj] (Either TypeError XObj)
    visitMultiSym allowAmbig env xobj@(XObj (MultiSym originalSymbolName paths) i t) =
      let Just actualType = t
          tys = map (typeFromPath env) paths
          tysToPathsDict = zip tys paths
      in  case filter (matchingSignature actualType) tysToPathsDict of
            [] ->
              --if allowAmbiguity
              --then return (Right xobj)
              --else
              return (Left (NoMatchingSignature xobj originalSymbolName actualType tysToPathsDict))
            [(theType, singlePath)] -> let Just t' = t
                                           fake1 = XObj (Sym (SymPath [] "theType") Symbol) Nothing Nothing
                                           fake2 = XObj (Sym (SymPath [] "xobjType") Symbol) Nothing Nothing
                                           Just i' = i
                                       in  case solve [Constraint theType t' fake1 fake2 OrdMultiSym] of
                                             Right mappings ->
                                               let replaced = replaceTyVars mappings t'
                                                   suffixed = suffixTyVars ("_x" ++ show (infoIdentifier i')) replaced -- Make sure it gets unique type variables. TODO: Is there a better way?
                                                   normalSymbol = XObj (Sym singlePath (LookupGlobal CarpLand)) i (Just suffixed)
                                               in visitSymbol allowAmbig env $ --(trace ("Disambiguated " ++ pretty xobj ++ " at " ++ prettyInfoFromXObj xobj ++ " to " ++ show singlePath ++ " : " ++ show suffixed ++ ", used to be " ++ show t' ++ ", theType = " ++ show theType ++ ", mappings = " ++ show mappings))
                                                              normalSymbol
                                             Left failure@(UnificationFailure _ _) ->
                                               return $ Left (UnificationFailed
                                                              (unificationFailure failure)
                                                              (unificationMappings failure)
                                                              [])
                                             Left (Holes holes) ->
                                               return $ Left (HolesFound holes)
            severalPaths -> return (Right xobj)
                            -- if allowAmbig
                            -- then
                            -- else return (Left (CantDisambiguate xobj originalSymbolName actualType severalPaths))

    visitMultiSym _ _ _ = error "Not a multi symbol."

    visitInterfaceSym :: Bool -> Env -> XObj -> State [XObj] (Either TypeError XObj)
    visitInterfaceSym allowAmbig env xobj@(XObj (InterfaceSym name) i t) =
      case lookupInEnv (SymPath [] name) (getTypeEnv typeEnv) of
        Just (_, Binder _ (XObj (Lst [XObj (Interface interfaceSignature interfacePaths) _ _, _]) _ _)) ->
          let Just actualType = t
              tys = map (typeFromPath env) interfacePaths
              tysToPathsDict = zip tys interfacePaths
          in  case filter (matchingSignature actualType) tysToPathsDict of
                [] -> return $ -- (trace ("No matching signatures for interface lookup of " ++ name ++ " of type " ++ show actualType ++ " " ++ prettyInfoFromXObj xobj ++ ", options are:\n" ++ joinWith "\n" (map show tysToPathsDict))) $
                                 if allowAmbig
                                 then (Right xobj) -- No exact match of types
                                 else (Left (NoMatchingSignature xobj name actualType tysToPathsDict))
                [(theType, singlePath)] ->
                  --(trace ("One matching signature for interface lookup of '" ++ name ++ "' with single path " ++ show singlePath ++ " of type " ++ show theType ++ " at " ++ prettyInfoFromXObj xobj ++ ", original symbol: " ++ show xobj)) $
                  let Just tt = t
                  in  if isTypeGeneric tt then return (Right xobj) else replace theType singlePath
                severalPaths ->
                    --(trace ("Several matching signatures for interface lookup of '" ++ name ++ "' of type " ++ show actualType ++ " " ++ prettyInfoFromXObj xobj ++ ", options are:\n" ++ joinWith "\n" (map show tysToPathsDict) ++ "\n  Filtered paths are:\n" ++ (joinWith "\n" (map show severalPaths)))) $
                    --(Left (CantDisambiguateInterfaceLookup xobj name interfaceType severalPaths)) -- TODO unnecessary error?
                    case filter (\(tt, _) -> actualType == tt) severalPaths of
                      []      -> return (Right xobj) -- No exact match of types
                      [(theType, singlePath)] -> replace theType singlePath -- Found an exact match, will ignore any "half matched" functions that might have slipped in.
                      _       -> return (Left (SeveralExactMatches xobj name actualType severalPaths))
              where replace theType singlePath =
                      let normalSymbol = XObj (Sym singlePath (LookupGlobal CarpLand)) i t
                      in visitSymbol allowAmbig env $ -- trace ("Replacing symbol " ++ pretty xobj ++ " with type " ++ show theType ++ " to single path " ++ show singlePath)
                                             normalSymbol

        Nothing ->
          error ("No interface named '" ++ name ++ "' found.")

-- | Find all lookups in a lambda body that should be captured by its environment
collectCapturedVars :: XObj -> [XObj]
collectCapturedVars root = removeDuplicates (map toGeneralSymbol (visit root))
  where
    removeDuplicates :: Ord a => [a] -> [a]
    removeDuplicates = Set.toList . Set.fromList

    toGeneralSymbol :: XObj -> XObj
    toGeneralSymbol (XObj (Sym path _) _ t) = XObj (Sym path Symbol) (Just dummyInfo) t
    toGeneralSymbol x = error ("Can't convert this to a general symbol: " ++ show x)

    visit xobj =
      case obj xobj of
        (Lst _) -> visitList xobj
        (Arr _) -> visitArray xobj
        (Sym path (LookupLocal Capture)) -> [xobj]
        _ -> []

    visitList :: XObj -> [XObj]
    visitList (XObj (Lst xobjs) _ _) =
      concatMap visit xobjs
    visitList _ = error "The function 'visitList' only accepts XObjs with lists in them."

    visitArray :: XObj -> [XObj]
    visitArray (XObj (Arr xobjs) _ _) =
      concatMap visit xobjs
    visitArray _ = error "The function 'visitArray' only accepts XObjs with arrays in them."

-- | Do the signatures match?
matchingSignature :: Ty -> (Ty, SymPath) -> Bool
matchingSignature tA (tB, _) = areUnifiable tA tB

-- | Does the type of an XObj require additional concretization of generic types or some typedefs for function types, etc?
-- | If so, perform the concretization and append the results to the list of dependencies.
concretizeTypeOfXObj :: TypeEnv -> XObj -> State [XObj] (Either TypeError ())
concretizeTypeOfXObj typeEnv (XObj _ _ (Just t)) =
  case concretizeType typeEnv t of
    Right t -> do modify (t ++)
                  return (Right ())
    Left err -> return (Left (InvalidMemberType err))
concretizeTypeOfXObj _ xobj = return (Right ()) --error ("Missing type: " ++ show xobj)

-- | Find all the concrete deps of a type.
concretizeType :: TypeEnv -> Ty -> Either String [XObj]
concretizeType _ ft@(FuncTy _ _) =
  if isTypeGeneric ft
  then Right []
  else Right [defineFunctionTypeAlias ft]
concretizeType typeEnv arrayTy@(StructTy "Array" varTys) =
  if isTypeGeneric arrayTy
  then Right []
  else do deps <- mapM (concretizeType typeEnv) varTys
          Right ([defineArrayTypeAlias arrayTy] ++ concat deps)
concretizeType typeEnv genericStructTy@(StructTy name _) =
  case lookupInEnv (SymPath [] name) (getTypeEnv typeEnv) of
    Just (_, Binder _ (XObj (Lst (XObj (Typ originalStructTy) _ _ : _ : rest)) _ _)) ->
      if isTypeGeneric originalStructTy
      then instantiateGenericStructType typeEnv originalStructTy genericStructTy rest
      else Right []
    Just (_, Binder _ (XObj (Lst (XObj ExternalType _ _ : _)) _ _)) ->
      Right []
    Just (_, Binder _ x) ->
      error ("Non-deftype found in type env: " ++ show x)
    Nothing ->
      error ("Can't find type " ++ show genericStructTy ++ " with name '" ++ name ++ "' in type env.")
concretizeType _ t =
    Right [] -- ignore all other types

-- | Given an generic struct type and a concrete version of it, generate all dependencies needed to use the concrete one.
instantiateGenericStructType :: TypeEnv -> Ty -> Ty -> [XObj] -> Either String [XObj]
instantiateGenericStructType typeEnv originalStructTy@(StructTy _ originalTyVars) genericStructTy membersXObjs =
  -- Turn (deftype (A a) [x a, y a]) into (deftype (A Int) [x Int, y Int])
  let fake1 = XObj (Sym (SymPath [] "a") Symbol) Nothing Nothing
      fake2 = XObj (Sym (SymPath [] "b") Symbol) Nothing Nothing
      XObj (Arr memberXObjs) _ _ = head membersXObjs
  in  case solve [Constraint originalStructTy genericStructTy fake1 fake2 OrdMultiSym] of
        Left e -> error (show e)
        Right mappings ->
          let concretelyTypedMembers = replaceGenericTypeSymbolsOnMembers mappings memberXObjs
          in  case validateMembers typeEnv originalTyVars concretelyTypedMembers of
                Left err -> Left err
                Right () ->
                  let deps = sequence (map (f typeEnv) (pairwise concretelyTypedMembers))
                  in case deps of
                       Left err -> Left err
                       Right okDeps ->
                         Right $ [ XObj (Lst (XObj (Typ genericStructTy) Nothing Nothing :
                                              XObj (Sym (SymPath [] (tyToC genericStructTy)) Symbol) Nothing Nothing :
                                              [(XObj (Arr concretelyTypedMembers) Nothing Nothing)])
                                        ) (Just dummyInfo) (Just TypeTy)
                                 ] ++ concat okDeps

f :: TypeEnv -> (XObj, XObj) -> Either String [XObj]
f typeEnv (_, tyXObj) =
  case (xobjToTy tyXObj) of
    Just okTy -> concretizeType typeEnv okTy
    Nothing -> error ("Failed to convert " ++ pretty tyXObj ++ "to a type.")

-- | Get the type of a symbol at a given path.
typeFromPath :: Env -> SymPath -> Ty
typeFromPath env p =
  case lookupInEnv p env of
    Just (e, Binder _ found)
      | envIsExternal e -> forceTy found
      | otherwise -> error "Local bindings shouldn't be ambiguous."
    Nothing -> error ("Couldn't find " ++ show p ++ " in env:\n" ++ prettyEnvironmentChain env)

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
            then return (trace ("Already visited " ++ show newPath) (withNewPath, []))
            else do (concrete, deps) <- concretizeXObj allowAmbiguity typeEnv globalEnv (newPath : visitedDefinitions) typed
                    (managed, memDeps) <- manageMemory typeEnv globalEnv concrete
                    return (managed, deps ++ memDeps)
          Left e -> Left e
      XObj (Lst (XObj (Deftemplate (TemplateCreator templateCreator)) _ _ : _)) _ _ ->
        let template = templateCreator typeEnv globalEnv
        in  Right (instantiateTemplate newPath concreteType template)
      XObj (Lst [XObj (External _) _ _, _]) _ _ ->
        if name == "NULL"
        then Right (definition, []) -- A hack to make all versions of NULL have the same name
        else let withNewPath = setPath definition newPath
                 withNewType = withNewPath { ty = Just concreteType }
             in  Right (withNewType, [])
      XObj (Lst [XObj (Instantiate template) _ _, _]) _ _ ->
        Right (instantiateTemplate newPath concreteType template)
      err ->
        Left $ CannotConcretize definition

-- | Find ALL functions with a certain name, matching a type signature.
allFunctionsWithNameAndSignature env functionName functionType =
  filter (predicate . ty . binderXObj . snd) (multiLookupALL functionName env)
  where
    predicate (Just t) = --trace ("areUnifiable? " ++ show functionType ++ " == " ++ show t ++ " " ++ show (areUnifiable functionType t)) $
                         areUnifiable functionType t

-- | Find all the dependencies of a polymorphic function with a name and a desired concrete type.
depsOfPolymorphicFunction :: TypeEnv -> Env -> [SymPath] -> String -> Ty -> [XObj]
depsOfPolymorphicFunction typeEnv env visitedDefinitions functionName functionType =
  case allFunctionsWithNameAndSignature env functionName functionType of
    [] ->
      (trace $ "[Warning] No '" ++ functionName ++ "' function found with type " ++ show functionType ++ ".")
      []
    -- TODO: this code was added to solve a bug (presumably) but it seems OK to comment it out?!
    -- [(_, (Binder xobj@(XObj (Lst (XObj (Instantiate template) _ _ : _)) _ _)))] ->
    --   []
    [(_, Binder _ single)] ->
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
depsForPrnFunc :: TypeEnv -> Env -> Ty -> [XObj]
depsForPrnFunc typeEnv env t =
  if isManaged typeEnv t
  then depsOfPolymorphicFunction typeEnv env [] "prn" (FuncTy [RefTy t] StringTy)
  else depsOfPolymorphicFunction typeEnv env [] "prn" (FuncTy [t] StringTy)

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
      [(_, Binder _ single)] ->
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
    [(_, Binder _ single)] ->
      let concretizedPath = getConcretizedPath single functionType
      in  FunctionFound (pathToC concretizedPath)
    _ -> FunctionNotFound ("Can't find a single '" ++ functionName ++ "' function for member '" ++
                           memberName ++ "' of type " ++ show functionType)



-- | Manage memory needs access to the concretizer
-- | (and the concretizer needs to manage memory)
-- | so they are put into the same module.

-- | Assign a set of Deleters to the 'infoDelete' field on Info.
setDeletersOnInfo :: Maybe Info -> Set.Set Deleter -> Maybe Info
setDeletersOnInfo i deleters = fmap (\i' -> i' { infoDelete = deleters }) i

-- | Helper function for setting the deleters for an XObj.
del :: XObj -> Set.Set Deleter -> XObj
del xobj deleters = xobj { info = setDeletersOnInfo (info xobj) deleters }

-- | To keep track of the deleters when recursively walking the form.
data MemState = MemState
                { memStateDeleters :: Set.Set Deleter
                , memStateDeps :: [XObj]
                } deriving Show

-- | Find out what deleters are needed and where in an XObj.
-- | Deleters will be added to the info field on XObj so that
-- | the code emitter can access them and insert calls to destructors.
manageMemory :: TypeEnv -> Env -> XObj -> Either TypeError (XObj, [XObj])
manageMemory typeEnv globalEnv root =
  let (finalObj, finalState) = runState (visit root) (MemState (Set.fromList []) [])
      deleteThese = memStateDeleters finalState
      deps = memStateDeps finalState
  in  -- (trace ("Delete these: " ++ joinWithComma (map show (Set.toList deleteThese)))) $
      case finalObj of
        Left err -> Left err
        Right ok -> let newInfo = fmap (\i -> i { infoDelete = deleteThese }) (info ok)
                    in  Right $ (ok { info = newInfo }, deps)

  where visit :: XObj -> State MemState (Either TypeError XObj)
        visit xobj =
          case obj xobj of
            Lst _ -> visitList xobj
            Arr _ -> visitArray xobj
            Str _ -> do manage xobj
                        return (Right xobj)
            _ -> return (Right xobj)

        visitArray :: XObj -> State MemState (Either TypeError XObj)
        visitArray xobj@(XObj (Arr arr) _ _) =
          do mapM_ visit arr
             results <- mapM unmanage arr
             case sequence results of
               Left e -> return (Left e)
               Right _ ->
                 do _ <- manage xobj -- TODO: result is discarded here, is that OK?
                    return (Right xobj)

        visitArray _ = error "Must visit array."

        visitList :: XObj -> State MemState (Either TypeError XObj)
        visitList xobj@(XObj (Lst lst) i t) =
          case lst of
            [defn@(XObj Defn _ _), nameSymbol@(XObj (Sym _ _) _ _), args@(XObj (Arr argList) _ _), body] ->
              let Just funcTy@(FuncTy _ defnReturnType) = t
              in case defnReturnType of
                   RefTy _ ->
                     return (Left (FunctionsCantReturnRefTy xobj funcTy))
                   _ ->
                     do mapM_ manage argList
                        visitedBody <- visit  body
                        result <- unmanage body
                        return $
                          case result of
                            Left e -> Left e
                            Right _ ->
                              do okBody <- visitedBody
                                 return (XObj (Lst [defn, nameSymbol, args, okBody]) i t)

            -- Fn / λ
            [fn@(XObj (Fn _ captures) _ _), args@(XObj (Arr argList) _ _), body] ->
              let Just funcTy@(FuncTy _ fnReturnType) = t
              in  do manage xobj -- manage inner lambdas but leave their bodies unvisited, they will be visited in the lifted version...
                     mapM_ unmanage captures
                     return (Right (XObj (Lst [fn, args, body]) i t))

            [def@(XObj Def _ _), nameSymbol@(XObj (Sym _ _) _ _), expr] ->
              do visitedExpr <- visit  expr
                 result <- unmanage expr
                 return $
                   case result of
                     Left e -> Left e
                     Right () ->
                       do okExpr <- visitedExpr
                          return (XObj (Lst [def, nameSymbol, okExpr]) i t)

            [letExpr@(XObj Let _ _), XObj (Arr bindings) bindi bindt, body] ->
              let Just letReturnType = t
              in case letReturnType of
                RefTy _ ->
                  return (Left (LetCantReturnRefTy xobj letReturnType))
                _ ->
                  do MemState preDeleters _ <- get
                     visitedBindings <- mapM (visitLetBinding ) (pairwise bindings)
                     visitedBody <- visit  body
                     result <- unmanage body
                     case result of
                       Left e -> return (Left e)
                       Right _ ->
                         do MemState postDeleters deps <- get
                            let diff = postDeleters Set.\\ preDeleters
                                newInfo = setDeletersOnInfo i diff
                                survivors = postDeleters Set.\\ diff -- Same as just pre deleters, right?!
                            put (MemState survivors deps)
                            --trace ("LET Pre: " ++ show preDeleters ++ "\nPost: " ++ show postDeleters ++ "\nDiff: " ++ show diff ++ "\nSurvivors: " ++ show survivors)
                            manage xobj
                            return $ do okBody <- visitedBody
                                        okBindings <- fmap (concatMap (\(n,x) -> [n, x])) (sequence visitedBindings)
                                        return (XObj (Lst [letExpr, XObj (Arr okBindings) bindi bindt, okBody]) newInfo t)

            -- Set!
            [setbangExpr@(XObj SetBang _ _), variable, value] ->
                 let varInfo = info variable
                     correctVariableAndMode =
                       case variable of
                         -- DISABLE FOR NOW: (XObj (Lst (XObj (Sym (SymPath _ "copy") _) _ _ : symObj@(XObj (Sym _ _) _ _) : _)) _ _) -> Right symObj
                         symObj@(XObj (Sym _ mode) _ _) -> Right (symObj, mode)
                         anythingElse -> Left (CannotSet anythingElse)
                 in
                 case correctVariableAndMode of
                   Left err ->
                     return (Left err)
                   Right (okCorrectVariable, okMode) ->
                     do MemState preDeleters _ <- get
                        ownsTheVarBefore <- case createDeleter okCorrectVariable of
                                              Nothing -> return (Right ())
                                              Just d -> if Set.member d preDeleters || isLookupGlobal okMode
                                                        then return (Right ())
                                                        else return (Left (UsingUnownedValue variable))

                        visitedValue <- visit  value
                        unmanage value -- The assigned value can't be used anymore
                        MemState managed deps <- get
                        -- Delete the value previously stored in the variable, if it's still alive
                        let deleters = case createDeleter okCorrectVariable of
                                         Just d  -> Set.fromList [d]
                                         Nothing -> Set.empty
                            newVariable =
                              case okMode of
                                Symbol -> error "How to handle this?"
                                LookupLocal captureMode ->
                                  if Set.size (Set.intersection managed deleters) == 1 -- The variable is still alive
                                  then variable { info = setDeletersOnInfo varInfo deleters }
                                  else variable -- don't add the new info = no deleter
                                LookupGlobal _ ->
                                  variable { info = setDeletersOnInfo varInfo deleters }

                            traceDeps = trace ("SET!-deleters for " ++ pretty xobj ++ " at " ++ prettyInfoFromXObj xobj ++ ":\n" ++
                                               "unmanaged " ++ pretty value ++ "\n" ++
                                               "managed: " ++ show managed ++ "\n" ++
                                               "deleters: " ++ show deleters ++ "\n")

                        case okMode of
                          Symbol -> error "Should only be be a global/local lookup symbol."
                          LookupLocal _ -> manage okCorrectVariable
                          LookupGlobal _ -> return ()

                        return $ do okValue <- visitedValue
                                    okOwnsTheVarBefore <- ownsTheVarBefore -- Force Either to fail
                                    return (XObj (Lst [setbangExpr, newVariable, okValue]) i t)

            [addressExpr@(XObj Address _ _), value] ->
              do visitedValue <- visit  value
                 return $ do okValue <- visitedValue
                             return (XObj (Lst [addressExpr, okValue]) i t)

            [theExpr@(XObj The _ _), typeXObj, value] ->
              do visitedValue <- visit  value
                 result <- transferOwnership  value xobj
                 return $ case result of
                            Left e -> Left e
                            Right _ -> do okValue <- visitedValue
                                          return (XObj (Lst [theExpr, typeXObj, okValue]) i t)

            [refExpr@(XObj Ref _ _), value] ->
              do visitedValue <- visit  value
                 case visitedValue of
                   Left e -> return (Left e)
                   Right visitedValue ->
                     do checkResult <- refCheck visitedValue
                        case checkResult of
                          Left e -> return (Left e)
                          Right () -> return $ Right (XObj (Lst [refExpr, visitedValue]) i t)

            doExpr@(XObj Do _ _) : expressions ->
              do visitedExpressions <- mapM (visit ) expressions
                 result <- transferOwnership  (last expressions) xobj
                 return $ case result of
                            Left e -> Left e
                            Right _ -> do okExpressions <- sequence visitedExpressions
                                          return (XObj (Lst (doExpr : okExpressions)) i t)

            [whileExpr@(XObj While _ _), expr, body] ->
              do MemState preDeleters _ <- get
                 visitedExpr <- visit  expr
                 MemState afterExprDeleters _ <- get
                 visitedBody <- visit  body
                 manage body
                 MemState postDeleters deps <- get
                 -- Visit an extra time to simulate repeated use
                 visitedExpr2 <- visit  expr
                 visitedBody2 <- visit  body
                 let diff = postDeleters \\ preDeleters
                 put (MemState (postDeleters \\ diff) deps) -- Same as just pre deleters, right?!
                 return $ do okExpr <- visitedExpr
                             okBody <- visitedBody
                             okExpr2 <- visitedExpr2 -- This evaluates the second visit so that it actually produces the error
                             okBody2 <- visitedBody2 -- And this one too. Laziness FTW.
                             let newInfo = setDeletersOnInfo i diff
                                 -- Also need to set deleters ON the expression (for first run through the loop)
                                 XObj objExpr objInfo objTy = okExpr
                                 newExprInfo = setDeletersOnInfo objInfo (afterExprDeleters \\ preDeleters)
                                 newExpr = XObj objExpr newExprInfo objTy
                             return (XObj (Lst [whileExpr, newExpr, okBody]) newInfo t)

            [ifExpr@(XObj If _ _), expr, ifTrue, ifFalse] ->
              do visitedExpr <- visit  expr
                 MemState preDeleters deps <- get

                 let (visitedTrue,  stillAliveTrue)  = runState (do { v <- visit  ifTrue;
                                                                      result <- transferOwnership  ifTrue xobj;
                                                                      return $ case result of
                                                                                 Left e -> error (show e) -- Left e
                                                                                 Right () -> v
                                                                    })
                                                       (MemState preDeleters deps)

                     (visitedFalse, stillAliveFalse) = runState (do { v <- visit  ifFalse;
                                                                      result <- transferOwnership   ifFalse xobj;
                                                                      return $ case result of
                                                                                 Left e -> error (show e) -- Left e
                                                                                 Right () -> v
                                                                    })
                                                       (MemState preDeleters deps)

                 let -- TODO! Handle deps from stillAliveTrue/stillAliveFalse
                     deletedInTrue  = preDeleters \\ (memStateDeleters stillAliveTrue)
                     deletedInFalse = preDeleters \\ (memStateDeleters stillAliveFalse)
                     deletedInBoth  = Set.intersection deletedInTrue deletedInFalse
                     createdInTrue  = (memStateDeleters stillAliveTrue)  \\ preDeleters
                     createdInFalse = (memStateDeleters stillAliveFalse) \\ preDeleters
                     selfDeleter = case createDeleter xobj of
                                     Just ok -> Set.fromList [ok]
                                     Nothing -> Set.empty
                     createdAndDeletedInTrue  = createdInTrue  \\ selfDeleter
                     createdAndDeletedInFalse = createdInFalse \\ selfDeleter
                     delsTrue  = Set.union (deletedInFalse \\ deletedInBoth) createdAndDeletedInTrue
                     delsFalse = Set.union (deletedInTrue  \\ deletedInBoth) createdAndDeletedInFalse
                     stillAliveAfter = preDeleters \\ (Set.union deletedInTrue deletedInFalse)

                     traceDeps = trace ("IF-deleters for " ++ pretty xobj ++ " at " ++ prettyInfoFromXObj xobj ++ " " ++ identifierStr xobj ++ ":\n" ++
                                        "preDeleters: " ++ show (preDeleters) ++ "\n" ++
                                        "stillAliveTrue: " ++ show (memStateDeleters stillAliveTrue) ++ "\n" ++
                                        "stillAliveFalse: " ++ show (memStateDeleters stillAliveFalse) ++ "\n" ++
                                        "createdInTrue: " ++ show (createdInTrue) ++ "\n" ++
                                        "createdInFalse: " ++ show (createdInFalse) ++ "\n" ++
                                        "createdAndDeletedInTrue: " ++ show (createdAndDeletedInTrue) ++ "\n" ++
                                        "createdAndDeletedInFalse: " ++ show (createdAndDeletedInFalse) ++ "\n" ++
                                        "deletedInTrue: " ++ show (deletedInTrue) ++ "\n" ++
                                        "deletedInFalse: " ++ show (deletedInFalse) ++ "\n" ++
                                        "deletedInBoth: " ++ show (deletedInBoth) ++ "\n" ++
                                        "delsTrue: " ++ show (delsTrue) ++ "\n" ++
                                        "delsFalse: " ++ show (delsFalse) ++ "\n" ++
                                        "stillAlive: " ++ show (stillAliveAfter) ++ "\n"
                                       )

                 put (MemState stillAliveAfter deps)
                 manage xobj

                 return $ do okExpr  <- visitedExpr
                             okTrue  <- visitedTrue
                             okFalse <- visitedFalse
                             return (XObj (Lst [ifExpr, okExpr, del okTrue delsTrue, del okFalse delsFalse]) i t)
            f : args ->
              do visitedF <- visit  f
                 visitedArgs <- sequence <$> mapM (visitArg ) args
                 manage xobj
                 return $ do okF <- visitedF
                             okArgs <- visitedArgs
                             Right (XObj (Lst (okF : okArgs)) i t)

            [] -> return (Right xobj)
        visitList _ = error "Must visit list."

        visitLetBinding :: (XObj, XObj) -> State MemState (Either TypeError (XObj, XObj))
        visitLetBinding  (name, expr) =
          do visitedExpr <- visit  expr
             result <- transferOwnership  expr name
             return $ case result of
                        Left e -> Left e
                        Right _ -> do okExpr <- visitedExpr
                                      return (name, okExpr)

        visitArg :: XObj -> State MemState (Either TypeError XObj)
        visitArg  xobj@(XObj _ _ (Just t)) =
          if isManaged typeEnv t
          then do visitedXObj <- visit  xobj
                  result <- unmanage xobj
                  case result of
                    Left e  -> return (Left e)
                    Right _ -> return visitedXObj
          else visit  xobj
        visitArg  xobj@XObj{} =
          visit  xobj

        createDeleter :: XObj -> Maybe Deleter
        createDeleter xobj =
          case ty xobj of
            Just t -> let var = varOfXObj xobj
                      in  if isManaged typeEnv t && not (isExternalType typeEnv t)
                          then case nameOfPolymorphicFunction typeEnv globalEnv (FuncTy [t] UnitTy) "delete" of
                                 Just pathOfDeleteFunc -> Just (ProperDeleter pathOfDeleteFunc var)
                                 Nothing -> --trace ("Found no delete function for " ++ var ++ " : " ++ (showMaybeTy (ty xobj)))
                                            Just (FakeDeleter var)
                          else Nothing
            Nothing -> error ("No type, can't manage " ++ show xobj)

        manage :: XObj -> State MemState ()
        manage xobj =
          if isSymbolThatCaptures xobj -- When visiting lifted lambdas, don't manage symbols that capture (they are owned by the environment).
          then return ()
          else case createDeleter xobj of
                 Just deleter -> do MemState deleters deps <- get
                                    let newDeleters = Set.insert deleter deleters
                                        Just t = ty xobj
                                        newDeps = deps ++ depsForDeleteFunc typeEnv globalEnv t
                                    put (MemState newDeleters newDeps)
                 Nothing -> return ()

        deletersMatchingXObj :: XObj -> Set.Set Deleter -> [Deleter]
        deletersMatchingXObj xobj deleters =
          let var = varOfXObj xobj
          in  Set.toList $ Set.filter (\d -> case d of
                                               ProperDeleter { deleterVariable = dv } -> dv == var
                                               FakeDeleter   { deleterVariable = dv } -> dv == var)
                                      deleters

        isSymbolThatCaptures :: XObj -> Bool
        isSymbolThatCaptures xobj =
          case xobj of
            XObj (Sym _ (LookupLocal Capture)) _ _ -> True
            _ -> False

        unmanage :: XObj -> State MemState (Either TypeError ())
        unmanage xobj =
          let Just t = ty xobj
              Just i = info xobj
          in if isManaged typeEnv t && not (isGlobalFunc xobj) && not (isExternalType typeEnv t) && not (isSymbolThatCaptures xobj)
             then do MemState deleters deps <- get
                     case deletersMatchingXObj xobj deleters of
                       [] -> return (Left (UsingUnownedValue xobj))
                       [one] -> let newDeleters = Set.delete one deleters
                                in  do put (MemState newDeleters deps)
                                       return (Right ())
                       _ -> error "Too many variables with the same name in set."
             else return (Right ())

        -- | Check that the value being referenced hasn't already been given away
        refCheck :: XObj -> State MemState (Either TypeError ())
        refCheck xobj =
          let Just i = info xobj
              Just t = ty xobj
              isGlobalVariable = case xobj of
                                   XObj (Sym _ (LookupGlobal _)) _ _ -> True
                                   _ -> False
          in if not isGlobalVariable && not (isGlobalFunc xobj) && isManaged typeEnv t && not (isExternalType typeEnv t)
             then do MemState deleters deps <- get
                     case deletersMatchingXObj xobj deleters of
                       [] ->  return (Left (GettingReferenceToUnownedValue xobj))
                       [_] -> return (return ())
                       _ -> error "Too many variables with the same name in set."
             else return (return ())

        transferOwnership :: XObj -> XObj -> State MemState (Either TypeError ())
        transferOwnership  from to =
          do result <- unmanage from
             case result of
               Left e -> return (Left e)
               Right _ -> do manage to --(trace ("Transfered from " ++ getName from ++ " '" ++ varOfXObj from ++ "' to " ++ getName to ++ " '" ++ varOfXObj to ++ "'") to)
                             return (Right ())

        varOfXObj :: XObj -> String
        varOfXObj xobj =
          case xobj of
            XObj (Sym (SymPath [] name) _) _ _ -> name
            _ -> case info xobj of
                   Just i -> freshVar i
                   Nothing -> error ("Missing info on " ++ show xobj)

suffixTyVars :: String -> Ty -> Ty
suffixTyVars suffix t =
  case t of
    (VarTy key) -> (VarTy (key ++ suffix))
    (FuncTy argTys retTy) -> FuncTy (map (suffixTyVars suffix) argTys) (suffixTyVars suffix retTy)
    (StructTy name tyArgs) -> StructTy name (fmap (suffixTyVars suffix) tyArgs)
    (PointerTy x) -> PointerTy (suffixTyVars suffix x)
    (RefTy x) -> RefTy (suffixTyVars suffix x)
    _ -> t

isGlobalFunc :: XObj -> Bool
isGlobalFunc xobj =
  case xobj of
    XObj (InterfaceSym _) _ (Just (FuncTy _ _)) -> True
    XObj (MultiSym _ _) _ (Just (FuncTy _ _)) -> True
    XObj (Sym _ (LookupGlobal _)) _ (Just (FuncTy _ _)) -> True
    XObj (Sym _ (LookupGlobalOverride _)) _ (Just (FuncTy _ _)) -> True
    _ -> False











data AllocationMode = StackAlloc | HeapAlloc

{-# ANN module "HLint: ignore Reduce duplication" #-}
-- | This function creates a "Type Module" with the same name as the type being defined.
--   A type module provides a namespace for all the functions that area automatically
--   generated by a deftype.
moduleForDeftype :: TypeEnv -> Env -> [String] -> String -> [Ty] -> [XObj] -> Maybe Info -> Maybe Env -> Either String (String, XObj, [XObj])
moduleForDeftype typeEnv env pathStrings typeName typeVariables rest i existingEnv =
  let typeModuleName = typeName
      typeModuleEnv = case existingEnv of
                             Just env -> env
                             Nothing -> Env (Map.fromList []) (Just env) (Just typeModuleName) [] ExternalEnv 0
      -- The variable 'insidePath' is the path used for all member functions inside the 'typeModule'.
      -- For example (module Vec2 [x Float]) creates bindings like Vec2.create, Vec2.x, etc.
      insidePath = pathStrings ++ [typeModuleName]
  in do validateMemberCases typeEnv typeVariables rest
        let structTy = StructTy typeName typeVariables
        (okMembers, membersDeps) <- templatesForMembers typeEnv env insidePath structTy rest
        okInit <- binderForInit insidePath structTy rest
        --okNew <- templateForNew insidePath structTy rest
        (okStr, strDeps) <- binderForStrOrPrn typeEnv env insidePath structTy rest "str"
        (okPrn, _) <- binderForStrOrPrn typeEnv env insidePath structTy rest "prn"
        (okDelete, deleteDeps) <- binderForDelete typeEnv env insidePath structTy rest
        (okCopy, copyDeps) <- binderForCopy typeEnv env insidePath structTy rest
        let funcs = okInit  : okStr : okPrn : okDelete : okCopy : okMembers
            moduleEnvWithBindings = addListOfBindings typeModuleEnv funcs
            typeModuleXObj = XObj (Mod moduleEnvWithBindings) i (Just ModuleTy)
            deps = deleteDeps ++ membersDeps ++ copyDeps ++ strDeps
        return (typeModuleName, typeModuleXObj, deps)

-- | Will generate getters/setters/updaters when registering EXTERNAL types.
-- | i.e. (register-type VRUnicornData [hp Int, magic Float])
-- | TODO: Remove duplication shared by moduleForDeftype-function.
bindingsForRegisteredType :: TypeEnv -> Env -> [String] -> String -> [XObj] -> Maybe Info -> Maybe Env -> Either String (String, XObj, [XObj])
bindingsForRegisteredType typeEnv env pathStrings typeName rest i existingEnv =
  let typeModuleName = typeName
      typeModuleEnv = case existingEnv of
                             Just env -> env
                             Nothing -> Env (Map.fromList []) (Just env) (Just typeModuleName) [] ExternalEnv 0
      insidePath = pathStrings ++ [typeModuleName]
  in do validateMemberCases typeEnv [] rest
        let structTy = StructTy typeName []
        (binders, deps) <- templatesForMembers typeEnv env insidePath structTy rest
        okInit <- binderForInit insidePath structTy rest
        --okNew <- templateForNew insidePath structTy rest
        (okStr, strDeps) <- binderForStrOrPrn typeEnv env insidePath structTy rest "str"
        (okPrn, _) <- binderForStrOrPrn typeEnv env insidePath structTy rest "prn"
        let moduleEnvWithBindings = addListOfBindings typeModuleEnv (okInit : okStr : okPrn : binders)
            typeModuleXObj = XObj (Mod moduleEnvWithBindings) i (Just ModuleTy)
        return (typeModuleName, typeModuleXObj, deps ++ strDeps)



-- | Generate all the templates for ALL the member variables in a deftype declaration.
templatesForMembers :: TypeEnv -> Env -> [String] -> Ty -> [XObj] -> Either String ([(String, Binder)], [XObj])
templatesForMembers typeEnv env insidePath structTy [XObj (Arr membersXobjs) _ _] =
  let bindersAndDeps = concatMap (templatesForSingleMember typeEnv env insidePath structTy) (pairwise membersXobjs)
  in  Right (map fst bindersAndDeps, concatMap snd bindersAndDeps)
templatesForMembers _ _ _ _ _ = Left "Can't create member functions for type with more than one case (yet)."

-- | Generate the templates for a single member in a deftype declaration.
templatesForSingleMember :: TypeEnv -> Env -> [String] -> Ty -> (XObj, XObj) -> [((String, Binder), [XObj])]
templatesForSingleMember typeEnv env insidePath p@(StructTy typeName _) (nameXObj, typeXObj) =
  let Just t = xobjToTy typeXObj
      memberName = getName nameXObj
  in [instanceBinderWithDeps (SymPath insidePath memberName) (FuncTy [RefTy p] (RefTy t)) (templateGetter (mangle memberName) t)
     , if isTypeGeneric t
       then (templateGenericSetter insidePath p t memberName, [])
       else instanceBinderWithDeps (SymPath insidePath ("set-" ++ memberName)) (FuncTy [p, t] p) (templateSetter typeEnv env (mangle memberName) t)
     ,instanceBinderWithDeps (SymPath insidePath ("set-" ++ memberName ++ "!")) (FuncTy [RefTy (p), t] UnitTy) (templateMutatingSetter typeEnv env (mangle memberName) t)
     ,instanceBinderWithDeps (SymPath insidePath ("update-" ++ memberName))
                                                            (FuncTy [p, FuncTy [t] t] p)
                                                            (templateUpdater (mangle memberName))]

-- | The template for getters of a deftype.
templateGetter :: String -> Ty -> Template
templateGetter member memberTy =
  Template
    (FuncTy [RefTy (VarTy "p")] (VarTy "t"))
    (const (toTemplate "$t $NAME($(Ref p) p)"))
    (const $
     let fixForVoidStarMembers =
           if isFunctionType memberTy && (not (isTypeGeneric memberTy))
           then "(" ++ tyToCLambdaFix (RefTy memberTy) ++ ")"
           else ""
     in  (toTemplate ("$DECL { return " ++ fixForVoidStarMembers ++ "(&(p->" ++ member ++ ")); }\n")))
    (const [])

-- | The template for setters of a concrete deftype.
templateSetter :: TypeEnv -> Env -> String -> Ty -> Template
templateSetter typeEnv env memberName memberTy =
  let callToDelete = memberDeletion typeEnv env (memberName, memberTy)
  in
  Template
    (FuncTy [VarTy "p", VarTy "t"] (VarTy "p"))
    (const (toTemplate "$p $NAME($p p, $t newValue)"))
    (const (toTemplate (unlines ["$DECL {"
                                ,callToDelete
                                ,"    p." ++ memberName ++ " = newValue;"
                                ,"    return p;"
                                ,"}\n"])))
    (\_ -> if | isManaged typeEnv memberTy -> depsOfPolymorphicFunction typeEnv env [] "delete" (typesDeleterFunctionType memberTy)
              | isFunctionType memberTy -> [defineFunctionTypeAlias memberTy]
              | otherwise -> [])

-- | The template for setters of a generic deftype.
templateGenericSetter :: [String] -> Ty -> Ty -> String -> (String, Binder)
templateGenericSetter pathStrings originalStructTy memberTy memberName =
  defineTypeParameterizedTemplate templateCreator path (FuncTy [originalStructTy, memberTy] originalStructTy)
  where path = SymPath pathStrings ("set-" ++ memberName)
        t = (FuncTy [VarTy "p", VarTy "t"] (VarTy "p"))
        templateCreator = TemplateCreator $
          \typeEnv env ->
            Template
            t
            (const (toTemplate "$p $NAME($p p, $t newValue)"))
            (\(FuncTy [_, memberTy] _) ->
               (let callToDelete = memberDeletion typeEnv env (memberName, memberTy)
                in  (toTemplate (unlines ["$DECL {"
                                         ,callToDelete
                                         ,"    p." ++ memberName ++ " = newValue;"
                                         ,"    return p;"
                                         ,"}\n"]))))
            (\(FuncTy [_, memberTy] _) ->
               if isManaged typeEnv memberTy
               then depsOfPolymorphicFunction typeEnv env [] "delete" (typesDeleterFunctionType memberTy)
               else [])

-- | The template for mutating setters of a deftype.
templateMutatingSetter :: TypeEnv -> Env -> String -> Ty -> Template
templateMutatingSetter typeEnv env memberName memberTy =
  Template
    (FuncTy [RefTy (VarTy "p"), VarTy "t"] UnitTy)
    (const (toTemplate "void $NAME($p* pRef, $t newValue)"))
    (const (toTemplate (unlines ["$DECL {"
                                ,"    pRef->" ++ memberName ++ " = newValue;"
                                ,"}\n"])))
    (const [])

-- | The template for updater functions of a deftype.
-- | (allows changing a variable by passing an transformation function).
templateUpdater :: String -> Template
templateUpdater member =
  Template
    (FuncTy [VarTy "p", FuncTy [VarTy "t"] (VarTy "t")] (VarTy "p"))
    (const (toTemplate "$p $NAME($p p, Lambda updater)")) -- "Lambda" used to be: $(Fn [t] t)
    (const (toTemplate (unlines ["$DECL {"
                                ,"    p." ++ member ++ " = " ++ (templateCodeForCallingLambda "updater" (FuncTy [VarTy "t"] (VarTy "t")) ["p." ++ member]) ++ ";"
                                ,"    return p;"
                                ,"}\n"])))
    (\(FuncTy [_, t@(FuncTy fArgTys fRetTy)] _) ->
       if isTypeGeneric fRetTy
       then []
       else [defineFunctionTypeAlias t, defineFunctionTypeAlias (FuncTy (lambdaEnvTy : fArgTys) fRetTy)])

-- | Helper function to create the binder for the 'init' template.
binderForInit :: [String] -> Ty -> [XObj] -> Either String (String, Binder)
binderForInit insidePath structTy@(StructTy typeName _) [XObj (Arr membersXObjs) _ _] =
  if isTypeGeneric structTy
  then Right (genericInit StackAlloc insidePath structTy membersXObjs)
  else Right $ instanceBinder (SymPath insidePath "init")
                (FuncTy (initArgListTypes membersXObjs) structTy)
                (concreteInit StackAlloc structTy membersXObjs)

-- | Generate a list of types from a deftype declaration.
initArgListTypes :: [XObj] -> [Ty]
initArgListTypes xobjs = map (\(_, x) -> fromJust (xobjToTy x)) (pairwise xobjs)

-- | The template for the 'init' and 'new' functions for a concrete deftype.
concreteInit :: AllocationMode -> Ty -> [XObj] -> Template
concreteInit allocationMode originalStructTy@(StructTy typeName typeVariables) membersXObjs =
  Template
    (FuncTy (map snd (memberXObjsToPairs membersXObjs)) (VarTy "p"))
    (\(FuncTy _ concreteStructTy) ->
     let mappings = unifySignatures originalStructTy concreteStructTy
         correctedMembers = replaceGenericTypeSymbolsOnMembers mappings membersXObjs
         memberPairs = memberXObjsToPairs correctedMembers
     in  (toTemplate $ "$p $NAME(" ++ joinWithComma (map memberArg memberPairs) ++ ")"))
    (const (tokensForInit allocationMode typeName membersXObjs))
    (\(FuncTy _ _) -> [])

-- | The template for the 'init' and 'new' functions for a generic deftype.
genericInit :: AllocationMode -> [String] -> Ty -> [XObj] -> (String, Binder)
genericInit allocationMode pathStrings originalStructTy@(StructTy typeName _) membersXObjs =
  defineTypeParameterizedTemplate templateCreator path t
  where path = SymPath pathStrings "init"
        t = (FuncTy (map snd (memberXObjsToPairs membersXObjs)) originalStructTy)
        templateCreator = TemplateCreator $
          \typeEnv env ->
            Template
            (FuncTy (map snd (memberXObjsToPairs membersXObjs)) (VarTy "p"))
            (\(FuncTy _ concreteStructTy) ->
               let mappings = unifySignatures originalStructTy concreteStructTy
                   correctedMembers = replaceGenericTypeSymbolsOnMembers mappings membersXObjs
                   memberPairs = memberXObjsToPairs correctedMembers
               in  (toTemplate $ "$p $NAME(" ++ joinWithComma (map memberArg memberPairs) ++ ")"))
            (const (tokensForInit allocationMode typeName membersXObjs))
            (\(FuncTy _ concreteStructTy) ->
               case concretizeType typeEnv concreteStructTy of
                 Left err -> error (err ++ ". This error should not crash the compiler - change return type to Either here.")
                 Right ok -> ok
            )

tokensForInit :: AllocationMode -> String -> [XObj] -> [Token]
tokensForInit allocationMode typeName membersXObjs =
  toTemplate $ unlines [ "$DECL {"
                       , case allocationMode of
                           StackAlloc -> "    $p instance;"
                           HeapAlloc ->  "    $p instance = CARP_MALLOC(sizeof(" ++ typeName ++ "));"
                       , joinWith "\n" (map (memberAssignment allocationMode) (memberXObjsToPairs membersXObjs))
                       , "    return instance;"
                       , "}"]

-- | Creates the C code for an arg to the init function.
-- | i.e. "(deftype A [x Int])" will generate "int x" which
-- | will be used in the init function like this: "A_init(int x)"
memberArg :: (String, Ty) -> String
memberArg (memberName, memberTy) =
  tyToCLambdaFix (templitizeTy memberTy) ++ " " ++ memberName

-- | If the type is just a type variable; create a template type variable by appending $ in front of it's name
templitizeTy :: Ty -> Ty
templitizeTy (VarTy vt) = VarTy ("$" ++ vt)
templitizeTy (FuncTy argTys retTy) = FuncTy (map templitizeTy argTys) (templitizeTy retTy)
templitizeTy (StructTy name tys) = StructTy name (map templitizeTy tys)
templitizeTy (RefTy t) = RefTy (templitizeTy t)
templitizeTy (PointerTy t) = PointerTy (templitizeTy t)
templitizeTy t = t

-- | Helper function to create the binder for the 'str' template.
binderForStrOrPrn :: TypeEnv -> Env -> [String] -> Ty -> [XObj] -> String -> Either String ((String, Binder), [XObj])
binderForStrOrPrn typeEnv env insidePath structTy@(StructTy typeName _) [XObj (Arr membersXObjs) _ _] strOrPrn =
  if isTypeGeneric structTy
  then Right (genericStr insidePath structTy membersXObjs strOrPrn, [])
  else Right (instanceBinderWithDeps (SymPath insidePath strOrPrn)
              (FuncTy [RefTy structTy] StringTy)
              (concreteStr typeEnv env structTy (memberXObjsToPairs membersXObjs) strOrPrn))

-- | The template for the 'str' function for a concrete deftype.
concreteStr :: TypeEnv -> Env -> Ty -> [(String, Ty)] -> String -> Template
concreteStr typeEnv env concreteStructTy@(StructTy typeName _) memberPairs strOrPrn =
  Template
    (FuncTy [RefTy concreteStructTy] StringTy)
    (\(FuncTy [RefTy structTy] StringTy) -> (toTemplate $ "String $NAME(" ++ tyToCLambdaFix structTy ++ " *p)"))
    (\(FuncTy [RefTy structTy@(StructTy _ concreteMemberTys)] StringTy) ->
        (tokensForStr typeEnv env typeName memberPairs concreteStructTy))
    (\(ft@(FuncTy [RefTy structTy@(StructTy _ concreteMemberTys)] StringTy)) ->
       concatMap (depsOfPolymorphicFunction typeEnv env [] "prn" . typesStrFunctionType typeEnv)
                 (filter (\t -> (not . isExternalType typeEnv) t && (not . isFullyGenericType) t)
                  (map snd memberPairs)))

-- | The template for the 'str' function for a generic deftype.
genericStr :: [String] -> Ty -> [XObj] -> String -> (String, Binder)
genericStr pathStrings originalStructTy@(StructTy typeName varTys) membersXObjs strOrPrn =
  defineTypeParameterizedTemplate templateCreator path t
  where path = SymPath pathStrings strOrPrn
        t = FuncTy [(RefTy originalStructTy)] StringTy
        members = memberXObjsToPairs membersXObjs
        templateCreator = TemplateCreator $
          \typeEnv env ->
            Template
            t
            (\(FuncTy [RefTy concreteStructTy] StringTy) ->
               (toTemplate $ "String $NAME(" ++ tyToCLambdaFix concreteStructTy ++ " *p)"))
            (\(FuncTy [RefTy concreteStructTy@(StructTy _ concreteMemberTys)] StringTy) ->
               let mappings = unifySignatures originalStructTy concreteStructTy
                   correctedMembers = replaceGenericTypeSymbolsOnMembers mappings membersXObjs
                   memberPairs = memberXObjsToPairs correctedMembers
               in (tokensForStr typeEnv env typeName memberPairs concreteStructTy))
            (\(ft@(FuncTy [RefTy concreteStructTy@(StructTy _ concreteMemberTys)] StringTy)) ->
               let mappings = unifySignatures originalStructTy concreteStructTy
                   correctedMembers = replaceGenericTypeSymbolsOnMembers mappings membersXObjs
                   memberPairs = memberXObjsToPairs correctedMembers
               in  concatMap (depsOfPolymorphicFunction typeEnv env [] "prn" . typesStrFunctionType typeEnv)
                   (filter (\t -> (not . isExternalType typeEnv) t && (not . isFullyGenericType) t)
                    (map snd memberPairs))
                   ++
                   (if isTypeGeneric concreteStructTy then [] else [defineFunctionTypeAlias ft]))

tokensForStr :: TypeEnv -> Env -> String -> [(String, Ty)] -> Ty -> [Token]
tokensForStr typeEnv env typeName memberPairs concreteStructTy  =
  (toTemplate $ unlines [ "$DECL {"
                        , "  // convert members to String here:"
                        , "  String temp = NULL;"
                        , "  int tempsize = 0;"
                        , "  (void)tempsize; // that way we remove the occasional unused warning "
                        , calculateStructStrSize typeEnv env memberPairs concreteStructTy
                        , "  String buffer = CARP_MALLOC(size);"
                        , "  String bufferPtr = buffer;"
                        , ""
                        , "  snprintf(bufferPtr, size, \"(%s \", \"" ++ typeName ++ "\");"
                        , "  bufferPtr += strlen(\"" ++ typeName ++ "\") + 2;\n"
                        , joinWith "\n" (map (memberPrn typeEnv env) memberPairs)
                        , "  bufferPtr--;"
                        , "  snprintf(bufferPtr, size, \")\");"
                        , "  return buffer;"
                        , "}"])

-- | Figure out how big the string needed for the string representation of the struct has to be.
calculateStructStrSize :: TypeEnv -> Env -> [(String, Ty)] -> Ty -> String
calculateStructStrSize typeEnv env members structTy@(StructTy name _) =
  "  int size = snprintf(NULL, 0, \"(%s )\", \"" ++ name ++ "\");\n" ++
    unlines (map memberPrnSize members)
  where memberPrnSize (memberName, memberTy) =
          let refOrNotRefType = if isManaged typeEnv memberTy then RefTy memberTy else memberTy
              maybeTakeAddress = if isManaged typeEnv memberTy then "&" else ""
              strFuncType = FuncTy [refOrNotRefType] StringTy
           in case nameOfPolymorphicFunction typeEnv env strFuncType "prn" of
                Just strFunctionPath ->
                  unlines ["  temp = " ++ pathToC strFunctionPath ++ "(" ++ maybeTakeAddress ++ "p->" ++ memberName ++ "); "
                          , "  size += snprintf(NULL, 0, \"%s \", temp);"
                          , "  if(temp) { CARP_FREE(temp); temp = NULL; }"
                          ]
                Nothing ->
                  if isExternalType typeEnv memberTy
                  then unlines [ "  size +=  snprintf(NULL, 0, \"%p \", p->" ++ memberName ++ ");"
                               , "  if(temp) { CARP_FREE(temp); temp = NULL; }"
                               ]
                  else "  // Failed to find str function for " ++ memberName ++ " : " ++ show memberTy ++ "\n"

-- | Generate C code for converting a member variable to a string and appending it to a buffer.
memberPrn :: TypeEnv -> Env -> (String, Ty) -> String
memberPrn typeEnv env (memberName, memberTy) =
  let refOrNotRefType = if isManaged typeEnv memberTy then RefTy memberTy else memberTy
      maybeTakeAddress = if isManaged typeEnv memberTy then "&" else ""
      strFuncType = FuncTy [refOrNotRefType] StringTy
   in case nameOfPolymorphicFunction typeEnv env strFuncType "prn" of
        Just strFunctionPath ->
          unlines ["  temp = " ++ pathToC strFunctionPath ++ "(" ++ maybeTakeAddress ++ "p->" ++ memberName ++ ");"
                  , "  snprintf(bufferPtr, size, \"%s \", temp);"
                  , "  bufferPtr += strlen(temp) + 1;"
                  , "  if(temp) { CARP_FREE(temp); temp = NULL; }"
                  ]
        Nothing ->
          if isExternalType typeEnv memberTy
          then unlines [ "  tempsize = snprintf(NULL, 0, \"%p\", p->" ++ memberName ++ ");"
                       , "  temp = malloc(tempsize);"
                       , "  snprintf(temp, tempsize, \"%p\", p->" ++ memberName ++ ");"
                       , "  snprintf(bufferPtr, size, \"%s \", temp);"
                       , "  bufferPtr += strlen(temp) + 1;"
                       , "  if(temp) { CARP_FREE(temp); temp = NULL; }"
                       ]
          else "  // Failed to find str function for " ++ memberName ++ " : " ++ show memberTy ++ "\n"

-- | Generate C code for assigning to a member variable.
-- | Needs to know if the instance is a pointer or stack variable.
memberAssignment :: AllocationMode -> (String, Ty) -> String
memberAssignment allocationMode (memberName, _) = "    instance" ++ sep ++ memberName ++ " = " ++ memberName ++ ";"
  where sep = case allocationMode of
                StackAlloc -> "."
                HeapAlloc -> "->"



-- | Helper function to create the binder for the 'delete' template.
binderForDelete :: TypeEnv -> Env -> [String] -> Ty -> [XObj] -> Either String ((String, Binder), [XObj])
binderForDelete typeEnv env insidePath structTy@(StructTy typeName _) [XObj (Arr membersXObjs) _ _] =
  if isTypeGeneric structTy
  then Right (genericDelete insidePath structTy membersXObjs, [])
  else Right (instanceBinderWithDeps (SymPath insidePath "delete")
             (FuncTy [structTy] UnitTy)
             (concreteDelete typeEnv env (memberXObjsToPairs membersXObjs)))

-- | The template for the 'delete' function of a concrete deftype.
concreteDelete :: TypeEnv -> Env -> [(String, Ty)] -> Template
concreteDelete typeEnv env members =
  Template
   (FuncTy [VarTy "p"] UnitTy)
   (const (toTemplate "void $NAME($p p)"))
   (const (toTemplate $ unlines [ "$DECL {"
                                , joinWith "\n" (map (memberDeletion typeEnv env) members)
                                , "}"]))
   (\_ -> concatMap (depsOfPolymorphicFunction typeEnv env [] "delete" . typesDeleterFunctionType)
                    (filter (isManaged typeEnv) (map snd members)))

-- | The template for the 'delete' function of a generic deftype.
genericDelete :: [String] -> Ty -> [XObj] -> (String, Binder)
genericDelete pathStrings originalStructTy membersXObjs =
  defineTypeParameterizedTemplate templateCreator path (FuncTy [originalStructTy] UnitTy)
  where path = SymPath pathStrings "delete"
        t = (FuncTy [VarTy "p"] UnitTy)
        templateCreator = TemplateCreator $
          \typeEnv env ->
            Template
            t
            (const (toTemplate "void $NAME($p p)"))
            (\(FuncTy [concreteStructTy] UnitTy) ->
               let mappings = unifySignatures originalStructTy concreteStructTy
                   correctedMembers = replaceGenericTypeSymbolsOnMembers mappings membersXObjs
                   memberPairs = memberXObjsToPairs correctedMembers
               in  (toTemplate $ unlines [ "$DECL {"
                                         , joinWith "\n" (map (memberDeletion typeEnv env) memberPairs)
                                         , "}"]))
            (\(FuncTy [concreteStructTy] UnitTy) ->
               let mappings = unifySignatures originalStructTy concreteStructTy
                   correctedMembers = replaceGenericTypeSymbolsOnMembers mappings membersXObjs
                   memberPairs = memberXObjsToPairs correctedMembers
               in  if isTypeGeneric concreteStructTy
                   then []
                   else concatMap (depsOfPolymorphicFunction typeEnv env [] "delete" . typesDeleterFunctionType)
                                  (filter (isManaged typeEnv) (map snd memberPairs)))

-- | Generate the C code for deleting a single member of the deftype.
-- | TODO: Should return an Either since this can fail!
memberDeletion :: TypeEnv -> Env -> (String, Ty) -> String
memberDeletion typeEnv env (memberName, memberType) =
  case findFunctionForMember typeEnv env "delete" (typesDeleterFunctionType memberType) (memberName, memberType) of
    FunctionFound functionFullName -> "    " ++ functionFullName ++ "(p." ++ memberName ++ ");"
    FunctionNotFound msg -> error msg
    FunctionIgnored -> "    /* Ignore non-managed member '" ++ memberName ++ "' */"



-- | Helper function to create the binder for the 'copy' template.
binderForCopy :: TypeEnv -> Env -> [String] -> Ty -> [XObj] -> Either String ((String, Binder), [XObj])
binderForCopy typeEnv env insidePath structTy@(StructTy typeName _) [XObj (Arr membersXObjs) _ _] =
  if isTypeGeneric structTy
  then Right (genericCopy insidePath structTy membersXObjs, [])
  else Right (instanceBinderWithDeps (SymPath insidePath "copy")
              (FuncTy [RefTy structTy] structTy)
              (concreteCopy typeEnv env (memberXObjsToPairs membersXObjs)))

-- | The template for the 'copy' function of a concrete deftype.
concreteCopy :: TypeEnv -> Env -> [(String, Ty)] -> Template
concreteCopy typeEnv env memberPairs =
  Template
   (FuncTy [RefTy (VarTy "p")] (VarTy "p"))
   (const (toTemplate "$p $NAME($p* pRef)"))
   (const (tokensForCopy typeEnv env memberPairs))
   (\_ -> concatMap (depsOfPolymorphicFunction typeEnv env [] "copy" . typesCopyFunctionType)
                    (filter (isManaged typeEnv) (map snd memberPairs)))

-- | The template for the 'copy' function of a generic deftype.
genericCopy :: [String] -> Ty -> [XObj] -> (String, Binder)
genericCopy pathStrings originalStructTy membersXObjs =
  defineTypeParameterizedTemplate templateCreator path (FuncTy [RefTy originalStructTy] originalStructTy)
  where path = SymPath pathStrings "copy"
        t = (FuncTy [RefTy (VarTy "p")] (VarTy "p"))
        templateCreator = TemplateCreator $
          \typeEnv env ->
            Template
            t
            (const (toTemplate "$p $NAME($p* pRef)"))
            (\(FuncTy [RefTy concreteStructTy] _) ->
               let mappings = unifySignatures originalStructTy concreteStructTy
                   correctedMembers = replaceGenericTypeSymbolsOnMembers mappings membersXObjs
                   memberPairs = memberXObjsToPairs correctedMembers
               in (tokensForCopy typeEnv env memberPairs))
            (\(FuncTy [RefTy concreteStructTy] _) ->
               let mappings = unifySignatures originalStructTy concreteStructTy
                   correctedMembers = replaceGenericTypeSymbolsOnMembers mappings membersXObjs
                   memberPairs = memberXObjsToPairs correctedMembers
               in  if isTypeGeneric concreteStructTy
                   then []
                   else concatMap (depsOfPolymorphicFunction typeEnv env [] "copy" . typesCopyFunctionType)
                                  (filter (isManaged typeEnv) (map snd memberPairs)))

tokensForCopy :: TypeEnv -> Env -> [(String, Ty)] -> [Token]
tokensForCopy typeEnv env memberPairs=
  (toTemplate $ unlines [ "$DECL {"
                        , "    $p copy = *pRef;"
                        , joinWith "\n" (map (memberCopy typeEnv env) memberPairs)
                        , "    return copy;"
                        , "}"])

-- | Generate the C code for copying the member of a deftype.
-- | TODO: Should return an Either since this can fail!
memberCopy :: TypeEnv -> Env -> (String, Ty) -> String
memberCopy typeEnv env (memberName, memberType) =
  case findFunctionForMember typeEnv env "copy" (typesCopyFunctionType memberType) (memberName, memberType) of
    FunctionFound functionFullName ->
      "    copy." ++ memberName ++ " = " ++ functionFullName ++ "(&(pRef->" ++ memberName ++ "));"
    FunctionNotFound msg -> error msg
    FunctionIgnored -> "    /* Ignore non-managed member '" ++ memberName ++ "' */"











-- | Create a binding pair used for adding a template instantiation to an environment.
instanceBinder :: SymPath -> Ty -> Template -> (String, Binder)
instanceBinder path@(SymPath _ name) actualType template =
  let (x, _) = instantiateTemplate path actualType template
  in  (name, Binder emptyMeta x)

-- | Create a binding pair and don't discard the dependencies
instanceBinderWithDeps :: SymPath -> Ty -> Template -> ((String, Binder), [XObj])
instanceBinderWithDeps path@(SymPath _ name) actualType template =
  let (x, deps) = instantiateTemplate path actualType template
  in  ((name, Binder emptyMeta x), deps)

-- | Templates are instructions for the compiler to generate some C-code
-- | based on some template and the names and types to fill into the template.
-- | Templates are generic and need to be given an explicit type to generate the
-- | correct code.

-- | Example:
-- | template1 : ((Array T) -> Int) = "int length__T(<T> xs) { return xs->len; }"
-- | Given the type ((Array Float) -> Int) the following code is produced:
-- | "int length__Float(Array__Float xs) { return xs->len; }"

-- | Create a binding pair used for adding a template definition to an environment.
defineTemplate :: SymPath -> Ty -> [Token] -> [Token] -> (Ty -> [XObj]) -> (String, Binder)
defineTemplate path t declaration definition depsFunc =
  let (SymPath _ name) = path
      template = Template t (const declaration) (const definition) depsFunc
      i = Info 0 0 (show path ++ ".template") Set.empty 0
      defLst = [XObj (Deftemplate (TemplateCreator (\_ _ -> template))) Nothing Nothing, XObj (Sym path Symbol) Nothing Nothing]
  in  (name, Binder emptyMeta (XObj (Lst defLst) (Just i) (Just t)))

-- | The more advanced version of a template, where the code can vary depending on the type.
defineTypeParameterizedTemplate :: TemplateCreator -> SymPath -> Ty -> (String, Binder)
defineTypeParameterizedTemplate templateCreator path t =
  let (SymPath _ name) = path
      i = Info 0 0 (show path ++ ".parameterizedTemplate") Set.empty 0
      defLst = [XObj (Deftemplate templateCreator) Nothing Nothing, XObj (Sym path Symbol) Nothing Nothing]
  in  (name, Binder emptyMeta (XObj (Lst defLst) (Just i) (Just t)))

-- | Concretizes the types used in @token
--   @cName is the name of the definition, i.e. the "foo" in "void foo() { ... }"
concretizeTypesInToken :: TypeMappings -> String -> [Token] -> Token -> [Token]
concretizeTypesInToken mappings cName decl token =
  case token of
    TokDecl -> concatMap (concretizeTypesInToken mappings cName (error "Nope.")) decl
    TokName -> [TokC cName]
    TokTy t mode -> [TokTy (replaceTyVars mappings t) mode]
    _ -> [token]

-- | High-level helper function for creating templates from strings of C code.
toTemplate :: String -> [Token]
toTemplate text = case Parsec.runParser templateSyntax 0 "(template)" text of
                    Right ok -> ok
                    Left err -> error (show err)
  where
    templateSyntax :: Parsec.Parsec String Int [Token]
    templateSyntax = Parsec.many parseTok

    parseTok = Parsec.try parseTokDecl <|>      --- $DECL
               Parsec.try parseTokName <|>      --- $NAME
               Parsec.try parseTokTyGrouped <|> --- i.e. $(Fn [Int] t)
               Parsec.try parseTokTyRawGrouped <|>
               Parsec.try parseTokTy <|>        --- i.e. $t
               parseTokC                        --- Anything else...

    parseTokDecl :: Parsec.Parsec String Int Token
    parseTokDecl = do _ <- Parsec.string "$DECL"
                      return TokDecl

    parseTokName :: Parsec.Parsec String Int Token
    parseTokName = do _ <- Parsec.string "$NAME"
                      return TokName

    parseTokC :: Parsec.Parsec String Int Token
    parseTokC = do s <- Parsec.many1 validInSymbol
                   return (TokC s)
      where validInSymbol = Parsec.choice [Parsec.letter, Parsec.digit, Parsec.oneOf validCharactersInTemplate]
            validCharactersInTemplate = " ><{}()[]|;:.,_-+*#/'^!?€%&=@\"\n\t"

    parseTokTy :: Parsec.Parsec String Int Token
    parseTokTy = do _ <- Parsec.char '$'
                    s <- Parsec.many1 Parsec.letter
                    return (toTokTy Normal s)

    parseTokTyGrouped :: Parsec.Parsec String Int Token
    parseTokTyGrouped = do _ <- Parsec.char '$'
                           _ <- Parsec.char '('
                           Parsec.putState 1 -- One paren to close.
                           s <- fmap ('(' :) (Parsec.many parseCharBalanced)
                           -- Note: The closing paren is read by parseCharBalanced.
                           return (toTokTy Normal s)

    parseTokTyRawGrouped :: Parsec.Parsec String Int Token
    parseTokTyRawGrouped = do _ <- Parsec.char '§'
                              _ <- Parsec.char '('
                              Parsec.putState 1 -- One paren to close.
                              s <- fmap ('(' :) (Parsec.many parseCharBalanced)
                              -- Note: The closing paren is read by parseCharBalanced.
                              return (toTokTy Raw s)

    parseCharBalanced :: Parsec.Parsec String Int Char
    parseCharBalanced = do balanceState <- Parsec.getState
                           if balanceState > 0
                             then Parsec.try openParen <|>
                                  Parsec.try closeParen <|>
                                  Parsec.anyChar
                             else Parsec.char '\0' -- Should always fail which will end the string.

    openParen :: Parsec.Parsec String Int Char
    openParen = do _ <- Parsec.char '('
                   Parsec.modifyState (+1)
                   return '('

    closeParen :: Parsec.Parsec String Int Char
    closeParen = do _ <- Parsec.char ')'
                    Parsec.modifyState (\x -> x - 1)
                    return ')'

-- | Converts a string containing a type to a template token ('TokTy').
-- | i.e. the string "(Array Int)" becomes (TokTy (StructTy "Array" IntTy)).
toTokTy :: TokTyMode -> String -> Token
toTokTy mode s =
  case parse s "" of
    Left err -> error (show err)
    Right [] -> error ("toTokTy got [] when parsing: '" ++ s ++ "'")
    Right [xobj] -> case xobjToTy xobj of
                      Just ok -> TokTy ok mode
                      Nothing -> error ("toTokTy failed to convert this s-expression to a type: " ++ pretty xobj)
    Right xobjs -> error ("toTokTy parsed too many s-expressions: " ++ joinWithSpace (map pretty xobjs))

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
  (toTemplate "void $NAME ($a* a)")
  (toTemplate "$DECL { }")
  (const [])

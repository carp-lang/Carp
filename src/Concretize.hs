{-# LANGUAGE LambdaCase #-}

module Concretize where

import AssignTypes
import Constraints
import Control.Monad.State
import Data.List (foldl')
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set ((\\))
import qualified Data.Set as Set
import Debug.Trace
import Env
import Info
import Lookup
import Managed
import Obj
import Polymorphism
import Reify
import SumtypeCase
import ToTemplate
import TypeError
import TypePredicates
import Types
import TypesToC
import Util
import Validate
import Prelude hiding (lookup)

data Level = Toplevel | Inside

-- | This function performs two things:
-- |  1. Finds out which polymorphic functions that needs to be added to the environment for the calls in the function to work.
-- |  2. Changes the name of symbols at call sites so they use the polymorphic name
-- |  Both of these results are returned in a tuple: (<new xobj>, <dependencies>)
concretizeXObj :: Bool -> TypeEnv -> Env -> [SymPath] -> XObj -> Either TypeError (XObj, [XObj])
concretizeXObj allowAmbiguityRoot typeEnv rootEnv visitedDefinitions root =
  case runState (visit allowAmbiguityRoot Toplevel rootEnv root) [] of
    (Left err, _) -> Left err
    (Right xobj, deps) -> Right (xobj, deps)
  where
    rootDefinitionPath :: SymPath
    rootDefinitionPath = getPath root
    visit :: Bool -> Level -> Env -> XObj -> State [XObj] (Either TypeError XObj)
    visit allowAmbig _ env xobj@(XObj (Sym _ _) _ _) = visitSymbol allowAmbig env xobj
    visit allowAmbig _ env xobj@(XObj (MultiSym _ _) _ _) = visitMultiSym allowAmbig env xobj
    visit allowAmbig _ env xobj@(XObj (InterfaceSym _) _ _) = visitInterfaceSym allowAmbig env xobj
    visit allowAmbig level env xobj@(XObj (Lst _) i t) =
      do
        visited <- visitList allowAmbig level env xobj
        pure $ do
          okVisited <- visited
          Right (XObj (Lst okVisited) i t)
    visit allowAmbig level env xobj@(XObj (Arr arr) i (Just t)) =
      do
        visited <- fmap sequence (mapM (visit allowAmbig level env) arr)
        _ <- concretizeTypeOfXObj typeEnv xobj
        pure $ do
          okVisited <- visited
          Right (XObj (Arr okVisited) i (Just t))
    visit allowAmbig level env xobj@(XObj (StaticArr arr) i (Just t)) =
      do
        visited <- fmap sequence (mapM (visit allowAmbig level env) arr)
        _ <- concretizeTypeOfXObj typeEnv xobj
        pure $ do
          okVisited <- visited
          Right (XObj (StaticArr okVisited) i (Just t))
    visit _ _ _ x = pure (Right x)
    visitList :: Bool -> Level -> Env -> XObj -> State [XObj] (Either TypeError [XObj])
    visitList _ _ _ (XObj (Lst []) _ _) = pure (Right [])
    visitList _ Toplevel env (XObj (Lst [defn@(XObj (Defn _) _ _), nameSymbol@(XObj (Sym (SymPath [] "main") _) _ _), args@(XObj (Arr argsArr) _ _), body]) _ _) =
      if not (null argsArr)
        then pure $ Left (MainCannotHaveArguments nameSymbol (length argsArr))
        else do
          _ <- concretizeTypeOfXObj typeEnv body
          visitedBody <- visit False Inside env body
          pure $ do
            okBody <- visitedBody
            let t = fromMaybe UnitTy (xobjTy okBody)
            if not (isTypeGeneric t) && t /= UnitTy && t /= IntTy
              then Left (MainCanOnlyReturnUnitOrInt nameSymbol t)
              else return [defn, nameSymbol, args, okBody]
    visitList _ Toplevel env (XObj (Lst [defn@(XObj (Defn _) _ _), nameSymbol, args@(XObj (Arr argsArr) _ _), body]) _ t) =
      do
        mapM_ (concretizeTypeOfXObj typeEnv) argsArr
        let functionEnv = Env Map.empty (Just env) Nothing [] InternalEnv 0
            envWithArgs =
              foldl'
                ( \e arg@(XObj (Sym (SymPath _ argSymName) _) _ _) ->
                    extendEnv e argSymName arg
                )
                functionEnv
                argsArr
            Just funcTy = t
            allowAmbig = isTypeGeneric funcTy
        _ <- concretizeTypeOfXObj typeEnv body
        visitedBody <- visit allowAmbig Inside (incrementEnvNestLevel envWithArgs) body
        pure $ do
          okBody <- visitedBody
          pure [defn, nameSymbol, args, okBody]
    visitList _ Inside _ xobj@(XObj (Lst [(XObj (Defn _) _ _), _, (XObj (Arr _) _ _), _]) _ _) =
      pure (Left (DefinitionsMustBeAtToplevel xobj))
    visitList allowAmbig _ env (XObj (Lst [XObj (Fn _ _) fni fnt, args@(XObj (Arr argsArr) ai at), body]) i t) =
      -- The basic idea of this function is to first visit the body of the lambda ("in place"),
      -- then take the resulting body and put into a separate function 'defn' with a new name
      -- in the global scope. That function definition will be set as the lambdas '.callback' in
      -- the C code.
      do
        mapM_ (concretizeTypeOfXObj typeEnv) argsArr
        let Just ii = i
            Just funcTy = t
            argObjs = map xobjObj argsArr
            -- TODO: This code is a copy of the one above in Defn, remove duplication:
            functionEnv = Env Map.empty (Just env) Nothing [] InternalEnv (envFunctionNestingLevel env)
            envWithArgs =
              foldl'
                ( \e arg@(XObj (Sym (SymPath _ argSymName) _) _ _) ->
                    extendEnv e argSymName arg
                )
                functionEnv
                argsArr
        visitedBody <- visit allowAmbig Inside (incrementEnvNestLevel envWithArgs) body
        case visitedBody of
          Right okBody ->
            let -- Analyse the body of the lambda to find what variables it captures
                capturedVarsRaw = collectCapturedVars okBody
                -- and then remove the captures that are actually our arguments
                capturedVars = filter (\xobj -> xobjObj (toGeneralSymbol xobj) `notElem` argObjs) capturedVarsRaw
                -- Create a new (top-level) function that will be used when the lambda is called.
                -- Its name will contain the name of the (normal, non-lambda) function it's contained within,
                -- plus the identifier of the particular s-expression that defines the lambda.
                SymPath spath name = rootDefinitionPath
                lambdaPath = SymPath spath ("_Lambda_" ++ lambdaToCName name (envFunctionNestingLevel envWithArgs) ++ "_" ++ show (infoIdentifier ii))
                lambdaNameSymbol = XObj (Sym lambdaPath Symbol) (Just dummyInfo) Nothing
                extendedArgs =
                  if null capturedVars
                    then args
                    else -- If the lambda captures anything it need an extra arg for its env:

                      XObj
                        ( Arr
                            ( XObj
                                (Sym (SymPath [] "_env") Symbol)
                                (Just dummyInfo)
                                (Just (PointerTy (StructTy (ConcreteNameTy environmentTypeName) []))) :
                              argsArr
                            )
                        )
                        ai
                        at
                lambdaCallback = XObj (Lst [XObj (Defn (Just (Set.fromList capturedVars))) (Just dummyInfo) Nothing, lambdaNameSymbol, extendedArgs, okBody]) i t
                -- The lambda will also carry with it a special made struct containing the variables it captures
                -- (if it captures at least one variable)
                structMemberPairs =
                  concatMap
                    ( \(XObj (Sym path _) _ (Just symTy)) ->
                        [XObj (Sym path Symbol) Nothing Nothing, reify symTy]
                    )
                    capturedVars
                environmentTypeName = pathToC lambdaPath ++ "_env"
                environmentStructTy = StructTy (ConcreteNameTy environmentTypeName) []
                environmentStruct =
                  XObj
                    ( Lst
                        [ XObj (Deftype environmentStructTy) Nothing Nothing,
                          XObj (Sym (SymPath [] environmentTypeName) Symbol) Nothing Nothing,
                          XObj (Arr structMemberPairs) Nothing Nothing
                        ]
                    )
                    i
                    (Just TypeTy)
                pairs = memberXObjsToPairs structMemberPairs
                deleteFnTy = typesDeleterFunctionType (PointerTy environmentStructTy)
                deleteFnTemplate = concreteDeleteTakePtr typeEnv env pairs
                (deleteFn, deleterDeps) = instantiateTemplate (SymPath [] (environmentTypeName ++ "_delete")) deleteFnTy deleteFnTemplate
                copyFnTy = typesCopyFunctionType environmentStructTy
                copyFnTemplate = concreteCopyPtr typeEnv env pairs
                (copyFn, copyDeps) = instantiateTemplate (SymPath [] (environmentTypeName ++ "_copy")) copyFnTy copyFnTemplate
                -- The type env has to contain the lambdas environment struct for 'concretizeDefinition' to work:
                extendedTypeEnv = TypeEnv (extendEnv (getTypeEnv typeEnv) environmentTypeName environmentStruct)
             in case concretizeDefinition allowAmbig extendedTypeEnv env visitedDefinitions lambdaCallback funcTy of
                  Left err -> pure (Left err)
                  Right (concreteLiftedLambda, deps) ->
                    do
                      unless (any (isTypeGeneric . snd) pairs) $
                        do
                          modify (concreteLiftedLambda :)
                          modify (deps ++)
                          unless (null capturedVars) $
                            do
                              modify (environmentStruct :)
                              modify (deleteFn :)
                              modify (deleterDeps ++)
                              modify (copyFn :)
                              modify (copyDeps ++)
                      pure (Right [XObj (Fn (Just lambdaPath) (Set.fromList capturedVars)) fni fnt, args, okBody])
          Left err ->
            pure (Left err)
    visitList _ Toplevel env (XObj (Lst [def@(XObj Def _ _), nameSymbol, body]) _ t) =
      do
        let Just defTy = t
            allowAmbig = isTypeGeneric defTy
        visitedBody <- visit allowAmbig Inside env body
        pure $ do
          okBody <- visitedBody
          pure [def, nameSymbol, okBody]
    visitList _ Inside _ xobj@(XObj (Lst [(XObj Def _ _), _, _]) _ _) =
      pure (Left (DefinitionsMustBeAtToplevel xobj))
    visitList allowAmbig level env (XObj (Lst [letExpr@(XObj Let _ _), XObj (Arr bindings) bindi bindt, body]) _ _) =
      do
        visitedBindings <- fmap sequence (mapM (visit allowAmbig level env) bindings)
        visitedBody <- visit allowAmbig level env body
        mapM_ (concretizeTypeOfXObj typeEnv . fst) (pairwise bindings)
        pure $ do
          okVisitedBindings <- visitedBindings
          okVisitedBody <- visitedBody
          pure [letExpr, XObj (Arr okVisitedBindings) bindi bindt, okVisitedBody]
    visitList allowAmbig level env (XObj (Lst [theExpr@(XObj The _ _), typeXObj, value]) _ _) =
      do
        visitedValue <- visit allowAmbig level env value
        pure $ do
          okVisitedValue <- visitedValue
          pure [theExpr, typeXObj, okVisitedValue]
    visitList allowAmbig level env (XObj (Lst (matchExpr@(XObj (Match _) _ _) : expr : rest)) _ _) =
      do
        _ <- concretizeTypeOfXObj typeEnv expr
        visitedExpr <- visit allowAmbig level env expr
        mapM_ (concretizeTypeOfXObj typeEnv . snd) (pairwise rest)
        visitedRest <- fmap sequence (mapM (visitMatchCase allowAmbig level env) (pairwise rest))
        pure $ do
          okVisitedExpr <- visitedExpr
          okVisitedRest <- fmap concat visitedRest
          pure ([matchExpr, okVisitedExpr] ++ okVisitedRest)
    visitList allowAmbig _ env (XObj (Lst [setbangExpr@(XObj SetBang _ _), variable, value]) _ _) =
      do
        visitedValue <- visit allowAmbig Inside env value
        pure $ do
          okVisitedValue <- visitedValue
          pure [setbangExpr, variable, okVisitedValue]
    visitList allowAmbig level env (XObj (Lst (func : args)) _ _) =
      do
        _ <- concretizeTypeOfXObj typeEnv func
        mapM_ (concretizeTypeOfXObj typeEnv) args
        f <- visit allowAmbig level env func
        a <- fmap sequence (mapM (visit allowAmbig level env) args)
        pure $ do
          okF <- f
          okA <- a
          pure (okF : okA)
    visitMatchCase :: Bool -> Level -> Env -> (XObj, XObj) -> State [XObj] (Either TypeError [XObj])
    visitMatchCase allowAmbig level env (lhs, rhs) =
      do
        visitedLhs <- visit allowAmbig level env lhs -- TODO! This changes the names of some tags (which is corrected in Emit) but perhaps there is a better way where they can be identified as tags and not changed?
        visitedRhs <- visit allowAmbig level env rhs
        pure $ do
          okVisitedLhs <- visitedLhs
          okVisitedRhs <- visitedRhs
          pure [okVisitedLhs, okVisitedRhs]
    visitSymbol :: Bool -> Env -> XObj -> State [XObj] (Either TypeError XObj)
    visitSymbol allowAmbig env xobj@(XObj (Sym path lookupMode) i t) =
      case lookupInEnv path env of
        Just (foundEnv, binder)
          | envIsExternal foundEnv ->
            let theXObj = binderXObj binder
                Just theType = xobjTy theXObj
                typeOfVisited = fromMaybe (error ("Missing type on " ++ show xobj ++ " at " ++ prettyInfoFromXObj xobj ++ " when looking up path " ++ show path)) t
             in if --(trace $ "CHECKING " ++ getName xobj ++ " : " ++ show theType ++ " with visited type " ++ show typeOfVisited ++ " and visited definitions: " ++ show visitedDefinitions) $
                isTypeGeneric theType && not (isTypeGeneric typeOfVisited)
                  then case concretizeDefinition allowAmbig typeEnv env visitedDefinitions theXObj typeOfVisited of
                    Left err -> pure (Left err)
                    Right (concrete, deps) ->
                      do
                        modify (concrete :)
                        modify (deps ++)
                        pure (Right (XObj (Sym (getPath concrete) lookupMode) i t))
                  else pure (Right xobj)
          | otherwise -> pure (Right xobj)
        Nothing -> pure (Right xobj)
    visitSymbol _ _ _ = error "Not a symbol."
    visitMultiSym :: Bool -> Env -> XObj -> State [XObj] (Either TypeError XObj)
    visitMultiSym allowAmbig env xobj@(XObj (MultiSym originalSymbolName paths) i t) =
      let Just actualType = t
          tys = map (typeFromPath env) paths
          modes = map (modeFromPath env) paths
          tysToPathsDict = zip tys paths
          tysPathsModes = zip3 tys paths modes
       in case filter (matchingSignature3 actualType) tysPathsModes of
            [] ->
              pure (Left (NoMatchingSignature xobj originalSymbolName actualType tysToPathsDict))
            [(theType, singlePath, mode)] ->
              let Just t' = t
                  fake1 = XObj (Sym (SymPath [] "theType") Symbol) Nothing Nothing
                  fake2 = XObj (Sym (SymPath [] "xobjType") Symbol) Nothing Nothing
                  Just i' = i
               in case solve [Constraint theType t' fake1 fake2 fake1 OrdMultiSym] of
                    Right mappings ->
                      let replaced = replaceTyVars mappings t'
                          suffixed = suffixTyVars ("_x" ++ show (infoIdentifier i')) replaced -- Make sure it gets unique type variables. TODO: Is there a better way?
                          normalSymbol = XObj (Sym singlePath mode) i (Just suffixed)
                       in visitSymbol
                            allowAmbig
                            env --(trace ("Disambiguated " ++ pretty xobj ++ " at " ++ prettyInfoFromXObj xobj ++ " to " ++ show singlePath ++ " : " ++ show suffixed ++ ", used to be " ++ show t' ++ ", theType = " ++ show theType ++ ", mappings = " ++ show mappings))
                            normalSymbol
                    Left failure@(UnificationFailure _ _) ->
                      pure $
                        Left
                          ( UnificationFailed
                              (unificationFailure failure)
                              (unificationMappings failure)
                              []
                          )
                    Left (Holes holes) ->
                      pure $ Left (HolesFound holes)
            _ -> pure (Right xobj)
    visitMultiSym _ _ _ = error "Not a multi symbol."
    visitInterfaceSym :: Bool -> Env -> XObj -> State [XObj] (Either TypeError XObj)
    visitInterfaceSym allowAmbig env xobj@(XObj (InterfaceSym name) i t) =
      case lookupBinder (SymPath [] name) (getTypeEnv typeEnv) of
        Just (Binder _ (XObj (Lst [XObj (Interface _ interfacePaths) _ _, _]) _ _)) ->
          let Just actualType = t
              tys = map (typeFromPath env) interfacePaths
              tysToPathsDict = zip tys interfacePaths
           in case filter (matchingSignature actualType) tysToPathsDict of
                [] ->
                  pure $ --(trace ("No matching signatures for interface lookup of " ++ name ++ " of type " ++ show actualType ++ " " ++ prettyInfoFromXObj xobj ++ ", options are:\n" ++ joinLines (map show tysToPathsDict))) $
                    if allowAmbig
                      then Right xobj -- No exact match of types
                      else Left (NoMatchingSignature xobj name actualType tysToPathsDict)
                [(theType, singlePath)] ->
                  --(trace ("One matching signature for interface lookup of '" ++ name ++ "' with single path " ++ show singlePath ++ " of type " ++ show theType ++ " at " ++ prettyInfoFromXObj xobj ++ ", original symbol: " ++ show xobj)) $
                  let Just tt = t
                   in if isTypeGeneric tt then pure (Right xobj) else replace theType singlePath
                severalPaths ->
                  --(trace ("Several matching signatures for interface lookup of '" ++ name ++ "' of type " ++ show actualType ++ " " ++ prettyInfoFromXObj xobj ++ ", options are:\n" ++ joinLines (map show tysToPathsDict) ++ "\n  Filtered paths are:\n" ++ (joinLines (map show severalPaths)))) $
                  case filter (\(tt, _) -> typeEqIgnoreLifetimes actualType tt) severalPaths of
                    [] ->
                      --trace ("No exact matches for '" ++ show actualType ++ "'") $
                      pure (Right xobj) -- No exact match of types
                    [(theType, singlePath)] -> replace theType singlePath -- Found an exact match, will ignore any "half matched" functions that might have slipped in.
                    _ -> pure (Left (SeveralExactMatches xobj name actualType severalPaths))
          where
            replace _ singlePath =
              let normalSymbol = XObj (Sym singlePath (LookupGlobal CarpLand AFunction)) i t -- TODO: Is it surely AFunction here? Could be AVariable as well...!?
               in visitSymbol
                    allowAmbig
                    env -- trace ("Replacing symbol " ++ pretty xobj ++ " with type " ++ show theType ++ " to single path " ++ show singlePath)
                    normalSymbol
        Nothing ->
          error ("No interface named '" ++ name ++ "' found.")

toGeneralSymbol :: XObj -> XObj
toGeneralSymbol (XObj (Sym path _) _ t) = XObj (Sym path Symbol) (Just dummyInfo) t
toGeneralSymbol x = error ("Can't convert this to a general symbol: " ++ show x)

-- | Find all lookups in a lambda body that should be captured by its environment
collectCapturedVars :: XObj -> [XObj]
collectCapturedVars root = removeDuplicates (map decreaseCaptureLevel (visit root))
  where
    removeDuplicates :: Ord a => [a] -> [a]
    removeDuplicates = Set.toList . Set.fromList
    decreaseCaptureLevel :: XObj -> XObj
    decreaseCaptureLevel (XObj (Sym path lookup) _ ty) =
      XObj
        ( Sym
            path
            ( case lookup of
                Symbol -> Symbol
                LookupLocal NoCapture -> Symbol
                LookupLocal (Capture n) ->
                  if n <= 1
                    then Symbol
                    else LookupLocal (Capture (n -1))
            )
        )
        (Just dummyInfo)
        ty
    visit xobj =
      case xobjObj xobj of
        -- don't peek inside lambdas, trust their capture lists:
        (Lst [XObj (Fn _ captures) _ _, _, _]) -> Set.toList captures
        -- in the case of lets, we have to remove new bindings from the list of captured variables,
        -- including the ones captured in later bindings
        (Lst [XObj Let _ _, XObj (Arr bindings) _ _, body]) ->
          let (bound, bindingsCaptured) =
                foldl
                  ( \(bound', captured) (XObj sym _ ty, expr) ->
                      let capt = filter (\x -> Set.notMember x bound') (visit expr)
                       in (Set.insert (XObj sym (Just dummyInfo) ty) bound', capt ++ captured)
                  )
                  (Set.empty, [])
                  (pairwise bindings)
           in let bodyCaptured = filter (\x -> Set.notMember x bound) (visit body)
               in bindingsCaptured ++ bodyCaptured
        (Lst _) -> visitList xobj
        (Arr _) -> visitArray xobj
        -- TODO: Static Arrays!
        sym@(Sym _ (LookupLocal (Capture _))) -> [XObj sym (Just dummyInfo) (xobjTy xobj)]
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

-- | Do the signatures match (tuple arity 3 version)?
matchingSignature3 :: Ty -> (Ty, SymPath, SymbolMode) -> Bool
matchingSignature3 tA (tB, _, _) = areUnifiable tA tB

-- | Does the type of an XObj require additional concretization of generic types or some typedefs for function types, etc?
-- | If so, perform the concretization and append the results to the list of dependencies.
concretizeTypeOfXObj :: TypeEnv -> XObj -> State [XObj] (Either TypeError ())
concretizeTypeOfXObj typeEnv (XObj _ _ (Just ty)) =
  case concretizeType typeEnv ty of
    Right t -> do
      modify (t ++)
      pure (Right ())
    Left err -> pure (Left err)
concretizeTypeOfXObj _ _ = pure (Right ())

-- | Find all the concrete deps of a type.
concretizeType :: TypeEnv -> Ty -> Either TypeError [XObj]
concretizeType _ ft@FuncTy {} =
  if isTypeGeneric ft
    then Right []
    else Right [defineFunctionTypeAlias ft]
concretizeType typeEnv arrayTy@(StructTy (ConcreteNameTy "Array") varTys) =
  if isTypeGeneric arrayTy
    then Right []
    else do
      deps <- mapM (concretizeType typeEnv) varTys
      Right (defineArrayTypeAlias arrayTy : concat deps)
-- TODO: Remove ugly duplication of code here:
concretizeType typeEnv arrayTy@(StructTy (ConcreteNameTy "StaticArray") varTys) =
  if isTypeGeneric arrayTy
    then Right []
    else do
      deps <- mapM (concretizeType typeEnv) varTys
      Right (defineStaticArrayTypeAlias arrayTy : concat deps)
-- TODO: handle polymorphic constructors (a b)
concretizeType typeEnv genericStructTy@(StructTy (ConcreteNameTy name) _) =
  case lookupInEnv (SymPath [] name) (getTypeEnv typeEnv) of
    Just (_, Binder _ (XObj (Lst (XObj (Deftype originalStructTy) _ _ : _ : rest)) _ _)) ->
      if isTypeGeneric originalStructTy
        then instantiateGenericStructType typeEnv originalStructTy genericStructTy rest
        else Right []
    Just (_, Binder _ (XObj (Lst (XObj (DefSumtype originalStructTy) _ _ : _ : rest)) _ _)) ->
      if isTypeGeneric originalStructTy
        then instantiateGenericSumtype typeEnv originalStructTy genericStructTy rest
        else Right []
    Just (_, Binder _ (XObj (Lst (XObj (ExternalType _) _ _ : _)) _ _)) ->
      Right []
    Just (_, Binder _ x) ->
      error ("Non-deftype found in type env: " ++ show x)
    Nothing ->
      Right []
concretizeType env (RefTy rt _) =
  concretizeType env rt
concretizeType env (PointerTy pt) =
  concretizeType env pt
concretizeType _ _ =
  Right [] -- ignore all other types

-- | Given an generic struct type and a concrete version of it, generate all dependencies needed to use the concrete one.
-- TODO: Handle polymorphic constructors (a b).
instantiateGenericStructType :: TypeEnv -> Ty -> Ty -> [XObj] -> Either TypeError [XObj]
instantiateGenericStructType typeEnv originalStructTy@(StructTy _ originalTyVars) genericStructTy membersXObjs =
  -- Turn (deftype (A a) [x a, y a]) into (deftype (A Int) [x Int, y Int])
  let fake1 = XObj (Sym (SymPath [] "a") Symbol) Nothing Nothing
      fake2 = XObj (Sym (SymPath [] "b") Symbol) Nothing Nothing
      XObj (Arr memberXObjs) _ _ = head membersXObjs
   in case solve [Constraint originalStructTy genericStructTy fake1 fake2 fake1 OrdMultiSym] of
        Left e -> error (show e)
        Right mappings ->
          let concretelyTypedMembers = replaceGenericTypeSymbolsOnMembers mappings memberXObjs
           in case validateMembers typeEnv originalTyVars concretelyTypedMembers of
                Left err -> Left err
                Right () ->
                  let deps = mapM (depsForStructMemberPair typeEnv) (pairwise concretelyTypedMembers)
                   in case deps of
                        Left err -> Left err
                        Right okDeps ->
                          Right $
                            XObj
                              ( Lst
                                  ( XObj (Deftype genericStructTy) Nothing Nothing :
                                    XObj (Sym (SymPath [] (tyToC genericStructTy)) Symbol) Nothing Nothing :
                                    [XObj (Arr concretelyTypedMembers) Nothing Nothing]
                                  )
                              )
                              (Just dummyInfo)
                              (Just TypeTy) :
                            concat okDeps

depsForStructMemberPair :: TypeEnv -> (XObj, XObj) -> Either TypeError [XObj]
depsForStructMemberPair typeEnv (_, tyXObj) =
  case xobjToTy tyXObj of
    Just okTy -> concretizeType typeEnv okTy
    Nothing -> error ("Failed to convert " ++ pretty tyXObj ++ " to a type.")

-- | Given an generic sumtype and a concrete version of it, generate all dependencies needed to use the concrete one.
instantiateGenericSumtype :: TypeEnv -> Ty -> Ty -> [XObj] -> Either TypeError [XObj]
instantiateGenericSumtype typeEnv originalStructTy@(StructTy _ originalTyVars) genericStructTy cases =
  -- Turn (deftype (Maybe a) (Just a) (Nothing)) into (deftype (Maybe Int) (Just Int) (Nothing))
  let fake1 = XObj (Sym (SymPath [] "a") Symbol) Nothing Nothing
      fake2 = XObj (Sym (SymPath [] "b") Symbol) Nothing Nothing
   in case solve [Constraint originalStructTy genericStructTy fake1 fake2 fake1 OrdMultiSym] of
        Left e -> error (show e)
        Right mappings ->
          let concretelyTypedCases = map (replaceGenericTypeSymbolsOnCase mappings) cases
              deps = mapM (depsForCase typeEnv) concretelyTypedCases
           in case toCases typeEnv originalTyVars concretelyTypedCases of -- Don't care about the cases, this is done just for validation.
                Left err -> Left err
                Right _ ->
                  case deps of
                    Right okDeps ->
                      Right $
                        XObj
                          ( Lst
                              ( XObj (DefSumtype genericStructTy) Nothing Nothing :
                                XObj (Sym (SymPath [] (tyToC genericStructTy)) Symbol) Nothing Nothing :
                                concretelyTypedCases
                              )
                          )
                          (Just dummyInfo)
                          (Just TypeTy) :
                        concat okDeps
                    Left err -> Left err

-- Resolves dependencies for sumtype cases.
-- NOTE: This function only accepts cases that are in "canonical form"
-- (Just [x]) aka XObj (Lst (Sym...) (Arr members))
-- On other cases it will return an error.
depsForCase :: TypeEnv -> XObj -> Either TypeError [XObj]
depsForCase typeEnv x@(XObj (Lst [_, XObj (Arr members) _ _]) _ _) =
  concat
    <$> mapM
      ( \m -> case xobjToTy m of
          Just okTy -> concretizeType typeEnv okTy
          Nothing -> error ("Failed to convert " ++ pretty m ++ " to a type: " ++ pretty x)
      )
      members
depsForCase _ x = Left (InvalidSumtypeCase x)

replaceGenericTypeSymbolsOnMembers :: Map.Map String Ty -> [XObj] -> [XObj]
replaceGenericTypeSymbolsOnMembers mappings memberXObjs =
  concatMap (\(v, t) -> [v, replaceGenericTypeSymbols mappings t]) (pairwise memberXObjs)

replaceGenericTypeSymbols :: Map.Map String Ty -> XObj -> XObj
replaceGenericTypeSymbols mappings xobj@(XObj (Sym (SymPath _ name) _) _ _) =
  let Just perhapsTyVar = xobjToTy xobj
   in if isFullyGenericType perhapsTyVar
        then case Map.lookup name mappings of
          Just found -> reify found
          Nothing -> xobj -- error ("Failed to concretize member '" ++ name ++ "' at " ++ prettyInfoFromXObj xobj ++ ", mappings: " ++ show mappings)
        else xobj
replaceGenericTypeSymbols mappings (XObj (Lst lst) i t) =
  XObj (Lst (map (replaceGenericTypeSymbols mappings) lst)) i t
replaceGenericTypeSymbols mappings (XObj (Arr arr) i t) =
  XObj (Arr (map (replaceGenericTypeSymbols mappings) arr)) i t
replaceGenericTypeSymbols _ xobj = xobj

replaceGenericTypeSymbolsOnCase :: Map.Map String Ty -> XObj -> XObj
replaceGenericTypeSymbolsOnCase mappings (XObj (Lst (caseNm : caseMembers)) i t) =
  XObj (Lst (caseNm : map replacer caseMembers)) i t
  where
    replacer memberXObj =
      replaceGenericTypeSymbols mappings memberXObj
-- Handle cases like `(State a) Done (Value [a]))`
-- `Done` is a Sym, not a Lst. DepsForCase, like this function
-- expects and only matches on a Lst, so we convert the problematic cases to a
-- canonical form. (see `depsForCase` above
replaceGenericTypeSymbolsOnCase _ nakedCase@(XObj (Sym (SymPath _ _) _) i t) =
  XObj (Lst [nakedCase, XObj (Arr []) i t]) i t -- NOTE: This transformation is done in some other pass too, just returning 'nakedCase' would be fine here.
replaceGenericTypeSymbolsOnCase _ unknownCase = unknownCase -- TODO: error out?

-- | Get the type of a symbol at a given path.
typeFromPath :: Env -> SymPath -> Ty
typeFromPath env p =
  case lookupInEnv p env of
    Just (e, Binder _ found)
      | envIsExternal e -> forceTy found
      | otherwise -> error "Local bindings shouldn't be ambiguous."
    Nothing -> error ("Couldn't find " ++ show p ++ " in env:\n" ++ prettyEnvironmentChain env)

-- | Get the mode of a symbol at a given path.
-- |
-- | TODO: this duplicates a bunch of functionality from  Qualify.hs, namely
-- | parts of doesNotBelongToAnInterface.
modeFromPath :: Env -> SymPath -> SymbolMode
modeFromPath env p =
  case lookupInEnv p env of
    Just (_, Binder _ (XObj (Lst (XObj (External (Just overrideWithName)) _ _ : _)) _ _)) ->
      LookupGlobalOverride overrideWithName
    Just (_, Binder _ (XObj (Lst (XObj (ExternalType (Just overrideWithName)) _ _ : _)) _ _)) ->
      LookupGlobalOverride overrideWithName
    Just (_, Binder _ found@(XObj (Lst (XObj (External _) _ _ : _)) _ _)) ->
      LookupGlobal ExternalCode (definitionMode found)
    Just (e, Binder _ found) ->
      case envMode e of
        ExternalEnv ->
          LookupGlobal CarpLand (definitionMode found)
        RecursionEnv -> LookupRecursive
        _ ->
          LookupLocal
            ( if envFunctionNestingLevel e < envFunctionNestingLevel env
                then Capture (envFunctionNestingLevel e - envFunctionNestingLevel env)
                else NoCapture
            )
    Nothing -> error ("Couldn't find " ++ show p ++ " in env:\n" ++ prettyEnvironmentChain env)

-- | Given a definition (def, defn, template, external) and
--   a concrete type (a type without any type variables)
--   this function returns a new definition with the concrete
--   types assigned, and a list of dependencies.
concretizeDefinition :: Bool -> TypeEnv -> Env -> [SymPath] -> XObj -> Ty -> Either TypeError (XObj, [XObj])
concretizeDefinition allowAmbiguity typeEnv globalEnv visitedDefinitions definition concreteType =
  let SymPath pathStrings name = getPath definition
      Just polyType = xobjTy definition
      suffix = polymorphicSuffix polyType concreteType
      newPath = SymPath pathStrings (name ++ suffix)
   in case definition of
        XObj (Lst (XObj (Defn _) _ _ : _)) _ _ ->
          let withNewPath = setPath definition newPath
              mappings = unifySignatures polyType concreteType
           in case assignTypes mappings withNewPath of
                Right typed ->
                  if newPath `elem` visitedDefinitions
                    then pure (trace ("Already visited " ++ show newPath) (withNewPath, []))
                    else do
                      (concrete, deps) <- concretizeXObj allowAmbiguity typeEnv globalEnv (newPath : visitedDefinitions) typed
                      (managed, memDeps) <- manageMemory typeEnv globalEnv concrete
                      pure (managed, deps ++ memDeps)
                Left e -> Left e
        XObj (Lst (XObj (Deftemplate (TemplateCreator templateCreator)) _ _ : _)) _ _ ->
          let template = templateCreator typeEnv globalEnv
           in Right (instantiateTemplate newPath concreteType template)
        XObj (Lst [XObj (External _) _ _, _, _]) _ _ ->
          if name == "NULL"
            then Right (definition, []) -- A hack to make all versions of NULL have the same name
            else
              let withNewPath = setPath definition newPath
                  withNewType = withNewPath {xobjTy = Just concreteType}
               in Right (withNewType, [])
        -- TODO: This old form shouldn't be necessary, but somehow, some External xobjs are still registered without a ty xobj position.
        XObj (Lst [XObj (External _) _ _, _]) _ _ ->
          if name == "NULL"
            then Right (definition, []) -- A hack to make all versions of NULL have the same name
            else
              let withNewPath = setPath definition newPath
                  withNewType = withNewPath {xobjTy = Just concreteType}
               in Right (withNewType, [])
        XObj (Lst [XObj (Instantiate template) _ _, _]) _ _ ->
          Right (instantiateTemplate newPath concreteType template)
        _ ->
          Left $ CannotConcretize definition

-- | Find ALL functions with a certain name, matching a type signature.
allFunctionsWithNameAndSignature :: Env -> String -> Ty -> [(Env, Binder)]
allFunctionsWithNameAndSignature env functionName functionType =
  filter (predicate . xobjTy . binderXObj . snd) (multiLookupEverywhere functionName env)
  where
    predicate (Just t) =
      --trace ("areUnifiable? " ++ show functionType ++ " == " ++ show t ++ " " ++ show (areUnifiable functionType t)) $
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
    tooMany ->
      (trace $ "Too many '" ++ functionName ++ "' functions found with type " ++ show functionType ++ ", can't figure out dependencies:\n  " ++ joinWith "\n  " (map ((" - " ++) . show . snd) tooMany))
        []

-- | Helper for finding the 'delete' function for a type.
depsForDeleteFunc :: TypeEnv -> Env -> Ty -> [XObj]
depsForDeleteFunc typeEnv env t =
  if isManaged typeEnv env t
    then depsOfPolymorphicFunction typeEnv env [] "delete" (FuncTy [t] UnitTy StaticLifetimeTy)
    else []

-- | Helper for finding the 'copy' function for a type.
depsForCopyFunc :: TypeEnv -> Env -> Ty -> [XObj]
depsForCopyFunc typeEnv env t =
  if isManaged typeEnv env t
    then depsOfPolymorphicFunction typeEnv env [] "copy" (FuncTy [RefTy t (VarTy "q")] t StaticLifetimeTy)
    else []

-- | Helper for finding the 'str' function for a type.
depsForPrnFunc :: TypeEnv -> Env -> Ty -> [XObj]
depsForPrnFunc typeEnv env t =
  if isManaged typeEnv env t
    then depsOfPolymorphicFunction typeEnv env [] "prn" (FuncTy [RefTy t (VarTy "q")] StringTy StaticLifetimeTy)
    else depsOfPolymorphicFunction typeEnv env [] "prn" (FuncTy [t] StringTy StaticLifetimeTy)

-- | The type of a type's str function.
typesStrFunctionType :: TypeEnv -> Env -> Ty -> Ty
typesStrFunctionType typeEnv env memberType =
  if isManaged typeEnv env memberType
    then FuncTy [RefTy memberType (VarTy "q")] StringTy StaticLifetimeTy
    else FuncTy [memberType] StringTy StaticLifetimeTy

-- | The various results when trying to find a function using 'findFunctionForMember'.
data FunctionFinderResult
  = FunctionFound String
  | FunctionNotFound String
  | FunctionIgnored
  deriving (Show)

-- | TODO: COMMENT THIS
getConcretizedPath :: XObj -> Ty -> SymPath
getConcretizedPath single functionType =
  let Just t' = xobjTy single
      (SymPath pathStrings name) = getPath single
      suffix = polymorphicSuffix t' functionType
   in SymPath pathStrings (name ++ suffix)

-- | Used for finding functions like 'delete' or 'copy' for members of a Deftype (or Array).
findFunctionForMember :: TypeEnv -> Env -> String -> Ty -> (String, Ty) -> FunctionFinderResult
findFunctionForMember typeEnv env functionName functionType (memberName, memberType)
  | isManaged typeEnv env memberType =
    case allFunctionsWithNameAndSignature env functionName functionType of
      [] ->
        FunctionNotFound
          ( "Can't find any '" ++ functionName ++ "' function for member '"
              ++ memberName
              ++ "' of type "
              ++ show functionType
          )
      [(_, Binder _ single)] ->
        let concretizedPath = getConcretizedPath single functionType
         in FunctionFound (pathToC concretizedPath)
      _ ->
        FunctionNotFound
          ( "Can't find a single '" ++ functionName ++ "' function for member '"
              ++ memberName
              ++ "' of type "
              ++ show functionType
          )
  | otherwise = FunctionIgnored

-- | TODO: should this be the default and 'findFunctionForMember' be the specific one
findFunctionForMemberIncludePrimitives :: TypeEnv -> Env -> String -> Ty -> (String, Ty) -> FunctionFinderResult
findFunctionForMemberIncludePrimitives _ env functionName functionType (memberName, _) =
  case allFunctionsWithNameAndSignature env functionName functionType of
    [] ->
      FunctionNotFound
        ( "Can't find any '" ++ functionName ++ "' function for member '"
            ++ memberName
            ++ "' of type "
            ++ show functionType
        )
    [(_, Binder _ single)] ->
      let concretizedPath = getConcretizedPath single functionType
       in FunctionFound (pathToC concretizedPath)
    _ ->
      FunctionNotFound
        ( "Can't find a single '" ++ functionName ++ "' function for member '"
            ++ memberName
            ++ "' of type "
            ++ show functionType
        )

-- | Manage memory needs access to the concretizer
-- | (and the concretizer needs to manage memory)
-- | so they are put into the same module.

-- | Assign a set of Deleters to the 'infoDelete' field on Info.
setDeletersOnInfo :: Maybe Info -> Set.Set Deleter -> Maybe Info
setDeletersOnInfo i deleters = fmap (\i' -> i' {infoDelete = deleters}) i

-- | Helper function for setting the deleters for an XObj.
del :: XObj -> Set.Set Deleter -> XObj
del xobj deleters = xobj {xobjInfo = setDeletersOnInfo (xobjInfo xobj) deleters}

-- | Differentiate between lifetimes depending on variables in a lexical scope and depending on something outside the function
data LifetimeMode
  = LifetimeInsideFunction String
  | LifetimeOutsideFunction
  deriving (Show)

-- | To keep track of the deleters when recursively walking the form.
data MemState = MemState
  { memStateDeleters :: Set.Set Deleter,
    memStateDeps :: [XObj],
    memStateLifetimes :: Map.Map String LifetimeMode
  }
  deriving (Show)

prettyLifetimeMappings :: Map.Map String LifetimeMode -> String
prettyLifetimeMappings mappings =
  joinLines (map prettyMapping (Map.toList mappings))
  where
    prettyMapping (key, value) = "  " ++ key ++ " => " ++ show value

-- | Find out what deleters are needed and where in an XObj.
-- | Deleters will be added to the info field on XObj so that
-- | the code emitter can access them and insert calls to destructors.
manageMemory :: TypeEnv -> Env -> XObj -> Either TypeError (XObj, [XObj])
manageMemory typeEnv globalEnv root =
  let (finalObj, finalState) = runState (visit root) (MemState (Set.fromList []) [] Map.empty)
      deleteThese = memStateDeleters finalState
      deps = memStateDeps finalState
   in -- (trace ("Delete these: " ++ joinWithComma (map show (Set.toList deleteThese)))) $
      case finalObj of
        Left err -> Left err
        Right ok ->
          let newInfo = fmap (\i -> i {infoDelete = deleteThese}) (xobjInfo ok)
           in -- This final check of lifetimes works on the lifetimes mappings after analyzing the function form, and
              --  after all the local variables in it have been deleted. This is needed for values that are created
              --  directly in body position, e.g. (defn f [] &[1 2 3])
              case evalState (checkThatRefTargetIsAlive ok) (MemState (Set.fromList []) [] (memStateLifetimes finalState)) of
                Left err -> Left err
                Right _ -> Right (ok {xobjInfo = newInfo}, deps)
  where
    visit :: XObj -> State MemState (Either TypeError XObj)
    visit xobj =
      do
        r <- case xobjObj xobj of
          Lst _ -> visitList xobj
          Arr _ -> visitArray xobj
          StaticArr _ -> visitStaticArray xobj
          Str _ -> do
            manage xobj
            addToLifetimesMappingsIfRef False xobj -- TODO: Should "internal = True" here? TODO: Possible to remove this one?
            pure (Right xobj)
          Pattern _ -> do
            manage xobj
            addToLifetimesMappingsIfRef False xobj -- TODO: Also possible to remove, *should* be superseeded by (***) below?
            pure (Right xobj)
          _ ->
            pure (Right xobj)
        case r of
          Right ok -> do
            MemState _ _ _ <- get
            r' <- checkThatRefTargetIsAlive ok -- trace ("CHECKING " ++ pretty ok ++ " : " ++ showMaybeTy (ty xobj) ++ ", mappings: " ++ prettyLifetimeMappings m) $
            addToLifetimesMappingsIfRef True ok -- (***)
            pure r'
          Left err -> pure (Left err)
    visitArray :: XObj -> State MemState (Either TypeError XObj)
    visitArray xobj@(XObj (Arr arr) _ _) =
      do
        mapM_ visit arr
        results <- mapM unmanage arr
        case sequence results of
          Left e -> pure (Left e)
          Right _ ->
            do
              _ <- manage xobj -- TODO: result is discarded here, is that OK?
              pure (Right xobj)
    visitArray _ = error "Must visit array."
    visitStaticArray :: XObj -> State MemState (Either TypeError XObj)
    visitStaticArray xobj@(XObj (StaticArr arr) _ _) =
      do
        mapM_ visit arr
        results <- mapM unmanage arr
        case sequence results of
          Left e -> pure (Left e)
          Right _ ->
            -- We know that we want to add a deleter for the static array here
            do
              let var = varOfXObj xobj
                  Just (RefTy t@(StructTy (ConcreteNameTy "StaticArray") [_]) _) = xobjTy xobj
                  deleter = case nameOfPolymorphicFunction typeEnv globalEnv (FuncTy [t] UnitTy StaticLifetimeTy) "delete" of
                    Just pathOfDeleteFunc ->
                      ProperDeleter pathOfDeleteFunc var
                    Nothing ->
                      error ("No deleter found for Static Array : " ++ show t) --Just (FakeDeleter var)
              MemState deleters deps lifetimes <- get
              let newDeleters = Set.insert deleter deleters
                  newDeps = deps ++ depsForDeleteFunc typeEnv globalEnv t
                  newState = (MemState newDeleters newDeps lifetimes)
              put newState --(trace (show newState) newState)
              pure (Right xobj)
    visitStaticArray _ = error "Must visit static array."
    visitList :: XObj -> State MemState (Either TypeError XObj)
    visitList xobj@(XObj (Lst lst) i t) =
      case lst of
        [defn@(XObj (Defn maybeCaptures) _ _), nameSymbol@(XObj (Sym _ _) _ _), args@(XObj (Arr argList) _ _), body] ->
          let captures = fromMaybe [] (fmap Set.toList maybeCaptures)
           in --case defnReturnType of
              -- RefTy _ _ ->
              --   pure (Left (FunctionsCantReturnRefTy xobj funcTy))
              --  _ ->
              do
                mapM_ manage argList
                -- Add the captured variables (if any, only happens in lifted lambdas) as fake deleters
                -- TODO: Use another kind of Deleter for this case since it's pretty special?
                mapM_
                  ( \cap ->
                      modify
                        ( \memState ->
                            memState {memStateDeleters = Set.insert (FakeDeleter cap) (memStateDeleters memState)}
                        )
                  )
                  (map getName captures)
                mapM_ (addToLifetimesMappingsIfRef False) argList
                mapM_ (addToLifetimesMappingsIfRef False) captures -- For captured variables inside of lifted lambdas
                visitedBody <- visit body
                result <- unmanage body
                pure $
                  case result of
                    Left e -> Left e
                    Right _ ->
                      do
                        okBody <- visitedBody
                        pure (XObj (Lst [defn, nameSymbol, args, okBody]) i t)
        -- Fn / λ (Lambda)
        [fn@(XObj (Fn _ captures) _ _), args@(XObj (Arr _) _ _), body] ->
          do
            manage xobj -- manage inner lambdas but leave their bodies unvisited, they will be visited in the lifted version...
            mapM_ unmanage captures
            pure (Right (XObj (Lst [fn, args, body]) i t))
        -- Def
        [def@(XObj Def _ _), nameSymbol@(XObj (Sym _ _) _ _), expr] ->
          do
            visitedExpr <- visit expr
            result <- unmanage expr
            pure $
              case result of
                Left e -> Left e
                Right () ->
                  do
                    okExpr <- visitedExpr
                    pure (XObj (Lst [def, nameSymbol, okExpr]) i t)
        -- Let
        [letExpr@(XObj Let _ _), XObj (Arr bindings) bindi bindt, body] ->
          do
            MemState preDeleters _ _ <- get
            visitedBindings <- mapM visitLetBinding (pairwise bindings)
            visitedBody <- visit body
            result <- unmanage body
            case result of
              Left e -> pure (Left e)
              Right _ ->
                do
                  MemState postDeleters deps postLifetimes <- get
                  let diff = postDeleters Set.\\ preDeleters
                      newInfo = setDeletersOnInfo i diff
                      survivors = postDeleters Set.\\ diff -- Same as just pre deleters, right?!
                  put (MemState survivors deps postLifetimes)
                  --trace ("LET Pre: " ++ show preDeleters ++ "\nPost: " ++ show postDeleters ++ "\nDiff: " ++ show diff ++ "\nSurvivors: " ++ show survivors)
                  manage xobj
                  pure $ do
                    okBody <- visitedBody
                    okBindings <- fmap (concatMap (\(n, x) -> [n, x])) (sequence visitedBindings)
                    pure (XObj (Lst [letExpr, XObj (Arr okBindings) bindi bindt, okBody]) newInfo t)
        -- Set!
        [setbangExpr@(XObj SetBang _ _), variable, value] ->
          let varInfo = xobjInfo variable
              correctVariableAndMode =
                case variable of
                  -- DISABLE FOR NOW: (XObj (Lst (XObj (Sym (SymPath _ "copy") _) _ _ : symObj@(XObj (Sym _ _) _ _) : _)) _ _) -> Right symObj
                  symObj@(XObj (Sym _ mode) _ _) -> Right (symObj, mode)
                  anythingElse -> Left (CannotSet anythingElse)
           in case correctVariableAndMode of
                Left err ->
                  pure (Left err)
                Right (okCorrectVariable, okMode) ->
                  do
                    MemState preDeleters _ _ <- get
                    ownsTheVarBefore <- pure $ case createDeleter okCorrectVariable of
                      Nothing -> Right ()
                      Just d ->
                        if Set.member d preDeleters || isLookupGlobal okMode
                          then (Right ())
                          else (Left (UsingUnownedValue variable))
                    visitedValue <- visit value
                    _ <- unmanage value -- The assigned value can't be used anymore
                    MemState managed _ _ <- get
                    -- Delete the value previously stored in the variable, if it's still alive
                    let deleters = case createDeleter okCorrectVariable of
                          Just d -> Set.fromList [d]
                          Nothing -> Set.empty
                        newVariable =
                          case okMode of
                            Symbol -> error "How to handle this?"
                            LookupLocal _ ->
                              if Set.size (Set.intersection managed deleters) == 1 -- The variable is still alive
                                then variable {xobjInfo = setDeletersOnInfo varInfo deleters}
                                else variable -- don't add the new info = no deleter
                            LookupGlobal _ _ ->
                              variable {xobjInfo = setDeletersOnInfo varInfo deleters}
                    -- traceDeps = trace ("SET!-deleters for " ++ pretty xobj ++ " at " ++ prettyInfoFromXObj xobj ++ ":\n" ++
                    --                    "unmanaged " ++ pretty value ++ "\n" ++
                    --                    "managed: " ++ show managed ++ "\n" ++
                    --                    "deleters: " ++ show deleters ++ "\n")

                    case okMode of
                      Symbol -> error "Should only be be a global/local lookup symbol."
                      LookupLocal _ -> manage okCorrectVariable
                      LookupGlobal _ _ -> pure ()
                    pure $ case okMode of
                      LookupLocal (Capture _) ->
                        Left (CannotSetVariableFromLambda variable setbangExpr)
                      _ ->
                        do
                          okValue <- visitedValue
                          _ <- ownsTheVarBefore -- Force Either to fail
                          pure (XObj (Lst [setbangExpr, newVariable, okValue]) i t)
        [addressExpr@(XObj Address _ _), value] ->
          do
            visitedValue <- visit value
            pure $ do
              okValue <- visitedValue
              pure (XObj (Lst [addressExpr, okValue]) i t)
        [theExpr@(XObj The _ _), typeXObj, value] ->
          do
            visitedValue <- visit value
            result <- transferOwnership value xobj
            pure $ case result of
              Left e -> Left e
              Right _ -> do
                okValue <- visitedValue
                pure (XObj (Lst [theExpr, typeXObj, okValue]) i t)
        [refExpr@(XObj Ref _ _), value] ->
          do
            visited <- visit value
            case visited of
              Left e -> pure (Left e)
              Right visitedValue ->
                do
                  checkResult <- refCheck visitedValue
                  case checkResult of
                    Left e -> pure (Left e)
                    Right () -> do
                      let reffed = XObj (Lst [refExpr, visitedValue]) i t
                      pure $ Right reffed
        (XObj Deref _ _ : _) ->
          error "Shouldn't end up here, deref only works when calling a function, i.e. ((deref f) 1 2 3)."
        doExpr@(XObj Do _ _) : expressions ->
          do
            visitedExpressions <- mapM visit expressions
            result <- transferOwnership (last expressions) xobj
            pure $ case result of
              Left e -> Left e
              Right _ -> do
                okExpressions <- sequence visitedExpressions
                pure (XObj (Lst (doExpr : okExpressions)) i t)
        [whileExpr@(XObj While _ _), expr, body] ->
          do
            MemState preDeleters _ _ <- get
            visitedExpr <- visit expr
            MemState afterExprDeleters _ _ <- get
            visitedBody <- visit body
            manage body
            MemState postDeleters deps postLifetimes <- get
            -- Visit an extra time to simulate repeated use
            visitedExpr2 <- visit expr
            visitedBody2 <- visit body
            let diff = postDeleters \\ preDeleters
            put (MemState (postDeleters \\ diff) deps postLifetimes) -- Same as just pre deleters, right?!
            pure $ do
              okExpr <- visitedExpr
              okBody <- visitedBody
              _ <- visitedExpr2 -- This evaluates the second visit so that it actually produces the error
              _ <- visitedBody2 -- And this one too. Laziness FTW.
              let newInfo = setDeletersOnInfo i diff
                  -- Also need to set deleters ON the expression (for first run through the loop)
                  XObj objExpr objInfo objTy = okExpr
                  newExprInfo = setDeletersOnInfo objInfo (afterExprDeleters \\ preDeleters)
                  newExpr = XObj objExpr newExprInfo objTy
              pure (XObj (Lst [whileExpr, newExpr, okBody]) newInfo t)
        [ifExpr@(XObj If _ _), expr, ifTrue, ifFalse] ->
          do
            visitedExpr <- visit expr
            MemState preDeleters deps lifetimes <- get
            let (visitedTrue, stillAliveTrue) =
                  runState
                    ( do
                        v <- visit ifTrue
                        result <- transferOwnership ifTrue xobj
                        pure $ case result of
                          Left e -> error (show e)
                          Right () -> v
                    )
                    (MemState preDeleters deps lifetimes)
                (visitedFalse, stillAliveFalse) =
                  runState
                    ( do
                        v <- visit ifFalse
                        result <- transferOwnership ifFalse xobj
                        pure $ case result of
                          Left e -> error (show e)
                          Right () -> v
                    )
                    (MemState preDeleters deps lifetimes)
            let deletedInTrue = preDeleters \\ memStateDeleters stillAliveTrue
                deletedInFalse = preDeleters \\ memStateDeleters stillAliveFalse
                deletedInBoth = Set.intersection deletedInTrue deletedInFalse
                createdInTrue = memStateDeleters stillAliveTrue \\ preDeleters
                createdInFalse = memStateDeleters stillAliveFalse \\ preDeleters
                selfDeleter = case createDeleter xobj of
                  Just ok -> Set.fromList [ok]
                  Nothing -> Set.empty
                createdAndDeletedInTrue = createdInTrue \\ selfDeleter
                createdAndDeletedInFalse = createdInFalse \\ selfDeleter
                delsTrue = Set.union (deletedInFalse \\ deletedInBoth) createdAndDeletedInTrue
                delsFalse = Set.union (deletedInTrue \\ deletedInBoth) createdAndDeletedInFalse
                stillAliveAfter = preDeleters \\ Set.union deletedInTrue deletedInFalse
                depsAfter = memStateDeps stillAliveTrue ++ memStateDeps stillAliveFalse ++ deps -- Note: This merges all previous deps and the new ones, could be optimized..?!

            -- traceDeps = trace ("IF-deleters for " ++ pretty xobj ++ " at " ++ prettyInfoFromXObj xobj ++ " " ++ identifierStr xobj ++ ":\n" ++
            --                    "preDeleters: " ++ show preDeleters ++ "\n" ++
            --                    "stillAliveTrue: " ++ show (memStateDeleters stillAliveTrue) ++ "\n" ++
            --                    "stillAliveFalse: " ++ show (memStateDeleters stillAliveFalse) ++ "\n" ++
            --                    "createdInTrue: " ++ show createdInTrue ++ "\n" ++
            --                    "createdInFalse: " ++ show createdInFalse ++ "\n" ++
            --                    "createdAndDeletedInTrue: " ++ show createdAndDeletedInTrue ++ "\n" ++
            --                    "createdAndDeletedInFalse: " ++ show createdAndDeletedInFalse ++ "\n" ++
            --                    "deletedInTrue: " ++ show deletedInTrue ++ "\n" ++
            --                    "deletedInFalse: " ++ show deletedInFalse ++ "\n" ++
            --                    "deletedInBoth: " ++ show deletedInBoth ++ "\n" ++
            --                    "delsTrue: " ++ show delsTrue ++ "\n" ++
            --                    "delsFalse: " ++ show delsFalse ++ "\n" ++
            --                    "stillAlive: " ++ show stillAliveAfter ++ "\n" ++
            --                    "depsAfter: " ++ show depsAfter ++ "\n"
            --                   )

            put (MemState stillAliveAfter depsAfter lifetimes)
            manage xobj
            pure $ do
              okExpr <- visitedExpr
              okTrue <- visitedTrue
              okFalse <- visitedFalse
              pure (XObj (Lst [ifExpr, okExpr, del okTrue delsTrue, del okFalse delsFalse]) i t)
        matchExpr@(XObj (Match _) _ _) : expr : cases ->
          -- General idea of how to figure out what to delete in a 'match' statement:
          -- 1. Visit each case and investigate which variables are deleted in each one of the cases
          -- 2. Variables deleted in at least one case has to be deleted in all, so make a union U of all such vars
          --    but remove the ones that were not present before the 'match'
          -- 3. In each case - take the intersection of U and the vars deleted in that case and add this result to its deleters
          do
            visitedExpr <- visit expr
            case visitedExpr of
              Left e -> pure (Left e)
              Right okVisitedExpr ->
                do
                  _ <- unmanage okVisitedExpr
                  MemState preDeleters deps lifetimes <- get
                  vistedCasesAndDeps <- mapM visitMatchCase (pairwise cases)
                  case sequence vistedCasesAndDeps of
                    Left e -> pure (Left e)
                    Right okCasesAndDeps ->
                      let visitedCases = map fst okCasesAndDeps
                          depsFromCases = concatMap snd okCasesAndDeps
                          (finalXObj, postDeleters) = figureOutStuff okVisitedExpr visitedCases preDeleters
                       in do
                            put (MemState postDeleters (deps ++ depsFromCases) lifetimes)
                            manage xobj
                            pure (Right finalXObj)
          where
            figureOutStuff ::
              XObj ->
              [(Set.Set Deleter, (XObj, XObj))] ->
              Set.Set Deleter ->
              (XObj, Set.Set Deleter)
            figureOutStuff okVisitedExpr visitedCasesWithDeleters preDeleters =
              let postDeleters = map fst visitedCasesWithDeleters
                  -- postDeletersUnion = unionOfSetsInList postDeleters
                  postDeletersIntersection = intersectionOfSetsInList postDeleters
                  deletersAfterTheMatch = Set.intersection preDeleters postDeletersIntersection
                  -- The "postDeletersUnionPreExisting" are the vars that existed before the match but needs to
                  -- be deleted after it has executed (because some branches delete them)
                  -- postDeletersUnionPreExisting = Set.intersection postDeletersUnion preDeleters
                  deletersForEachCase = map (\\ deletersAfterTheMatch) postDeleters
                  -- These are the surviving vars after the 'match' expression:

                  okVisitedCases = map snd visitedCasesWithDeleters
                  okVisitedCasesWithAllDeleters =
                    zipWith
                      ( \(lhs, rhs) finalSetOfDeleters ->
                          -- Putting the deleter info on the lhs,
                          -- because the right one can collide with
                          -- the other expressions, e.g. a 'let'
                          let newLhsInfo = setDeletersOnInfo (xobjInfo lhs) finalSetOfDeleters
                           in [lhs {xobjInfo = newLhsInfo}, rhs]
                      )
                      okVisitedCases
                      deletersForEachCase
               in -- trace ("post deleters: " ++ show postDeleters)
                  -- trace ("\npost deleters union: " ++ show postDeletersUnion)
                  -- trace ("\npost deleters intersection: " ++ show postDeletersIntersection)
                  -- trace ("Post deleters union pre-existing: " ++ show postDeletersUnionPreExisting)
                  -- trace ("Post deleters for each case: " ++ show postDeleters)
                  ( XObj (Lst ([matchExpr, okVisitedExpr] ++ concat okVisitedCasesWithAllDeleters)) i t,
                    deletersAfterTheMatch
                  )
        XObj (Lst [deref@(XObj Deref _ _), f]) xi xt : uargs ->
          do
            -- Do not visit f in this case, we don't want to manage it's memory since it is a ref!
            visitedArgs <- sequence <$> mapM visitArg uargs
            case visitedArgs of
              Left err -> pure (Left err)
              Right args ->
                do
                  unmanagedArgs <- sequence <$> mapM unmanageArg args
                  manage xobj
                  pure $ do
                    okArgs <- unmanagedArgs
                    Right (XObj (Lst (XObj (Lst [deref, f]) xi xt : okArgs)) i t)
        f : uargs ->
          do
            visitedF <- visit f
            visitedArgs <- sequence <$> mapM visitArg uargs
            case visitedArgs of
              Left err -> pure (Left err)
              Right args -> do
                unmanagedArgs <- sequence <$> mapM unmanageArg args
                manage xobj
                pure $ do
                  okF <- visitedF
                  okArgs <- unmanagedArgs
                  Right (XObj (Lst (okF : okArgs)) i t)
        [] -> pure (Right xobj)
    visitList _ = error "Must visit list."
    visitMatchCase :: (XObj, XObj) -> State MemState (Either TypeError ((Set.Set Deleter, (XObj, XObj)), [XObj]))
    visitMatchCase (lhs@(XObj _ _ _), rhs@XObj {}) =
      do
        MemState preDeleters _ _ <- get
        _ <- visitCaseLhs lhs
        visitedRhs <- visit rhs
        _ <- unmanage rhs
        MemState postDeleters postDeps postLifetimes <- get
        -- let diff = postDeleters \\ preDeleters
        put (MemState preDeleters postDeps postLifetimes) -- Restore managed variables, TODO: Use a "local" state monad instead?
        pure $ do
          okVisitedRhs <- visitedRhs
          -- trace ("\npre: " ++ show preDeleters ++
          --        "\npost: " ++ show postDeleters ++
          --        "\ndiff: " ++ show diff)
          --   $
          pure ((postDeleters, (lhs, okVisitedRhs)), postDeps)
    visitCaseLhs :: XObj -> State MemState (Either TypeError [()])
    visitCaseLhs (XObj (Lst vars) _ _) =
      do
        result <- mapM visitCaseLhs vars
        let result' = sequence result
        pure (fmap concat result')
    visitCaseLhs xobj@(XObj (Sym (SymPath _ name) _) _ _)
      | isVarName name = do
        manage xobj
        pure (Right [])
      | otherwise = pure (Right [])
    visitCaseLhs (XObj Ref _ _) =
      pure (Right [])
    visitCaseLhs x =
      error ("Unhandled: " ++ show x)
    addToLifetimesMappingsIfRef :: Bool -> XObj -> State MemState ()
    addToLifetimesMappingsIfRef internal xobj =
      case xobjTy xobj of
        Just (RefTy _ (VarTy lt)) ->
          do
            m@(MemState _ _ lifetimes) <- get
            case Map.lookup lt lifetimes of
              Just _ ->
                --trace ("\nThere is already a mapping for '" ++ pretty xobj ++ "' from the lifetime '" ++ lt ++ "' to " ++ show existing ++ ", won't add " ++ show (makeLifetimeMode xobj)) $
                pure ()
              Nothing ->
                do
                  let lifetimes' = Map.insert lt makeLifetimeMode lifetimes
                  put $ --(trace $ "\nExtended lifetimes mappings for '" ++ pretty xobj ++ "' with " ++ show lt ++ " => " ++ show (makeLifetimeMode xobj) ++ " at " ++ prettyInfoFromXObj xobj ++ ":\n" ++ prettyLifetimeMappings lifetimes') $
                    m {memStateLifetimes = lifetimes'}
                  pure ()
        Just _ ->
          --trace ("Won't add to mappings! " ++ pretty xobj ++ " : " ++ show notThisType ++ " at " ++ prettyInfoFromXObj xobj) $
          pure ()
        _ ->
          --trace ("No type on " ++ pretty xobj ++ " at " ++ prettyInfoFromXObj xobj) $
          pure ()
      where
        makeLifetimeMode =
          if internal
            then LifetimeInsideFunction $
              case xobj of
                XObj (Lst [XObj Ref _ _, target]) _ _ -> varOfXObj target
                _ -> varOfXObj xobj
            else LifetimeOutsideFunction
    checkThatRefTargetIsAlive :: XObj -> State MemState (Either TypeError XObj)
    checkThatRefTargetIsAlive xobj =
      -- TODO: Replace this whole thing with a function that collects all lifetime variables in a type.
      case xobjTy xobj of
        Just (RefTy _ (VarTy lt)) ->
          performCheck lt
        Just (FuncTy _ _ (VarTy lt)) ->
          performCheck lt
        -- HACK (not exhaustive):
        Just (FuncTy _ (RefTy _ (VarTy lt)) _) ->
          performCheck lt
        _ ->
          pure -- trace ("Won't check " ++ pretty xobj ++ " : " ++ show (ty xobj))
            (Right xobj)
      where
        performCheck :: String -> State MemState (Either TypeError XObj)
        performCheck lt =
          do
            MemState deleters _ lifetimeMappings <- get
            case Map.lookup lt lifetimeMappings of
              Just (LifetimeInsideFunction deleterName) ->
                let matchingDeleters =
                      Set.toList $
                        Set.filter
                          ( \case
                              ProperDeleter {deleterVariable = dv} -> dv == deleterName
                              FakeDeleter {deleterVariable = dv} -> dv == deleterName
                              PrimDeleter {aliveVariable = dv} -> dv == deleterName
                              RefDeleter {refVariable = dv} -> dv == deleterName
                          )
                          deleters
                 in case matchingDeleters of
                      [] ->
                        --trace ("Can't use reference " ++ pretty xobj ++ " (with lifetime '" ++ lt ++ "', depending on " ++ show deleterName ++ ") at " ++ prettyInfoFromXObj xobj ++ ", it's not alive here:\n" ++ show xobj ++ "\nMappings: " ++ prettyLifetimeMappings lifetimeMappings ++ "\nAlive: " ++ show deleters ++ "\n") $
                        --pure (Right xobj)
                        pure (Left (UsingDeadReference xobj deleterName))
                      _ ->
                        --trace ("CAN use reference " ++ pretty xobj ++ " (with lifetime '" ++ lt ++ "', depending on " ++ show deleterName ++ ") at " ++ prettyInfoFromXObj xobj ++ ", it's not alive here:\n" ++ show xobj ++ "\nMappings: " ++ prettyLifetimeMappings lifetimeMappings ++ "\nAlive: " ++ show deleters ++ "\n") $
                        pure (Right xobj)
              Just LifetimeOutsideFunction ->
                --trace ("Lifetime OUTSIDE function: " ++ pretty xobj ++ " at " ++ prettyInfoFromXObj xobj) $
                pure (Right xobj)
              Nothing ->
                pure (Right xobj)
    -- case xobj of
    --   XObj (Sym _ (LookupLocal Capture)) _ _ ->
    --     -- Ignore these for the moment! TODO: FIX!!!
    --     pure (Right xobj)
    --   _ ->
    --     trace ("Failed to find lifetime key (when checking) '" ++ lt ++ "' for " ++ pretty xobj ++ " in mappings at " ++ prettyInfoFromXObj xobj) $
    --     pure (Right xobj)

    visitLetBinding :: (XObj, XObj) -> State MemState (Either TypeError (XObj, XObj))
    visitLetBinding (name, expr) =
      do
        visitedExpr <- visit expr
        addToLifetimesMappingsIfRef True expr
        result <- transferOwnership expr name
        pure $ case result of
          Left e -> Left e
          Right _ -> do
            okExpr <- visitedExpr
            pure (name, okExpr)
    visitArg :: XObj -> State MemState (Either TypeError XObj)
    visitArg xobj@(XObj _ _ (Just _)) =
      do
        afterVisit <- visit xobj
        case afterVisit of
          Right okAfterVisit -> do
            addToLifetimesMappingsIfRef True okAfterVisit
            pure (Right okAfterVisit)
          Left err -> pure (Left err)
    visitArg xobj@XObj {} =
      visit xobj
    unmanageArg :: XObj -> State MemState (Either TypeError XObj)
    unmanageArg xobj@(XObj _ _ (Just t)) =
      if isManaged typeEnv globalEnv t
        then do
          r <- unmanage xobj
          pure $ case r of
            Left err -> Left err
            Right () -> Right xobj
        else pure (Right xobj)
    unmanageArg xobj@XObj {} =
      pure (Right xobj)
    createDeleter :: XObj -> Maybe Deleter
    createDeleter xobj =
      case xobjTy xobj of
        Just (RefTy _ _) -> Just (RefDeleter (varOfXObj xobj))
        Just t ->
          let var = varOfXObj xobj
           in if isManaged typeEnv globalEnv t
                then case nameOfPolymorphicFunction typeEnv globalEnv (FuncTy [t] UnitTy StaticLifetimeTy) "delete" of
                  Just pathOfDeleteFunc ->
                    Just (ProperDeleter pathOfDeleteFunc var)
                  Nothing ->
                    --trace ("Found no delete function for " ++ var ++ " : " ++ (showMaybeTy (ty xobj)))
                    Just (FakeDeleter var)
                else Just (PrimDeleter var)
        Nothing -> error ("No type, can't manage " ++ show xobj)
    manage :: XObj -> State MemState ()
    manage xobj =
      if isSymbolThatCaptures xobj -- When visiting lifted lambdas, don't manage symbols that capture (they are owned by the environment).
        then pure ()
        else case createDeleter xobj of
          Just deleter -> do
            MemState deleters deps lifetimes <- get
            let newDeleters = Set.insert deleter deleters
                Just t = xobjTy xobj
                newDeps = deps ++ depsForDeleteFunc typeEnv globalEnv t
            put (MemState newDeleters newDeps lifetimes)
          Nothing -> pure ()
    deletersMatchingXObj :: XObj -> Set.Set Deleter -> [Deleter]
    deletersMatchingXObj xobj deleters =
      let var = varOfXObj xobj
       in Set.toList $
            Set.filter
              ( \case
                  ProperDeleter {deleterVariable = dv} -> dv == var
                  FakeDeleter {deleterVariable = dv} -> dv == var
                  PrimDeleter {aliveVariable = dv} -> dv == var
                  RefDeleter {refVariable = dv} -> dv == var
              )
              deleters
    isSymbolThatCaptures :: XObj -> Bool
    isSymbolThatCaptures xobj =
      case xobj of
        XObj (Sym _ (LookupLocal (Capture _))) _ _ -> True
        _ -> False
    unmanage :: XObj -> State MemState (Either TypeError ())
    unmanage xobj =
      let Just t = xobjTy xobj
       in if isManaged typeEnv globalEnv t && not (isGlobalFunc xobj)
            then do
              MemState deleters deps lifetimes <- get
              case deletersMatchingXObj xobj deleters of
                [] ->
                  pure $
                    if isSymbolThatCaptures xobj
                      then Left (UsingCapturedValue xobj)
                      else Left (UsingUnownedValue xobj)
                [one] ->
                  let newDeleters = Set.delete one deleters
                   in do
                        put (MemState newDeleters deps lifetimes)
                        pure (Right ())
                tooMany -> error ("Too many variables with the same name in set: " ++ show tooMany)
            else pure (Right ())
    refCheck :: XObj -> State MemState (Either TypeError ())
    refCheck xobj =
      let Just t = xobjTy xobj
          isGlobalVariable = case xobj of
            XObj (Sym _ (LookupGlobal _ _)) _ _ -> True
            _ -> False
       in if not isGlobalVariable && not (isGlobalFunc xobj) && isManaged typeEnv globalEnv t && not (isSymbolThatCaptures xobj) -- TODO: The 'isManaged typeEnv t' boolean check should be removed!
            then do
              MemState deleters _ _ <- get
              pure $ case deletersMatchingXObj xobj deleters of
                [] -> Left (GettingReferenceToUnownedValue xobj)
                [_] -> pure ()
                _ -> error $ "Too many variables with the same name in set (was looking for " ++ pretty xobj ++ " at " ++ prettyInfoFromXObj xobj ++ ")"
            else pure (pure ())
    transferOwnership :: XObj -> XObj -> State MemState (Either TypeError ())
    transferOwnership from to =
      do
        result <- unmanage from
        case result of
          Left e -> pure (Left e)
          Right _ -> do
            manage to --(trace ("Transfered from " ++ getName from ++ " '" ++ varOfXObj from ++ "' to " ++ getName to ++ " '" ++ varOfXObj to ++ "'") to)
            pure (Right ())

varOfXObj :: XObj -> String
varOfXObj xobj =
  case xobj of
    XObj (Sym path _) _ _ -> pathToC path
    _ -> case xobjInfo xobj of
      Just i -> freshVar i
      Nothing -> error ("Missing info on " ++ show xobj)

suffixTyVars :: String -> Ty -> Ty
suffixTyVars suffix t =
  case t of
    VarTy key -> VarTy (key ++ suffix)
    FuncTy argTys retTy ltTy -> FuncTy (map (suffixTyVars suffix) argTys) (suffixTyVars suffix retTy) (suffixTyVars suffix ltTy)
    StructTy name tyArgs -> StructTy name (fmap (suffixTyVars suffix) tyArgs)
    PointerTy x -> PointerTy (suffixTyVars suffix x)
    RefTy x lt -> RefTy (suffixTyVars suffix x) (suffixTyVars suffix lt)
    _ -> t

-- | The following functions will generate deleters and copy:ing methods for structs, they are shared with the Deftype module
data AllocationMode = StackAlloc | HeapAlloc

-- | The template for the 'delete' function of a concrete deftype.
concreteDelete :: TypeEnv -> Env -> [(String, Ty)] -> Template
concreteDelete typeEnv env members =
  Template
    (FuncTy [VarTy "p"] UnitTy StaticLifetimeTy)
    (const (toTemplate "void $NAME($p p)"))
    ( const
        ( toTemplate $
            unlines
              [ "$DECL {",
                joinLines (map (memberDeletion typeEnv env) members),
                "}"
              ]
        )
    )
    ( \_ ->
        concatMap
          (depsOfPolymorphicFunction typeEnv env [] "delete" . typesDeleterFunctionType)
          (filter (isManaged typeEnv env) (map snd members))
    )

-- | The template for the 'delete' function of a concrete deftype BUT it takes a pointer.
concreteDeleteTakePtr :: TypeEnv -> Env -> [(String, Ty)] -> Template
concreteDeleteTakePtr typeEnv env members =
  Template
    (FuncTy [PointerTy (VarTy "p")] UnitTy StaticLifetimeTy)
    (const (toTemplate "void $NAME($p* p)"))
    ( const
        ( toTemplate $
            unlines
              [ "$DECL {",
                joinLines (map (memberDeletionGeneral "->" typeEnv env) members),
                "}"
              ]
        )
    )
    ( \_ ->
        concatMap
          (depsOfPolymorphicFunction typeEnv env [] "delete" . typesDeleterFunctionType)
          (filter (isManaged typeEnv env) (map snd members))
    )

-- | Generate the C code for deleting a single member of the deftype.
-- | TODO: Should return an Either since this can fail!
memberDeletionGeneral :: String -> TypeEnv -> Env -> (String, Ty) -> String
memberDeletionGeneral separator typeEnv env (memberName, memberType) =
  case findFunctionForMember typeEnv env "delete" (typesDeleterFunctionType memberType) (memberName, memberType) of
    FunctionFound functionFullName -> "    " ++ functionFullName ++ "(p" ++ separator ++ memberName ++ ");"
    FunctionNotFound msg -> error msg
    FunctionIgnored -> "    /* Ignore non-managed member '" ++ memberName ++ "' : " ++ show memberType ++ " */"

memberDeletion :: TypeEnv -> Env -> (String, Ty) -> String
memberDeletion = memberDeletionGeneral "."

memberRefDeletion :: TypeEnv -> Env -> (String, Ty) -> String
memberRefDeletion = memberDeletionGeneral "Ref->"

-- | The template for the 'copy' function of a concrete deftype.
concreteCopy :: TypeEnv -> Env -> [(String, Ty)] -> Template
concreteCopy typeEnv env memberPairs =
  Template
    (FuncTy [RefTy (VarTy "p") (VarTy "q")] (VarTy "p") StaticLifetimeTy)
    (const (toTemplate "$p $NAME($p* pRef)"))
    (const (tokensForCopy typeEnv env memberPairs))
    ( \_ ->
        concatMap
          (depsOfPolymorphicFunction typeEnv env [] "copy" . typesCopyFunctionType)
          (filter (isManaged typeEnv env) (map snd memberPairs))
    )

tokensForCopy :: TypeEnv -> Env -> [(String, Ty)] -> [Token]
tokensForCopy typeEnv env memberPairs =
  toTemplate $
    unlines
      [ "$DECL {",
        "    $p copy = *pRef;",
        joinLines (map (memberCopy typeEnv env) memberPairs),
        "    return copy;",
        "}"
      ]

-- | Generate the C code for copying the member of a deftype.
-- | TODO: Should return an Either since this can fail!
memberCopy :: TypeEnv -> Env -> (String, Ty) -> String
memberCopy typeEnv env (memberName, memberType) =
  case findFunctionForMember typeEnv env "copy" (typesCopyFunctionType memberType) (memberName, memberType) of
    FunctionFound functionFullName ->
      "    copy." ++ memberName ++ " = " ++ functionFullName ++ "(&(pRef->" ++ memberName ++ "));"
    FunctionNotFound msg -> error msg
    FunctionIgnored -> "    /* Ignore non-managed member '" ++ memberName ++ "' : " ++ show memberType ++ " */"

concreteCopyPtr :: TypeEnv -> Env -> [(String, Ty)] -> Template
concreteCopyPtr typeEnv env memberPairs =
  Template
    (FuncTy [RefTy (VarTy "p") (VarTy "q")] (VarTy "p") StaticLifetimeTy)
    (const (toTemplate "$p* $NAME($p* pRef)"))
    (const (tokensForCopyPtr typeEnv env memberPairs))
    ( \_ ->
        concatMap
          (depsOfPolymorphicFunction typeEnv env [] "copy" . typesCopyFunctionType)
          (filter (isManaged typeEnv env) (map snd memberPairs))
    )

tokensForCopyPtr :: TypeEnv -> Env -> [(String, Ty)] -> [Token]
tokensForCopyPtr typeEnv env memberPairs =
  toTemplate $
    unlines
      [ "$DECL {",
        "    $p* copy = CARP_MALLOC(sizeof(*pRef));",
        "    *copy = *pRef;",
        joinLines (map (memberCopyPtr typeEnv env) memberPairs),
        "    return copy;",
        "}"
      ]

memberCopyPtr :: TypeEnv -> Env -> (String, Ty) -> String
memberCopyPtr typeEnv env (memberName, memberType) =
  case findFunctionForMember typeEnv env "copy" (typesCopyFunctionType memberType) (memberName, memberType) of
    FunctionFound functionFullName ->
      "    copy->" ++ memberName ++ " = " ++ functionFullName ++ "(&(pRef->" ++ memberName ++ "));"
    FunctionNotFound msg -> error msg
    FunctionIgnored -> "    /* Ignore non-managed member '" ++ memberName ++ "' : " ++ show memberType ++ " */"

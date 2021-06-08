{-# LANGUAGE LambdaCase #-}

module Concretize where

import AssignTypes
import Constraints
import Control.Monad.State
import Data.Either (fromRight)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Debug.Trace
import Env (envIsExternal, findPoly, getTypeBinder, getValue, insert, insertX, lookupEverywhere, searchValue)
import FindFunction
import Info
import InitialTypes
import Managed
import qualified Map
import Memory (manageMemory)
import Obj
import Polymorphism
import Reify
import Set ((\\))
import qualified Set
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
        concretizeResult <- concretizeTypeOfXObj typeEnv xobj
        whenRight concretizeResult $
          pure $ do
            okVisited <- visited
            Right (XObj (Arr okVisited) i (Just t))
    visit allowAmbig level env xobj@(XObj (StaticArr arr) i (Just t)) =
      do
        visited <- fmap sequence (mapM (visit allowAmbig level env) arr)
        concretizeResult <- concretizeTypeOfXObj typeEnv xobj
        whenRight concretizeResult $
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
          concretizeResult <- concretizeTypeOfXObj typeEnv body
          whenRight concretizeResult $ do
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
        let functionEnv = Env Map.empty (Just env) Nothing Set.empty InternalEnv 0
            envWithArgs =
              foldl'
                ( \e arg@(XObj (Sym path _) _ _) ->
                    -- n.b. this won't fail since we're inserting unqualified args into a fresh env
                    -- TODO: Still, it'd be nicer and more flexible to catch failures here.
                    let Right v = insertX e path arg in v
                )
                functionEnv
                argsArr
            Just funcTy = t
            allowAmbig = isTypeGeneric funcTy
        concretizeResult <- concretizeTypeOfXObj typeEnv body
        whenRight concretizeResult $ do
          visitedBody <- visit allowAmbig Inside (incrementEnvNestLevel envWithArgs) body
          pure $ do
            okBody <- visitedBody
            pure [defn, nameSymbol, args, okBody]
    visitList _ Inside _ xobj@(XObj (Lst [XObj (Defn _) _ _, _, XObj (Arr _) _ _, _]) _ _) =
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
            functionEnv = Env Map.empty (Just env) Nothing Set.empty InternalEnv (envFunctionNestingLevel env)
            envWithArgs =
              foldl'
                ( \e arg@(XObj (Sym path _) _ _) ->
                    let Right v = insertX e path arg in v
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
                lambdaPath = SymPath spath ("_Lambda_" ++ lambdaToCName name (envFunctionNestingLevel envWithArgs) ++ "_" ++ show (infoIdentifier ii) ++ "_env")
                lambdaNameSymbol = XObj (Sym lambdaPath Symbol) (Just dummyInfo) Nothing
                -- Anonymous functions bound to a let name might call themselves. These recursive instances will have already been qualified as LookupRecursive symbols.
                -- Rename the recursive calls according to the generated lambda name so that we can call these correctly from C.
                renameRecursives (XObj (Sym _ LookupRecursive) si st) = (XObj (Sym lambdaPath LookupRecursive) si st)
                renameRecursives x = x
                recBody = walk renameRecursives okBody
                environmentTypeName = pathToC lambdaPath ++ "_ty"
                tyPath = (SymPath [] environmentTypeName)
                extendedArgs =
                  if null capturedVars
                    then args
                    else -- If the lambda captures anything it need an extra arg for its env:

                      XObj
                        ( Arr
                            ( XObj
                                (Sym (SymPath [] "_env") Symbol)
                                (Just dummyInfo)
                                (Just (PointerTy (StructTy (ConcreteNameTy tyPath) []))) :
                              argsArr
                            )
                        )
                        ai
                        at
                lambdaCallback = XObj (Lst [XObj (Defn (Just (Set.fromList capturedVars))) (Just dummyInfo) Nothing, lambdaNameSymbol, extendedArgs, recBody]) i t
                -- The lambda will also carry with it a special made struct containing the variables it captures
                -- (if it captures at least one variable)
                structMemberPairs =
                  concatMap
                    ( \(XObj (Sym path _) _ (Just symTy)) ->
                        [XObj (Sym path Symbol) Nothing Nothing, reify symTy]
                    )
                    capturedVars
                environmentStructTy = StructTy (ConcreteNameTy tyPath) []
                environmentStruct =
                  XObj
                    ( Lst
                        [ XObj (Deftype environmentStructTy) Nothing Nothing,
                          XObj (Sym tyPath Symbol) Nothing Nothing,
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
                -- TODO: Fixup: Support modules in type envs.
                extendedTypeEnv = replaceLeft (FailedToAddLambdaStructToTyEnv tyPath environmentStruct) (insert typeEnv tyPath (toBinder environmentStruct))
             in case (extendedTypeEnv >>= \ext -> concretizeDefinition allowAmbig ext env visitedDefinitions lambdaCallback funcTy) of
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
                      pure (Right [XObj (Fn (Just lambdaPath) (Set.fromList capturedVars)) fni fnt, args, recBody])
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
    visitList _ Inside _ xobj@(XObj (Lst [XObj Def _ _, _, _]) _ _) =
      pure (Left (DefinitionsMustBeAtToplevel xobj))
    visitList allowAmbig level env (XObj (Lst [letExpr@(XObj Let _ _), XObj (Arr bindings) bindi bindt, body]) _ _) =
      do
        visitedBindings <- fmap sequence (mapM (visit allowAmbig level env) bindings)
        visitedBody <- visit allowAmbig level env body
        concretizeResults <- mapM (concretizeTypeOfXObj typeEnv . fst) (pairwise bindings)
        whenRight (sequence concretizeResults) $
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
        concretizeResult <- concretizeTypeOfXObj typeEnv expr
        whenRight concretizeResult $ do
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
        concretizeResult <- concretizeTypeOfXObj typeEnv func
        whenRight concretizeResult $ do
          concretizeResults <- mapM (concretizeTypeOfXObj typeEnv) args
          whenRight (sequence concretizeResults) $ do
            f <- visit allowAmbig level env func
            a <- fmap sequence (mapM (visit allowAmbig level env) args)
            pure $ do
              okF <- f
              okA <- a
              pure (okF : okA)
    visitList _ _ _ _ = error "visitlist"
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
      case searchValue env path of
        Right (foundEnv, binder)
          | envIsExternal foundEnv ->
            let theXObj = binderXObj binder
                Just theType = xobjTy theXObj
                typeOfVisited = fromMaybe (error ("Missing type on " ++ show xobj ++ " at " ++ prettyInfoFromXObj xobj ++ " when looking up path " ++ show path)) t
             in if --(trace $ "CHECKING " ++ getName xobj ++ " : " ++ show theType ++ " with visited type " ++ show typeOfVisited ++ " and visited definitions: " ++ show visitedDefinitions) $
                (isTypeGeneric theType && not (isTypeGeneric typeOfVisited))
                  then case concretizeDefinition allowAmbig typeEnv env visitedDefinitions theXObj typeOfVisited of
                    Left err -> pure (Left err)
                    Right (concrete, deps) ->
                      do
                        modify (concrete :)
                        modify (deps ++)
                        pure (Right (XObj (Sym (getPath concrete) lookupMode) i t))
                  else pure (Right xobj)
          | otherwise -> pure (Right xobj)
        _ -> pure (Right xobj)
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
                      let replaced = (replaceTyVars mappings t')
                          suffixed = suffixTyVars ("_x" ++ show (infoIdentifier i')) replaced -- Make sure it gets unique type variables. TODO: Is there a better way?
                          normalSymbol = XObj (Sym singlePath mode) i (Just suffixed)
                       in visitSymbol
                            allowAmbig
                            env
                            --(trace ("Disambiguated " ++ pretty xobj ++ " at " ++ prettyInfoFromXObj xobj ++ " to " ++ show singlePath ++ " : " ++ show suffixed ++ ", used to be " ++ show t' ++ ", theType = " ++ show theType ++ ", mappings = " ++ show mappings) normalSymbol) normalSymbol
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
      case getTypeBinder typeEnv name of
        Right (Binder _ (XObj (Lst [XObj (Interface _ interfacePaths) _ _, _]) _ _)) ->
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
        Right _ -> error "visitinterfacesym1"
        Left _ ->
          error ("No interface named '" ++ name ++ "' found.")
    visitInterfaceSym _ _ _ = error "visitinterfacesym"

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
    decreaseCaptureLevel (XObj (Sym path lookup') _ ty) =
      XObj
        ( Sym
            path
            ( case lookup' of
                Symbol -> Symbol
                LookupLocal NoCapture -> Symbol
                LookupLocal (Capture n) ->
                  if n <= 1
                    then Symbol
                    else LookupLocal (Capture (n -1))
                _ -> error "decreasecapturelevel1"
            )
        )
        (Just dummyInfo)
        ty
    decreaseCaptureLevel _ = error "decreasecapturelevel"
    visit xobj =
      case xobjObj xobj of
        -- don't peek inside lambdas, trust their capture lists:
        (Lst [XObj (Fn _ captures) _ _, _, _]) -> Set.toList captures
        -- in the case of lets, we have to remove new bindings from the list of captured variables,
        -- including the ones captured in later bindings
        (Lst [XObj Let _ _, XObj (Arr bindings) _ _, body]) ->
          let (bound, bindingsCaptured) =
                foldl'
                  ( \(bound', captured) (XObj sym _ ty, expr) ->
                      let capt = filter (`Set.notMember` bound') (visit expr)
                       in (Set.insert (XObj sym (Just dummyInfo) ty) bound', capt ++ captured)
                  )
                  (Set.empty, [])
                  (pairwise bindings)
           in let bodyCaptured = filter (`Set.notMember` bound) (visit body)
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
concretizeType typeEnv arrayTy@(StructTy (ConcreteNameTy (SymPath [] "Array")) varTys) =
  if isTypeGeneric arrayTy
    then Right []
    else do
      deps <- mapM (concretizeType typeEnv) varTys
      Right (defineArrayTypeAlias arrayTy : concat deps)
-- TODO: Remove ugly duplication of code here:
concretizeType typeEnv arrayTy@(StructTy (ConcreteNameTy (SymPath [] "StaticArray")) varTys) =
  if isTypeGeneric arrayTy
    then Right []
    else do
      deps <- mapM (concretizeType typeEnv) varTys
      Right (defineStaticArrayTypeAlias arrayTy : concat deps)
concretizeType typeEnv genericStructTy@(StructTy (ConcreteNameTy (SymPath _ name)) _) =
  -- TODO: This function only looks up direct children of the type environment.
  -- However, spath can point to types that belong to a module. Pass the global env here.
  case (getTypeBinder typeEnv name) of
    Right (Binder _ x) -> go x
    _ -> Right []
  where
    go :: XObj -> Either TypeError [XObj]
    go (XObj (Lst (XObj (Deftype originalStructTy) _ _ : _ : rest)) _ _) =
      if isTypeGeneric originalStructTy
        then instantiateGenericStructType typeEnv originalStructTy genericStructTy rest
        else Right []
    go (XObj (Lst (XObj (DefSumtype originalStructTy) _ _ : _ : rest)) _ _) =
      if isTypeGeneric originalStructTy
        then instantiateGenericSumtype typeEnv originalStructTy genericStructTy rest
        else Right []
    go (XObj (Lst (XObj (ExternalType _) _ _ : _)) _ _) = Right []
    go x = error ("Non-deftype found in type env: " ++ pretty x)
concretizeType t (RefTy rt _) =
  concretizeType t rt
concretizeType t (PointerTy pt) =
  concretizeType t pt
concretizeType _ _ =
  Right [] -- ignore all other types

-- | Renames the type variable literals in a sum type for temporary validation.
renameGenericTypeSymbolsOnSum :: [(Ty, Ty)] -> XObj -> XObj
renameGenericTypeSymbolsOnSum varpairs x@(XObj (Lst (caseNm : caseMembers)) i t) =
  case caseMembers of
    [XObj (Arr arr) ii tt] ->
      XObj (Lst (caseNm : [XObj (Arr (map replacer arr)) ii tt])) i t
    _ -> x
  where
    mapp = Map.fromList varpairs
    replacer mem@(XObj (Sym (SymPath [] name) _) _ _) =
      let Just perhapsTyVar = xobjToTy mem
       in if isFullyGenericType perhapsTyVar
            then case Map.lookup (VarTy name) mapp of
              Just new -> reify new
              _ -> mem
            else mem
    replacer y = y
renameGenericTypeSymbolsOnSum _ x = x

-- | Renames the type variable literals in a product type for temporary validation.
renameGenericTypeSymbolsOnProduct :: [Ty] -> [XObj] -> [XObj]
renameGenericTypeSymbolsOnProduct vars members =
  concatMap (\(var, (v, t)) -> [v, rename var t]) (zip vars (pairwise members))
  where
    rename var mem =
      let Just perhapsTyVar = xobjToTy mem
       in if isFullyGenericType perhapsTyVar
            then reify var
            else mem

-- | Given an generic struct type and a concrete version of it, generate all dependencies needed to use the concrete one.
-- TODO: Handle polymorphic constructors (a b).
instantiateGenericStructType :: TypeEnv -> Ty -> Ty -> [XObj] -> Either TypeError [XObj]
instantiateGenericStructType typeEnv originalStructTy@(StructTy _ _) genericStructTy membersXObjs =
  -- Turn (deftype (A a) [x a, y a]) into (deftype (A Int) [x Int, y Int])
  let fake1 = XObj (Sym (SymPath [] "a") Symbol) Nothing Nothing
      fake2 = XObj (Sym (SymPath [] "b") Symbol) Nothing Nothing
      XObj (Arr memberXObjs) _ _ = head membersXObjs
      rename@(StructTy _ renamedOrig) = evalState (renameVarTys originalStructTy) 0
   in case solve [Constraint originalStructTy genericStructTy fake1 fake2 fake1 OrdMultiSym] of
        Left e -> error (show e)
        Right mappings ->
          let Right mapp = solve [Constraint rename genericStructTy fake1 fake2 fake1 OrdMultiSym]
              nameFixedMembers = renameGenericTypeSymbolsOnProduct renamedOrig memberXObjs
              validMembers = replaceGenericTypeSymbolsOnMembers mapp nameFixedMembers
              concretelyTypedMembers = replaceGenericTypeSymbolsOnMembers mappings memberXObjs
           in -- We only used the renamed types for validation--passing the
              -- renamed xobjs further down leads to syntactical issues.
              case validateMembers AllowAnyTypeVariableNames typeEnv renamedOrig validMembers of
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
instantiateGenericStructType _ _ _ _ = error "instantiategenericstructtype"

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
      rename@(StructTy _ renamedOrig) = evalState (renameVarTys originalStructTy) 0
   in case solve [Constraint rename genericStructTy fake1 fake2 fake1 OrdMultiSym] of
        Left e -> error (show e)
        Right mappings ->
          let nameFixedCases = map (renameGenericTypeSymbolsOnSum (zip originalTyVars renamedOrig)) cases
              concretelyTypedCases = map (replaceGenericTypeSymbolsOnCase mappings) nameFixedCases
              deps = mapM (depsForCase typeEnv) concretelyTypedCases
           in case toCases typeEnv AllowAnyTypeVariableNames renamedOrig concretelyTypedCases of -- Don't care about the cases, this is done just for validation.
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
instantiateGenericSumtype _ _ _ _ = error "instantiategenericsumtype"

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
        then maybe xobj reify (Map.lookup name mappings)
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
  case searchValue env p of
    Right (e, Binder _ found)
      | envIsExternal e -> forceTy found
      | otherwise -> error "Local bindings shouldn't be ambiguous."
    _ -> error ("Couldn't find " ++ show p ++ " in env:\n" ++ prettyEnvironmentChain env)

-- | Get the mode of a symbol at a given path.
-- |
-- | TODO: this duplicates a bunch of functionality from  Qualify.hs, namely
-- | parts of doesNotBelongToAnInterface.
modeFromPath :: Env -> SymPath -> SymbolMode
modeFromPath env p =
  case searchValue env p of
    Right (_, Binder _ (XObj (Lst (XObj (External (Just overrideWithName)) _ _ : _)) _ _)) ->
      LookupGlobalOverride overrideWithName
    Right (_, Binder _ (XObj (Lst (XObj (ExternalType (Just overrideWithName)) _ _ : _)) _ _)) ->
      LookupGlobalOverride overrideWithName
    Right (_, Binder _ found@(XObj (Lst (XObj (External _) _ _ : _)) _ _)) ->
      LookupGlobal ExternalCode (definitionMode found)
    Right (e, Binder _ found) ->
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
    _ -> error ("Couldn't find " ++ show p ++ " in env:\n" ++ prettyEnvironmentChain env)

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
        -- todo: repeat of defn below
        XObj (Lst (XObj Def _ _ : _)) _ _ ->
          let withNewPath = setPath definition newPath
              mappings = unifySignatures polyType concreteType
           in case assignTypes mappings withNewPath of
                Right typed ->
                  if newPath `elem` visitedDefinitions
                    then pure (withNewPath, [])
                    else do
                      (concrete, deps) <- concretizeXObj allowAmbiguity typeEnv globalEnv (newPath : visitedDefinitions) typed
                      (managed, memDeps) <- manageMemory typeEnv globalEnv concrete
                      pure (managed, deps ++ memDeps)
                Left e -> Left e
        XObj (Lst (XObj (Defn _) _ _ : _)) _ _ ->
          let withNewPath = setPath definition newPath
              mappings = unifySignatures polyType concreteType
           in case assignTypes mappings withNewPath of
                Right typed ->
                  if newPath `elem` visitedDefinitions
                    then pure (withNewPath, [])
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

-- | Find all the dependencies of a polymorphic function with a name and a desired concrete type.
depsOfPolymorphicFunction :: TypeEnv -> Env -> [SymPath] -> String -> Ty -> [XObj]
depsOfPolymorphicFunction typeEnv env visitedDefinitions functionName functionType =
  case allImplementations typeEnv env functionName functionType of
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

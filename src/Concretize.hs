{-# LANGUAGE LambdaCase #-}

-- | Module Concretize determines the dependencies of polymorphic objects and
-- resolves the object into a "concrete" version, where its types are no longer
-- variables.
module Concretize
  ( concretizeXObj,
    concretizeType,
    depsForCopyFunc,
    depsForPrnFunc,
    depsForDeleteFunc,
    depsForDeleteFuncs,
    depsOfPolymorphicFunction,
    typesStrFunctionType,
    concreteDelete,
    memberDeletion,
    memberRefDeletion,
    concreteCopy,
    tokensForCopy,
    memberCopy,
    replaceGenericTypeSymbolsOnMembers,
    replaceGenericTypeSymbolsOnFields,
  )
where

import AssignTypes
import Constraints
import Control.Applicative
import Control.Monad.State
import Data.Either (fromRight)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Debug.Trace
import Env (EnvironmentError, empty, envIsExternal, findTypeBinder, getTypeBinder, insert, insertX, searchValue)
import Forms
import Info
import InitialTypes
import Managed
import qualified Map
import Memory (manageMemory)
import Obj
import Polymorphism
import Reify
import qualified Set
import ToTemplate
import qualified TypeCandidate as TC
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
  case runState (visit (visitedDefinitions ++ [getPath root]) allowAmbiguityRoot Toplevel typeEnv rootEnv root) [] of
    (Left err, _) -> Left err
    (Right xobj, deps) -> Right (xobj, deps)

--------------------------------------------------------------------------------
-- Visit functions
--
-- These functions take a state and supporting information and gradually
-- convert generically typed xobjs into concretely typed xobjs.
--
-- The functions prefixed "visit" primarily just recur into the component parts
-- of different Carp forms while the "concretizeType..." functions perform the
-- actual type conversion work.

-- | The type of visit functions. These functions convert the types of the
-- components of a form into concrete types and take the following arguments:
--   - A List of paths that have already been visited.
--   - A bool indicating whether or not type variables are allowed
--   - A level indicating if we are in an inner component of a form or the top level
--   - A type environment
--   - A value environment
--   - The xobj to concretize
type Visitor = [SymPath] -> Bool -> Level -> TypeEnv -> Env -> XObj -> State [XObj] (Either TypeError [XObj])

-- | Process the components of a form, yielding a concretely typed (no
-- generics) version of the form.
visit :: [SymPath] -> Bool -> Level -> TypeEnv -> Env -> XObj -> State [XObj] (Either TypeError XObj)
visit visited ambig _ tenv env xobj@(SymPat _ _) = visitSymbol visited ambig tenv env xobj
visit visited ambig _ tenv env xobj@(MultiSymPat _ _) = visitMultiSym visited ambig tenv env xobj
visit visited ambig _ tenv env xobj@(InterfaceSymPat _) = visitInterfaceSym visited ambig tenv env xobj
visit visited allowAmbig level tenv env xobj@(ListPat _) =
  do
    vLst <- visitList visited allowAmbig level tenv env xobj
    pure (vLst >>= \ok -> pure (setObj xobj (Lst ok)))
visit visited allowAmbig level tenv env xobj@(ArrPat arr) =
  do
    vArr <- fmap sequence (mapM (visit visited allowAmbig level tenv env) arr)
    c <- concretizeTypeOfXObj tenv env xobj
    pure (c >> vArr >>= \ok -> pure (setObj xobj (Arr ok)))
visit visited allowAmbig level tenv env xobj@(StaticArrPat arr) =
  do
    vArr <- fmap sequence (mapM (visit visited allowAmbig level tenv env) arr)
    c <- concretizeTypeOfXObj tenv env xobj
    pure (c >> vArr >>= \ok -> pure (setObj xobj (StaticArr ok)))
visit _ _ _ _ _ x = pure (Right x)

-- | Entry point for concretely typing the components of a list form.
visitList :: Visitor
visitList _ _ _ _ _ (ListPat []) = pure (Right [])
visitList p a l t e x@(ListPat (DefPat _ _ _)) = visitDef p a l t e x
visitList p a l t e x@(ListPat (DefnPat _ _ _ _)) = visitDefn p a l t e x
visitList p a l t e x@(ListPat (LetPat _ _ _)) = visitLet p a l t e x
visitList p a l t e x@(ListPat (ThePat _ _ _)) = visitThe p a l t e x
visitList p a l t e x@(ListPat (MatchPat _ _ _)) = visitMatch p a l t e x
visitList p a l t e x@(ListPat (SetPat _ _ _)) = visitSetBang p a l t e x
visitList p a l t e x@(ListPat (FnPat _ _ _)) = visitFn p a l t e x
visitList p a l t e x@(ListPat (AppPat _ _)) = visitApp p a l t e x
visitList _ _ _ _ _ x = pure (Left (CannotConcretize x))

-- | Helper for producing a new environment with all a functions argument symbols.
--
-- Used to concretize defn and fn forms.
envWithFunctionArgs :: Env -> [XObj] -> Either EnvironmentError Env
envWithFunctionArgs env arr =
  let functionEnv = Env Map.empty (Just env) Nothing Set.empty InternalEnv (envFunctionNestingLevel env)
   in foldM
        (\e arg@(XObj (Sym path _) _ _) -> insertX e path arg)
        functionEnv
        arr

-- | Concretely type a function definition.
--
-- "main" is treated as a special case.
visitDefn :: Visitor
visitDefn p a l t e x@(ListPat (DefnPat _ (SymPat (SymPath [] "main") _) _ _)) = visitMain p a l t e x
visitDefn visited _ Toplevel tenv env x@(ListPat (DefnPat defn name args@(ArrPat arr) body)) =
  do
    mapM_ (concretizeTypeOfXObj tenv env) arr
    let envWithArgs = fromRight Env.empty (envWithFunctionArgs env arr)
        allowAmbig = maybe True isTypeGeneric (xobjTy x)
    c <- concretizeTypeOfXObj tenv env body
    vBody <- visit (getPath x : visited) allowAmbig Inside tenv (incrementEnvNestLevel envWithArgs) body
    pure (c >> vBody >>= go)
  where
    go b = pure [defn, name, args, b]
visitDefn _ _ Inside _ _ x@(ListPat (DefnPat _ _ _ _)) =
  pure (Left (DefinitionsMustBeAtToplevel x))
visitDefn _ _ _ _ _ x = pure (Left (CannotConcretize x))

-- | Concretely type a program entry point. Can only return Int or Unit.
visitMain :: Visitor
visitMain visited _ Toplevel tenv env (ListPat (DefnPat defn name@(SymPat (SymPath [] "main") _) args@(ArrPat []) body)) =
  do
    c <- concretizeTypeOfXObj tenv env body
    vBody <- visit visited False Inside tenv env body
    pure (c >> vBody >>= typeCheck)
  where
    typeCheck b =
      let t = fromMaybe UnitTy (xobjTy b)
       in if (t `elem` validMainTypes) || isTypeGeneric t
            then pure [defn, name, args, b]
            else Left (MainCanOnlyReturnUnitOrInt name t)
    validMainTypes = [UnitTy, IntTy]
visitMain _ _ _ _ _ (ListPat (DefnPat _ name@(SymPat (SymPath [] "main") _) (ArrPat arr) _)) =
  pure (Left (MainCannotHaveArguments name (length arr)))
visitMain _ _ _ _ _ x = pure (Left (CannotConcretize x))

-- | Concretely type a Def form.
visitDef :: Visitor
visitDef visited _ Toplevel tenv env x@(ListPat (DefPat def name body)) =
  do
    vBody <- visit visited allowAmbig Inside tenv env body
    pure (vBody >>= \ok -> pure [def, name, ok])
  where
    allowAmbig = isTypeGeneric (fromMaybe (VarTy "a") (xobjTy x))
visitDef _ _ Inside _ _ x@(ListPat (DefPat _ _ _)) =
  pure (Left (DefinitionsMustBeAtToplevel x))
visitDef _ _ _ _ _ x = pure (Left (CannotConcretize x))

-- | Concretely type a Let (let [bindings...] <body>) form.
visitLet :: Visitor
visitLet visited allowAmbig level tenv env (ListPat (LetPat letExpr arr@(ArrPat bindings) body)) =
  do
    bindings' <- fmap sequence (mapM (visit visited allowAmbig level tenv env) bindings)
    body' <- visit visited allowAmbig level tenv env body
    c <- mapM (concretizeTypeOfXObj tenv env . fst) (pairwise bindings)
    pure (sequence c >> go bindings' body')
  where
    go x' y = do
      okBindings <- x'
      okBody <- y
      pure [letExpr, (setObj arr (Arr okBindings)), okBody]
visitLet _ _ _ _ _ x = pure (Left (CannotConcretize x))

-- | Concretely type a The (the <type> <value>) form.
visitThe :: Visitor
visitThe visited allowAmbig level tenv env (ListPat (ThePat the ty value)) =
  do
    vVal <- visit visited allowAmbig level tenv env value
    pure (vVal >>= \ok -> pure [the, ty, ok])
visitThe _ _ _ _ _ x = pure (Left (CannotConcretize x))

-- | Concretely type a Match (match <expr> <clauses...>) form.
visitMatch :: Visitor
visitMatch visited allowAmbig level tenv env (ListPat (MatchPat match expr rest)) =
  do
    c <- concretizeTypeOfXObj tenv env expr
    vExpr <- visit visited allowAmbig level tenv env expr
    mapM_ (concretizeTypeOfXObj tenv env . snd) (pairwise rest)
    vCases <- fmap sequence (mapM (visitMatchCase visited allowAmbig level tenv env) (pairwise rest))
    pure (c >> go vExpr vCases)
  where
    go x y = do
      okExpr <- x
      okRest <- fmap concat y
      pure ([match, okExpr] ++ okRest)
visitMatch _ _ _ _ _ x = pure (Left (CannotConcretize x))

-- | Concretely type a Match form case.
visitMatchCase :: [SymPath] -> Bool -> Level -> TypeEnv -> Env -> (XObj, XObj) -> State [XObj] (Either TypeError [XObj])
visitMatchCase visited allowAmbig level tenv env (lhs, rhs) =
  -- TODO! This changes the names of some tags (which is corrected in Emit) but perhaps there is a better way where they can be identified as tags and not changed?
  do
    vl <- visit visited allowAmbig level tenv env lhs
    vr <- visit visited allowAmbig level tenv env rhs
    pure (liftA2 (\x y -> [x, y]) vl vr)

-- | Concretely type a Set (set! <var> <value>) form.
visitSetBang :: Visitor
visitSetBang visited allowAmbig _ tenv env (ListPat (SetPat set var value)) =
  do
    vVal <- visit visited allowAmbig Inside tenv env value
    pure (vVal >>= \ok -> pure [set, var, ok])
visitSetBang _ _ _ _ _ x = pure (Left (CannotConcretize x))

-- | Concretely type a function application (<function> <args...>) form.
visitApp :: Visitor
visitApp visited allowAmbig level tenv env (ListPat (AppPat func args)) =
  do
    c <- concretizeTypeOfXObj tenv env func
    cs <- fmap sequence $ mapM (concretizeTypeOfXObj tenv env) args
    vFunc <- visit visited allowAmbig level tenv env func
    vArgs <- fmap sequence (mapM (visit visited allowAmbig level tenv env) args)
    pure (c >> cs >> liftA2 (:) vFunc vArgs)
visitApp _ _ _ _ _ x = pure (Left (CannotConcretize x))

-- | Concretely type an anonymous function and convert it into a
-- resolvable/retrievable lambda.
mkLambda :: Visitor
mkLambda visited allowAmbig _ tenv env root@(ListPat (FnPat fn arr@(ArrPat args) body)) =
  let capturedVars = filter (\xobj -> xobjObj (toGeneralSymbol xobj) `notElem` map xobjObj args) (collectCapturedVars body)
      -- Create a new (top-level) function that will be used when the lambda is called.
      -- Its name will contain the name of the (normal, non-lambda) function it's contained within,
      -- plus the identifier of the particular s-expression that defines the lambda.
      SymPath spath name = last visited
      Just (RefTy lambdaTyNoRef _) = xobjTy root
      lambdaPath = SymPath spath ("_Lambda_" ++ lambdaToCName name (envFunctionNestingLevel env) ++ "_" ++ show (maybe 0 infoIdentifier (xobjInfo root)) ++ "_callback")
      lambdaNameSymbol = XObj (Sym lambdaPath Symbol) (Just dummyInfo) Nothing
      -- Anonymous functions bound to a let name might call themselves. These recursive instances will have already been qualified as LookupRecursive symbols.
      -- Rename the recursive calls according to the generated lambda name so that we can call these correctly from C.
      renameRecursives (XObj (Sym _ LookupRecursive) si st) = XObj (Sym lambdaPath LookupRecursive) si st
      renameRecursives x = x
      recBody = walk renameRecursives body
      environmentTypeName = pathToC lambdaPath ++ "_ty"
      tyPath = SymPath [] environmentTypeName
      extendedArgs =
        if null capturedVars
          then arr
          else -- If the lambda captures anything it need an extra arg for its env:

            ( setObj
                arr
                ( Arr
                    ( XObj
                        (Sym (SymPath [] "_env") Symbol)
                        (Just dummyInfo)
                        (Just (PointerTy (StructTy (ConcreteNameTy tyPath) []))) :
                      args
                    )
                )
            )
      lambdaCallback = XObj (Lst [XObj (Defn (Just (Set.fromList capturedVars))) (Just dummyInfo) Nothing, lambdaNameSymbol, extendedArgs, recBody]) (xobjInfo root) (Just lambdaTyNoRef)
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
          (xobjInfo root)
          (Just TypeTy)
      pairs = memberXObjsToPairs structMemberPairs
      deleteFnTy = typesDeleterFunctionType (PointerTy environmentStructTy)
      deleteFnTemplate = concreteDeleteTakePtr tenv env pairs
      (deleteFn, deleterDeps) = instantiateTemplate (SymPath [] (environmentTypeName ++ "_delete")) deleteFnTy deleteFnTemplate
      copyFnTy = typesCopyFunctionType environmentStructTy
      copyFnTemplate = concreteCopyPtr tenv env pairs
      (copyFn, copyDeps) = instantiateTemplate (SymPath [] (environmentTypeName ++ "_copy")) copyFnTy copyFnTemplate
      -- The type env has to contain the lambdas environment struct for 'concretizeDefinition' to work:
      -- TODO: Support modules in type envs.
      extendedTypeEnv = replaceLeft (FailedToAddLambdaStructToTyEnv tyPath environmentStruct) (insert tenv tyPath (toBinder environmentStruct))
   in --(fromMaybe UnitTy (xobjTy root))
      case extendedTypeEnv >>= \ext -> concretizeDefinition allowAmbig ext env visited lambdaCallback lambdaTyNoRef of
        Left e -> pure (Left e)
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
            pure (Right [XObj (Fn (Just lambdaPath) (Set.fromList capturedVars)) (xobjInfo fn) (xobjTy fn), arr, body])
mkLambda _ _ _ _ _ root = pure (Left (CannotConcretize root))

-- | Concretize an anonymous function (fn [args...] <body>)
--
-- The basic idea of this function is to first visit the body of the lambda ("in place"),
-- then take the resulting body and put into a separate function 'defn' with a new name
-- in the global scope. That function definition will be set as the lambdas '.callback' in
-- the C code.
visitFn :: Visitor
visitFn visited allowAmbig level tenv env x@(ListPat (FnPat fn args@(ArrPat arr) body)) =
  do
    mapM_ (concretizeTypeOfXObj tenv env) arr
    let envWithArgs = fromRight Env.empty (envWithFunctionArgs env arr)
    vBody <- visit visited allowAmbig Inside tenv (incrementEnvNestLevel envWithArgs) body
    either (pure . Left) (\b -> mkLambda visited allowAmbig level tenv envWithArgs (setObj x (Lst [fn, args, b]))) vBody
visitFn _ _ _ _ _ x = pure (Left (CannotConcretize x))

--------------------------------------------------------------------------------
-- Symbol concretization functions
--
-- Functions that concretely type arbitrary symbols, like `foo`
-- This differ slightly from the functions that concretely type carp forms.
--
-- Symbols can designate:
--   - A unique, and thus uniquely typed symbol.
--   - An ambiguous "multi" symbol, the correct type of which is context-dependent
--   - An interface symbol, which may be implemented by several concrete
--     symbols of potentially different concrete types. Like the multi-symbol
--     case, depends on context and type checking.

-- | Concretely type a unique symbol.
visitSymbol :: [SymPath] -> Bool -> TypeEnv -> Env -> XObj -> State [XObj] (Either TypeError XObj)
visitSymbol visited allowAmbig tenv env xobj@(SymPat path mode) =
  case searchValue env path of
    Right (foundEnv, binder)
      | envIsExternal foundEnv ->
        let theXObj = binderXObj binder
            Just theType = xobjTy theXObj
            typeOfVisited = fromMaybe (error ("Missing type on " ++ show xobj ++ " at " ++ prettyInfoFromXObj xobj ++ " when looking up path " ++ show path)) (xobjTy xobj)
         in if --(trace $ "CHECKING " ++ getName xobj ++ " : " ++ show theType ++ " with visited type " ++ show typeOfVisited ++ " and visited definitions: " ++ show visitedDefinitions) $
            (isTypeGeneric theType && not (isTypeGeneric typeOfVisited))
              then case concretizeDefinition allowAmbig tenv env visited theXObj typeOfVisited of
                Left err -> pure (Left err)
                Right (concrete, deps) ->
                  do
                    modify (concrete :)
                    modify (deps ++)
                    pure (Right (XObj (Sym (getPath concrete) mode) (xobjInfo xobj) (xobjTy xobj)))
              else pure (Right xobj)
      | otherwise -> pure (Right xobj)
    _ -> pure (Right xobj)
visitSymbol _ _ _ _ x = pure (Left (CannotConcretize x))

-- | Concretely type a context-dependent multi-symbol.
visitMultiSym :: [SymPath] -> Bool -> TypeEnv -> Env -> XObj -> State [XObj] (Either TypeError XObj)
visitMultiSym visited allowAmbig tenv env xobj@(MultiSymPat name paths) =
  case (filter (matchingSignature3 actualType) tysPathsModes) of
    [] -> pure (Left (NoMatchingSignature xobj name actualType tysToPathsDict))
    [x] -> go x
    _ -> pure (Right xobj)
  where
    Just actualType = xobjTy xobj
    tys = map (typeFromPath env) paths
    modes = map (modeFromPath env) paths
    tysToPathsDict = zip tys paths
    tysPathsModes = zip3 tys paths modes
    fake1 = XObj (Sym (SymPath [] "theType") Symbol) Nothing Nothing
    fake2 = XObj (Sym (SymPath [] "xobjType") Symbol) Nothing Nothing
    go :: (Ty, SymPath, SymbolMode) -> State [XObj] (Either TypeError XObj)
    go (ty, path, mode) =
      either
        (pure . convertError)
        (visitSymbol visited allowAmbig tenv env)
        ( solve [Constraint ty actualType fake1 fake2 fake1 OrdMultiSym]
            >>= pure . (flip replaceTyVars) actualType
            >>= pure . suffixTyVars ("_x" ++ show (infoIdentifier (fromMaybe dummyInfo (xobjInfo xobj))))
            >>= \t' -> pure (XObj (Sym path mode) (xobjInfo xobj) (Just t'))
        )
    convertError :: UnificationFailure -> Either TypeError XObj
    convertError failure@(UnificationFailure _ _) =
      Left (UnificationFailed (unificationFailure failure) (unificationMappings failure) [])
    convertError (Holes holes) = Left (HolesFound holes)
visitMultiSym _ _ _ _ x = pure (Left (CannotConcretize x))

-- | Concretely type an interface symbol.
visitInterfaceSym :: [SymPath] -> Bool -> TypeEnv -> Env -> XObj -> State [XObj] (Either TypeError XObj)
visitInterfaceSym visited allowAmbig tenv env xobj@(InterfaceSymPat name) =
  either (pure . const (Left (CannotConcretize xobj))) go (getTypeBinder tenv name)
  where
    Just actualType = (xobjTy xobj)
    go :: Binder -> State [XObj] (Either TypeError XObj)
    go (Binder _ (ListPat (InterfacePat _ paths))) =
      let tys = map (typeFromPath env) paths
          tysToPathsDict = zip tys paths
       in case filter (matchingSignature actualType) tysToPathsDict of
            [] -> pure $ if allowAmbig then Right xobj else Left (NoMatchingSignature xobj name actualType tysToPathsDict)
            [x] -> updateSym x
            xs -> case filter (typeEqIgnoreLifetimes actualType . fst) xs of
              [] -> pure (Right xobj) -- No exact match of types
              [y] -> updateSym y
              ps -> pure (Left (SeveralExactMatches xobj name actualType ps))
    go _ = pure (Left (CannotConcretize xobj))
    -- TODO: Should we also check for allowAmbig here?
    updateSym (_, path) = if isTypeGeneric actualType then pure (Right xobj) else replace path
    replace path =
      -- We pass the original xobj ty here, should we be passing the type found via matching signature?
      let normalSymbol = XObj (Sym path (LookupGlobal CarpLand AFunction)) (xobjInfo xobj) (xobjTy xobj) -- TODO: Is it surely AFunction here? Could be AVariable as well...!?
       in visitSymbol
            visited
            allowAmbig
            tenv
            env -- trace ("Replacing symbol " ++ pretty xobj ++ " with type " ++ show theType ++ " to single path " ++ show singlePath)
            normalSymbol
visitInterfaceSym _ _ _ _ x = pure (Left (CannotConcretize x))

toGeneralSymbol :: XObj -> XObj
toGeneralSymbol (XObj (Sym path _) _ t) = XObj (Sym path Symbol) (Just dummyInfo) t
toGeneralSymbol x = error ("Can't convert this to a general symbol: " ++ show x)

-- | Find all lookups in a lambda body that should be captured by its environment
collectCapturedVars :: XObj -> [XObj]
collectCapturedVars root = removeDuplicates (map decreaseCaptureLevel (visit' root))
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
                    else LookupLocal (Capture (n - 1))
                _ -> error "decreasecapturelevel1"
            )
        )
        (Just dummyInfo)
        ty
    decreaseCaptureLevel _ = error "decreasecapturelevel"
    visit' xobj =
      case xobjObj xobj of
        -- don't peek inside lambdas, trust their capture lists:
        (Lst [XObj (Fn _ captures) _ _, _, _]) -> Set.toList captures
        -- in the case of lets, we have to remove new bindings from the list of captured variables,
        -- including the ones captured in later bindings
        (Lst [XObj Let _ _, XObj (Arr bindings) _ _, body]) ->
          let (bound, bindingsCaptured) =
                foldl'
                  ( \(bound', captured) (XObj sym _ ty, expr) ->
                      let capt = filter (`Set.notMember` bound') (visit' expr)
                       in (Set.insert (XObj sym (Just dummyInfo) ty) bound', capt ++ captured)
                  )
                  (Set.empty, [])
                  (pairwise bindings)
           in let bodyCaptured = filter (`Set.notMember` bound) (visit' body)
               in bindingsCaptured ++ bodyCaptured
        (Lst _) -> visitList' xobj
        (Arr _) -> visitArray' xobj
        -- TODO: Static Arrays!
        sym@(Sym _ (LookupLocal (Capture _))) -> [XObj sym (Just dummyInfo) (xobjTy xobj)]
        _ -> []
    visitList' :: XObj -> [XObj]
    visitList' (XObj (Lst xobjs) _ _) =
      concatMap visit' xobjs
    visitList' _ = error "The function 'visitList' only accepts XObjs with lists in them."
    visitArray' :: XObj -> [XObj]
    visitArray' (XObj (Arr xobjs) _ _) =
      concatMap visit' xobjs
    visitArray' _ = error "The function 'visitArray' only accepts XObjs with arrays in them."

-- | Do the signatures match?
matchingSignature :: Ty -> (Ty, SymPath) -> Bool
matchingSignature tA (tB, _) = areUnifiable tA tB

-- | Do the signatures match (tuple arity 3 version)?
matchingSignature3 :: Ty -> (Ty, SymPath, SymbolMode) -> Bool
matchingSignature3 tA (tB, _, _) = areUnifiable tA tB

--------------------------------------------------------------------------------
-- Type concretization
--
-- These functions perform the actual work of converting generic types to concrete types.

-- | Does the type of an XObj require additional concretization of generic types or some typedefs for function types, etc?
-- | If so, perform the concretization and append the results to the list of dependencies.
concretizeTypeOfXObj :: TypeEnv -> Env -> XObj -> State [XObj] (Either TypeError ())
concretizeTypeOfXObj typeEnv env (XObj _ _ (Just ty)) =
  either (pure . Left) success (concretizeType typeEnv env ty)
  where
    success :: [XObj] -> State [XObj] (Either TypeError ())
    success xs = modify (xs ++) >> pure (Right ())
concretizeTypeOfXObj _ _ _ = pure (Right ())

-- | Find all the concrete deps of a type.
concretizeType :: TypeEnv -> Env -> Ty -> Either TypeError [XObj]
concretizeType _ _ ft@FuncTy {} =
  if isTypeGeneric ft
    then Right []
    else Right [defineFunctionTypeAlias ft]
concretizeType typeEnv env arrayTy@(StructTy (ConcreteNameTy (SymPath [] "Array")) varTys) =
  if isTypeGeneric arrayTy
    then Right []
    else do
      deps <- mapM (concretizeType typeEnv env) varTys
      Right (defineArrayTypeAlias arrayTy : concat deps)
-- TODO: Remove ugly duplication of code here:
concretizeType typeEnv env arrayTy@(StructTy (ConcreteNameTy (SymPath [] "StaticArray")) varTys) =
  if isTypeGeneric arrayTy
    then Right []
    else do
      deps <- mapM (concretizeType typeEnv env) varTys
      Right (defineStaticArrayTypeAlias arrayTy : concat deps)
concretizeType typeEnv env genericStructTy@(StructTy (ConcreteNameTy path@(SymPath _ name)) _) =
  case (getTypeBinder typeEnv name) <> (findTypeBinder env path) of
    Right (Binder _ x) -> go x
    _ -> Right []
  where
    go :: XObj -> Either TypeError [XObj]
    go (XObj (Lst (XObj (Deftype originalStructTy) _ _ : _ : rest)) _ _) =
      if isTypeGeneric originalStructTy
        then instantiateGenericStructType typeEnv env originalStructTy genericStructTy rest
        else Right []
    go (XObj (Lst (XObj (DefSumtype originalStructTy) _ _ : _ : rest)) _ _) =
      if isTypeGeneric originalStructTy
        then instantiateGenericSumtype typeEnv env originalStructTy genericStructTy rest
        else Right []
    go (XObj (Lst (XObj (ExternalType _) _ _ : _)) _ _) = Right []
    go x = error ("Non-deftype found in type env: " ++ pretty x)
concretizeType t e (RefTy rt _) =
  concretizeType t e rt
concretizeType t e (PointerTy pt) =
  concretizeType t e pt
concretizeType _ _ _ =
  Right [] -- ignore all other types

-- | Renames the type variable literals in a sum type for temporary validation.
renameGenericTypeSymbolsOnSum :: [(Ty, Ty)] -> XObj -> XObj
renameGenericTypeSymbolsOnSum varpairs x@(XObj (Lst (caseNm : [a@(XObj (Arr arr) _ _)])) _ _) =
  setObj x (Lst [caseNm, setObj a (Arr (map replacer arr))])
  where
    --XObj (Lst (caseNm : [XObj (Arr (map replacer arr)) ii tt])) i t

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
--
-- Turns (deftype (A a) [x a, y a]) into (deftype (A Int) [x Int, y Int])
instantiateGenericStructType :: TypeEnv -> Env -> Ty -> Ty -> [XObj] -> Either TypeError [XObj]
instantiateGenericStructType typeEnv env originalStructTy@(StructTy _ _) genericStructTy membersXObjs =
  (replaceLeft (FailedToInstantiateGenericType originalStructTy) solution >>= go)
  where
    fake1 = XObj (Sym (SymPath [] "a") Symbol) Nothing Nothing
    fake2 = XObj (Sym (SymPath [] "b") Symbol) Nothing Nothing
    XObj (Arr memberXObjs) _ _ = head membersXObjs
    rename@(StructTy _ renamedOrig) = evalState (renameVarTys originalStructTy) 0
    solution = solve [Constraint originalStructTy genericStructTy fake1 fake2 fake1 OrdMultiSym]
    go mappings = do
      mappings' <- replaceLeft (FailedToInstantiateGenericType originalStructTy) (solve [Constraint rename genericStructTy fake1 fake2 fake1 OrdMultiSym])
      let nameFixedMembers = renameGenericTypeSymbolsOnProduct renamedOrig memberXObjs
          validMembers = replaceGenericTypeSymbolsOnMembers mappings' nameFixedMembers
          concretelyTypedMembers = replaceGenericTypeSymbolsOnMembers mappings memberXObjs
          sname = getStructName originalStructTy
      candidate <- TC.mkStructCandidate sname renamedOrig typeEnv env validMembers (getPathFromStructName sname)
      validateType (TC.setRestriction candidate TC.AllowAny)
      deps <- mapM (depsForStructMemberPair typeEnv env) (pairwise concretelyTypedMembers)
      let xobj =
            XObj
              ( Lst
                  ( XObj (Deftype genericStructTy) Nothing Nothing :
                    XObj (Sym (SymPath [] (tyToC genericStructTy)) Symbol) Nothing Nothing :
                    [XObj (Arr concretelyTypedMembers) Nothing Nothing]
                  )
              )
              (Just dummyInfo)
              (Just TypeTy) :
            concat deps
      pure xobj
instantiateGenericStructType _ _ t _ _ = Left (FailedToInstantiateGenericType t)

depsForStructMemberPair :: TypeEnv -> Env -> (XObj, XObj) -> Either TypeError [XObj]
depsForStructMemberPair typeEnv env (_, tyXObj) =
  maybe (Left (NotAType tyXObj)) (concretizeType typeEnv env) (xobjToTy tyXObj)

-- | Given an generic sumtype and a concrete version of it, generate all dependencies needed to use the concrete one.
--
-- Turn (deftype (Maybe a) (Just a) (Nothing)) into (deftype (Maybe Int) (Just Int) (Nothing))
instantiateGenericSumtype :: TypeEnv -> Env -> Ty -> Ty -> [XObj] -> Either TypeError [XObj]
instantiateGenericSumtype typeEnv env originalStructTy@(StructTy _ originalTyVars) genericStructTy cases =
  let fake1 = XObj (Sym (SymPath [] "a") Symbol) Nothing Nothing
      fake2 = XObj (Sym (SymPath [] "b") Symbol) Nothing Nothing
      rename@(StructTy _ renamedOrig) = evalState (renameVarTys originalStructTy) 0
      nameFixedCases = map (renameGenericTypeSymbolsOnSum (zip originalTyVars renamedOrig)) cases
      fixLeft l = replaceLeft (FailedToInstantiateGenericType originalStructTy) l
   in do
        mappings <- fixLeft $ solve [Constraint rename genericStructTy fake1 fake2 fake1 OrdMultiSym]
        let concretelyTypedCases = map (replaceGenericTypeSymbolsOnCase mappings) nameFixedCases
            sname = (getStructName originalStructTy)
        deps <- mapM (depsForCase typeEnv env) concretelyTypedCases
        candidate <- TC.mkSumtypeCandidate sname renamedOrig typeEnv env concretelyTypedCases (getPathFromStructName sname)
        validateType (TC.setRestriction candidate TC.AllowAny)
        pure
          ( XObj
              ( Lst
                  ( XObj (DefSumtype genericStructTy) Nothing Nothing :
                    XObj (Sym (SymPath [] (tyToC genericStructTy)) Symbol) Nothing Nothing :
                    concretelyTypedCases
                  )
              )
              (Just dummyInfo)
              (Just TypeTy) :
            concat deps
          )
instantiateGenericSumtype _ _ _ _ _ = error "instantiategenericsumtype"

-- Resolves dependencies for sumtype cases.
-- NOTE: This function only accepts cases that are in "canonical form"
-- (Just [x]) aka XObj (Lst (Sym...) (Arr members))
-- On other cases it will return an error.
depsForCase :: TypeEnv -> Env -> XObj -> Either TypeError [XObj]
depsForCase typeEnv env (XObj (Lst [_, XObj (Arr members) _ _]) _ _) =
  concat
    <$> mapM
      (\t -> maybe (Left (NotAType t)) (concretizeType typeEnv env) (xobjToTy t))
      members
depsForCase _ _ x = Left (InvalidSumtypeCase x)

-- | Replace instances of generic types in type candidate field definitions.
replaceGenericTypeSymbolsOnFields :: Map.Map String Ty -> [TC.TypeField] -> [TC.TypeField]
replaceGenericTypeSymbolsOnFields ms fields = map go fields
  where
    go (TC.StructField name t) = (TC.StructField name (replaceTyVars ms t))
    go (TC.SumField name ts) = (TC.SumField name (map (replaceTyVars ms) ts))

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
                      (managed, memDepsTys) <- manageMemory typeEnv globalEnv concrete
                      let memDeps = depsForDeleteFuncs typeEnv globalEnv memDepsTys
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
                      (managed, memDepsTys) <- manageMemory typeEnv globalEnv concrete
                      let memDeps = depsForDeleteFuncs typeEnv globalEnv memDepsTys
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

-- | Helper for finding the 'delete' functions for several types.
depsForDeleteFuncs :: TypeEnv -> Env -> Set.Set Ty -> [XObj]
depsForDeleteFuncs typeEnv env tys = concatMap (depsForDeleteFunc typeEnv env) (Set.toList tys)

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

suffixTyVars :: String -> Ty -> Ty
suffixTyVars suffix t =
  case t of
    VarTy key -> VarTy (key ++ suffix)
    FuncTy argTys retTy ltTy -> FuncTy (map (suffixTyVars suffix) argTys) (suffixTyVars suffix retTy) (suffixTyVars suffix ltTy)
    StructTy name tyArgs -> StructTy name (fmap (suffixTyVars suffix) tyArgs)
    PointerTy x -> PointerTy (suffixTyVars suffix x)
    RefTy x lt -> RefTy (suffixTyVars suffix x) (suffixTyVars suffix lt)
    _ -> t

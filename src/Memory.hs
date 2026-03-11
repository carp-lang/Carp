{-# LANGUAGE LambdaCase #-}

module Memory (manageMemory) where

import Control.Monad.State
import Data.Maybe (fromMaybe)
import Forms
import Info
import Managed
import qualified Map
import Obj
import Polymorphism
import Set ((\\))
import qualified Set
import TypeError
import Types
import Util
import Prelude hiding (lookup)

-- | To keep track of the deleters when recursively walking the form.
-- | To avoid having to concretize the deleters here, they are just stored as their Ty in `memStateDeps`.
data MemState = MemState
  { memStateDeleters :: Set.Set Deleter,
    memStateDeps :: Set.Set Ty,
    memStateLifetimes :: Map.Map String LifetimeMode,
    memStateLocalRefSources :: Map.Map String [(String, XObj)],
    memStateParamDeleters :: Set.Set Deleter
  }
  deriving (Show)

-- | Differentiate between lifetimes depending on variables in a lexical scope and depending on something outside the function.
data LifetimeMode
  = LifetimeInsideFunction (Set.Set String)
  | LifetimeOutsideFunction
  deriving (Show)

-- | Find out what deleters are needed and where in an XObj.
-- | Deleters will be added to the info field on XObj so that
-- | the code emitter can access them and insert calls to destructors.
manageMemory :: TypeEnv -> Env -> XObj -> Either TypeError (XObj, Set.Set Ty)
manageMemory typeEnv globalEnv root =
  let (finalObj, finalState) = runState (visit root) (MemState Set.empty Set.empty Map.empty Map.empty Set.empty)
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
              case evalState (refTargetIsAlive ok) (MemState Set.empty Set.empty (memStateLifetimes finalState) Map.empty Set.empty) of
                Left err -> Left err
                Right _ ->
                  -- Check local ref sources: when a lifetime variable was
                  -- mapped to LifetimeOutsideFunction (from a function arg)
                  -- but a (ref local) also uses that lifetime (via explicit
                  -- sig annotation), the local must be alive. Function params
                  -- are alive for the whole function, but let-bound locals
                  -- are not.
                  case checkLocalRefSources (xobjTy ok) (memStateParamDeleters finalState) (memStateLocalRefSources finalState) of
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
            manage typeEnv globalEnv xobj
            addToLifetimesMappingsIfRef False xobj -- TODO: Should "internal = True" here? TODO: Possible to remove this one?
            pure (Right xobj)
          Pattern _ -> do
            manage typeEnv globalEnv xobj
            addToLifetimesMappingsIfRef False xobj -- TODO: Also possible to remove, *should* be superseeded by (***) below?
            pure (Right xobj)
          _ ->
            pure (Right xobj)
        case r of
          Right ok -> do
            MemState {} <- get
            r' <- refTargetIsAlive ok -- trace ("CHECKING " ++ pretty ok ++ " : " ++ showMaybeTy (ty xobj) ++ ", mappings: " ++ prettyLifetimeMappings m) $
            addToLifetimesMappingsIfRef True ok -- (***)
            pure r'
          Left err -> pure (Left err)
    visitArray :: XObj -> State MemState (Either TypeError XObj)
    visitArray xobj@(ArrPat arr) =
      do
        mapM_ visit arr
        results <- mapM (unmanage typeEnv globalEnv) arr
        whenRight (sequence results) $
          do
            _ <- manage typeEnv globalEnv xobj -- TODO: result is discarded here, is that OK?
            pure (Right xobj)
    visitArray _ = error "Must visit array."
    visitStaticArray :: XObj -> State MemState (Either TypeError XObj)
    visitStaticArray xobj@(StaticArrPat arr) =
      do
        mapM_ visit arr
        results <- mapM (unmanage typeEnv globalEnv) arr
        whenRight (sequence results) $ do
          -- We know that we want to add a deleter for the static array here
          let var = varOfXObj xobj
              t = case xobjTy xobj of
                Just (RefTy (StructTy (ConcreteNameTy (SymPath [] "StaticArray")) [vs]) _) -> (StructTy (ConcreteNameTy (SymPath [] "StaticArray")) [vs])
                _ -> error "memory: can't visit static array of non static array type"
              deleter = case nameOfPolymorphicFunction typeEnv globalEnv (FuncTy [t] UnitTy StaticLifetimeTy) "delete" of
                Just pathOfDeleteFunc ->
                  ProperDeleter pathOfDeleteFunc (getDropFunc typeEnv globalEnv (xobjInfo xobj) t) var
                Nothing ->
                  error ("No deleter found for Static Array : " ++ show t) --Just (FakeDeleter var)
          m <- get
          let newDeleters = Set.insert deleter (memStateDeleters m)
              newDeps = Set.insert t (memStateDeps m)
          put $ m {memStateDeleters = newDeleters, memStateDeps = newDeps}
          pure (Right xobj)
    visitStaticArray _ = error "Must visit static array."
    visitList :: XObj -> State MemState (Either TypeError XObj)
    visitList xobj@(XObj (Lst lst) i t) =
      case lst of
        [defn@(XObj (Defn maybeCaptures) _ _), nameSymbol@(XObj (Sym _ _) _ _), args@(XObj (Arr argList) _ _), body] ->
          let captures = maybe [] Set.toList maybeCaptures
           in do
                mapM_ (manage typeEnv globalEnv) argList
                -- Add the captured variables (if any, only happens in lifted lambdas) as fake deleters
                -- TODO: Use another kind of Deleter for this case since it's pretty special?
                mapM_
                  ( ( \cap ->
                        modify
                          ( \memState ->
                              memState {memStateDeleters = Set.insert (FakeDeleter cap) (memStateDeleters memState)}
                          )
                    )
                      . getName
                  )
                  captures
                mapM_ (addToLifetimesMappingsIfRef False) argList
                mapM_ (addToLifetimesMappingsIfRef False) captures -- For captured variables inside of lifted lambdas
                -- Record the param deleters so the final check can
                -- distinguish function params from let-bound locals.
                paramDels <- gets memStateDeleters
                modify (\m -> m {memStateParamDeleters = paramDels})
                visitedBody <- visit body
                result <- unmanage typeEnv globalEnv body
                whenRightReturn result $
                  do
                    okBody <- visitedBody
                    Right (XObj (Lst [defn, nameSymbol, args, okBody]) i t)

        -- Fn / λ (Lambda)
        [fn@(XObj (Fn _ captures) _ _), args@(XObj (Arr _) _ _), body] ->
          do
            manage typeEnv globalEnv xobj -- manage inner lambdas but leave their bodies unvisited, they will be visited in the lifted version...
            mapM_ (unmanage typeEnv globalEnv) captures
            pure (Right (XObj (Lst [fn, args, body]) i t))

        -- Def
        DefPat def nameSymbol expr ->
          do
            visitedExpr <- visit expr
            result <- unmanage typeEnv globalEnv expr
            whenRightReturn result $
              do
                okExpr <- visitedExpr
                Right (XObj (Lst [def, nameSymbol, okExpr]) i t)
        -- Let
        LetPat letExpr (XObj (Arr bindings) bindi bindt) body ->
          do
            preDeleters <- gets memStateDeleters
            visitedBindings <- mapM visitLetBinding (pairwise bindings)
            visitedBody <- visit body
            result <- unmanage typeEnv globalEnv body
            whenRight result $
              do
                postDeleters <- gets memStateDeleters
                let diff = postDeleters Set.\\ preDeleters
                    newInfo = setDeletersOnInfo i diff
                    survivors = postDeleters Set.\\ diff -- Same as just pre deleters, right?!
                modify (\m -> m {memStateDeleters = survivors})
                --trace ("LET Pre: " ++ show preDeleters ++ "\nPost: " ++ show postDeleters ++ "\nDiff: " ++ show diff ++ "\nSurvivors: " ++ show survivors)
                manage typeEnv globalEnv xobj
                pure $ do
                  okBody <- visitedBody
                  let finalBody = searchForInnerBreak diff okBody
                  okBindings <- fmap (concatMap (\(n, x) -> [n, x])) (sequence visitedBindings)
                  pure (XObj (Lst [letExpr, XObj (Arr okBindings) bindi bindt, finalBody]) newInfo t)

        -- Set!
        SetPat setbangExpr variable value ->
          let varInfo = xobjInfo variable
              correctVariableAndMode =
                case variable of
                  symObj@(XObj (Sym _ mode) _ _) -> Right (symObj, mode)
                  anythingElse -> Left (CannotSet anythingElse)
           in case correctVariableAndMode of
                Left err ->
                  pure (Left err)
                Right (okCorrectVariable, okMode) ->
                  do
                    preDeleters <- gets memStateDeleters
                    let ownsTheVarBefore = case createDeleter typeEnv globalEnv okCorrectVariable of
                          Nothing -> Right ()
                          Just d ->
                            if Set.member d preDeleters || isLookupGlobal okMode
                              then Right ()
                              else Left (UsingUnownedValue variable)
                    -- Clear the lifetime mapping so the value's ref sources
                    -- establish the new mapping via addToLifetimesMappingsIfRef
                    clearLifetimeMapping variable
                    visitedValue <- visit value
                    _ <- unmanage typeEnv globalEnv value -- The assigned value can't be used anymore
                    managed <- gets memStateDeleters
                    -- Delete the value previously stored in the variable, if it's still alive
                    let deleters = case createDeleter typeEnv globalEnv okCorrectVariable of
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
                            _ -> error "memory: unexpected variable mode in set!"
                    case okMode of
                      Symbol -> error "Should only be a global/local lookup symbol."
                      LookupLocal _ -> manage typeEnv globalEnv okCorrectVariable
                      LookupGlobal _ _ -> pure ()
                      _ -> error "memory: unexpected ok mode in set!"
                    pure $ case okMode of
                      LookupLocal (Capture _) ->
                        Left (CannotSetVariableFromLambda variable setbangExpr)
                      _ ->
                        do
                          okValue <- visitedValue
                          _ <- ownsTheVarBefore -- Force Either to fail
                          pure (XObj (Lst [setbangExpr, newVariable, okValue]) i t)

        -- The
        ThePat theExpr typeXObj value ->
          do
            visitedValue <- visit value
            result <- transferOwnership typeEnv globalEnv value xobj
            whenRightReturn result $
              do
                okValue <- visitedValue
                Right (XObj (Lst [theExpr, typeXObj, okValue]) i t)

        -- Ref
        RefPat refExpr value ->
          do
            visited <- visit value
            case visited of
              Left e -> pure (Left e)
              Right visitedValue ->
                do
                  result <- canBeReferenced typeEnv globalEnv visitedValue
                  whenRightReturn result $ do
                    Right (XObj (Lst [refExpr, visitedValue]) i t)

        -- Deref
        (XObj Deref _ _ : _) ->
          error "Shouldn't end up here, deref only works when calling a function, i.e. ((deref f) 1 2 3)."
        -- Do
        DoPat doExpr expressions ->
          do
            visitedExpressions <- mapM visit expressions
            result <- transferOwnership typeEnv globalEnv (last expressions) xobj
            whenRightReturn result $ do
              okExpressions <- sequence visitedExpressions
              Right (XObj (Lst (doExpr : okExpressions)) i t)

        -- While
        WhilePat whileExpr expr body ->
          do
            preDeleters <- gets memStateDeleters
            visitedExpr <- visit expr
            afterExprDeleters <- gets memStateDeleters
            visitedBody <- visit body
            manage typeEnv globalEnv body
            postDeleters <- gets memStateDeleters
            -- Visit an extra time to simulate repeated use
            visitedExpr2 <- visit expr
            visitedBody2 <- visit body
            let diff = postDeleters \\ preDeleters
            modify (\m -> m {memStateDeleters = postDeleters \\ diff})
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
                  finalBody = searchForInnerBreak diff okBody
              pure (XObj (Lst [whileExpr, newExpr, finalBody]) newInfo t)

        -- If
        IfPat ifExpr expr ifTrue ifFalse ->
          do
            visitedExpr <- visit expr
            preState <- get
            let preDeleters = memStateDeleters preState
                deps = memStateDeps preState
                (visitedTrue, stillAliveTrue) =
                  runState
                    ( do
                        v <- visit ifTrue
                        result <- transferOwnership typeEnv globalEnv ifTrue xobj
                        pure $ case result of
                          Left e -> error (show e)
                          Right () -> v
                    )
                    preState
                (visitedFalse, stillAliveFalse) =
                  runState
                    ( do
                        v <- visit ifFalse
                        result <- transferOwnership typeEnv globalEnv ifFalse xobj
                        pure $ case result of
                          Left e -> error (show e)
                          Right () -> v
                    )
                    preState
            let deletedInTrue = preDeleters \\ memStateDeleters stillAliveTrue
                deletedInFalse = preDeleters \\ memStateDeleters stillAliveFalse
                deletedInBoth = Set.intersection deletedInTrue deletedInFalse
                createdInTrue = memStateDeleters stillAliveTrue \\ preDeleters
                createdInFalse = memStateDeleters stillAliveFalse \\ preDeleters
                selfDeleter = case createDeleter typeEnv globalEnv xobj of
                  Just ok -> Set.fromList [ok]
                  Nothing -> Set.empty
                createdAndDeletedInTrue = createdInTrue \\ selfDeleter
                createdAndDeletedInFalse = createdInFalse \\ selfDeleter
                delsTrue = Set.union (deletedInFalse \\ deletedInBoth) createdAndDeletedInTrue
                delsFalse = Set.union (deletedInTrue \\ deletedInBoth) createdAndDeletedInFalse
                stillAliveAfter = preDeleters \\ Set.union deletedInTrue deletedInFalse
                -- Note: The following line merges all previous deps and the new ones, could be optimized?
                depsAfter = Set.unions [memStateDeps stillAliveTrue, memStateDeps stillAliveFalse, deps]
            put $ preState {memStateDeleters = stillAliveAfter, memStateDeps = depsAfter}
            manage typeEnv globalEnv xobj
            pure $ do
              okExpr <- visitedExpr
              okTrue <- visitedTrue
              okFalse <- visitedFalse
              pure (XObj (Lst [ifExpr, okExpr, setDeletersOnXObj okTrue delsTrue, setDeletersOnXObj okFalse delsFalse]) i t)

        -- Match
        -- The general idea of how to figure out what to delete in a 'match' statement:
        -- 1. Visit each case and investigate which variables are deleted in each one of the cases
        -- 2. Variables deleted in at least one case has to be deleted in all, so make a union U of all such vars
        --    but remove the ones that were not present before the 'match'
        -- 3. In each case - take the intersection of U and the vars deleted in that case and add this result to its deleters
        matchExpr@(XObj (Match matchMode) _ _) : expr : cases ->
          do
            visitedExpr <- visit expr
            case visitedExpr of
              Left e -> pure (Left e)
              Right okVisitedExpr ->
                do
                  _ <- unmanage typeEnv globalEnv okVisitedExpr
                  preDeleters <- gets memStateDeleters
                  vistedCasesAndDeps <- mapM (visitMatchCase matchMode) (pairwise cases)
                  case sequence vistedCasesAndDeps of
                    Left e -> pure (Left e)
                    Right okCasesAndDeps ->
                      let visitedCases = map fst okCasesAndDeps
                          depsFromCases = Set.unions (map snd okCasesAndDeps)
                          (finalXObj, postDeleters) = analyzeFinal okVisitedExpr visitedCases preDeleters
                       in do
                            modify (\m -> m {memStateDeleters = postDeleters, memStateDeps = Set.union (memStateDeps m) depsFromCases})
                            manage typeEnv globalEnv xobj
                            pure (Right finalXObj)
          where
            analyzeFinal :: XObj -> [(Set.Set Deleter, (XObj, XObj))] -> Set.Set Deleter -> (XObj, Set.Set Deleter)
            analyzeFinal okVisitedExpr visitedCasesWithDeleters preDeleters =
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
               in ( XObj (Lst ([matchExpr, okVisitedExpr] ++ concat okVisitedCasesWithAllDeleters)) i t,
                    deletersAfterTheMatch
                  )
        -- Deref (only works in function application)
        XObj (Lst [deref@(XObj Deref _ _), f]) xi xt : uargs ->
          do
            -- Do not visit f in this case, we don't want to manage it's memory since it is a ref!
            visitedArgs <- sequence <$> mapM visitArg uargs
            case visitedArgs of
              Left err -> pure (Left err)
              Right args ->
                do
                  unmanagedArgs <- sequence <$> mapM unmanageArg args
                  manage typeEnv globalEnv xobj
                  pure $ do
                    okArgs <- unmanagedArgs
                    Right (XObj (Lst (XObj (Lst [deref, f]) xi xt : okArgs)) i t)

        -- Function application
        f : uargs ->
          do
            visitedF <- visit f
            visitedArgs <- sequence <$> mapM visitArg uargs
            case visitedArgs of
              Left err -> pure (Left err)
              Right args -> do
                unmanagedArgs <- sequence <$> mapM unmanageArg args
                manage typeEnv globalEnv xobj
                pure $ do
                  okF <- visitedF
                  okArgs <- unmanagedArgs
                  Right (XObj (Lst (okF : okArgs)) i t)
        [] -> pure (Right xobj)
    visitList _ = error "Must visit list."
    visitMatchCase :: MatchMode -> (XObj, XObj) -> State MemState (Either TypeError ((Set.Set Deleter, (XObj, XObj)), Set.Set Ty))
    visitMatchCase matchMode (lhs@XObj {}, rhs@XObj {}) =
      do
        preDeleters <- gets memStateDeleters
        _ <- visitCaseLhs matchMode lhs
        visitedRhs <- visit rhs
        _ <- unmanage typeEnv globalEnv rhs
        postDeleters <- gets memStateDeleters
        postDeps <- gets memStateDeps
        modify (\m -> m {memStateDeleters = preDeleters}) -- Restore managed variables, TODO: Use a "local" state monad instead?
        pure $ do
          okVisitedRhs <- visitedRhs
          pure ((postDeleters, (lhs, okVisitedRhs)), postDeps)
    visitCaseLhs :: MatchMode -> XObj -> State MemState (Either TypeError [()])
    visitCaseLhs matchMode (XObj (Lst vars) _ _) =
      do
        result <- mapM (visitCaseLhs matchMode) vars
        let result' = sequence result
        pure (fmap concat result')
    visitCaseLhs matchMode xobj@(XObj (Sym (SymPath _ name) _) _ _)
      | (matchMode == MatchValue) && isVarName name = do
        manage typeEnv globalEnv xobj
        pure (Right [])
      | otherwise = pure (Right [])
    visitCaseLhs _ (XObj Ref _ _) =
      pure (Right [])
    visitCaseLhs _ x =
      error ("Unhandled: " ++ show x)
    visitLetBinding :: (XObj, XObj) -> State MemState (Either TypeError (XObj, XObj))
    visitLetBinding (name, expr) =
      do
        visitedExpr <- visit expr
        addToLifetimesMappingsIfRef True expr
        -- ensures this deleter is the only deleter associated with name for the duration of the let scope (shadowing).
        result <- exclusiveTransferOwnership typeEnv globalEnv expr name
        whenRightReturn result $ do
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
          r <- unmanage typeEnv globalEnv xobj
          pure $ case r of
            Left err -> Left err
            Right () -> Right xobj
        else pure (Right xobj)
    unmanageArg xobj@XObj {} =
      pure (Right xobj)

--------------------------------------------------------------------------------
-- The basic primitives of memory management

-- | Add `xobj` to the set of alive variables, in need of deletion at end of scope.
manage :: TypeEnv -> Env -> XObj -> State MemState ()
manage typeEnv globalEnv xobj =
  if isSymbolThatCaptures xobj -- When visiting lifted lambdas, don't manage symbols that capture (they are owned by the environment).
    then pure ()
    else case createDeleter typeEnv globalEnv xobj of
      Just deleter -> do
        m <- get
        let newDeleters = Set.insert deleter (memStateDeleters m)
            t = fromMaybe (error "memory: can't manage xobj without type") $ xobjTy xobj
            newDeps = Set.insert t (memStateDeps m)
        put $ m {memStateDeleters = newDeleters, memStateDeps = newDeps}
      Nothing -> pure ()

-- | Remove `xobj` from the set of alive variables, in need of deletion at end of scope.
unmanage :: TypeEnv -> Env -> XObj -> State MemState (Either TypeError ())
unmanage typeEnv globalEnv xobj =
  let t = fromMaybe (error "memory: can't unmanage xobj without type") $ xobjTy xobj
   in if isManaged typeEnv globalEnv t && not (isGlobalFunc xobj)
        then do
          m <- get
          case deletersMatchingXObj xobj (memStateDeleters m) of
            [] ->
              pure $
                if isSymbolThatCaptures xobj
                  then Left (UsingCapturedValue xobj)
                  else Left (UsingUnownedValue xobj)
            [one] ->
              let newDeleters = Set.delete one (memStateDeleters m)
               in do
                    put $ m {memStateDeleters = newDeleters}
                    pure (Right ())
            tooMany -> error ("Too many variables with the same name in set: " ++ show tooMany)
        else pure (Right ())

-- | A combination of `manage` and `unmanage`.
transferOwnership :: TypeEnv -> Env -> XObj -> XObj -> State MemState (Either TypeError ())
transferOwnership typeEnv globalEnv from to =
  do
    result <- unmanage typeEnv globalEnv from
    whenRight result $ do
      manage typeEnv globalEnv to --(trace ("Transfered from " ++ getName from ++ " '" ++ varOfXObj from ++ "' to " ++ getName to ++ " '" ++ varOfXObj to ++ "'") to)
      pure (Right ())

-- | Transfer ownership, and ensure the resulting set has only *one* deleter per variable.
--
-- Multiple deleters can arise when one variable shadows an earlier one from a wider scope.
-- see issue #597
exclusiveTransferOwnership :: TypeEnv -> Env -> XObj -> XObj -> State MemState (Either TypeError ())
exclusiveTransferOwnership tenv genv from to =
  do
    result <- unmanage tenv genv from
    whenRight result $ do
      m <- get
      let pre = memStateDeleters m
      put $ m {memStateDeleters = Set.empty} -- add just this new deleter to the set
      manage tenv genv to
      post <- gets memStateDeleters
      modify (\s -> s {memStateDeleters = uniqueDeleter post pre}) -- replace any duplicates and union with the prior set
      pure (Right ())

-- | Control that an `xobj` is OK to reference
canBeReferenced :: TypeEnv -> Env -> XObj -> State MemState (Either TypeError ())
canBeReferenced typeEnv globalEnv xobj =
  let t = fromMaybe (error "memory: xobj without type") $ xobjTy xobj
      isGlobalVariable = case xobj of
        XObj (Sym _ (LookupGlobal _ _)) _ _ -> True
        _ -> False
   in -- TODO: The 'isManaged typeEnv t' boolean check should be removed
      if not isGlobalVariable && not (isGlobalFunc xobj) && isManaged typeEnv globalEnv t && not (isSymbolThatCaptures xobj)
        then do
          deleters <- gets memStateDeleters
          pure $ case deletersMatchingXObj xobj deleters of
            [] -> Left (GettingReferenceToUnownedValue xobj)
            [_] -> pure ()
            _ -> error $ "Too many variables with the same name in set (was looking for " ++ pretty xobj ++ " at " ++ prettyInfoFromXObj xobj ++ ")"
        else pure (Right ())

-- | Makes sure that whatever a reference is refering too, is still alive (i.e. in the set of live Deleters)
refTargetIsAlive :: XObj -> State MemState (Either TypeError XObj)
refTargetIsAlive xobj =
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
        m <- get
        case Map.lookup lt (memStateLifetimes m) of
          Just (LifetimeInsideFunction deleterNames) ->
            let deadVars = Set.filter (not . isAlive (memStateDeleters m)) deleterNames
             in case Set.toList deadVars of
                  [] -> pure (Right xobj)
                  (deadName : _) ->
                    pure
                      ( case xobjObj xobj of
                          (Lst (LetPat _ _ body)) -> Left (UsingDeadReference body deadName)
                          _ -> Left (UsingDeadReference xobj deadName)
                      )
          Just LifetimeOutsideFunction ->
            pure (Right xobj)
          Nothing ->
            pure (Right xobj)
    isAlive :: Set.Set Deleter -> String -> Bool
    isAlive deleters name =
      any
        ( \case
            ProperDeleter {deleterVariable = dv} -> dv == name
            FakeDeleter {deleterVariable = dv} -> dv == name
            PrimDeleter {aliveVariable = dv} -> dv == name
            RefDeleter {refVariable = dv} -> dv == name
        )
        (Set.toList deleters)

-- | Map from lifetime variables (of refs) to a `LifetimeMode`
-- | (usually containing the name of the XObj that the lifetime is tied to).
addToLifetimesMappingsIfRef :: Bool -> XObj -> State MemState ()
addToLifetimesMappingsIfRef internal xobj =
  case xobjTy xobj of
    Just (RefTy _ (VarTy lt)) ->
      do
        m <- get
        let lifetimes = memStateLifetimes m
        case Map.lookup lt lifetimes of
          Just _ ->
            -- The lifetime is already mapped (e.g. from a function arg).
            -- If this xobj is a (ref target) node creating a new internal
            -- reference, record the target as an additional local ref source.
            -- This catches the soundness hole where an explicit lifetime
            -- annotation (sig) unifies an arg's lifetime with a local's,
            -- hiding the dangling ref from the main lifetime check.
            case xobj of
              XObj (Lst [XObj Ref _ _, target]) _ _ ->
                let targetVar = varOfXObj target
                    localRefs = memStateLocalRefSources m
                    existing = fromMaybe [] (Map.lookup lt localRefs)
                    localRefs' = Map.insert lt ((targetVar, xobj) : existing) localRefs
                 in put $ m {memStateLocalRefSources = localRefs'}
              _ -> pure ()
          Nothing ->
            do
              let lifetimes' = Map.insert lt makeLifetimeMode lifetimes
              put $ m {memStateLifetimes = lifetimes'}
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
        then
          LifetimeInsideFunction $
            Set.fromList
              [ case xobj of
                  XObj (Lst [XObj Ref _ _, target]) _ _ -> varOfXObj target
                  _ -> varOfXObj xobj
              ]
        else LifetimeOutsideFunction

-- | Clear the lifetime mapping for a ref variable's lifetime variable.
-- | Used before visiting a set! value so that addToLifetimesMappingsIfRef can
-- | rebuild the mapping from the new value's ref sources rather than keeping
-- | the stale mapping from the initial binding.
clearLifetimeMapping :: XObj -> State MemState ()
clearLifetimeMapping variable =
  case xobjTy variable of
    Just (RefTy _ (VarTy lt)) ->
      modify (\m -> m {memStateLifetimes = Map.delete lt (memStateLifetimes m)})
    _ -> pure ()

--------------------------------------------------------------------------------
-- Deleters

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

-- | Helper function for setting the deleters for an XObj.
setDeletersOnXObj :: XObj -> Set.Set Deleter -> XObj
setDeletersOnXObj xobj deleters = xobj {xobjInfo = setDeletersOnInfo (xobjInfo xobj) deleters}

createDeleter :: TypeEnv -> Env -> XObj -> Maybe Deleter
createDeleter typeEnv globalEnv xobj =
  case xobjTy xobj of
    Just (RefTy _ _) -> Just (RefDeleter (varOfXObj xobj))
    Just t ->
      let var = varOfXObj xobj
       in if isManaged typeEnv globalEnv t
            then case nameOfPolymorphicFunction typeEnv globalEnv (FuncTy [t] UnitTy StaticLifetimeTy) "delete" of
              Just pathOfDeleteFunc ->
                Just (ProperDeleter pathOfDeleteFunc (getDropFunc typeEnv globalEnv (xobjInfo xobj) t) var)
              Nothing ->
                --trace ("Found no delete function for " ++ var ++ " : " ++ (showMaybeTy (ty xobj)))
                Just (FakeDeleter var)
            else Just (PrimDeleter var)
    Nothing -> error ("No type, can't manage " ++ show xobj)

getDropFunc :: TypeEnv -> Env -> Maybe Info -> Ty -> Maybe SymPath
getDropFunc typeEnv globalEnv i t =
  nameOfPolymorphicFunction typeEnv globalEnv (FuncTy [RefTy t (VarTy (makeTypeVariableNameFromInfo i))] UnitTy StaticLifetimeTy) "drop"

-- | To make the `while` form behave correctly with memory management rules
searchForInnerBreak :: Set.Set Deleter -> XObj -> XObj
searchForInnerBreak diff (XObj (Lst [(XObj Break i' t')]) xi xt) =
  let ni = addDeletersToInfo i' diff
   in XObj (Lst [(XObj Break ni t')]) xi xt
searchForInnerBreak _ x@(XObj (Lst ((XObj While _ _) : _)) _ _) = x
searchForInnerBreak diff (XObj (Lst elems) i' t') =
  let newElems = map (searchForInnerBreak diff) elems
   in XObj (Lst newElems) i' t'
searchForInnerBreak _ e = e

--------------------------------------------------------------------------------
-- Helpers

-- | Check if any local ref sources are dangling. Only checks lifetime
-- | variables that appear in the function's return type, since those are the
-- | ones that could escape. A local ref source is dead if its target
-- | variable is not a function parameter.
checkLocalRefSources :: Maybe Ty -> Set.Set Deleter -> Map.Map String [(String, XObj)] -> Either TypeError ()
checkLocalRefSources maybeFnTy paramDeleters localRefSources =
  let returnLifetimes = case maybeFnTy of
        Just (FuncTy _ retTy _) -> collectLifetimeVars retTy
        _ -> []
      relevantEntries = concatMap (\lt -> fromMaybe [] (Map.lookup lt localRefSources)) returnLifetimes
   in case filter (\(name, _) -> not (isAliveInParams name)) relevantEntries of
        [] -> Right ()
        ((deadName, refXObj) : _) -> Left (UsingDeadReference refXObj deadName)
  where
    collectLifetimeVars (RefTy _ (VarTy lt)) = [lt]
    collectLifetimeVars (RefTy inner lt) = collectLifetimeVars inner ++ collectLifetimeVars lt
    collectLifetimeVars (FuncTy args retTy lt) = concatMap collectLifetimeVars args ++ collectLifetimeVars retTy ++ collectLifetimeVars lt
    collectLifetimeVars (StructTy _ tys) = concatMap collectLifetimeVars tys
    collectLifetimeVars _ = []
    isAliveInParams name =
      any
        ( \case
            ProperDeleter {deleterVariable = dv} -> dv == name
            FakeDeleter {deleterVariable = dv} -> dv == name
            PrimDeleter {aliveVariable = dv} -> dv == name
            RefDeleter {refVariable = dv} -> dv == name
        )
        (Set.toList paramDeleters)

isSymbolThatCaptures :: XObj -> Bool
isSymbolThatCaptures xobj =
  case xobj of
    XObj (Sym _ (LookupLocal (Capture _))) _ _ -> True
    _ -> False

-- | Show lifetime mappings in a more readable way.
-- prettyLifetimeMappings :: Map.Map String LifetimeMode -> String
-- prettyLifetimeMappings mappings =
--   joinLines (map prettyMapping (Map.toList mappings))
--   where
--     prettyMapping (key, value) = "  " ++ key ++ " => " ++ show value

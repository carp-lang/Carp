module InitialTypes where

import Control.Monad.State
import Data.Either (fromRight)
import Env as E
import Info
import qualified Map
import Obj
import qualified Set
import TypeError
import Types
import Util

-- | Create a fresh type variable (eg. 'VarTy t0', 'VarTy t1', etc...)
genVarTyWithPrefix :: String -> State Integer Ty
genVarTyWithPrefix prefix =
  do
    x <- get
    put (x + 1)
    pure (VarTy (prefix ++ show x))

genVarTy :: State Integer Ty
genVarTy = genVarTyWithPrefix "t"

-- | Create a list of type variables with increasing names
genVarTys :: Int -> State Integer [Ty]
genVarTys n = replicateM n genVarTy

-- | Gives all type variables new names ("t<n>", counting from current state) while
--   still preserving the same name for type variables with a shared name.
--   Example: (t0, t1, t1) -> t0
--   becomes: (r2, r3, r3) -> r2
renameVarTys :: Ty -> State Integer Ty
renameVarTys rootType = do
  n <- get
  let (result, (n', _)) = runState (rename rootType) (n, Map.empty)
  put n'
  pure result
  where
    rename :: Ty -> State (Integer, Map.Map String Ty) Ty
    rename (FuncTy argTys retTy ltTy) = do
      ltTy' <- rename ltTy
      argTys' <- mapM rename argTys
      retTy' <- rename retTy
      pure (FuncTy argTys' retTy' ltTy')
    rename (VarTy v) = do
      (n, mappings) <- get
      case Map.lookup v mappings of
        Just found -> pure found
        Nothing -> do
          let varTy = VarTy ("r" ++ show n)
              newMappings = Map.insert v varTy mappings
          put (n + 1, newMappings)
          pure varTy
    rename (StructTy name tyArgs) = do
      tyArgs' <- mapM rename tyArgs
      name' <- rename name
      pure (StructTy name' tyArgs')
    rename (PointerTy x) = do
      x' <- rename x
      pure (PointerTy x')
    rename (RefTy x lt) = do
      x' <- rename x
      lt' <- rename lt
      pure (RefTy x' lt')
    rename x = pure x

-- | Adds initial types to a s-expression and all its sub-nodes.
-- | Example: (f 10) => <(<f : (Fn [Int] Bool>) <10 : Int>) : t0>
initialTypes :: TypeEnv -> Env -> XObj -> Either TypeError XObj
initialTypes typeEnv rootEnv root = evalState (visit rootEnv root) 0
  where
    visit :: Env -> XObj -> State Integer (Either TypeError XObj)
    visit env xobj = case xobjObj xobj of
      (Num t _) -> pure (Right (xobj {xobjTy = Just t}))
      (Bol _) -> pure (Right (xobj {xobjTy = Just BoolTy}))
      (C _) -> pure (Right xobj {xobjTy = Just CTy})
      (Str _) -> do
        lt <- genVarTy
        pure (Right (xobj {xobjTy = Just (RefTy StringTy lt)}))
      (Pattern _) -> do
        lt <- genVarTy
        pure (Right (xobj {xobjTy = Just (RefTy PatternTy lt)}))
      (Chr _) -> pure (Right (xobj {xobjTy = Just CharTy}))
      Break -> pure (Right (xobj {xobjTy = Just (FuncTy [] UnitTy StaticLifetimeTy)}))
      (Command _) -> pure (Right (xobj {xobjTy = Just DynamicTy}))
      (Lst _) -> visitList env xobj
      (Arr _) -> visitArray env xobj
      (StaticArr _) -> visitStaticArray env xobj
      (Dict _) -> visitDictionary env xobj
      (Sym symPath _) -> visitSymbol env xobj symPath
      (MultiSym _ paths) -> visitMultiSym env xobj paths
      (InterfaceSym _) -> visitInterfaceSym env xobj
      e@(Defn _) -> pure (Left (InvalidObj e xobj))
      Def -> pure (Left (InvalidObj Def xobj))
      DefDynamic -> pure (Left (InvalidObj DefDynamic xobj))
      e@(Fn _ _) -> pure (Left (InvalidObj e xobj))
      Let -> pure (Left (InvalidObj Let xobj))
      If -> pure (Left (InvalidObj If xobj))
      While -> pure (Left (InvalidObj While xobj))
      Do -> pure (Left (InvalidObj Do xobj))
      m@(Mod _ _) -> pure (Left (InvalidObj m xobj))
      e@(Deftype _) -> pure (Left (InvalidObj e xobj))
      e@(External _) -> pure (Left (InvalidObj e xobj))
      e@(ExternalType _) -> pure (Left (InvalidObj e xobj))
      e@(Deftemplate _) -> pure (Left (InvalidObj e xobj))
      e@(Instantiate _) -> pure (Left (InvalidObj e xobj))
      e@(Defalias _) -> pure (Left (InvalidObj e xobj))
      SetBang -> pure (Left (InvalidObj SetBang xobj))
      Macro -> pure (Left (InvalidObj Macro xobj))
      The -> pure (Left (InvalidObj The xobj))
      Dynamic -> pure (Left (InvalidObj Dynamic xobj))
      Ref -> pure (Left (InvalidObj Ref xobj))
      Deref -> pure (Left (InvalidObj Deref xobj))
      With -> pure (Left (InvalidObj With xobj))
      -- catchall case for exhaustive patterns
      unknown -> pure (Left (InvalidObj unknown xobj))
    visitSymbol :: Env -> XObj -> SymPath -> State Integer (Either TypeError XObj)
    visitSymbol e xobj@(XObj (Sym name LookupRecursive) _ _) _ =
      case E.searchValueBinder e name of
        -- If this recursive symbol is already typed in this environment, use that type.
        -- This is relevant for, e.g. recursive function calls.
        -- We need to use search here to check parents as our let-binding handling possibly puts recursive
        -- environments as the parent of a more local environment for the let bindings.
        Right (Binder _ found) -> pure (Right xobj {xobjTy = xobjTy found})
        -- Other recursive lookups are left untouched (this avoids problems with looking up the thing they're referring to)
        Left _ -> do
          freshTy <- genVarTy
          pure (Right xobj {xobjTy = Just freshTy})
    visitSymbol env xobj symPath =
      case symPath of
        -- Symbols with leading ? are 'holes'.
        SymPath _ name@('?' : _) -> pure (Right (xobj {xobjTy = Just (VarTy name)}))
        SymPath _ (':' : _) -> pure (Left (LeadingColon xobj))
        _ ->
          case E.searchValue env symPath of
            Right (foundEnv, binder) ->
              case xobjTy (binderXObj binder) of
                -- Don't rename internal symbols like parameters etc!
                Just theType
                  | envIsExternal foundEnv -> do
                    renamed <- renameVarTys theType
                    pure (Right (xobj {xobjTy = Just renamed}))
                  | otherwise -> pure (Right (xobj {xobjTy = Just theType}))
                Nothing -> pure (Left (SymbolMissingType xobj foundEnv))
            Left _ -> pure (Left (SymbolNotDefined symPath xobj env)) -- Gives the error message "Trying to refer to an undefined symbol ..."
    visitMultiSym :: Env -> XObj -> [SymPath] -> State Integer (Either TypeError XObj)
    visitMultiSym _ xobj@(XObj (MultiSym _ _) _ _) _ =
      do
        freshTy <- genVarTy
        pure (Right xobj {xobjTy = Just freshTy})
    visitMultiSym _ _ _ = error "visitmultisym"
    visitInterfaceSym :: Env -> XObj -> State Integer (Either TypeError XObj)
    visitInterfaceSym _ xobj@(XObj (InterfaceSym name) _ _) =
      do
        freshTy <- case getTypeBinder typeEnv name of
          Right (Binder _ (XObj (Lst [XObj (Interface interfaceSignature _) _ _, _]) _ _)) -> renameVarTys interfaceSignature
          Right (Binder _ x) -> error ("A non-interface named '" ++ name ++ "' was found in the type environment: " ++ pretty x)
          Left _ -> genVarTy
        pure (Right xobj {xobjTy = Just freshTy})
    visitInterfaceSym _ _ = error "visitinterfacesym"
    visitArray :: Env -> XObj -> State Integer (Either TypeError XObj)
    visitArray env (XObj (Arr xobjs) i _) =
      do
        visited <- mapM (visit env) xobjs
        arrayVarTy <- genVarTy
        pure $ do
          okVisited <- sequence visited
          Right (XObj (Arr okVisited) i (Just (StructTy (ConcreteNameTy (SymPath [] "Array")) [arrayVarTy])))
    visitArray _ _ = error "The function 'visitArray' only accepts XObj:s with arrays in them."
    visitStaticArray :: Env -> XObj -> State Integer (Either TypeError XObj)
    visitStaticArray env (XObj (StaticArr xobjs) i _) =
      do
        visited <- mapM (visit env) xobjs
        arrayVarTy <- genVarTy
        lt <- genVarTy
        pure $ do
          okVisited <- sequence visited
          Right (XObj (StaticArr okVisited) i (Just (RefTy (StructTy (ConcreteNameTy (SymPath [] "StaticArray")) [arrayVarTy]) lt)))
    visitStaticArray _ _ = error "The function 'visitStaticArray' only accepts XObj:s with arrays in them."
    visitDictionary :: Env -> XObj -> State Integer (Either TypeError XObj)
    visitDictionary env (XObj (Dict xobjs) i _) =
      do
        visited <- mapM (visit env) xobjs
        arrayVarTy <- genVarTy
        pure $ do
          okVisited <- sequence visited
          Right (XObj (Dict okVisited) i (Just (StructTy (ConcreteNameTy (SymPath [] "Dictionary")) [arrayVarTy])))
    visitDictionary _ _ = error "The function 'visitArray' only accepts XObj:s with dictionaries in them."
    getTys env argList =
      do
        argTypes <- genVarTys (length argList)
        returnType <- genVarTy
        funcScopeEnv <- extendEnvWithParamList env argList
        pure (argTypes, returnType, funcScopeEnv)
    visitList :: Env -> XObj -> State Integer (Either TypeError XObj)
    visitList env xobj@(XObj (Lst xobjs) i _) =
      case xobjs of
        -- Defn
        [defn@(XObj (Defn _) _ _), nameSymbol@(XObj (Sym (SymPath _ name) _) _ _), XObj (Arr argList) argsi argst, body] ->
          do
            (argTypes, returnType, funcScopeEnv) <- getTys env argList
            let funcTy = Just (FuncTy argTypes returnType StaticLifetimeTy)
                typedNameSymbol = nameSymbol {xobjTy = funcTy}
                -- TODO! After the introduction of 'LookupRecursive' this env shouldn't be needed anymore? (but it is for some reason...)
                envWithSelf = fromRight funcScopeEnv (E.insertX funcScopeEnv (SymPath [] name) typedNameSymbol)
            visitedBody <- visit envWithSelf body
            visitedArgs <- mapM (visit envWithSelf) argList
            pure $ do
              okBody <- visitedBody
              okArgs <- sequence visitedArgs
              pure (XObj (Lst [defn, nameSymbol, XObj (Arr okArgs) argsi argst, okBody]) i funcTy)
        [XObj (Defn _) _ _, XObj (Sym _ _) _ _, XObj (Arr _) _ _] -> pure (Left (NoFormsInBody xobj))
        XObj defn@(Defn _) _ _ : _ ->
          pure (Left (InvalidObjExample defn xobj "(defn <name> [<arguments>] <body>)"))
        -- Anonymous function bound to a let name
        -- Supports recursion by assigning the same type to recursive calls ("let-rec").
        [XObj LocalDef _ _, XObj (Sym path _) si _, XObj (Lst [fn@(XObj (Fn _ _) _ _), XObj (Arr argList) argsi argst, body]) _ _] ->
          do
            (argTypes, returnType, funcScopeEnv) <- getTys env argList
            lt <- genVarTy
            let funcTy = Just (FuncTy argTypes returnType lt)
                typedNameSymbol = XObj (Sym path LookupRecursive) si funcTy
                envWithSelf = fromRight funcScopeEnv (E.insertX funcScopeEnv path typedNameSymbol)
            visitedBody <- visit envWithSelf body
            visitedArgs <- mapM (visit envWithSelf) argList
            pure $ do
              okBody <- visitedBody
              okArgs <- sequence visitedArgs
              let final = XObj (Lst [fn, XObj (Arr okArgs) argsi argst, okBody]) i funcTy
              pure final --(trace ("FINAL: " ++ show final) final)
              -- Let bindings
        [XObj LocalDef _ _, _, value] ->
          visit env value
        -- Unbound anonymous Fn
        [fn@(XObj (Fn _ _) _ _), XObj (Arr argList) argsi argst, body] ->
          do
            (argTypes, returnType, funcScopeEnv) <- getTys env argList
            lt <- genVarTy
            let funcTy = Just (FuncTy argTypes returnType lt)
            visitedBody <- visit funcScopeEnv body
            visitedArgs <- mapM (visit funcScopeEnv) argList
            pure $ do
              okBody <- visitedBody
              okArgs <- sequence visitedArgs
              let final = XObj (Lst [fn, XObj (Arr okArgs) argsi argst, okBody]) i funcTy
              pure final --(trace ("FINAL: " ++ show final) final)
        [XObj (Fn _ _) _ _, XObj (Arr _) _ _] -> pure (Left (NoFormsInBody xobj)) -- TODO: Special error message for lambdas needed?
        XObj fn@(Fn _ _) _ _ : _ ->
          pure (Left (InvalidObjExample fn xobj "(fn [<arguments>] <body>)"))
        -- Def
        [def@(XObj Def _ _), nameSymbol, expression] ->
          do
            definitionType <- genVarTy
            visitedExpr <- visit env expression
            pure $ do
              okExpr <- visitedExpr
              pure (XObj (Lst [def, nameSymbol, okExpr]) i (Just definitionType))
        XObj Def _ _ : _ ->
          pure (Left (InvalidObjExample Def xobj "(def <name> <expression>)"))
        -- DefDynamic
        [def@(XObj DefDynamic _ _), nameSymbol, expression] ->
          pure $ pure (XObj (Lst [def, nameSymbol, expression]) i (Just DynamicTy))
        XObj DefDynamic _ _ : _ ->
          pure (Left (InvalidObjExample Def xobj "(defdynamic <name> <expression>"))
        -- Let binding
        [letExpr@(XObj Let _ _), XObj (Arr bindings) bindi bindt, body] ->
          do
            wholeExprType <- genVarTy
            letScopeEnv <- extendEnvWithLetBindings env bindings
            case letScopeEnv of
              Right okLetScopeEnv ->
                do
                  visitedBindings <- mapM (visit okLetScopeEnv) bindings
                  visitedBody <- visit okLetScopeEnv body
                  pure $ do
                    okBindings <- sequence visitedBindings
                    case getDuplicate [] okBindings of
                      Just dup -> Left (DuplicateBinding dup)
                      Nothing -> do
                        okBody <- visitedBody
                        Right (XObj (Lst [letExpr, XObj (Arr okBindings) bindi bindt, okBody]) i (Just wholeExprType))
              Left err -> pure (Left err)
          where
            getDuplicate _ [] = Nothing
            getDuplicate names (o@(XObj (Sym (SymPath _ x) _) _ _) : _ : xs) =
              if x `elem` names then Just o else getDuplicate (x : names) xs
            getDuplicate _ _ = error "getduplicate"
        [XObj Let _ _, XObj (Arr _) _ _] ->
          pure (Left (NoFormsInBody xobj))
        XObj Let _ _ : XObj (Arr _) _ _ : _ ->
          pure (Left (TooManyFormsInBody xobj))
        XObj Let _ _ : _ ->
          pure (Left (InvalidObjExample Let xobj "(let [<variable> <expression ...] <body>)"))
        -- If
        [ifExpr@(XObj If _ _), expr, ifTrue, ifFalse] ->
          do
            visitedExpr <- visit env expr
            visitedTrue <- visit env ifTrue
            visitedFalse <- visit env ifFalse
            returnType <- genVarTy
            pure $ do
              okExpr <- visitedExpr
              okTrue <- visitedTrue
              okFalse <- visitedFalse
              pure
                ( XObj
                    ( Lst
                        [ ifExpr,
                          okExpr,
                          okTrue,
                          okFalse
                        ]
                    )
                    i
                    (Just returnType)
                )
        XObj If _ _ : _ ->
          pure (Left (InvalidObjExample If xobj "(if <condition> <then-expression> <else-expression>)"))
        -- Match
        matchExpr@(XObj (Match _) _ _) : expr : cases ->
          do
            visitedExpr <- visit env expr
            visitedCases <-
              sequence
                <$> mapM
                  ( \(lhs, rhs) -> do
                      let lhs' = uniquifyWildcardNames (helpWithParens lhs) -- Add parens if missing
                      env' <- extendEnvWithCaseMatch env lhs'
                      visitedLhs <- visit env' lhs'
                      visitedRhs <- visit env' rhs
                      pure $ do
                        okLhs <- visitedLhs
                        okRhs <- visitedRhs
                        pure (okLhs, okRhs)
                  )
                  (pairwise cases)
            returnType <- genVarTy
            pure $ do
              okExpr <- visitedExpr
              okCases <- visitedCases
              let okCasesConcatenated = concatMap (\(a, b) -> [a, b]) okCases
              pure
                ( XObj
                    (Lst ([matchExpr, okExpr] ++ okCasesConcatenated))
                    i
                    (Just returnType)
                )
        XObj (Match m) _ _ : _ ->
          pure (Left (InvalidObjExample (Match m) xobj "(match <to-match> <condition> <clause> ...)"))
        -- While (always return Unit)
        [whileExpr@(XObj While _ _), expr, body] ->
          do
            visitedExpr <- visit env expr
            visitedBody <- visit env body
            pure $ do
              okExpr <- visitedExpr
              okBody <- visitedBody
              pure (XObj (Lst [whileExpr, okExpr, okBody]) i (Just UnitTy))
        [XObj While _ _, _] ->
          pure (Left (NoFormsInBody xobj))
        XObj While _ _ : _ ->
          pure (Left (TooManyFormsInBody xobj))
        -- Do
        doExpr@(XObj Do _ _) : expressions ->
          do
            t <- genVarTy
            visitedExpressions <- fmap sequence (mapM (visit env) expressions)
            pure $ do
              okExpressions <- visitedExpressions
              pure (XObj (Lst (doExpr : okExpressions)) i (Just t))
        -- Set!
        [setExpr@(XObj SetBang _ _), variable, value] ->
          do
            visitedVariable <- visit env variable
            visitedValue <- visit env value
            pure $ do
              okVariable <- visitedVariable
              okValue <- visitedValue
              pure (XObj (Lst [setExpr, okVariable, okValue]) i (Just UnitTy))
        XObj SetBang _ _ : _ ->
          pure (Left (InvalidObjExample SetBang xobj "(set! <variable> <new-value>)"))
        -- The
        [theExpr@(XObj The _ _), typeXObj, value] ->
          do
            visitedValue <- visit env value
            pure $ do
              okValue <- visitedValue
              case xobjToTy typeXObj of
                Just okType -> pure (XObj (Lst [theExpr, typeXObj, okValue]) i (Just okType))
                Nothing -> Left (NotAType typeXObj)
        XObj The _ _ : _ ->
          pure (Left (InvalidObjExample The xobj "(the <type> <expression>)"))
        -- Ref
        [refExpr@(XObj Ref _ _), value] ->
          do
            visitedValue <- visit env value
            lt <- case value of -- This is to not get lifetime errors when using globals. TODO: Is there a better way?!
              XObj (Sym _ (LookupGlobal _ _)) _ _ -> pure StaticLifetimeTy
              _
                | isLiteral value -> pure StaticLifetimeTy
                | otherwise -> genVarTy
            pure $ do
              okValue <- visitedValue
              let valueTy = case xobjTy okValue of
                    Just vt -> (Just (RefTy vt lt))
                    Nothing -> Nothing
              pure (XObj (Lst [refExpr, okValue]) i valueTy)
        -- Deref (error!)
        [XObj Deref _ _, _] ->
          pure (Left (CantUseDerefOutsideFunctionApplication xobj))
        -- Function application with Deref
        XObj (Lst [deref@(XObj Deref _ _), func]) xi _ : args ->
          -- TODO: Remove code duplication (taken from function application below)
          do
            t <- genVarTy
            derefTy <- genVarTy
            visitedFunc <- visit env func
            visitedArgs <- fmap sequence (mapM (visit env) args)
            pure $ do
              okFunc <- visitedFunc
              okArgs <- visitedArgs
              pure (XObj (Lst (XObj (Lst [deref, okFunc]) xi (Just derefTy) : okArgs)) i (Just t))
        -- Function application
        func : args ->
          do
            t <- genVarTy
            visitedFunc <- visit env func
            visitedArgs <- fmap sequence (mapM (visit env) args)
            pure $ do
              okFunc <- visitedFunc
              okArgs <- visitedArgs
              pure (XObj (Lst (okFunc : okArgs)) i (Just t))
        -- Empty list
        [] -> pure (Right xobj {xobjTy = Just UnitTy})
    visitList _ _ = error "Must match on list!"
    extendEnvWithLetBindings :: Env -> [XObj] -> State Integer (Either TypeError Env)
    extendEnvWithLetBindings env xobjs =
      let pairs = pairwise xobjs
          emptyInnerEnv =
            Env
              { envBindings = Map.empty,
                envParent = Just env,
                envModuleName = Nothing,
                envUseModules = Set.empty,
                envMode = InternalEnv,
                envFunctionNestingLevel = envFunctionNestingLevel env
              }
       in -- Need to fold (rather than map) to make the previous bindings accessible to the later ones, i.e. (let [a 100 b a] ...)
          foldM createBinderForLetPair (Right emptyInnerEnv) pairs
      where
        -- Cast binders to Local Defs so that we can account for recursion ("let-rec").
        -- A local def carries the binder name along with its value, so we can appropriately type recursive uses.
        -- e.g. (let [f (fn [x] (if (= x 1) x (f (dec x))))])
        createBinderForLetPair :: Either TypeError Env -> (XObj, XObj) -> State Integer (Either TypeError Env)
        createBinderForLetPair envOrErr (sym, expr) =
          case envOrErr of
            Left err -> pure (Left err)
            Right env' ->
              case xobjObj sym of
                (Sym (SymPath _ name) _) ->
                  do
                    visited <- visit env' (toLocalDef name expr)
                    pure
                      ( join
                          (replaceLeft (InvalidLetBinding xobjs (sym, expr)) . E.insert env' (SymPath [] name) . Binder emptyMeta <$> visited)
                      )
                _ -> pure (Left (InvalidLetBinding xobjs (sym, expr)))
    extendEnvWithParamList :: Env -> [XObj] -> State Integer Env
    extendEnvWithParamList env xobjs =
      do
        binders' <- mapM createBinderForParam xobjs
        pure
          Env
            { envBindings = Map.fromList binders',
              envParent = Just env,
              envModuleName = Nothing,
              envUseModules = Set.empty,
              envMode = InternalEnv,
              envFunctionNestingLevel = envFunctionNestingLevel env
            }
      where
        createBinderForParam :: XObj -> State Integer (String, Binder)
        createBinderForParam xobj =
          case xobjObj xobj of
            (Sym (SymPath _ name) _) ->
              do
                t <- genVarTy
                let xobjWithTy = xobj {xobjTy = Just t}
                pure (name, Binder emptyMeta xobjWithTy)
            _ -> error "Can't create binder for non-symbol parameter."
    extendEnvWithCaseMatch :: Env -> XObj -> State Integer Env
    extendEnvWithCaseMatch env caseRoot =
      do
        binders' <- createBindersForCaseVariable caseRoot
        pure
          Env
            { envBindings = Map.fromList binders',
              envParent = Just env,
              envModuleName = Nothing,
              envUseModules = Set.empty,
              envMode = InternalEnv,
              envFunctionNestingLevel = envFunctionNestingLevel env
            }
      where
        createBindersForCaseVariable :: XObj -> State Integer [(String, Binder)]
        createBindersForCaseVariable xobj@(XObj (Sym (SymPath _ name) _) _ _) = createBinderInternal xobj name
        createBindersForCaseVariable xobj@(XObj (MultiSym name _) _ _) = createBinderInternal xobj name
        createBindersForCaseVariable xobj@(XObj (InterfaceSym name) _ _) = createBinderInternal xobj name
        createBindersForCaseVariable (XObj (Lst lst) _ _) = do
          binders' <- mapM createBindersForCaseVariable lst
          pure (concat binders')
        createBindersForCaseVariable (XObj Ref _ _) = pure []
        createBindersForCaseVariable x = error ("Can't create binder for non-symbol in 'case' variable match:" ++ show x) -- TODO: Should use proper error mechanism
        createBinderInternal :: XObj -> String -> State Integer [(String, Binder)]
        createBinderInternal xobj name =
          if isVarName name
            then -- A variable that will bind to something:
            do
              freshTy <- genVarTy
              pure [(name, Binder emptyMeta xobj {xobjTy = Just freshTy})]
            else -- Tags for the sumtypes won't bind to anything:
              pure []

uniquifyWildcardNames :: XObj -> XObj
uniquifyWildcardNames (XObj (Sym (SymPath [] "_") mode) (Just i) t) =
  let uniqueName = "wildcard_" ++ show (infoIdentifier i)
   in XObj (Sym (SymPath [] uniqueName) mode) (Just i) t
uniquifyWildcardNames (XObj (Lst xobjs) i t) =
  XObj (Lst (map uniquifyWildcardNames xobjs)) i t
uniquifyWildcardNames (XObj (Arr xobjs) i t) =
  XObj (Arr (map uniquifyWildcardNames xobjs)) i t
uniquifyWildcardNames x =
  x

-- | Help our programmer friend using Carp to add/remove parens around the lhs of a match
helpWithParens :: XObj -> XObj
helpWithParens xobj@(XObj (Sym (SymPath _ name) _) _ _)
  | isVarName name = xobj -- Don't wrap
  | otherwise = wrapInParens xobj
helpWithParens outer@(XObj (Lst [inner@(XObj (Sym (SymPath _ name) _) _ _)]) _ _)
  | isVarName name = inner -- Unwrap
  | otherwise = outer -- Keep wrapped
helpWithParens xobj =
  wrapInParens xobj

module InitialTypes where

import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Debug.Trace

import Types
import Obj
import Util
import TypeError
import Lookup

-- | Create a fresh type variable (eg. 'VarTy t0', 'VarTy t1', etc...)
genVarTyWithPrefix :: String -> State Integer Ty
genVarTyWithPrefix prefix =
  do x <- get
     put (x + 1)
     return (VarTy (prefix ++ show x))

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
renameVarTys rootType = do n <- get
                           let (result, (n', _)) = runState (rename rootType) (n, Map.empty)
                           put n'
                           return result
  where
    rename :: Ty -> State (Integer, Map.Map String Ty) Ty
    rename (FuncTy argTys retTy) = do argTys' <- mapM rename argTys
                                      retTy' <- rename retTy
                                      return (FuncTy argTys' retTy')
    rename (VarTy v) = do (n, mappings) <- get
                          case Map.lookup v mappings of
                            Just found -> return found
                            Nothing -> do let varTy = VarTy ("r" ++ show n)
                                              newMappings = Map.insert v varTy mappings
                                          put (n + 1, newMappings)
                                          return varTy
    rename (StructTy name tyArgs) = do tyArgs' <- mapM rename tyArgs
                                       return (StructTy name tyArgs')

    rename (PointerTy x) = do x' <- rename x
                              return (PointerTy x')

    rename (RefTy x) = do x' <- rename x
                          return (RefTy x')

    rename x = return x

-- | Adds initial types to a s-expression and all its sub-nodes.
-- | Example: (f 10) => <(<f : (Fn [Int] Bool>) <10 : Int>) : t0>
initialTypes :: TypeEnv -> Env -> XObj -> Either TypeError XObj
initialTypes typeEnv rootEnv root = evalState (visit rootEnv root) 0
  where
    visit :: Env -> XObj -> State Integer (Either TypeError XObj)
    visit env xobj = case obj xobj of
                       (Num t _)          -> return (Right (xobj { ty = Just t }))
                       (Bol _)            -> return (Right (xobj { ty = Just BoolTy }))
                       (Str _)            -> return (Right (xobj { ty = Just (RefTy StringTy) }))
                       (Pattern _)        -> return (Right (xobj { ty = Just (RefTy PatternTy) }))
                       (Chr _)            -> return (Right (xobj { ty = Just CharTy }))
                       Break              -> return (Right (xobj { ty = Just (FuncTy [] UnitTy)}))
                       (Command _)        -> return (Right (xobj { ty = Just DynamicTy }))
                       (Lst _)            -> visitList env xobj
                       (Arr _)            -> visitArray env xobj
                       (Dict _)           -> visitDictionary env xobj
                       (Sym symPath _)    -> visitSymbol env xobj symPath
                       (MultiSym _ paths) -> visitMultiSym env xobj paths
                       (InterfaceSym _)   -> visitInterfaceSym env xobj
                       Defn               -> return (Left (InvalidObj Defn xobj))
                       Def                -> return (Left (InvalidObj Def xobj))
                       e@(Fn _ _)         -> return (Left (InvalidObj e xobj))
                       Let                -> return (Left (InvalidObj Let xobj))
                       If                 -> return (Left (InvalidObj If xobj))
                       While              -> return (Left (InvalidObj While xobj))
                       Do                 -> return (Left (InvalidObj Do xobj))
                       (Mod _)            -> return (Left (InvalidObj If xobj))
                       e@(Typ _)          -> return (Left (InvalidObj e xobj))
                       e@(External _)     -> return (Left (InvalidObj e xobj))
                       ExternalType       -> return (Left (InvalidObj ExternalType xobj))
                       e@(Deftemplate _)  -> return (Left (InvalidObj e xobj))
                       e@(Instantiate _)  -> return (Left (InvalidObj e xobj))
                       e@(Defalias _)     -> return (Left (InvalidObj e xobj))
                       Address            -> return (Left (InvalidObj Address xobj))
                       SetBang            -> return (Left (InvalidObj SetBang xobj))
                       Macro              -> return (Left (InvalidObj Macro xobj))
                       The                -> return (Left (InvalidObj The xobj))
                       Dynamic            -> return (Left (InvalidObj Dynamic xobj))
                       Ref                -> return (Left (InvalidObj Ref xobj))
                       Deref              -> return (Left (InvalidObj Deref xobj))
                       With               -> return (Left (InvalidObj With xobj))

    visitSymbol :: Env -> XObj -> SymPath -> State Integer (Either TypeError XObj)
    visitSymbol _ xobj@(XObj (Sym _ LookupRecursive) _ _) _ =
      -- Recursive lookups are left untouched (this avoids problems with looking up the thing they're referring to)
      do freshTy <- genVarTy
         return (Right xobj { ty = Just freshTy })
    visitSymbol env xobj symPath =
      case symPath of
        -- Symbols with leading ? are 'holes'.
        SymPath _ name@('?' : _) -> return (Right (xobj { ty = Just (VarTy name) }))
        SymPath _ (':' : _) -> return (Left (LeadingColon xobj))
        _ ->
          case lookupInEnv symPath env of
            Just (foundEnv, binder) ->
              case ty (binderXObj binder) of
                -- Don't rename internal symbols like parameters etc!
                Just theType | envIsExternal foundEnv -> do renamed <- renameVarTys theType
                                                            return (Right (xobj { ty = Just renamed }))
                             | otherwise -> return (Right (xobj { ty = Just theType }))
                Nothing -> return (Left (SymbolMissingType xobj foundEnv))
            Nothing -> return (Left (SymbolNotDefined symPath xobj env)) -- Gives the error message "Trying to refer to an undefined symbol ..."

    visitMultiSym :: Env -> XObj -> [SymPath] -> State Integer (Either TypeError XObj)
    visitMultiSym _ xobj@(XObj (MultiSym name _) _ _) _ =
      do freshTy <- genVarTy
         return (Right xobj { ty = Just freshTy })

    visitInterfaceSym :: Env -> XObj -> State Integer (Either TypeError XObj)
    visitInterfaceSym env xobj@(XObj (InterfaceSym name) _ _) =
      do freshTy <- case lookupInEnv (SymPath [] name) (getTypeEnv typeEnv) of
                      Just (_, Binder _ (XObj (Lst [XObj (Interface interfaceSignature _) _ _, _]) _ _)) -> renameVarTys interfaceSignature
                      Just (_, Binder _ x) -> error ("A non-interface named '" ++ name ++ "' was found in the type environment: " ++ show x)
                      Nothing -> genVarTy
         return (Right xobj { ty = Just freshTy })

    visitArray :: Env -> XObj -> State Integer (Either TypeError XObj)
    visitArray env (XObj (Arr xobjs) i _) =
      do visited <- mapM (visit env) xobjs
         arrayVarTy <- genVarTy
         return $ do okVisited <- sequence visited
                     Right (XObj (Arr okVisited) i (Just (StructTy "Array" [arrayVarTy])))

    visitArray _ _ = error "The function 'visitArray' only accepts XObj:s with arrays in them."

    visitDictionary :: Env -> XObj -> State Integer (Either TypeError XObj)
    visitDictionary env (XObj (Dict xobjs) i _) =
      do visited <- mapM (visit env) xobjs
         arrayVarTy <- genVarTy
         return $ do okVisited <- sequence visited
                     Right (XObj (Dict okVisited) i (Just (StructTy "Dictionary" [arrayVarTy])))

    visitDictionary _ _ = error "The function 'visitArray' only accepts XObj:s with dictionaries in them."

    getTys env argList =
      do argTypes <- genVarTys (length argList)
         returnType <- genVarTy
         funcScopeEnv <- extendEnvWithParamList env argList
         return (argTypes, returnType, funcScopeEnv)

    visitList :: Env -> XObj -> State Integer (Either TypeError XObj)
    visitList env xobj@(XObj (Lst xobjs) i _) =
      case xobjs of
        -- Defn
        [defn@(XObj Defn _ _), nameSymbol@(XObj (Sym (SymPath _ name) _) _ _), XObj (Arr argList) argsi argst, body] ->
          do (argTypes, returnType, funcScopeEnv) <- getTys env argList
             let funcTy = Just (FuncTy argTypes returnType)
                 typedNameSymbol = nameSymbol { ty = funcTy }
                 -- TODO! After the introduction of 'LookupRecursive' this env shouldn't be needed anymore? (but it is for some reason...)
                 envWithSelf = extendEnv funcScopeEnv name typedNameSymbol
             visitedBody <- visit envWithSelf body
             visitedArgs <- mapM (visit envWithSelf) argList
             return $ do okBody <- visitedBody
                         okArgs <- sequence visitedArgs
                         return (XObj (Lst [defn, nameSymbol, XObj (Arr okArgs) argsi argst, okBody]) i funcTy)

        [XObj Defn _ _, XObj (Sym _ _) _ _, XObj (Arr _) _ _] -> return (Left (NoFormsInBody xobj))
        XObj Defn _ _ : _  -> return (Left (InvalidObj Defn xobj))

        -- Fn
        [fn@(XObj (Fn _ _) _ _), XObj (Arr argList) argsi argst, body] ->
          do (argTypes, returnType, funcScopeEnv) <- getTys env argList
             let funcTy = Just (FuncTy argTypes returnType)
             visitedBody <- visit funcScopeEnv body
             visitedArgs <- mapM (visit funcScopeEnv) argList
             return $ do okBody <- visitedBody
                         okArgs <- sequence visitedArgs
                         let final = XObj (Lst [fn, XObj (Arr okArgs) argsi argst, okBody]) i funcTy
                         return final --(trace ("FINAL: " ++ show final) final)

        [XObj (Fn _ _) _ _, XObj (Arr _) _ _] -> return (Left (NoFormsInBody xobj)) -- TODO: Special error message for lambdas needed?
        XObj fn@(Fn _ _) _ _ : _  -> return (Left (InvalidObj fn xobj))

        -- Def
        [def@(XObj Def _ _), nameSymbol, expression]->
          do definitionType <- genVarTy
             visitedExpr <- visit env expression
             return $ do okExpr <- visitedExpr
                         return (XObj (Lst [def, nameSymbol, okExpr]) i (Just definitionType))

        XObj Def _ _ : _ -> return (Left (InvalidObj Def xobj))

        -- Let binding
        [letExpr@(XObj Let _ _), XObj (Arr bindings) bindi bindt, body] ->
          do wholeExprType <- genVarTy
             letScopeEnv <- extendEnvWithLetBindings env bindings
             case letScopeEnv of
               Right okLetScopeEnv ->
                 do visitedBindings <- mapM (visit okLetScopeEnv) bindings
                    visitedBody <- visit okLetScopeEnv body
                    return $ do okBindings <- sequence visitedBindings
                                okBody <- visitedBody
                                return (XObj (Lst [letExpr, XObj (Arr okBindings) bindi bindt, okBody]) i (Just wholeExprType))
               Left err -> return (Left err)

        [XObj Let _ _, XObj (Arr _) _ _] ->
          return (Left (NoFormsInBody xobj))
        XObj Let _ _ : XObj (Arr _) _ _ : _ ->
          return (Left (TooManyFormsInBody xobj))
        XObj Let _ _ : _ ->
          return (Left (InvalidObj Let xobj))

        -- If
        [ifExpr@(XObj If _ _), expr, ifTrue, ifFalse] ->
          do visitedExpr <- visit env expr
             visitedTrue <- visit env ifTrue
             visitedFalse <- visit env ifFalse
             returnType <- genVarTy
             return $ do okExpr <- visitedExpr
                         okTrue <- visitedTrue
                         okFalse <- visitedFalse
                         return (XObj (Lst [ifExpr
                                           ,okExpr
                                           ,okTrue
                                           ,okFalse
                                           ]) i (Just returnType))

        XObj If _ _ : _ -> return (Left (InvalidObj If xobj))

        -- Match
        matchExpr@(XObj Match _ _) : expr : cases ->
          do visitedExpr <- visit env expr
             visitedCases <- sequence <$>
              mapM (\(lhs, rhs) -> do let lhs' = uniquifyWildcardNames (helpWithParens lhs) -- Add parens if missing
                                      env' <- extendEnvWithCaseMatch env lhs'
                                      visitedLhs <- visit env' lhs'
                                      visitedRhs <- visit env' rhs
                                      return $ do okLhs <- visitedLhs
                                                  okRhs <- visitedRhs
                                                  return (okLhs, okRhs))
                                                  (pairwise cases)
             returnType <- genVarTy
             return $ do okExpr <- visitedExpr
                         okCases <- visitedCases
                         let okCasesConcatenated = concatMap (\(a, b) -> [a, b]) okCases
                         return (XObj (Lst ([matchExpr, okExpr] ++ okCasesConcatenated))
                                  i (Just returnType))

        XObj Match _ _ : _ -> return (Left (InvalidObj Match xobj))

        -- While (always return Unit)
        [whileExpr@(XObj While _ _), expr, body] ->
          do visitedExpr <- visit env expr
             visitedBody <- visit env body
             return $ do okExpr <- visitedExpr
                         okBody <- visitedBody
                         return (XObj (Lst [whileExpr, okExpr, okBody]) i (Just UnitTy))

        [XObj While _ _, _] ->
          return (Left (NoFormsInBody xobj))
        XObj While _ _ : _ ->
          return (Left (TooManyFormsInBody xobj))

        -- Do
        doExpr@(XObj Do _ _) : expressions ->
          do t <- genVarTy
             visitedExpressions <- fmap sequence (mapM (visit env) expressions)
             return $ do okExpressions <- visitedExpressions
                         return (XObj (Lst (doExpr : okExpressions)) i (Just t))

        -- Address
        [addressExpr@(XObj Address _ _), value] ->
          do visitedValue <- visit env value
             return $ do okValue <- visitedValue
                         let Just t' = ty okValue
                         return (XObj (Lst [addressExpr, okValue]) i (Just (PointerTy t')))

        -- Set!
        [setExpr@(XObj SetBang _ _), variable, value] ->
          do visitedVariable <- visit env variable
             visitedValue <- visit env value
             return $ do okVariable <- visitedVariable
                         okValue <- visitedValue
                         return (XObj (Lst [setExpr, okVariable, okValue]) i (Just UnitTy))
        XObj SetBang _ _ : _ -> return (Left (InvalidObj SetBang xobj))

        -- The
        [theExpr@(XObj The _ _), typeXObj, value] ->
          do visitedValue <- visit env value
             return $ do okValue <- visitedValue
                         case xobjToTy typeXObj of
                           Just okType -> return (XObj (Lst [theExpr, typeXObj, okValue]) i (Just okType))
                           Nothing -> Left (NotAType typeXObj)
        XObj The _ _ : _ -> return (Left (InvalidObj The xobj))

        -- Ref
        [refExpr@(XObj Ref _ _), value] ->
          do visitedValue <- visit env value
             return $ do okValue <- visitedValue
                         let Just valueTy = ty okValue
                         return (XObj (Lst [refExpr, okValue]) i (Just (RefTy valueTy)))

        -- Deref (error!)
        [XObj Deref _ _, value] ->
          return (Left (CantUseDerefOutsideFunctionApplication xobj))

        -- Function application with Deref
        XObj (Lst [deref@(XObj Deref _ _), func]) xi xt : args ->
          -- TODO: Remove code duplication (taken from function application below)
          do t <- genVarTy
             derefTy <- genVarTy
             visitedFunc <- visit env func
             visitedArgs <- fmap sequence (mapM (visit env) args)
             return $ do okFunc <- visitedFunc
                         okArgs <- visitedArgs
                         return (XObj (Lst (XObj (Lst [deref, okFunc]) xi (Just derefTy) : okArgs)) i (Just t))

        -- Function application
        func : args ->
          do t <- genVarTy
             visitedFunc <- visit env func
             visitedArgs <- fmap sequence (mapM (visit env) args)
             return $ do okFunc <- visitedFunc
                         okArgs <- visitedArgs
                         return (XObj (Lst (okFunc : okArgs)) i (Just t))

        -- Empty list
        [] -> return (Right xobj { ty = Just UnitTy })

    visitList _ _ = error "Must match on list!"

    extendEnvWithLetBindings :: Env -> [XObj] -> State Integer (Either TypeError Env)
    extendEnvWithLetBindings env xobjs =
      let pairs = pairwise xobjs
          emptyInnerEnv = Env { envBindings = Map.fromList []
                              , envParent = Just env
                              , envModuleName = Nothing
                              , envUseModules = []
                              , envMode = InternalEnv
                              , envFunctionNestingLevel = envFunctionNestingLevel env
                              }
      -- Need to fold (rather than map) to make the previous bindings accessible to the later ones, i.e. (let [a 100 b a] ...)
      in  foldM createBinderForLetPair (Right emptyInnerEnv) pairs
      where
        createBinderForLetPair :: Either TypeError Env -> (XObj, XObj) -> State Integer (Either TypeError Env)
        createBinderForLetPair envOrErr (sym, expr) =
          case envOrErr of
            Left err -> return (Left err)
            Right env' ->
              case obj sym of
                (Sym (SymPath _ name) _) ->
                  do visited <- visit env' expr
                     return (envAddBinding env' name . Binder emptyMeta <$> visited)
                _ -> error ("Can't create let-binder for non-symbol: " ++ show sym) -- TODO: Use proper error mechanism

    extendEnvWithParamList :: Env -> [XObj] -> State Integer Env
    extendEnvWithParamList env xobjs =
      do binders <- mapM createBinderForParam xobjs
         return Env { envBindings = Map.fromList binders
                    , envParent = Just env
                    , envModuleName = Nothing
                    , envUseModules = []
                    , envMode = InternalEnv
                    , envFunctionNestingLevel = envFunctionNestingLevel env
                    }
      where
        createBinderForParam :: XObj -> State Integer (String, Binder)
        createBinderForParam xobj =
          case obj xobj of
            (Sym (SymPath _ name) _) ->
              do t <- genVarTy
                 let xobjWithTy = xobj { ty = Just t }
                 return (name, Binder emptyMeta xobjWithTy)
            _ -> error "Can't create binder for non-symbol parameter."

    extendEnvWithCaseMatch :: Env -> XObj -> State Integer Env
    extendEnvWithCaseMatch env singleCaseList@(XObj (Lst xs) _ _) =
      do binders <- fmap catMaybes (mapM createBinderForCaseVariable xs)
         return Env { envBindings = Map.fromList binders
                    , envParent = Just env
                    , envModuleName = Nothing
                    , envUseModules = []
                    , envMode = InternalEnv
                    , envFunctionNestingLevel = envFunctionNestingLevel env
                    }
      where
        createBinderForCaseVariable :: XObj -> State Integer (Maybe (String, Binder))
        createBinderForCaseVariable xobj =
          case obj xobj of
            (Sym (SymPath _ name) _) ->
              createBinderInternal xobj name
            (MultiSym name _) ->
              createBinderInternal xobj name
            (InterfaceSym name) ->
              createBinderInternal xobj name
            x -> error ("Can't create binder for non-symbol in 'case' variable match:" ++ show x) -- TODO: Should use proper error mechanism

        createBinderInternal :: XObj -> String -> State Integer (Maybe (String, Binder))
        createBinderInternal xobj name =
          if isVarName name
          -- A variable that will bind to something:
          then do freshTy <- genVarTy
                  return (Just (name, Binder emptyMeta xobj { ty = Just freshTy }))
          -- Tags for the sumtypes won't bind to anything:
          else return Nothing
    extendEnvWithCaseMatch env xobj@(XObj (Sym (SymPath _ name) _) _ _) =
      do freshTy <- genVarTy
         return Env { envBindings = Map.fromList [(name, Binder emptyMeta xobj { ty = Just freshTy })]
                    , envParent = Just env
                    , envModuleName = Nothing
                    , envUseModules = []
                    , envMode = InternalEnv
                    , envFunctionNestingLevel = envFunctionNestingLevel env
                    }
    extendEnvWithCaseMatch env _ =
      return env -- TODO: Handle nesting!!!

uniquifyWildcardNames :: XObj -> XObj
uniquifyWildcardNames (XObj (Sym (SymPath [] "_") mode) (Just i) t) =
  let uniqueName = "wildcard_" ++ show (infoIdentifier i)
  in  XObj (Sym (SymPath [] uniqueName) mode) (Just i) t
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

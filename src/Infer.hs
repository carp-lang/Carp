module Infer (annotate
             ,initialTypes
             ,genConstraints
             ,assignTypes
             ,concretizeXObj
             ,concretizeDefinition
             ,manageMemory
             ,insideArrayDeleteDeps
             ,insideArrayCopyDeps
             ) where

import Control.Monad.State
import Control.Monad (replicateM)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (foldl', sort)
import Data.Maybe (mapMaybe)
import Debug.Trace

import Obj
import Constraints
import Types
import Util
import Eval

data TypeError = SymbolMissingType XObj Env
               | DefnMissingType XObj
               | DefMissingType XObj
               | ExpressionMissingType XObj
               | SymbolNotDefined SymPath XObj
               | InvalidObj Obj XObj
               | WrongArgCount XObj
               | NotAFunction XObj
               | NoStatementsInDo XObj
               | TooManyFormsInBody XObj
               | LeadingColon XObj
               | UnificationFailed Constraint TypeMappings [Constraint]
               | CantDisambiguate XObj String Ty [(Ty, SymPath)]
               | NoMatchingSignature XObj String Ty [(Ty, SymPath)]
               | HolesFound [(String, Ty)]
               | FailedToExpand XObj EvalError
               | NotAValidType XObj
               | CantReturnRefTy XObj

instance Show TypeError where
  show (SymbolMissingType xobj env) =
    "Symbol '" ++ getName xobj ++ "' missing type at " ++ prettyInfoFromXObj xobj ++ " in env:\n" ++ prettyEnvironment env
  show (DefnMissingType xobj) =
    "Function definition '" ++ getName xobj ++ "' missing type at " ++ prettyInfoFromXObj xobj  ++ "."
  show (DefMissingType xobj) =
    "Variable definition '" ++ getName xobj ++ "' missing type at " ++ prettyInfoFromXObj xobj  ++ "."
  show (ExpressionMissingType xobj)=
    "Expression '" ++ pretty xobj ++ "' missing type at " ++ prettyInfoFromXObj xobj ++ "."
  show (SymbolNotDefined symPath xobj) =
    "Trying to refer to an undefined symbol '" ++ show symPath ++ "' at " ++ prettyInfoFromXObj xobj ++ "."
  show (InvalidObj Defn xobj) =
    "Invalid function definition at " ++ prettyInfoFromXObj xobj ++ "."
  show (InvalidObj If xobj) =
    "Invalid if-statement at " ++ prettyInfoFromXObj xobj ++ "."
  show (InvalidObj o xobj) =
    "Invalid obj '" ++ show o ++ "' at " ++ prettyInfoFromXObj xobj ++ "."
  show (WrongArgCount xobj) =
    "Wrong argument count in call to '" ++ getName xobj ++ "' at " ++ prettyInfoFromXObj xobj ++ "."
  show (NotAFunction xobj) =
    "Trying to call non-function '" ++ getName xobj ++ "' at " ++ prettyInfoFromXObj xobj ++ "."
  show (NoStatementsInDo xobj) =
    "The do-statement has no expressions inside of it at " ++ prettyInfoFromXObj xobj ++ "."
  show (TooManyFormsInBody xobj) =
    "The statement has too many expressions in body position " ++ prettyInfoFromXObj xobj ++ "."
  show (UnificationFailed constraint@(Constraint a b aObj bObj _) mappings constraints) =
    "Can't unify \n\n" ++ --show aObj ++ " WITH " ++ show bObj ++ "\n\n" ++ 
    "  " ++ pretty aObj ++ " : " ++ show (recursiveLookupTy mappings a) ++ " (" ++ prettyInfoFromXObj aObj ++ ")" ++
    "\n\nwith \n\n" ++
    "  " ++ pretty bObj ++ " : " ++ show (recursiveLookupTy mappings b) ++ " (" ++ prettyInfoFromXObj bObj ++ ")\n\n"
    -- ++
    -- "Constraint: " ++ show constraint ++ "\n\n" ++
    -- "All constraints:\n" ++ show constraints ++ "\n\n" ++
    -- "Mappings: \n" ++ show mappings ++ "\n\n"
  show (CantDisambiguate xobj originalName theType options) =
    "Can't disambiguate symbol '" ++ originalName ++ "' of type " ++ show theType ++ " at " ++ prettyInfoFromXObj xobj ++
    "\nPossibilities:\n    " ++ joinWith "\n    " (map (\(t, p) -> show p ++ " : " ++ show t) options)
  show (NoMatchingSignature xobj originalName theType options) =
    "Can't find matching lookup for symbol '" ++ originalName ++
    "' of type " ++ show theType ++ " at " ++ prettyInfoFromXObj xobj ++
    "\nNone of the possibilities have the correct signature:\n    " ++ joinWith
    "\n    " (map (\(t, p) -> show p ++ " : " ++ show t) options)
  show (LeadingColon xobj) =
    "Symbol '" ++ pretty xobj ++ "' starting with colon at " ++ prettyInfoFromXObj xobj ++ "."
  show (HolesFound holes) =
    "Holes found:\n\n    " ++ joinWith "\n    " (map (\(name, t) -> name ++ " : " ++ show t) holes) ++ "\n"
  show (FailedToExpand xobj (EvalError errorMessage)) =
    "Failed to expand at " ++ prettyInfoFromXObj xobj ++ ": " ++ errorMessage
  show (NotAValidType xobj) =
    "Not a valid type: " ++ pretty xobj ++ " at " ++ prettyInfoFromXObj xobj
  show (CantReturnRefTy xobj) =
    "Functions can't return references: '" ++ getName xobj ++ "' at " ++ prettyInfoFromXObj xobj
    
recursiveLookupTy :: TypeMappings -> Ty -> Ty
recursiveLookupTy mappings t = case t of
                                 (VarTy v) -> case recursiveLookup mappings v of
                                                Just ok -> ok
                                                Nothing -> t
                                 (RefTy r) -> RefTy (recursiveLookupTy mappings r)
                                 (PointerTy p) -> PointerTy (recursiveLookupTy mappings p)
                                 (StructTy n innerTys) -> StructTy n (map (recursiveLookupTy mappings) innerTys)
                                 (FuncTy argTys retTy) -> FuncTy (map (recursiveLookupTy mappings) argTys)
                                                                 (recursiveLookupTy mappings retTy)
                                 _ -> t

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
initialTypes :: Env -> XObj -> Either TypeError XObj
initialTypes rootEnv root = evalState (visit rootEnv root) 0
  where
    visit :: Env -> XObj -> State Integer (Either TypeError XObj)
    visit env xobj = case obj xobj of
                       (Num t _)          -> return (Right (xobj { ty = Just t }))
                       (Bol _)            -> return (Right (xobj { ty = Just BoolTy }))
                       (Str _)            -> return (Right (xobj { ty = Just StringTy }))
                       (Chr _)            -> return (Right (xobj { ty = Just CharTy }))
                       (Lst _)            -> visitList env xobj
                       (Arr _)            -> visitArray env xobj
                       (Sym symPath)      -> visitSymbol env xobj symPath
                       (MultiSym _ paths) -> visitMultiSym env xobj paths
                       Defn               -> return (Left (InvalidObj Defn xobj))
                       Def                -> return (Left (InvalidObj Def xobj))
                       Let                -> return (Left (InvalidObj Let xobj))
                       If                 -> return (Left (InvalidObj If xobj))
                       While              -> return (Left (InvalidObj While xobj))
                       Do                 -> return (Left (InvalidObj Do xobj))
                       (Mod _)            -> return (Left (InvalidObj If xobj))
                       Typ                -> return (Left (InvalidObj Typ xobj))
                       External           -> return (Left (InvalidObj External xobj))
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

    visitSymbol :: Env -> XObj -> SymPath -> State Integer (Either TypeError XObj)
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
            Nothing -> return (Left (SymbolNotDefined symPath xobj))

    visitMultiSym :: Env -> XObj -> [SymPath] -> State Integer (Either TypeError XObj)
    visitMultiSym _ xobj _ =
      do freshTy <- genVarTy
         return (Right xobj { ty = Just freshTy })

    visitArray :: Env -> XObj -> State Integer (Either TypeError XObj)
    visitArray env (XObj (Arr xobjs) i _) =
      do visited <- mapM (visit env) xobjs
         arrayVarTy <- genVarTy
         return $ do okVisited <- sequence visited
                     Right (XObj (Arr okVisited) i (Just (StructTy "Array" [arrayVarTy])))
         
    visitArray _ _ = compilerError "The function 'visitArray' only accepts XObj:s with arrays in them."
      
    visitList :: Env -> XObj -> State Integer (Either TypeError XObj)
    visitList env xobj@(XObj (Lst xobjs) i _) =
      case xobjs of
        -- Defn
        defn@(XObj Defn _ _) : nameSymbol@(XObj (Sym (SymPath _ name)) _ _) : (XObj (Arr argList) argsi argst) : body : [] ->
          do argTypes <- genVarTys (length argList)
             returnType <- genVarTy
             funcScopeEnv <- extendEnvWithParamList env argList
             let funcTy = Just (FuncTy argTypes returnType)
                 typedNameSymbol = nameSymbol { ty = funcTy }
                 -- This environment binding is for self-recursion, allows lookup of the symbol:
                 envWithSelf = extendEnv funcScopeEnv name typedNameSymbol 
             visitedBody <- visit envWithSelf body
             visitedArgs <- mapM (visit envWithSelf) argList
             return $ do okBody <- visitedBody
                         okArgs <- sequence visitedArgs
                         return (XObj (Lst [defn, nameSymbol, XObj (Arr okArgs) argsi argst, okBody]) i funcTy)
                 
        XObj Defn _ _ : _ -> return (Left (InvalidObj Defn xobj))

        -- Def
        def@(XObj Def _ _) : nameSymbol : expression : [] ->
          do definitionType <- genVarTy
             visitedExpr <- visit env expression
             return $ do okExpr <- visitedExpr
                         return (XObj (Lst [def, nameSymbol, okExpr]) i (Just definitionType))

        (XObj Def _ _) : _ -> return (Left (InvalidObj Def xobj))
        
        -- Let binding
        letExpr@(XObj Let _ _) : (XObj (Arr bindings) bindi bindt) : body : [] ->
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

        XObj Let _ _ : XObj (Arr _) _ _ : _ ->
          return (Left (TooManyFormsInBody xobj))
        XObj Let _ _ : _ ->
          return (Left (InvalidObj Let xobj))
                         
        -- If
        ifExpr@(XObj If _ _) : expr : ifTrue : ifFalse : [] ->
          do visitedExpr <- visit env expr
             visitedTrue <- visit env ifTrue
             visitedFalse <- visit env ifFalse
             returnType <- genVarTy
             return $ do okExpr <- visitedExpr
                         okTrue <- visitedTrue
                         okFalse <- visitedFalse
                         return (XObj (Lst [ifExpr, okExpr, okTrue, okFalse]) i (Just returnType))

        XObj If _ _ : _ -> return (Left (InvalidObj If xobj))

        -- While (always return Unit)
        whileExpr@(XObj While _ _) : expr : body : [] ->
          do visitedExpr <- visit env expr
             visitedBody <- visit env body
             return $ do okExpr <- visitedExpr
                         okBody <- visitedBody
                         return (XObj (Lst [whileExpr, okExpr, okBody]) i (Just UnitTy))

        XObj While _ _ : _ -> return (Left (InvalidObj While xobj))
        
        -- Do
        doExpr@(XObj Do _ _) : expressions ->
          do t <- genVarTy
             visitedExpressions <- fmap sequence (mapM (visit env) expressions)
             return $ do okExpressions <- visitedExpressions
                         return (XObj (Lst (doExpr : okExpressions)) i (Just t))

        -- Address
        addressExpr@(XObj Address _ _) : value : [] ->
          do visitedValue <- visit env value
             return $ do okValue <- visitedValue
                         let Just t' = ty okValue
                         return (XObj (Lst [addressExpr, okValue]) i (Just (PointerTy t')))

        -- Set!
        setExpr@(XObj SetBang _ _) : variable : value : [] ->
          do visitedVariable <- visit env variable
             visitedValue <- visit env value
             return $ do okVariable <- visitedVariable
                         okValue <- visitedValue
                         return (XObj (Lst (setExpr : okVariable : okValue : [])) i (Just UnitTy))
        XObj SetBang _ _ : _ -> return (Left (InvalidObj SetBang xobj))

        -- The
        theExpr@(XObj The _ _) : typeXObj : value : [] ->
          do visitedValue <- visit env value
             return $ do okValue <- visitedValue
                         case xobjToTy typeXObj of
                           Just okType -> return (XObj (Lst [theExpr, typeXObj, okValue]) i (Just okType))
                           Nothing -> error ("Not a type: " ++ show typeXObj)
        (XObj The _ _) : _ -> return (Left (InvalidObj The xobj))

        -- Ref
        refExpr@(XObj Ref _ _) : value : [] ->
          do visitedValue <- visit env value
             return $ do okValue <- visitedValue
                         let Just valueTy = ty okValue
                         return (XObj (Lst (refExpr : okValue : [])) i (Just (RefTy valueTy)))
     
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
             
    visitList _ _ = compilerError "Must match on list!"

    extendEnvWithLetBindings :: Env -> [XObj] -> State Integer (Either TypeError Env)
    extendEnvWithLetBindings env xobjs =
      let pairs = pairwise xobjs
          emptyInnerEnv = Env { envBindings = Map.fromList []
                              , envParent = Just env
                              , envModuleName = Nothing
                              , envImports = []
                              , envMode = InternalEnv
                              }
      -- Need to fold (rather than map) to make the previous bindings accesible to the later ones, i.e. (let [a 100 b a] ...)
      in  foldM createBinderForLetPair (Right emptyInnerEnv) pairs
      where
        createBinderForLetPair :: Either TypeError Env -> (XObj, XObj) -> State Integer (Either TypeError Env)
        createBinderForLetPair envOrErr (sym, expr) =
          case envOrErr of
            Left err -> return (Left err)
            Right env' ->
              case obj sym of
                (Sym (SymPath _ name)) -> do visited <- visit env' expr
                                             return $ do okVisited <- visited
                                                         return (envAddBinding env' name (Binder okVisited))
                _ -> error ("Can't create let-binder for non-symbol: " ++ show sym)

    extendEnvWithParamList :: Env -> [XObj] -> State Integer Env
    extendEnvWithParamList env xobjs =
      do binders <- mapM createBinderForParam xobjs
         return Env { envBindings = Map.fromList binders
                    , envParent = Just env
                    , envModuleName = Nothing
                    , envImports = []
                    , envMode = InternalEnv
                    }
      where
        createBinderForParam :: XObj -> State Integer (String, Binder)
        createBinderForParam xobj =
          case obj xobj of
            (Sym (SymPath _ name)) -> do t <- genVarTy
                                         let xobjWithTy = xobj { ty = Just t }
                                         return (name, Binder xobjWithTy)
            _ -> error "Can't create binder for non-symbol parameter."

genConstraints :: XObj -> Either TypeError [Constraint]
genConstraints root = fmap sort (gen root)
  where gen xobj =
          case obj xobj of
            (Lst lst) -> case lst of
                           -- Defn
                           (XObj Defn _ _) : _ : (XObj (Arr args) _ _) : body : [] ->
                             do insideBodyConstraints <- gen body
                                xobjType <- toEither (ty xobj) (DefnMissingType xobj)
                                bodyType <- toEither (ty body) (ExpressionMissingType xobj)
                                let (FuncTy argTys retTy) = xobjType
                                    bodyConstr = Constraint retTy bodyType xobj body OrdDefnBody
                                    argConstrs = zipWith3 (\a b aObj -> Constraint a b aObj xobj OrdArg) (map forceTy args) argTys args
                                return (bodyConstr : argConstrs ++ insideBodyConstraints)

                           -- Def
                           (XObj Def _ _) : _ : expr : [] ->
                             do insideExprConstraints <- gen expr
                                xobjType <- toEither (ty xobj) (DefMissingType xobj)
                                exprType <- toEither (ty expr) (ExpressionMissingType xobj)
                                let defConstraint = Constraint xobjType exprType xobj expr OrdDefExpr
                                return (defConstraint : insideExprConstraints)

                           -- Let
                           XObj Let _ _ : XObj (Arr bindings) _ _ : body : [] ->
                             do insideBodyConstraints <- gen body
                                insideBindingsConstraints <- fmap join (mapM gen bindings)
                                bodyType <- toEither (ty body) (ExpressionMissingType body)                                
                                let Just xobjTy = ty xobj
                                    wholeStatementConstraint = Constraint bodyType xobjTy body xobj OrdLetBody
                                    bindingsConstraints = zipWith (\(symTy, exprTy) (symObj, exprObj) ->
                                                                     Constraint symTy exprTy symObj exprObj OrdLetBind)
                                                                  (map (\(a, b) -> (forceTy a, forceTy b)) (pairwise bindings))
                                                                  (pairwise bindings)
                                return (wholeStatementConstraint : insideBodyConstraints ++
                                        bindingsConstraints ++ insideBindingsConstraints)
                           
                           -- If
                           XObj If _ _ : expr : ifTrue : ifFalse : [] ->
                             do insideConditionConstraints <- gen expr
                                insideTrueConstraints <- gen ifTrue
                                insideFalseConstraints <- gen ifFalse
                                exprType <- toEither (ty expr) (ExpressionMissingType expr)
                                trueType <- toEither (ty ifTrue) (ExpressionMissingType ifTrue)
                                falseType <- toEither (ty ifFalse) (ExpressionMissingType ifFalse)
                                let expected = XObj (Sym (SymPath [] "condition in if-value")) (info xobj) (ty xobj)
                                    conditionConstraint = Constraint exprType BoolTy expr expected OrdIfCondition
                                    sameReturnConstraint = Constraint trueType falseType ifTrue ifFalse OrdIfReturn
                                    Just t = ty xobj
                                    wholeStatementConstraint = Constraint trueType t ifTrue xobj OrdIfWhole
                                return (conditionConstraint : sameReturnConstraint :
                                        wholeStatementConstraint : insideConditionConstraints ++
                                        insideTrueConstraints ++ insideFalseConstraints)

                           -- While
                           XObj While _ _ : expr : body : [] ->
                             do insideConditionConstraints <- gen expr
                                insideBodyConstraints <- gen body
                                exprType <- toEither (ty expr) (ExpressionMissingType expr)
                                bodyType <- toEither (ty body) (ExpressionMissingType body)                                
                                let expectedCond = XObj (Sym (SymPath [] "condition in while-expression")) (info xobj) (ty xobj)
                                    expectedBody = XObj (Sym (SymPath [] "body in while-expression")) (info xobj) (ty xobj)
                                    conditionConstraint = Constraint exprType BoolTy expr expectedCond OrdWhileCondition
                                    wholeStatementConstraint = Constraint bodyType UnitTy body expectedBody OrdWhileBody
                                return (conditionConstraint : wholeStatementConstraint :
                                        insideConditionConstraints ++ insideBodyConstraints)
                                  
                           -- Do
                           XObj Do _ _ : expressions ->
                             case expressions of
                               [] -> Left (NoStatementsInDo xobj)
                               _ -> let lastExpr = last expressions
                                    in do insideExpressionsConstraints <- fmap join (mapM gen expressions)
                                          xobjType <- toEither (ty xobj) (DefMissingType xobj)
                                          lastExprType <- toEither (ty lastExpr) (ExpressionMissingType xobj)
                                          let retConstraint = Constraint xobjType lastExprType xobj lastExpr OrdDoReturn
                                              must = XObj (Sym (SymPath [] "statement in do-expression")) (info xobj) (ty xobj)
                                              mkConstr x@(XObj _ _ (Just t)) = Just (Constraint t UnitTy x must OrdDoStatement)
                                              mkConstr _ = Nothing
                                              expressionsShouldReturnUnit = mapMaybe mkConstr (init expressions)
                                          return (retConstraint : insideExpressionsConstraints ++ expressionsShouldReturnUnit)

                           -- Address
                           XObj Address _ _ : value : [] ->
                             gen value

                           -- Set!
                           XObj SetBang _ _ : variable : value : [] ->
                             do insideValueConstraints <- gen value
                                variableType <- toEither (ty variable) (ExpressionMissingType variable)
                                valueType <- toEither (ty value) (ExpressionMissingType value)
                                let sameTypeConstraint = Constraint variableType valueType variable value OrdSetBang
                                return (sameTypeConstraint : insideValueConstraints)

                           -- The
                           XObj The _ _ : _ : value : [] ->
                             do insideValueConstraints <- gen value
                                xobjType <- toEither (ty xobj) (DefMissingType xobj)
                                valueType <- toEither (ty value) (DefMissingType value)
                                let theTheConstraint = Constraint xobjType valueType xobj value OrdThe
                                return (theTheConstraint : insideValueConstraints)

                           -- Ref
                           XObj Ref _ _ : value : [] ->
                             gen value
                             
                           -- Function application
                           func : args ->
                             do insideArgsConstraints <- fmap join (mapM gen args)
                                funcTy <- toEither (ty func) (ExpressionMissingType func)
                                case funcTy of
                                  (FuncTy argTys retTy) ->
                                    if length args /= length argTys then
                                      Left (WrongArgCount func)
                                    else
                                      let expected = XObj (Sym (SymPath [] ("expected argument to '" ++ getName func ++ "'")))
                                                          (info func) Nothing
                                          argConstraints = zipWith3 (\a t aObj -> Constraint a t aObj expected OrdFuncAppArg)
                                                                    (map forceTy args)
                                                                    argTys
                                                                    args
                                          Just xobjTy = ty xobj
                                          retConstraint = Constraint xobjTy retTy xobj func OrdFuncAppRet
                                      in  return (retConstraint : argConstraints ++ insideArgsConstraints)
                                  funcVarTy@(VarTy _) ->
                                    let fabricatedFunctionType = FuncTy (map forceTy args) (forceTy xobj)
                                        expected = XObj (Sym (SymPath [] ("calling '" ++ getName func ++ "'"))) (info func) Nothing
                                        wholeTypeConstraint = Constraint funcVarTy fabricatedFunctionType func expected OrdFuncAppVarTy
                                    in  return (wholeTypeConstraint : insideArgsConstraints)
                                  _ -> Left (NotAFunction func)
                               
                           -- Empty list
                           [] -> Right []

            (Arr arr) ->
              case arr of
                [] -> Right []
                x:xs -> do insideExprConstraints <- fmap join (mapM gen arr)
                           let Just headTy = ty x
                               Just (StructTy "Array" [t]) = ty xobj
                               betweenExprConstraints = map (\o -> Constraint headTy (forceTy o) x o OrdArrBetween) xs
                               headConstraint = Constraint headTy t x xobj OrdArrHead
                           return (headConstraint : insideExprConstraints ++ betweenExprConstraints)              
                           
            _ -> Right []

-- | Unsafe way of getting the type from an XObj
forceTy :: XObj -> Ty
forceTy xobj = case ty xobj of
                 Just t -> t
                 Nothing -> error ("No type in " ++ show xobj)

-- | Walk the whole expression tree and replace all occurences of VarTy with their corresponding actual type.
assignTypes :: TypeMappings -> XObj -> XObj
assignTypes mappings root = visit root
  where
    visit xobj =
      case obj xobj of
        (Lst _) -> visitList xobj
        (Arr _) -> visitArray xobj
        _ -> assignType xobj

    visitList (XObj (Lst xobjs) i t) =
      let visited = map (assignTypes mappings) xobjs
          xobj' = XObj (Lst visited) i t
      in  assignType xobj'
    visitList _ = compilerError "The function 'visitList' only accepts XObjs with lists in them."

    visitArray (XObj (Arr xobjs) i t) =
      let visited = map (assignTypes mappings) xobjs
          xobj' = XObj (Arr visited) i t
      in  assignType xobj'
    visitArray _ = compilerError "The function 'visitArray' only accepts XObjs with arrays in them."

    assignType :: XObj -> XObj
    assignType xobj = case ty xobj of
      Just startingType -> xobj { ty = Just (replaceTyVars mappings startingType) }
      Nothing -> xobj

-- | This function performs two things:
--   1. Finds out which polymorphic functions that needs to be added to the environment for the calls in the function to work.
--   2. Changes the name of symbols at call sites so they use the polymorphic name
--   Both of these results are returned in a tuple: (<new xobj>, <dependencies>)
concretizeXObj :: Bool -> Env -> Env -> XObj -> Either TypeError (XObj, [XObj])
concretizeXObj allowAmbiguity typeEnv rootEnv root =
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
                                               modify ((insideArrayDeleteDeps typeEnv env t) ++ )
                                               modify ((defineArrayTypeAlias t) : )
                                               return $ do okVisited <- visited
                                                           Right (XObj (Arr okVisited) i (Just t))
    visit _ x = return (Right x)

    visitList :: Env -> [XObj] -> State [XObj] (Either TypeError [XObj])
    visitList _ [] = return (Right [])

    visitList env (defn@(XObj Defn _ _) : nameSymbol : args@(XObj (Arr argsArr) _ _) : body : []) =
      do mapM_ checkForNeedOfTypedefs argsArr
         let functionEnv = Env Map.empty (Just env) Nothing [] InternalEnv
             envWithArgs = foldl' (\e arg@(XObj (Sym (SymPath _ argSymName)) _ _) ->
                                     extendEnv e argSymName arg)
                                  functionEnv argsArr
         visitedBody <- (visit envWithArgs) body
         return $ do okBody <- visitedBody
                     return [defn, nameSymbol, args, okBody]        
    visitList env (letExpr@(XObj Let _ _) : (XObj (Arr bindings) bindi bindt) : body : []) =
      do visitedBindings <- fmap sequence (mapM (visit env) bindings)
         visitedBody <- (visit env) body         
         return $ do okVisitedBindings <- visitedBindings
                     okVisitedBody <- visitedBody
                     return [letExpr, XObj (Arr okVisitedBindings) bindi bindt, okVisitedBody]
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
            in if --(trace $ "CHECKING " ++ getName xobj ++ " : " ++ show theType ++ " with visited type " ++ show t) $
                  typeIsGeneric theType && not (typeIsGeneric typeOfVisited)
                  then case concretizeDefinition allowAmbiguity typeEnv env theXObj typeOfVisited of
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
    visitMultiSym env xobj@(XObj (MultiSym originalSymbolName paths) i t) =
      let Just actualType = t
          tys = map (typeFromPath env) paths
          tysToPathsDict = zip tys paths
      in  case filter (matchingSignature actualType) tysToPathsDict of
            [] -> return (Left (NoMatchingSignature xobj originalSymbolName actualType tysToPathsDict))
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
concretizeDefinition :: Bool -> Env -> Env -> XObj -> Ty -> Either TypeError (XObj, [XObj])
concretizeDefinition allowAmbiguity typeEnv globalEnv definition concreteType =
  let SymPath pathStrings name = getPath definition
      Just polyType = ty definition
      suffix = polymorphicSuffix polyType concreteType
      newPath = SymPath pathStrings (name ++ suffix)
  in
    case definition of
      XObj (Lst ((XObj Defn _ _) : _)) _ _ ->
        let withNewPath = setPath definition newPath
            mappings = unifySignatures polyType concreteType
            typed = assignTypes mappings withNewPath
        in  do (concrete, deps) <- concretizeXObj allowAmbiguity typeEnv globalEnv typed
               managed <- manageMemory typeEnv globalEnv concrete
               return (managed, deps)
      XObj (Lst ((XObj (Deftemplate (TemplateCreator templateCreator)) _ _) : _)) _ _ ->
        let template = templateCreator typeEnv globalEnv
        in  Right (instantiateTemplate newPath concreteType template)
      XObj (Lst ((XObj External _ _) : _ : [])) _ _ ->
        if name == "NULL"
        then Right (definition, []) -- A hack to make all versions of NULL have the same name
        else let withNewPath = setPath definition newPath
                 withNewType = withNewPath { ty = Just concreteType }
             in  Right (withNewType, [])
      err ->
        compilerError ("Can't concretize " ++ show err ++ ": " ++ pretty definition)

type MemState = Set.Set Deleter

manageMemory :: Env -> Env -> XObj -> Either TypeError XObj
manageMemory typeEnv globalEnv root =
  let (finalObj, deleteThese) = runState (visit root) (Set.fromList [])
  in  -- (trace ("Delete these: " ++ joinWithComma (map show (Set.toList deleteThese)))) $
      case finalObj of
        Left err -> Left err
        Right ok -> let newInfo = fmap (\i -> i { infoDelete = deleteThese }) (info ok)
                    in  Right $ ok { info = newInfo }
                                          
  where visit :: XObj -> State MemState (Either TypeError XObj)
        visit xobj =
          case obj xobj of
            Lst _ -> visitList xobj
            Arr _ -> visitArray xobj
            Str _ -> do manage xobj
                        return (Right xobj)
            _ -> do return (Right xobj)

        visitArray :: XObj -> State MemState (Either TypeError XObj)
        visitArray xobj@(XObj (Arr arr) _ _) =
          do mapM_ visit arr
             mapM_ unmanage arr
             _ <- manage xobj
             return (Right xobj)

        visitArray _ = error "Must visit array."

        visitList :: XObj -> State MemState (Either TypeError XObj)
        visitList xobj@(XObj (Lst lst) i t) =
          case lst of
            defn@(XObj Defn _ _) : nameSymbol@(XObj (Sym _) _ _) : args@(XObj (Arr argList) _ _) : body : [] ->
              do mapM_ manage argList
                 visitedBody <- visit body
                 unmanage body
                 return $ do okBody <- visitedBody                             
                             return (XObj (Lst (defn : nameSymbol : args : okBody : [])) i t)
            letExpr@(XObj Let _ _) : (XObj (Arr bindings) bindi bindt) : body : [] ->
              do preDeleters <- get
                 visitedBindings <- mapM visitLetBinding (pairwise bindings)
                 visitedBody <- visit body
                 unmanage body
                 postDeleters <- get
                 let diff = postDeleters Set.\\ preDeleters
                     newInfo = setDeletersOnInfo i diff
                     survivors = (postDeleters Set.\\ diff) -- Same as just pre deleters, right?!
                 put survivors
                 --trace ("LET Pre: " ++ show preDeleters ++ "\nPost: " ++ show postDeleters ++ "\nDiff: " ++ show diff ++ "\nSurvivors: " ++ show survivors)
                 manage xobj
                 return $ do okBody <- visitedBody
                             okBindings <- fmap (concatMap (\(n,x) -> [n, x])) (sequence visitedBindings)
                             return (XObj (Lst (letExpr : (XObj (Arr okBindings) bindi bindt) : okBody : [])) newInfo t)
            addressExpr@(XObj Address _ _) : value : [] ->
              do visitedValue <- visit value
                 return $ do okValue <- visitedValue
                             return (XObj (Lst (addressExpr : okValue : [])) i t)
            theExpr@(XObj The _ _) : typeXObj : value : [] ->
              do visitedValue <- visit value
                 transferOwnership value xobj
                 return $ do okValue <- visitedValue
                             return (XObj (Lst (theExpr : typeXObj : okValue : [])) i t)
            refExpr@(XObj Ref _ _) : value : [] ->
              do visitedValue <- visit value
                 refCheck value
                 return $ do okValue <- visitedValue
                             return (XObj (Lst (refExpr : okValue : [])) i t)
            doExpr@(XObj Do _ _) : expressions ->
              do visitedExpressions <- mapM visit expressions
                 transferOwnership (last expressions) xobj
                 return $ do okExpressions <- sequence visitedExpressions
                             return (XObj (Lst (doExpr : okExpressions)) i t)
            whileExpr@(XObj While _ _) : expr : body : [] ->
              do preDeleters <- get
                 visitedExpr <- visit expr
                 visitedBody <- visit body
                 manage body
                 postDeleters <- get
                 -- Visit an extra time to simulate repeated use
                 _ <- visit expr
                 _ <- visit body
                 let diff = postDeleters Set.\\ preDeleters
                 put (postDeleters Set.\\ diff) -- Same as just pre deleters, right?!
                 return $ do okExpr <- visitedExpr
                             okBody <- visitedBody
                             let newInfo = setDeletersOnInfo i diff
                             return (XObj (Lst (whileExpr : okExpr : okBody : [])) newInfo t)
              
            ifExpr@(XObj If _ _) : expr : ifTrue : ifFalse : [] ->
              do visitedExpr <- visit expr
                 deleters <- get
                 
                 let (visitedTrue,  stillAliveTrue)  = runState (do { v <- visit ifTrue;
                                                                      transferOwnership ifTrue xobj;
                                                                      return v
                                                                    })
                                                       deleters
                                                       
                     (visitedFalse, stillAliveFalse) = runState (do { v <- visit ifFalse;
                                                                      transferOwnership ifFalse xobj;
                                                                      return v
                                                                    })
                                                       deleters

                 let removeTrue  = stillAliveTrue
                     removeFalse = stillAliveFalse
                     deletedInTrue  = deleters Set.\\ removeTrue
                     deletedInFalse = deleters Set.\\ removeFalse
                     common = Set.intersection deletedInTrue deletedInFalse
                     delsTrue  = deletedInFalse Set.\\ common
                     delsFalse = deletedInTrue  Set.\\ common
                     stillAlive = deleters Set.\\ (Set.union deletedInTrue deletedInFalse)

                 put stillAlive
                 manage xobj
                     
                 return $ do okExpr  <- visitedExpr
                             okTrue  <- visitedTrue
                             okFalse <- visitedFalse
                             return (XObj (Lst (ifExpr : okExpr : (del okTrue delsTrue) : (del okFalse delsFalse) : [])) i t)
            f : args ->
              do _ <- visit f
                 mapM_ visitArg args
                 manage xobj
                 return (Right (XObj (Lst (f : args)) i t))

            [] -> return (Right xobj)              
        visitList _ = error "Must visit list."

        visitLetBinding :: (XObj, XObj) -> State MemState (Either TypeError (XObj, XObj))
        visitLetBinding (name, expr) =
          do visitedExpr <- visit expr
             transferOwnership expr name
             return $ do okExpr <- visitedExpr
                         return (name, okExpr)

        visitArg :: XObj -> State MemState (Either TypeError XObj)
        visitArg xobj@(XObj _ _ (Just t)) =
          if isManaged t
          then do visitedXObj <- visit xobj
                  unmanage xobj
                  return $ do okXObj <- visitedXObj
                              return okXObj
          else do --(trace ("Ignoring arg " ++ show xobj ++ " because it's not managed."))
                    (visit xobj)
        visitArg xobj@(XObj _ _ _) =
          visit xobj

        createDeleter :: XObj -> Maybe Deleter
        createDeleter xobj =
          case ty xobj of
            Just t -> let var = varOfXObj xobj
                      in  if isManaged t && not (isExternalType typeEnv t)
                          then case nameOfPolymorphicFunction globalEnv t "delete" of
                                 Just pathOfDeleteFunc -> Just (ProperDeleter pathOfDeleteFunc var)
                                 Nothing -> --trace ("Found no delete function for " ++ var ++ " : " ++ (showMaybeTy (ty xobj)))
                                            Just (FakeDeleter var)
                          else Nothing
            Nothing -> error ("No type, can't manage " ++ show xobj)
          
        manage :: XObj -> State MemState ()
        manage xobj =
          case createDeleter xobj of
            Just deleter -> modify (Set.insert deleter)
            Nothing -> return ()

        deletersMatchingXObj :: XObj -> Set.Set Deleter -> [Deleter]
        deletersMatchingXObj xobj deleters =
          let var = varOfXObj xobj
          in  Set.toList $ Set.filter (\d -> case d of
                                               ProperDeleter { deleterVariable = dv } -> dv == var
                                               FakeDeleter   { deleterVariable = dv } -> dv == var)
                                      deleters

        unmanage :: XObj -> State MemState ()
        unmanage xobj =
          let Just t = ty xobj 
              Just i = info xobj
          in if isManaged t && not (isExternalType typeEnv t)
             then do deleters <- get
                     case deletersMatchingXObj xobj deleters of
                       [] -> trace ("Trying to use '" ++ getName xobj ++ "' (expression " ++ freshVar i ++ ") at " ++ prettyInfoFromXObj xobj ++
                                    " but it has already been given away.") (return ())
                       [one] -> let newDeleters = Set.delete one deleters
                                in  put newDeleters
                       _ -> error "Too many variables with the same name in set."                                  
             else return ()

        -- | Check that the value being referenced hasn't already been given away
        refCheck :: XObj -> State MemState ()
        refCheck xobj =
          let Just i = info xobj
              Just t = ty xobj
          in if isManaged t && not (isExternalType typeEnv t)
             then do deleters <- get
                     case deletersMatchingXObj xobj deleters of
                       [] -> trace ("Trying to get reference from '" ++ getName xobj ++
                                    "' (expression " ++ freshVar i ++ ") at " ++ prettyInfoFromXObj xobj ++
                                    " but it has already been given away.") (return ())
                       [_] -> return ()
                       _ -> error "Too many variables with the same name in set."                                  
             else return ()

        transferOwnership :: XObj -> XObj -> State MemState ()
        transferOwnership from to =
          do unmanage from
             manage to
             --trace ("Transfered from " ++ getName from ++ " '" ++ varOfXObj from ++ "' to " ++ getName to ++ " '" ++ varOfXObj to ++ "'") $ return ()

        varOfXObj :: XObj -> String
        varOfXObj xobj =
          case xobj of
            XObj (Sym (SymPath [] name)) _ _ -> name
            _ -> let Just i = info xobj
                 in  freshVar i

setDeletersOnInfo :: Maybe Info -> Set.Set Deleter -> Maybe Info
setDeletersOnInfo i deleters = fmap (\i' -> i' { infoDelete = deleters }) i

del :: XObj -> Set.Set Deleter -> XObj
del xobj deleters = xobj { info = (setDeletersOnInfo (info xobj) deleters) }

isExternalType :: Env -> Ty -> Bool
isExternalType typeEnv (StructTy name _) =
  case lookupInEnv (SymPath [] name) typeEnv of
    Just (_, Binder (XObj (Lst (XObj ExternalType _ _ : _)) _ _)) -> True
    Just _ -> False
    Nothing -> False
isExternalType _ _ =
  False

nameOfPolymorphicFunction :: Env -> Ty -> String -> Maybe SymPath
nameOfPolymorphicFunction env t lookupName
  | isManaged t =
    case filter ((\(Just t') -> areUnifiable (FuncTy [t] UnitTy) t') . ty . binderXObj . snd) (multiLookupALL lookupName env) of
      [] -> Nothing
      [(_, Binder single)] ->
        let Just t' = ty single
            (SymPath pathStrings name) = getPath single
            suffix = polymorphicSuffix t' (FuncTy [t] UnitTy)
            concretizedPath = SymPath pathStrings (name ++ suffix)
        in  Just concretizedPath
      _ -> Nothing
  | otherwise   = Nothing

-- depsOfPolymorphicFunction :: Env -> Ty -> String -> [XObj]
-- depsOfPolymorphicFunction env t name
--   | isManaged t =
--     case filter ((\(Just t') -> (areUnifiable (FuncTy [t] UnitTy) t')) . ty . binderXObj . snd) (multiLookupALL name env) of
--       [] -> (trace $ "No dependency found for " ++ show t) []
--       [(_, Binder (XObj (Lst ((XObj (Instantiate _) _ _) : _)) _ _))] ->
--         []
--       [(_, Binder single)] ->
--         case concretizeDefinition False env single (FuncTy [t] (UnitTy)) of
--           Left err -> error (show err)
--           Right (ok, deps) -> (ok : deps)
--       _ -> (trace $ "Too many dependencies found for " ++ show t) []
--   | otherwise = []

insideArrayDeleteDeps :: Env -> Env -> Ty -> [XObj]
insideArrayDeleteDeps typeEnv env t
  | isManaged t =
    case filter ((\(Just t') -> (areUnifiable (FuncTy [t] UnitTy) t')) . ty . binderXObj . snd) (multiLookupALL "delete" env) of
      [] -> --(trace $ "No 'delete' function found for " ++ show t)
        []
      [(_, Binder (XObj (Lst ((XObj (Instantiate _) _ _) : _)) _ _))] ->
        []
      [(_, Binder single)] ->
        case concretizeDefinition False typeEnv env single (FuncTy [t] (UnitTy)) of
          Left err -> error (show err)
          Right (ok, deps) -> (ok : deps)
      _ -> (trace $ "Too many 'delete' functions found for " ++ show t) []
  | otherwise = []

-- TODO: merge with "insideArrayDeleteDeps"
insideArrayCopyDeps :: Env -> Env -> Ty -> [XObj]
insideArrayCopyDeps typeEnv env t
  | isManaged t =
    case filter ((\(Just t') -> (areUnifiable (FuncTy [(RefTy t)] t) t')) . ty . binderXObj . snd) (multiLookupALL "copy" env) of
      [] -> --(trace $ "No 'copy' function found for " ++ show t)
        []
      [(_, Binder (XObj (Lst ((XObj (Instantiate _) _ _) : _)) _ _))] ->
        []
      [(_, Binder single)] ->
        case concretizeDefinition False typeEnv env single (FuncTy [(RefTy t)] t) of
          Left err -> error (show err)
          Right (ok, deps) -> (ok : deps)
      _ -> (trace $ "Too many 'copy' functions found for " ++ show t) []
  | otherwise = []
                
-- | Convert from the type 'UnificationFailure' to 'TypeError' (enables monadic chaining of Either).
solveConstraintsAndConvertErrorIfNeeded :: [Constraint] -> Either TypeError TypeMappings
solveConstraintsAndConvertErrorIfNeeded constraints =
  case solve constraints of
    Left failure@(UnificationFailure _ _) -> Left (UnificationFailed
                                                   (unificationFailure failure)
                                                   (unificationMappings failure)
                                                   constraints)
    Left (Holes holes) -> Left (HolesFound holes)
    Right ok -> Right ok

check :: XObj -> Either TypeError ()
check xobj@(XObj (Lst (XObj Defn _ _ : _)) _ t) =
  case t of
    Just (FuncTy _ (RefTy _)) -> Left (CantReturnRefTy xobj)
    Just _ -> return ()
    Nothing -> Left (DefnMissingType xobj)
check _ = return ()

annotateOne :: Env -> Env -> XObj -> Bool -> Either TypeError (XObj, [XObj])
annotateOne typeEnv env xobj allowAmbiguity = do
  constraints <- genConstraints xobj
  mappings <- solveConstraintsAndConvertErrorIfNeeded constraints
  let typed = assignTypes mappings xobj
  concretizeXObj allowAmbiguity typeEnv env typed
  
-- | Performs all the steps of creating initial types, solving constraints and assigning the types.
-- | Returns a list of all the bindings that need to be added for the new form to work.
-- | The concretization of MultiSym:s (= ambiguous use of symbols, resolved by type usage)
-- | makes it possible to solve more types so let's do it several times.  
annotate :: Env -> Env -> XObj -> Either TypeError [XObj]
annotate typeEnv globalEnv xobj =
  do initiated <- initialTypes globalEnv xobj
     (annotated, dependencies) <- foldM (\(x, deps) allowAmbiguity ->
                                           do (x', deps') <- annotateOne typeEnv globalEnv x allowAmbiguity
                                              return (x', deps ++ deps'))
                                  (initiated, [])
                                  [True, False]
     final <- manageMemory typeEnv globalEnv annotated
     check final
     _ <- mapM check dependencies
     return (final : dependencies)

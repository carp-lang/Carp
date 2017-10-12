module ManageMemory (manageMemory) where

import Control.Monad.State
import qualified Data.Set as Set
import Debug.Trace

import Types
import Obj
import Constraints
import Util
import TypeError
import Polymorphism

-- | Assign a set of Deleters to the 'infoDelete' field on Info.
setDeletersOnInfo :: Maybe Info -> Set.Set Deleter -> Maybe Info
setDeletersOnInfo i deleters = fmap (\i' -> i' { infoDelete = deleters }) i

-- | Helper function for setting the deleters for an XObj.
del :: XObj -> Set.Set Deleter -> XObj
del xobj deleters = xobj { info = (setDeletersOnInfo (info xobj) deleters) }

-- | To keep track of the deleters when recursively walking the form.
type MemState = Set.Set Deleter

-- | Find out what deleters are needed and where in an XObj.
-- | Deleters will be added to the info field on XObj so that
-- | the code emitter can access them and insert calls to destructors.
manageMemory :: TypeEnv -> Env -> XObj -> Either TypeError XObj
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
            defn@(XObj Defn _ _) : nameSymbol@(XObj (Sym _) _ _) : args@(XObj (Arr argList) _ _) : body : [] ->
              let Just funcTy@(FuncTy _ defnReturnType) = t
              in case defnReturnType of
                   RefTy _ ->
                     return (Left (FunctionsCantReturnRefTy xobj funcTy))
                   _ ->
                     do mapM_ manage argList
                        visitedBody <- visit body
                        result <- unmanage body
                        return $
                          case result of
                            Left e -> Left e
                            Right _ ->
                              do okBody <- visitedBody
                                 return (XObj (Lst (defn : nameSymbol : args : okBody : [])) i t)
            letExpr@(XObj Let _ _) : (XObj (Arr bindings) bindi bindt) : body : [] ->
              let Just letReturnType = t
              in case letReturnType of
                RefTy _ ->
                  return (Left (LetCantReturnRefTy xobj letReturnType))
                _ ->
                  do preDeleters <- get
                     visitedBindings <- mapM visitLetBinding (pairwise bindings)
                     visitedBody <- visit body
                     result <- unmanage body
                     case result of
                       Left e -> return (Left e)
                       Right _ ->
                         do postDeleters <- get
                            let diff = postDeleters Set.\\ preDeleters
                                newInfo = setDeletersOnInfo i diff
                                survivors = (postDeleters Set.\\ diff) -- Same as just pre deleters, right?!
                            put survivors
                            --trace ("LET Pre: " ++ show preDeleters ++ "\nPost: " ++ show postDeleters ++ "\nDiff: " ++ show diff ++ "\nSurvivors: " ++ show survivors)
                            manage xobj
                            return $ do okBody <- visitedBody
                                        okBindings <- fmap (concatMap (\(n,x) -> [n, x])) (sequence visitedBindings)
                                        return (XObj (Lst (letExpr : (XObj (Arr okBindings) bindi bindt) : okBody : [])) newInfo t)
            setbangExpr@(XObj SetBang _ _) : variable : value : [] ->
              do visitedValue <- visit value
                 unmanage value
                 let varInfo = info variable
                     correctVariable = case variable of
                                         XObj (Lst (XObj Ref _ _ : x : _)) _ _ -> x -- Peek inside the ref to get the actual variable to set
                                         x -> x
                     deleters = case createDeleter correctVariable of
                                  Just d  -> Set.fromList [d]
                                  Nothing -> Set.empty
                     newVarInfo = setDeletersOnInfo varInfo deleters
                     newVariable = variable { info = newVarInfo }
                 return $ do okValue <- visitedValue
                             return (XObj (Lst (setbangExpr : newVariable : okValue : [])) i t)
            addressExpr@(XObj Address _ _) : value : [] ->
              do visitedValue <- visit value
                 return $ do okValue <- visitedValue
                             return (XObj (Lst (addressExpr : okValue : [])) i t)
            theExpr@(XObj The _ _) : typeXObj : value : [] ->
              do visitedValue <- visit value
                 result <- transferOwnership value xobj
                 return $ case result of
                            Left e -> Left e
                            Right _ -> do okValue <- visitedValue
                                          return (XObj (Lst (theExpr : typeXObj : okValue : [])) i t)
            refExpr@(XObj Ref _ _) : value : [] ->
              do visitedValue <- visit value
                 case visitedValue of
                   Left e -> return (Left e)
                   Right visitedValue ->
                     do checkResult <- refCheck visitedValue
                        case checkResult of
                          Left e -> return (Left e)
                          Right () -> return $ Right (XObj (Lst (refExpr : visitedValue : [])) i t)
            doExpr@(XObj Do _ _) : expressions ->
              do visitedExpressions <- mapM visit expressions
                 result <- transferOwnership (last expressions) xobj
                 return $ case result of
                            Left e -> Left e
                            Right _ -> do okExpressions <- sequence visitedExpressions
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
                                                                      result <- transferOwnership ifTrue xobj;
                                                                      return $ case result of
                                                                                 Left e -> Left e
                                                                                 Right _ -> v
                                                                    })
                                                       deleters
                                                       
                     (visitedFalse, stillAliveFalse) = runState (do { v <- visit ifFalse;
                                                                      result <- transferOwnership ifFalse xobj;
                                                                      return $ case result of
                                                                                 Left e -> Left e
                                                                                 Right _ -> v
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
              do visitedF <- visit f
                 visitedArgs <- fmap sequence $ mapM visitArg args
                 manage xobj
                 return $ do okF <- visitedF
                             okArgs <- visitedArgs
                             (Right (XObj (Lst (okF : okArgs)) i t))

            [] -> return (Right xobj)              
        visitList _ = error "Must visit list."

        visitLetBinding :: (XObj, XObj) -> State MemState (Either TypeError (XObj, XObj))
        visitLetBinding (name, expr) =
          do visitedExpr <- visit expr
             result <- transferOwnership expr name
             return $ case result of
                        Left e -> Left e
                        Right _ -> do okExpr <- visitedExpr
                                      return (name, okExpr)

        visitArg :: XObj -> State MemState (Either TypeError XObj)
        visitArg xobj@(XObj _ _ (Just t)) =
          if isManaged typeEnv t
          then do visitedXObj <- visit xobj
                  result <- unmanage xobj
                  case result of
                    Left e  -> return (Left e)
                    Right _ -> return $ do okXObj <- visitedXObj
                                           return okXObj
          else do --(trace ("Ignoring arg " ++ show xobj ++ " because it's not managed."))
                    (visit xobj)
        visitArg xobj@(XObj _ _ _) =
          visit xobj

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

        unmanage :: XObj -> State MemState (Either TypeError ())
        unmanage xobj =
          let Just t = ty xobj 
              Just i = info xobj
          in if isManaged typeEnv t && not (isExternalType typeEnv t)
             then do deleters <- get
                     case deletersMatchingXObj xobj deleters of
                       [] -> return (Left (UsingUnownedValue xobj))
                       [one] -> let newDeleters = Set.delete one deleters
                                in  do put newDeleters
                                       return (Right ())
                       _ -> error "Too many variables with the same name in set."                                  
             else return (Right ())

        -- | Check that the value being referenced hasn't already been given away
        refCheck :: XObj -> State MemState (Either TypeError ())
        refCheck xobj =
          let Just i = info xobj
              Just t = ty xobj
          in if isManaged typeEnv t && not (isExternalType typeEnv t)
             then do deleters <- get
                     case deletersMatchingXObj xobj deleters of
                       [] ->  return (Left (GettingReferenceToUnownedValue xobj))
                       [_] -> return (return ())
                       _ -> error "Too many variables with the same name in set."                                  
             else return (return ())

        transferOwnership :: XObj -> XObj -> State MemState (Either TypeError ())
        transferOwnership from to =
          do result <- unmanage from
             case result of
               Left e -> return (Left e)
               Right _ -> do manage to
                             return (Right ())
             --trace ("Transfered from " ++ getName from ++ " '" ++ varOfXObj from ++ "' to " ++ getName to ++ " '" ++ varOfXObj to ++ "'") $ return ()

        varOfXObj :: XObj -> String
        varOfXObj xobj =
          case xobj of
            XObj (Sym (SymPath [] name)) _ _ -> name
            _ -> let Just i = info xobj
                 in  freshVar i

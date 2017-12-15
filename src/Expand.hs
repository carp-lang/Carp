module Expand (expandAll) where

import Obj
import Util
import Control.Monad.State
import Control.Monad.State.Lazy (StateT(..), runStateT, liftIO, modify, get, put)

-- | Used for calling back to the 'eval' function in Eval.hs
type DynamicEvaluator = Env -> XObj -> StateT Context IO (Either EvalError XObj)

-- | Keep expanding the form until it doesn't change anymore.
-- | Note: comparing environments is tricky! Make sure they *can* be equal, otherwise this won't work at all!
expandAll :: DynamicEvaluator -> Env -> XObj -> StateT Context IO (Either EvalError XObj)
expandAll eval env root =
  do fullyExpanded <- expandAllInternal root
     return (fmap setNewIdentifiers fullyExpanded)
  where expandAllInternal xobj =
          do expansionResult <- expand eval env xobj
             case expansionResult of
               Right expanded -> if expanded == xobj
                                 then return (Right expanded)
                                 else expandAll eval env expanded
               err -> return err

-- | Macro expansion of a single form
expand :: DynamicEvaluator -> Env -> XObj -> StateT Context IO (Either EvalError XObj)
expand eval env xobj =
  case obj xobj of
  --case obj (trace ("Expand: " ++ pretty xobj) xobj) of
    Lst _ -> expandList xobj
    Arr _ -> expandArray xobj
    Sym _ -> expandSymbol xobj
    _     -> return (Right xobj)

  where
    expandList :: XObj -> StateT Context IO (Either EvalError XObj)
    expandList (XObj (Lst xobjs) i t) =
      case xobjs of
        [] -> return (Right xobj)
        XObj External _ _ : _ -> return (Right xobj)
        XObj (Instantiate _) _ _ : _ -> return (Right xobj)
        XObj (Deftemplate _) _ _ : _ -> return (Right xobj)
        XObj (Defalias _) _ _ : _ -> return (Right xobj)
        [defnExpr@(XObj Defn _ _), name, args, body] ->
          do expandedBody <- expand eval env body
             return $ do okBody <- expandedBody
                         Right (XObj (Lst [defnExpr, name, args, okBody]) i t)
        [defExpr@(XObj Def _ _), name, expr] ->
          do expandedExpr <- expand eval env expr
             return $ do okExpr <- expandedExpr
                         Right (XObj (Lst [defExpr, name, okExpr]) i t)
        [theExpr@(XObj The _ _), typeXObj, value] ->
          do expandedValue <- expand eval env value
             return $ do okValue <- expandedValue
                         Right (XObj (Lst [theExpr, typeXObj, okValue]) i t)
        [letExpr@(XObj Let _ _), XObj (Arr bindings) bindi bindt, body] ->
          if even (length bindings)
          then do bind <- mapM (\(n, x) -> do x' <- expand eval env x
                                              return $ do okX <- x'
                                                          (Right [n, okX]))
                               (pairwise bindings)
                  expandedBody <- expand eval env body
                  return $ do okBindings <- sequence bind
                              okBody <- expandedBody
                              Right (XObj (Lst [letExpr, XObj (Arr (concat okBindings)) bindi bindt, okBody]) i t)
          else return (Left (EvalError ("Uneven number of forms in let-statement: " ++ pretty xobj)))
        doExpr@(XObj Do _ _) : expressions ->
          do expandedExpressions <- mapM (expand eval env) expressions
             return $ do okExpressions <- sequence expandedExpressions
                         Right (XObj (Lst (doExpr : okExpressions)) i t)
        XObj Mod{} _ _ : _ ->
          return (Left (EvalError "Can't eval module"))
        f:args -> do expandedF <- expand eval env f
                     expandedArgs <- fmap sequence (mapM (expand eval env) args)
                     case expandedF of
                       Right (XObj (Lst [XObj Dynamic _ _, _, XObj (Arr _) _ _, _]) _ _) ->
                         --trace ("Found dynamic: " ++ pretty xobj)
                         eval env xobj
                       Right (XObj (Lst [XObj Macro _ _, _, XObj (Arr _) _ _, _]) _ _) ->
                         --trace ("Found macro: " ++ pretty xobj)
                         eval env xobj
                       Right (XObj (Lst [XObj (Command callback) _ _, _]) _ _) ->
                         (getCommand callback) args
                       Right _ ->
                         return $ do okF <- expandedF
                                     okArgs <- expandedArgs
                                     Right (XObj (Lst (okF : okArgs)) i t)
                       Left err -> return (Left err)
    expandList _ = error "Can't expand non-list in expandList."

    expandArray :: XObj -> StateT Context IO (Either EvalError XObj)
    expandArray (XObj (Arr xobjs) i t) =
      do evaledXObjs <- fmap sequence (mapM (expand eval env) xobjs)
         return $ do okXObjs <- evaledXObjs
                     Right (XObj (Arr okXObjs) i t)
    expandArray _ = error "Can't expand non-array in expandArray."

    expandSymbol :: XObj -> StateT Context IO (Either a XObj)
    expandSymbol (XObj (Sym path) _ _) =
      case lookupInEnv path env of
        Just (_, Binder (XObj (Lst (XObj External _ _ : _)) _ _)) -> return (Right xobj)
        Just (_, Binder (XObj (Lst (XObj (Instantiate _) _ _ : _)) _ _)) -> return (Right xobj)
        Just (_, Binder (XObj (Lst (XObj (Deftemplate _) _ _ : _)) _ _)) -> return (Right xobj)
        Just (_, Binder (XObj (Lst (XObj Defn _ _ : _)) _ _)) -> return (Right xobj)
        Just (_, Binder (XObj (Lst (XObj Def _ _ : _)) _ _)) -> return (Right xobj)
        Just (_, Binder (XObj (Lst (XObj (Defalias _) _ _ : _)) _ _)) -> return (Right xobj)
        Just (_, Binder found) -> return (Right found) -- use the found value
        Nothing -> return (Right xobj) -- symbols that are not found are left as-is
    expandSymbol _ = error "Can't expand non-symbol in expandSymbol."

-- | Replace all the infoIdentifier:s on all nested XObj:s
setNewIdentifiers :: XObj -> XObj
setNewIdentifiers root = let final = evalState (visit root) 0
                         in final
                           --trace ("ROOT: " ++ prettyTyped root ++ "FINAL: " ++ prettyTyped final) final
  where
    visit :: XObj -> State Int XObj
    visit xobj =
      case obj xobj of
        (Lst _) -> visitList xobj
        (Arr _) -> visitArray xobj
        _ -> bumpAndSet xobj

    visitList :: XObj -> State Int XObj
    visitList (XObj (Lst xobjs) i t) =
      do visited <- mapM visit xobjs
         let xobj' = XObj (Lst visited) i t
         bumpAndSet xobj'
    visitList _ = error "The function 'visitList' only accepts XObjs with lists in them."

    visitArray :: XObj -> State Int XObj
    visitArray (XObj (Arr xobjs) i t) =
      do visited <- mapM visit xobjs
         let xobj' = XObj (Arr visited) i t
         bumpAndSet xobj'
    visitArray _ = error "The function 'visitArray' only accepts XObjs with arrays in them."

    bumpAndSet :: XObj -> State Int XObj
    bumpAndSet xobj =
      do counter <- get
         put (counter + 1)
         case info xobj of
           Just i -> return (xobj { info = Just (i { infoIdentifier = counter })})
           Nothing -> return xobj

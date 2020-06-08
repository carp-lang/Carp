module Expand (expandAll, replaceSourceInfoOnXObj) where

import Control.Monad.State (evalState, get, put, State)
import Data.Foldable (foldlM)
import Debug.Trace

import Types
import Obj
import Project
import Util
import Lookup
import TypeError
import Info

-- | Used for calling back to the 'eval' function in Eval.hs
type DynamicEvaluator = Context -> XObj -> IO (Context, Either EvalError XObj)

-- | Keep expanding the form until it doesn't change anymore.
-- | Note: comparing environments is tricky! Make sure they *can* be equal, otherwise this won't work at all!
expandAll :: DynamicEvaluator -> Context -> XObj -> IO (Context, Either EvalError XObj)
expandAll eval ctx root =
  do (ctx, fullyExpanded) <- expandAllInternal root
     return (ctx, fmap setNewIdentifiers fullyExpanded)
  where expandAllInternal xobj =
          do (newCtx, expansionResult) <- expand eval ctx xobj
             case expansionResult of
               Right expanded -> if expanded == xobj
                                 then return (ctx, Right expanded)
                                 else expandAll eval newCtx expanded
               err -> return (newCtx, err)

-- | Macro expansion of a single form
expand :: DynamicEvaluator -> Context -> XObj -> IO (Context, Either EvalError XObj)
expand eval ctx xobj =
  case obj xobj of
  --case obj (trace ("Expand: " ++ pretty xobj) xobj) of
    Lst _ -> expandList xobj
    Arr _ -> expandArray xobj
    Sym _ _ -> return (ctx, expandSymbol xobj)
    _     -> return (ctx, Right xobj)

  where
    expandList :: XObj -> IO (Context, Either EvalError XObj)
    expandList (XObj (Lst xobjs) i t) = do
      let fppl = projectFilePathPrintLength (contextProj ctx)
      case xobjs of
        [] -> return (ctx, Right xobj)
        XObj (External _) _ _ : _ -> return (ctx, Right xobj)
        XObj (Instantiate _) _ _ : _ -> return (ctx, Right xobj)
        XObj (Deftemplate _) _ _ : _ -> return (ctx, Right xobj)
        XObj (Defalias _) _ _ : _ -> return (ctx, Right xobj)
        [defnExpr@(XObj (Defn _) _ _), name, args, body] ->
          do (ctx, expandedBody) <- expand eval ctx body
             return (ctx, do okBody <- expandedBody
                             Right (XObj (Lst [defnExpr, name, args, okBody]) i t))
        [defExpr@(XObj Def _ _), name, expr] ->
          do (ctx, expandedExpr) <- expand eval ctx expr
             return (ctx, do okExpr <- expandedExpr
                             Right (XObj (Lst [defExpr, name, okExpr]) i t))
        [theExpr@(XObj The _ _), typeXObj, value] ->
          do (ctx, expandedValue) <- expand eval ctx value
             return (ctx, do okValue <- expandedValue
                             Right (XObj (Lst [theExpr, typeXObj, okValue]) i t))
        (XObj The _ _ : _) ->
            return (evalError ctx ("I didn’t understand the `the` at " ++ prettyInfoFromXObj xobj ++ ":\n\n" ++ pretty xobj ++ "\n\nIs it valid? Every `the` needs to follow the form `(the type expression)`.") Nothing)
        [ifExpr@(XObj If _ _), condition, trueBranch, falseBranch] ->
          do (ctx, expandedCondition) <- expand eval ctx condition
             (ctx, expandedTrueBranch) <- expand eval ctx trueBranch
             (ctx, expandedFalseBranch) <- expand eval ctx falseBranch
             return (ctx, do okCondition <- expandedCondition
                             okTrueBranch <- expandedTrueBranch
                             okFalseBranch <- expandedFalseBranch
                             -- This is a HACK so that each branch of the if statement
                             -- has a "safe place" (= a do-expression with just one element)
                             -- where it can store info about its deleters. Without this,
                             -- An if statement with let-expression inside will duplicate
                             -- the calls to Delete when emitting code.
                             let wrappedTrue =
                                   case okTrueBranch of
                                     XObj (Lst (XObj Do _ _ : _)) _ _ -> okTrueBranch -- Has a do-expression already
                                     _ -> XObj (Lst [XObj Do Nothing Nothing, okTrueBranch]) (info okTrueBranch) Nothing
                                 wrappedFalse =
                                   case okFalseBranch of
                                     XObj (Lst (XObj Do _ _ : _)) _ _ -> okFalseBranch -- Has a do-expression already
                                     _ -> XObj (Lst [XObj Do Nothing Nothing, okFalseBranch]) (info okFalseBranch) Nothing

                             Right (XObj (Lst [ifExpr, okCondition, wrappedTrue, wrappedFalse]) i t))
        [letExpr@(XObj Let _ _), XObj (Arr bindings) bindi bindt, body] ->
          if even (length bindings)
          then do (ctx, bind) <- foldlM successiveExpand (ctx, Right []) (pairwise bindings)
                  (newCtx, expandedBody) <- expand eval ctx body
                  return (newCtx, do okBindings <- bind
                                     okBody <- expandedBody
                                     Right (XObj (Lst [letExpr, XObj (Arr (concat okBindings)) bindi bindt, okBody]) i t))
          else return (evalError ctx (
            "I ecountered an odd number of forms inside a `let` (`" ++
            pretty xobj ++ "`)") (info xobj))
          where successiveExpand (ctx, acc) (n, x) =
                  case acc of
                    Left err -> return (ctx, acc)
                    Right l -> do
                      (newCtx, x') <- expand eval ctx x
                      case x' of
                        Left err -> return (newCtx, Left err)
                        Right okX -> return (newCtx, Right (l ++ [[n, okX]]))

        matchExpr@(XObj (Match _) _ _) : (expr : rest)
          | null rest ->
              return (evalError ctx "I encountered a `match` without forms" (info xobj))
          | even (length rest) ->
              do (ctx, expandedExpr) <- expand eval ctx expr
                 (newCtx, expandedPairs) <- foldlM successiveExpand (ctx, Right []) (pairwise rest)
                 return (newCtx, do okExpandedExpr <- expandedExpr
                                    okExpandedPairs <- expandedPairs
                                    Right (XObj (Lst (matchExpr : okExpandedExpr : (concat okExpandedPairs))) i t))
          | otherwise -> return (evalError ctx
                    "I encountered an odd number of forms inside a `match`" (info xobj))
          where successiveExpand (ctx, acc) (l, r) =
                  case acc of
                    Left err -> return (ctx, acc)
                    Right lst -> do
                      (newCtx, expandedR) <- expand eval ctx r
                      case expandedR of
                        Left err -> return (newCtx, Left err)
                        Right v -> return (newCtx, Right (lst ++ [[l, v]]))

        doExpr@(XObj Do _ _) : expressions ->
          do (newCtx, expandedExpressions) <- foldlM successiveExpand (ctx, Right []) expressions
             return (newCtx, do okExpressions <- expandedExpressions
                                Right (XObj (Lst (doExpr : okExpressions)) i t))
        [withExpr@(XObj With _ _), pathExpr@(XObj (Sym path _) _ _), expression] ->
          do (newCtx, expandedExpression) <- expand eval ctx expression
             return (newCtx, do okExpression <- expandedExpression
                                Right (XObj (Lst [withExpr, pathExpr , okExpression]) i t)) -- Replace the with-expression with just the expression!
        [withExpr@(XObj With _ _), _, _] ->
          return (evalError ctx ("I encountered the value `" ++ pretty xobj ++
            "` inside a `with` at " ++ prettyInfoFromXObj xobj ++
            ".\n\n`with` accepts only symbols.") Nothing)
        XObj With _ _ : _ ->
          return (evalError ctx (
            "I encountered multiple forms inside a `with` at " ++
            prettyInfoFromXObj xobj ++
            ".\n\n`with` accepts only one expression, except at the top level.") Nothing)
        XObj Mod{} _ _ : _ ->
          return (evalError ctx ("I can’t evaluate the module `" ++ pretty xobj ++ "`") (info xobj))
        f:args ->
          do (ctx', expandedF) <- expand eval ctx f
             (ctx'', expandedArgs) <- foldlM successiveExpand (ctx, Right []) args
             case expandedF of
               Right (XObj (Lst [XObj Dynamic _ _, _, XObj (Arr _) _ _, _]) _ _) ->
                 --trace ("Found dynamic: " ++ pretty xobj)
                 eval ctx'' xobj
               Right (XObj (Lst [XObj Macro _ _, _, XObj (Arr _) _ _, _]) _ _) ->
                 --trace ("Found macro: " ++ pretty xobj ++ " at " ++ prettyInfoFromXObj xobj)
                 eval ctx'' xobj
               Right (XObj (Lst [XObj (Command callback) _ _, _]) _ _) ->
                 getCommand callback ctx args
               Right _ ->
                 return (ctx'', do okF <- expandedF
                                   okArgs <- expandedArgs
                                   Right (XObj (Lst (okF : okArgs)) i t))
               Left err -> return (ctx'', Left err)
    expandList _ = error "Can't expand non-list in expandList."

    expandArray :: XObj -> IO (Context, Either EvalError XObj)
    expandArray (XObj (Arr xobjs) i t) =
      do (newCtx, evaledXObjs) <- foldlM successiveExpand (ctx, Right []) xobjs
         return (newCtx, do okXObjs <- evaledXObjs
                            Right (XObj (Arr okXObjs) i t))
    expandArray _ = error "Can't expand non-array in expandArray."

    expandSymbol :: XObj -> Either a XObj
    expandSymbol (XObj (Sym path _) _ _) =
      case lookupInEnv path (contextEnv ctx) of
        Just (_, Binder _ (XObj (Lst (XObj (External _) _ _ : _)) _ _)) -> Right xobj
        Just (_, Binder _ (XObj (Lst (XObj (Instantiate _) _ _ : _)) _ _)) -> Right xobj
        Just (_, Binder _ (XObj (Lst (XObj (Deftemplate _) _ _ : _)) _ _)) -> Right xobj
        Just (_, Binder _ (XObj (Lst (XObj (Defn _) _ _ : _)) _ _)) -> Right xobj
        Just (_, Binder _ (XObj (Lst (XObj Def _ _ : _)) _ _)) -> Right xobj
        Just (_, Binder _ (XObj (Lst (XObj (Defalias _) _ _ : _)) _ _)) -> Right xobj
        Just (_, Binder _ found) -> Right found -- use the found value
        Nothing -> Right xobj -- symbols that are not found are left as-is
    expandSymbol _ = error "Can't expand non-symbol in expandSymbol."

    successiveExpand (ctx, acc) e =
      case acc of
        Left err -> return (ctx, acc)
        Right lst -> do
          (newCtx, expanded) <- expand eval ctx e
          case expanded of
            Right e -> do
              return (newCtx, Right (lst ++ [e]))
            Left err -> return (ctx, Left err)

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
        (StaticArr _) -> visitStaticArray xobj
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

    visitStaticArray :: XObj -> State Int XObj
    visitStaticArray (XObj (StaticArr xobjs) i t) =
      do visited <- mapM visit xobjs
         let xobj' = XObj (StaticArr visited) i t
         bumpAndSet xobj'
    visitStaticArray _ = error "The function 'visitStaticArray' only accepts XObjs with arrays in them."

    bumpAndSet :: XObj -> State Int XObj
    bumpAndSet xobj =
      do counter <- get
         put (counter + 1)
         case info xobj of
           Just i -> return (xobj { info = Just (i { infoIdentifier = counter })})
           Nothing -> return xobj

-- | Replaces the file, line and column info on an XObj an all its children.
replaceSourceInfo :: FilePath -> Int -> Int -> XObj -> XObj
replaceSourceInfo newFile newLine newColumn root = visit root
  where
    visit :: XObj -> XObj
    visit xobj =
      case obj xobj of
        (Lst _) -> visitList xobj
        (Arr _) -> visitArray xobj
        _ -> setNewInfo xobj

    visitList :: XObj -> XObj
    visitList (XObj (Lst xobjs) i t) =
      setNewInfo (XObj (Lst (map visit xobjs)) i t)
    visitList _ =
      error "The function 'visitList' only accepts XObjs with lists in them."

    visitArray :: XObj -> XObj
    visitArray (XObj (Arr xobjs) i t) =
      setNewInfo (XObj (Arr (map visit xobjs)) i t)
    visitArray _ = error "The function 'visitArray' only accepts XObjs with arrays in them."

    setNewInfo :: XObj -> XObj
    setNewInfo xobj =
      case info xobj of
        Just i -> (xobj { info = Just (i { infoFile = newFile
                                         , infoLine = newLine
                                         , infoColumn = newColumn
                                         })})
        Nothing -> xobj

replaceSourceInfoOnXObj :: Maybe Info -> XObj -> XObj
replaceSourceInfoOnXObj newInfo xobj =
  case newInfo of
    Just i  -> replaceSourceInfo (infoFile i) (infoLine i) (infoColumn i) xobj
    Nothing -> xobj

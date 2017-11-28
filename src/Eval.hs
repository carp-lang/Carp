module Eval (expandAll, eval, EvalError(..)) where

import qualified Data.Map as Map
import Data.List (foldl', null)
import Data.List.Split (splitWhen)
import Control.Monad.State
import Control.Monad.State.Lazy (StateT(..), liftIO)
import Obj
import Types
import Util
import Debug.Trace

newtype EvalError = EvalError String deriving (Eq)

instance Show EvalError where
  show (EvalError msg) = msg

isRestArgSeparator :: String -> Bool
isRestArgSeparator ":rest" = True
isRestArgSeparator _ = False

eval :: Env -> XObj -> StateT Context IO (Either EvalError XObj)
eval env xobj =
  case obj xobj of -- (trace ("Eval " ++ pretty xobj) xobj) of
    Lst _ -> evalList xobj
    Arr _ -> evalArray xobj
    Sym _ -> evalSymbol xobj
    _     -> return (Right xobj)

  where
    evalList :: XObj -> StateT Context IO (Either EvalError XObj)
    evalList (XObj (Lst xobjs) i t) =
      case xobjs of
        [] ->
          return (Right xobj)
        [XObj (Sym (SymPath [] "quote")) _ _, target] ->
          return (Right target)
        XObj (Sym (SymPath [] "list")) _ _ : rest ->
          do evaledList <- fmap sequence (mapM (eval env) rest)
             return $ do okList <- evaledList
                         Right (XObj (Lst okList) i t)
        [XObj (Sym (SymPath [] "list?")) _ _, x] ->
          do evaled <- eval env x
             return $ do okEvaled <- evaled
                         case okEvaled of
                           XObj (Lst _) _ _ -> Right trueXObj
                           _ -> Right falseXObj
        XObj (Sym (SymPath [] "array")) _ _ : rest ->
          do evaledArray <- fmap sequence (mapM (eval env) rest)
             return $ do okEvaledArray <- evaledArray
                         Right (XObj (Arr okEvaledArray) i t)
        [XObj (Sym (SymPath [] "=")) _ _, a, b] ->
          do evaledA <- eval env a
             evaledB <- eval env b
             return $ do okA <- evaledA
                         okB <- evaledB
                         case (okA, okB) of
                           (XObj (Num IntTy aNum) _ _, XObj (Num IntTy bNum) _ _) ->
                             if (round aNum :: Int) == (round bNum :: Int)
                             then Right trueXObj else Right falseXObj
                           _ ->
                             Left (EvalError ("Can't compare " ++ pretty okA ++ " with " ++ pretty okB))
        [XObj (Sym (SymPath [] "count")) _ _, target] ->
          do evaled <- eval env target
             return $ do okEvaled <- evaled
                         case okEvaled of
                           XObj (Lst lst) _ _ -> Right (XObj (Num IntTy (fromIntegral (length lst))) Nothing Nothing)
                           XObj (Arr arr) _ _ -> Right (XObj (Num IntTy (fromIntegral (length arr))) Nothing Nothing)
                           _ -> Left (EvalError ("Applying 'count' to non-list: " ++ pretty okEvaled))
        [XObj (Sym (SymPath [] "car")) _ _, target] ->
          do evaled <- eval env target
             return $ do okEvaled <- evaled
                         case okEvaled of
                           XObj (Lst (car : _)) _ _ -> Right car
                           XObj (Arr (car : _)) _ _ -> Right car
                           _ -> Left (EvalError ("Applying 'car' to non-list: " ++ pretty okEvaled))
        [XObj (Sym (SymPath [] "cdr")) _ _, target] ->
          do evaled <- eval env target
             return $ do okEvaled <- evaled
                         case okEvaled of
                           XObj (Lst (_ : cdr)) _ _ -> Right (XObj (Lst cdr) Nothing Nothing)
                           XObj (Arr (_ : cdr)) _ _ -> Right (XObj (Arr cdr) Nothing Nothing)
                           _ -> Left (EvalError "Applying 'cdr' to non-list or empty list")
        [XObj (Sym (SymPath [] "last")) _ _, target] ->
          do evaled <- eval env target
             return $ do okEvaled <- evaled
                         case okEvaled of
                           XObj (Lst lst) _ _ -> Right (last lst)
                           XObj (Arr arr) _ _ -> Right (last arr)
                           _ -> Left (EvalError "Applying 'last' to non-list or empty list")
        [XObj (Sym (SymPath [] "init")) _ _, target] ->
          do evaled <- eval env target
             return $ do okEvaled <- evaled
                         case okEvaled of
                           XObj (Lst lst) _ _ -> Right (XObj (Lst (init lst)) Nothing Nothing)
                           XObj (Arr arr) _ _ -> Right (XObj (Arr (init arr)) Nothing Nothing)
                           _ -> Left (EvalError "Applying 'init' to non-list or empty list")
        [XObj (Sym (SymPath [] "cons")) _ _, x, xs] ->
          do evaledX <- eval env x
             evaledXS <- eval env xs
             return $ do okEvaledX <- evaledX
                         okEvaledXS <- evaledXS
                         case okEvaledXS of
                           XObj (Lst lst) _ _ -> Right (XObj (Lst (okEvaledX : lst)) i t) -- TODO: probably not correct to just copy 'i' and 't'?
                           _ -> Left (EvalError "Applying 'cons' to non-list or empty list")
        [XObj (Sym (SymPath [] "cons-last")) _ _, x, xs] ->
          do evaledX <- eval env x
             evaledXS <- eval env xs
             return $ do okEvaledX <- evaledX
                         okEvaledXS <- evaledXS
                         case okEvaledXS of
                           XObj (Lst lst) _ _ -> Right (XObj (Lst (lst ++ [okEvaledX])) i t) -- TODO: should they get their own i:s and t:s
                           _ -> Left (EvalError "Applying 'cons-last' to non-list or empty list")
        [XObj (Sym (SymPath [] "append")) _ _, xs, ys] ->
          do evaledXS <- eval env xs
             evaledYS <- eval env ys
             return $ do okEvaledXS <- evaledXS
                         okEvaledYS <- evaledYS
                         case (okEvaledXS, okEvaledYS) of
                           (XObj (Lst lst1) _ _, XObj (Lst lst2) _ _) ->
                             return (XObj (Lst (lst1 ++ lst2)) i t) -- TODO: should they get their own i:s and t:s
                           _ ->
                             Left (EvalError "Applying 'append' to non-list or empty list")
        [XObj (Sym (SymPath [] "macro-error")) _ _, arg] ->
          do evaledArg <- eval env arg
             return $ do okArg <- evaledArg
                         case okArg of
                           XObj (Str msg) _ _ -> Left (EvalError msg)
                           _                  -> Left (EvalError "Calling 'macro-error' with non-string argument")
        [XObj (Sym (SymPath [] "macro-log")) _ _, arg] ->
          do evaledArg <- eval env arg
             case evaledArg of
               Right okArg -> do liftIO (putStrLn (pretty okArg))
                                 return (Right (XObj (Lst []) (Just dummyInfo) (Just UnitTy)))
               Left err -> return (Left err)
        [XObj If _ _, condition, ifTrue, ifFalse] ->
          do evaledCondition <- eval env condition
             case evaledCondition of
               Right okCondition ->
                 case obj okCondition of
                   Bol b -> if b
                            then eval env ifTrue
                            else eval env ifFalse
                   _ -> return (Left (EvalError ("Non-boolean expression in if-statement: " ++ pretty okCondition)))
               Left err -> return (Left err)
        [defnExpr@(XObj Defn _ _), name, args, body] ->
          do evaledBody <- eval env body
             return $ do okBody <- evaledBody
                         Right (XObj (Lst [defnExpr, name, args, okBody]) i t)
        [defExpr@(XObj Def _ _), name, expr] ->
          do evaledExpr <- expand env expr
             return $ do okExpr <- evaledExpr
                         Right (XObj (Lst [defExpr, name, okExpr]) i t)
        [theExpr@(XObj The _ _), typeXObj, value] ->
          do evaledValue <- expand env value
             return $ do okValue <- evaledValue
                         Right (XObj (Lst [theExpr, typeXObj, okValue]) i t)
        [letExpr@(XObj Let _ _), XObj (Arr bindings) bindi bindt, body] ->
          if even (length bindings)
          then do bind <- mapM (\(n, x) -> do x' <- eval env x
                                              return $ do okX <- x'
                                                          (Right [n, okX]))
                               (pairwise bindings)
                  evaledBody <- eval env body
                  return $ do okBindings <- sequence bind
                              okBody <- evaledBody
                              Right (XObj (Lst [letExpr, XObj (Arr (concat okBindings)) bindi bindt, okBody]) i t)
          else return (Left (EvalError ("Uneven number of forms in let-statement: " ++ pretty xobj)))
        doExpr@(XObj Do _ _) : expressions ->
          do evaledExpressions <- fmap sequence (mapM (eval env) expressions)
             return $ do okExpressions <- evaledExpressions
                         Right (XObj (Lst (doExpr : okExpressions)) i t)
        f:args -> do evaledF <- eval env f
                     case evaledF of
                       Right (XObj (Lst [XObj Dynamic _ _, _, XObj (Arr params) _ _, body]) _ _) ->
                         do evaledArgs <- fmap sequence (mapM (eval env) args)
                            case evaledArgs of
                              Right okArgs -> apply env body params okArgs
                              Left err -> return (Left err)
                       Right (XObj (Lst [XObj Macro _ _, _, XObj (Arr params) _ _, body]) _ _) ->
                         apply env body params args
                       _ ->
                         return (Right xobj)
                         --Left (EvalError ("Can't eval non-macro / non-dynamic function: " ++ pretty xobj))

    evalList _ = error "Can't eval non-list in evalList."

    evalSymbol :: XObj -> StateT Context IO (Either EvalError XObj)
    evalSymbol xobj@(XObj (Sym path) _ _) =
      case lookupInEnv path env of
        Just (_, Binder (XObj (Lst (XObj External _ _ : _)) _ _)) -> return (Right xobj)
        Just (_, Binder (XObj (Lst (XObj (Instantiate _) _ _ : _)) _ _)) -> return (Right xobj)
        Just (_, Binder (XObj (Lst (XObj (Deftemplate _) _ _ : _)) _ _)) -> return (Right xobj)
        Just (_, Binder (XObj (Lst (XObj Defn _ _ : _)) _ _)) -> return (Right xobj)
        Just (_, Binder (XObj (Lst (XObj Def _ _ : _)) _ _)) -> return (Right xobj)
        Just (_, Binder (XObj (Lst (XObj (Defalias _) _ _ : _)) _ _)) -> return (Right xobj)
        Just (_, Binder found) -> return (Right found) -- use the found value
        Nothing -> return (Left (EvalError ("Can't find symbol '" ++ show path ++ "' at " ++ prettyInfoFromXObj xobj)))
    evalSymbol _ = error "Can't eval non-symbol in evalSymbol."

    evalArray :: XObj -> StateT Context IO (Either EvalError XObj)
    evalArray (XObj (Arr xobjs) i t) =
      do evaledXObjs <- fmap sequence (mapM (eval env) xobjs)
         return $ do okXObjs <- evaledXObjs
                     Right (XObj (Arr okXObjs) i t)
    evalArray _ = error "Can't eval non-array in evalArray."

apply :: Env -> XObj -> [XObj] -> [XObj] -> StateT Context IO (Either EvalError XObj)
apply env body params args =
  let insideEnv = Env Map.empty (Just env) Nothing [] InternalEnv
      allParams = map getName params
      [properParams, restParams] = case splitWhen isRestArgSeparator allParams of
                                     [a, b] -> [a, b]
                                     [a] -> [a, []]
                                     _ -> error ("Invalid split of args: " ++ joinWith "," allParams)
      n = length properParams
      insideEnv' = foldl' (\e (p, x) -> extendEnv e p x) insideEnv (zip properParams (take n args))
      insideEnv'' = if null restParams
                    then insideEnv'
                    else extendEnv insideEnv'
                         (head restParams)
                         (XObj (Lst (drop n args)) Nothing Nothing)
      result = eval insideEnv'' body
  in  --trace ("Result: " ++ show result)
      result

trueXObj :: XObj
trueXObj = XObj (Bol True) Nothing Nothing

falseXObj :: XObj
falseXObj = XObj (Bol False) Nothing Nothing

-- | Keep expanding the form until it doesn't change anymore.
-- | Note: comparing environments is tricky! Make sure they *can* be equal, otherwise this won't work at all!
expandAll :: Env -> XObj -> StateT Context IO (Either EvalError XObj)
expandAll env xobj =
  do expansionResult <- expand env xobj
     let resultWithNewIds = fmap setNewIdentifiers expansionResult
     case resultWithNewIds of
       Right expanded -> if expanded == xobj
                         then return (Right expanded)
                         else expandAll env expanded
       err -> return err

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
    visitList _ = compilerError "The function 'visitList' only accepts XObjs with lists in them."

    visitArray :: XObj -> State Int XObj
    visitArray (XObj (Arr xobjs) i t) =
      do visited <- mapM visit xobjs
         let xobj' = XObj (Arr visited) i t
         bumpAndSet xobj'
    visitArray _ = compilerError "The function 'visitArray' only accepts XObjs with arrays in them."

    bumpAndSet :: XObj -> State Int XObj
    bumpAndSet xobj =
      do counter <- get
         put (counter + 1)
         case info xobj of
           Just i -> return (xobj { info = Just (i { infoIdentifier = counter })})
           Nothing -> return xobj

-- | Macro expansion of a single form
expand :: Env -> XObj -> StateT Context IO (Either EvalError XObj)
expand env xobj =
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
          do expandedBody <- expand env body
             return $ do okBody <- expandedBody
                         Right (XObj (Lst [defnExpr, name, args, okBody]) i t)
        [defExpr@(XObj Def _ _), name, expr] ->
          do expandedExpr <- expand env expr
             return $ do okExpr <- expandedExpr
                         Right (XObj (Lst [defExpr, name, okExpr]) i t)
        [theExpr@(XObj The _ _), typeXObj, value] ->
          do expandedValue <- expand env value
             return $ do okValue <- expandedValue
                         Right (XObj (Lst [theExpr, typeXObj, okValue]) i t)
        [letExpr@(XObj Let _ _), XObj (Arr bindings) bindi bindt, body] ->
          if even (length bindings)
          then do bind <- mapM (\(n, x) -> do x' <- expand env x
                                              return $ do okX <- x'
                                                          (Right [n, okX]))
                               (pairwise bindings)
                  expandedBody <- expand env body
                  return $ do okBindings <- sequence bind
                              okBody <- expandedBody
                              Right (XObj (Lst [letExpr, XObj (Arr (concat okBindings)) bindi bindt, okBody]) i t)
          else return (Left (EvalError ("Uneven number of forms in let-statement: " ++ pretty xobj)))
        doExpr@(XObj Do _ _) : expressions ->
          do expandedExpressions <- mapM (expand env) expressions
             return $ do okExpressions <- sequence expandedExpressions
                         Right (XObj (Lst (doExpr : okExpressions)) i t)
        XObj Mod{} _ _ : _ ->
          return (Left (EvalError "Can't eval module"))
        f:args -> do expandedF <- expand env f
                     expandedArgs <- fmap sequence (mapM (expand env) args)
                     case expandedF of
                       Right (XObj (Lst [XObj Dynamic _ _, _, XObj (Arr _) _ _, _]) _ _) ->
                         --trace ("Found dynamic: " ++ pretty xobj)
                         eval env xobj
                       Right (XObj (Lst [XObj Macro _ _, _, XObj (Arr _) _ _, _]) _ _) ->
                         --trace ("Found macro: " ++ pretty xobj)
                         eval env xobj
                       Right _ ->
                         return $ do okF <- expandedF
                                     okArgs <- expandedArgs
                                     Right (XObj (Lst (okF : okArgs)) i t)
                       Left err -> return (Left err)
    expandList _ = error "Can't expand non-list in expandList."

    expandArray :: XObj -> StateT Context IO (Either EvalError XObj)
    expandArray (XObj (Arr xobjs) i t) =
      do evaledXObjs <- fmap sequence (mapM (expand env) xobjs)
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

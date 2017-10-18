module Eval (expandAll, eval, EvalError(..)) where

import qualified Data.Map as Map
import Data.List (foldl', null)
import Data.List.Split (splitWhen)
import Control.Monad.State
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

type Identifier = Int

eval :: Int -> Env -> XObj -> (Either EvalError XObj, Identifier)
eval startIdentifier env root =

  runState (evalInternal root) startIdentifier
    
  where
    evalInternal :: XObj -> State Identifier (Either EvalError XObj)
    evalInternal xobj =
      case obj xobj of -- (trace ("Eval " ++ pretty xobj) xobj)
        Lst _ -> evalList xobj
        Arr _ -> evalArray xobj
        Sym _ -> evalSymbol xobj
        _     -> return (Right xobj)
    
    evalList :: XObj -> State Identifier (Either EvalError XObj)
    evalList xobj@(XObj (Lst xobjs) i t) =
      case xobjs of
        [] -> return (Right xobj)
        XObj (Sym (SymPath [] "quote")) _ _ : target : [] ->
          return (Right target)
        XObj (Sym (SymPath [] "list")) _ _ : rest ->
          do evaledList <- mapM evalInternal rest
             return $ do okList <- sequence evaledList
                         Right (XObj (Lst okList) i t)
        XObj (Sym (SymPath [] "array")) _ _ : rest ->
          do evaledArray <- mapM evalInternal rest
             return $ do okArray <- sequence evaledArray
                         Right (XObj (Arr okArray) i t)
        XObj (Sym (SymPath [] "=")) _ _ : a : b : [] ->
          do evaledA <- evalInternal a
             evaledB <- evalInternal b
             return $ do okA <- evaledA
                         okB <- evaledB
                         case (okA, okB) of
                           (XObj (Num IntTy aNum) _ _, XObj (Num IntTy bNum) _ _) ->
                             if ((round aNum) :: Int) == ((round bNum) :: Int)
                             then Right trueXObj else Right falseXObj
                           _ ->
                             Left (EvalError ("Can't compare " ++ pretty okA ++ " with " ++ pretty okB))
        XObj (Sym (SymPath [] "count")) _ _ : target : [] ->
          do evaled <- evalInternal target
             return $ do okEvaled <- evaled
                         case okEvaled of
                           XObj (Lst lst) _ _ ->
                             Right (XObj (Num IntTy (fromIntegral (length lst))) Nothing Nothing)
                           XObj (Arr arr) _ _ ->
                             Right (XObj (Num IntTy (fromIntegral (length arr))) Nothing Nothing)
                           _ ->
                             Left (EvalError ("Applying 'count' to non-list: " ++ pretty okEvaled))             
        XObj (Sym (SymPath [] "car")) _ _ : target : [] ->
          do evaled <- evalInternal target
             return $ do okEvaled <- evaled
                         case okEvaled of
                           XObj (Lst (car : _)) _ _ -> return car
                           XObj (Arr (car : _)) _ _ -> return car
                           _ -> Left (EvalError ("Applying 'car' to non-list: " ++ pretty okEvaled))
        XObj (Sym (SymPath [] "cdr")) _ _ : target : [] ->
          do evaled <- evalInternal target
             return $ do okEvaled <- evaled
                         case okEvaled of
                           XObj (Lst (_ : cdr)) _ _ -> return (XObj (Lst cdr) Nothing Nothing)
                           XObj (Arr (_ : cdr)) _ _ -> return (XObj (Arr cdr) Nothing Nothing)
                           _ -> Left (EvalError "Applying 'cdr' to non-list or empty list")
        XObj (Sym (SymPath [] "cons")) _ _ : x : xs : [] ->
          do evaledX <- evalInternal x
             evaledXS <- evalInternal xs
             return $ do okEvaledX <- evaledX
                         okEvaledXS <- evaledXS
                         case okEvaledXS of
                           XObj (Lst lst) _ _ ->
                             Right (XObj (Lst (okEvaledX : lst)) Nothing Nothing) -- TODO: should use okEvaledXS here instead of lst?
                           _ ->
                             Left (EvalError "Applying 'cons' to non-list or empty list")
        XObj If _ _ : condition : ifTrue : ifFalse : [] ->
          do evaledCondition <- evalInternal condition
             case evaledCondition of
               Right (XObj (Bol b) _ _) -> if b
                                           then evalInternal ifTrue
                                           else evalInternal ifFalse
               _ -> case evaledCondition of
                      Right cond -> return (Left (EvalError ("Non-boolean expression in if-statement: " ++ pretty cond)))
                      Left e -> return (Left e)
        defnExpr@(XObj Defn _ _) : name : args : body : [] ->
          do evaledBody <- evalInternal body
             return $ do okBody <- evaledBody
                         Right (XObj (Lst [defnExpr, name, args, okBody]) i t)
        defExpr@(XObj Def _ _) : name : expr : [] ->
          do oldIdentifier <- get
             let (evaledExpr, newIdentifier) = expand oldIdentifier env expr
             put newIdentifier
             return $ do okExpr <- evaledExpr
                         Right (XObj (Lst [defExpr, name, okExpr]) i t)
        theExpr@(XObj The _ _) : typeXObj : value : [] ->
          do oldIdentifier <- get
             let (evaledValue, newIdentifier) = expand oldIdentifier env value
             put newIdentifier
             return $ do okValue <- evaledValue
                         Right (XObj (Lst [theExpr, typeXObj, okValue]) i t)
        letExpr@(XObj Let _ _) : (XObj (Arr bindings) bindi bindt) : body : [] ->
          if even (length bindings)
          then do bind <- mapM mapper (pairwise bindings)
                  evaledBody <- evalInternal body
                  return $ do okBindings <- fmap concat (sequence bind)
                              okBody <- evaledBody                              
                              Right (XObj (Lst [letExpr, XObj (Arr okBindings) bindi bindt, okBody]) i t)
          else return (Left (EvalError ("Uneven number of forms in let-statement: " ++ pretty xobj)))
          where mapper :: (XObj, XObj) -> State Identifier (Either EvalError [XObj])
                mapper (n, x) =
                  do x' <- evalInternal x
                     case x' of
                       Left e -> return (Left e)
                       Right ok -> return (Right [n, ok]) -- [n, ok]
        doExpr@(XObj Do _ _) : expressions ->
          do evaledExpressions <- mapM evalInternal expressions
             return $ do okExpressions <- sequence evaledExpressions
                         Right (XObj (Lst (doExpr : okExpressions)) i t)
        f:args ->
          do evaledF <- evalInternal f
             evaledArgs <- mapM evalInternal args
             case (evaledF, sequence evaledArgs) of
               (Right okF, Right okArgs) ->
                 case okF of
                   XObj (Lst [XObj Dynamic _ _, _, XObj (Arr params) _ _, body]) _ _ ->
                     apply env body params okArgs
                   XObj (Lst [XObj Macro _ _, _, XObj (Arr params) _ _, body]) _ _ ->
                     apply env body params okArgs
                   _ ->
                     return (Right xobj)
                     --Left (EvalError ("Can't eval non-macro / non-dynamic function: " ++ pretty xobj))
               (Left e, _) ->
                 return (Left e)
               (_, Left e) ->
                 return (Left e)
                     
    evalList _ = error "Can't eval non-list in evalList."

    evalSymbol :: XObj -> State Identifier (Either EvalError XObj)
    evalSymbol xobj@(XObj (Sym path) _ _) =
      case lookupInEnv path env of
        Just (_, Binder (XObj (Lst (XObj External _ _ : _)) _ _)) -> return (Right xobj)
        Just (_, Binder (XObj (Lst (XObj (Instantiate _) _ _ : _)) _ _)) -> return (Right xobj)
        Just (_, Binder (XObj (Lst (XObj (Deftemplate _) _ _ : _)) _ _)) -> return (Right xobj)
        Just (_, Binder (XObj (Lst (XObj Defn _ _ : _)) _ _)) -> return (Right xobj)
        Just (_, Binder (XObj (Lst (XObj Def _ _ : _)) _ _)) -> return (Right xobj)
        Just (_, Binder (XObj (Lst (XObj (Defalias _) _ _ : _)) _ _)) -> return (Right xobj)
        Just (_, Binder found) -> return (Right found) -- use the found value
        Nothing -> return (Left (EvalError ("Can't find symbol '" ++ show path ++ "'.")))
    evalSymbol _ = error "Can't eval non-symbol in evalSymbol."
    
    evalArray :: XObj -> State Identifier (Either EvalError XObj)
    evalArray (XObj (Arr xobjs) i t) =
      do evaledXObjs <- mapM evalInternal xobjs
         return $ do okXObjs <- sequence evaledXObjs
                     Right (XObj (Arr okXObjs) i t)
    evalArray _ = error "Can't eval non-array in evalArray."

apply :: Env -> XObj -> [XObj] -> [XObj] -> State Identifier (Either EvalError XObj)
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
  in do oldIdentifier <- get
        let (result, newIdentifier) = (eval oldIdentifier insideEnv'' body)
        put newIdentifier
        return result        

trueXObj :: XObj
trueXObj = XObj (Bol True) Nothing Nothing

falseXObj :: XObj
falseXObj = XObj (Bol False) Nothing Nothing

-- | Keep expanding the form until it doesn't change anymore.
expandAll :: Int -> Env -> XObj -> (Either EvalError XObj, Int)
expandAll startIdentifier env xobj =
  case expand startIdentifier env xobj of
    -- | Note: comparing environments is tricky! Make sure they *can* be equal, otherwise this won't work at all:
    (Right expanded, newIdentifier) -> if expanded == xobj
                                       then (Right expanded, newIdentifier)
                                       else expandAll newIdentifier env expanded
    err -> err

bumpIdentifier :: State Identifier ()
bumpIdentifier = do identifier <- get
                    put (identifier + 1)

setNewIdentifier :: Info -> State Identifier Info
setNewIdentifier i = do bumpIdentifier
                        newIdentifier <- get
                        return (i { infoIdentifier = newIdentifier })

expand :: Int -> Env -> XObj -> (Either EvalError XObj, Int)
expand startIdentifier env root =
  runState (expandInternal root) startIdentifier

  where
    expandInternal :: XObj -> State Identifier (Either EvalError XObj)
    expandInternal xobj =
      case obj xobj of 
        --case obj (trace ("Expand: " ++ pretty xobj) xobj) of
        Lst _ -> expandList xobj
        Arr _ -> expandArray xobj
        Sym _ -> expandSymbol xobj
        _     -> return (Right xobj)
    
    expandList :: XObj -> State Identifier (Either EvalError XObj)
    expandList xobj@(XObj (Lst xobjs) i t) =
      case xobjs of
        [] -> return (Right xobj)
        XObj External _ _ : _ -> return (Right xobj)
        XObj (Instantiate _) _ _ : _ -> return (Right xobj)
        XObj (Deftemplate _) _ _ : _ -> return (Right xobj)
        XObj (Defalias _) _ _ : _ -> return (Right xobj)
        defnExpr@(XObj Defn _ _) : name : args : body : [] ->
          do expandedBody <- expandInternal body
             return $ do okBody <- expandedBody
                         Right (XObj (Lst [defnExpr, name, args, okBody]) i t)
        defExpr@(XObj Def _ _) : name : expr : [] ->
          do expandedExpr <- expandInternal expr
             return $ do okExpr <- expandedExpr
                         Right (XObj (Lst [defExpr, name, okExpr]) i t)
        theExpr@(XObj The _ _) : typeXObj : value : [] ->
          do expandedValue <- expandInternal value
             return $ do okValue <- expandedValue
                         Right (XObj (Lst [theExpr, typeXObj, okValue]) i t)
        letExpr@(XObj Let _ _) : (XObj (Arr bindings) bindi bindt) : body : [] ->
          if even (length bindings)
          then do bind <- mapM mapper (pairwise bindings)
                  expandedBody <- expandInternal body
                  return $ do okBindings <- fmap concat (sequence bind)
                              okBody <- expandedBody
                              (Right (XObj (Lst [letExpr, XObj (Arr okBindings) bindi bindt, okBody]) i t))
          else return (Left (EvalError ("Uneven number of forms in let-statement: " ++ pretty xobj)))
          where mapper :: (XObj, XObj) -> State Identifier (Either EvalError [XObj])
                mapper (n, x) =
                  do x' <- expandInternal x
                     case x' of
                       Left e -> return (Left e)
                       Right ok -> return (Right [n, ok]) -- [n, ok]
        doExpr@(XObj Do _ _) : expressions ->
          do expandedExpressions <- mapM expandInternal expressions
             return $ do okExpressions <- sequence expandedExpressions
                         Right (XObj (Lst (doExpr : okExpressions)) i t)
        (XObj (Mod _) _ _) : _ ->
          return (Left (EvalError "Can't eval module"))
        f:args -> do expandedF <- expandInternal f
                     expandedArgs <- mapM expandInternal args
                     case (expandedF, sequence expandedArgs) of
                        (Right okF, Right okArgs) ->
                          case okF of
                            XObj (Lst [XObj Dynamic _ _, _, XObj (Arr _) _ _, _]) _ _ ->
                              --trace ("Found dynamic: " ++ pretty xobj)
                              afterEval
                            XObj (Lst [XObj Macro _ _, _, XObj (Arr _) _ _, _]) _ _ ->
                              --trace ("Found macro: " ++ pretty xobj)
                              afterEval
                            _ ->
                              return (Right (XObj (Lst (okF : okArgs)) i t))
                          where afterEval :: State Identifier (Either EvalError XObj)
                                afterEval = do oldIdentifier <- get
                                               let (result, newIdentifier) = eval oldIdentifier env xobj
                                               put newIdentifier
                                               return result
                        (Left e, _) -> return (Left e)
                        (_, Left e) -> return (Left e)
    expandList _ = error "Can't expand non-list in expandList."

    expandArray :: XObj -> State Identifier (Either EvalError XObj)
    expandArray (XObj (Arr xobjs) i t) =
      do evaledXObjs <- mapM expandInternal xobjs
         return $ do okXObjs <- sequence evaledXObjs
                     Right (XObj (Arr okXObjs) i t)
    expandArray _ = error "Can't expand non-array in expandArray."
    
    expandSymbol :: XObj -> State Identifier (Either a XObj)
    expandSymbol xobj@(XObj (Sym path) _ _) =
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

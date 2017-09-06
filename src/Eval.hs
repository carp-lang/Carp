module Eval (expandAll, eval, EvalError(..)) where

import qualified Data.Map as Map
import Data.List (foldl', null)
import Data.List.Split (splitWhen)
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

eval :: Env -> XObj -> Either EvalError XObj
eval env xobj =
  case obj xobj of -- (trace ("Eval " ++ pretty xobj) xobj)
    Lst _ -> evalList xobj
    Arr _ -> evalArray xobj
    Sym _ -> evalSymbol xobj
    _     -> Right xobj

  where
    evalList :: XObj -> Either EvalError XObj
    evalList (XObj (Lst xobjs) i t) =
      case xobjs of
        [] -> Right xobj
        XObj (Sym (SymPath [] "quote")) _ _ : target : [] ->
          return target
        XObj (Sym (SymPath [] "list")) _ _ : rest ->
          do evaledList <- mapM (eval env) rest
             return (XObj (Lst evaledList) i t)
        XObj (Sym (SymPath [] "array")) _ _ : rest ->
          do evaledArray <- mapM (eval env) rest
             return (XObj (Arr evaledArray) i t)
        XObj (Sym (SymPath [] "=")) _ _ : a : b : [] ->
          do evaledA <- eval env a
             evaledB <- eval env b
             case (evaledA, evaledB) of
               (XObj (Num IntTy aNum) _ _, XObj (Num IntTy bNum) _ _) ->
                 if ((round aNum) :: Int) == ((round bNum) :: Int)
                 then Right trueXObj else Right falseXObj
               _ ->
                 --Right falseXObj
                 Left (EvalError ("Can't compare " ++ pretty evaledA ++ " with " ++ pretty evaledB))
        XObj (Sym (SymPath [] "count")) _ _ : target : [] ->
          do evaled <- eval env target
             case evaled of
               XObj (Lst lst) _ _ -> return (XObj (Num IntTy (fromIntegral (length lst))) Nothing Nothing)
               XObj (Arr arr) _ _ -> return (XObj (Num IntTy (fromIntegral (length arr))) Nothing Nothing)
               _ -> Left (EvalError ("Applying 'count' to non-list: " ++ pretty evaled))             
        XObj (Sym (SymPath [] "car")) _ _ : target : [] ->
          do evaled <- eval env target
             case evaled of
               XObj (Lst (car : _)) _ _ -> return car
               XObj (Arr (car : _)) _ _ -> return car
               _ -> Left (EvalError ("Applying 'car' to non-list: " ++ pretty evaled))
        XObj (Sym (SymPath [] "cdr")) _ _ : target : [] ->
          do evaled <- eval env target
             case evaled of
               XObj (Lst (_ : cdr)) _ _ -> return (XObj (Lst cdr) Nothing Nothing)
               XObj (Arr (_ : cdr)) _ _ -> return (XObj (Arr cdr) Nothing Nothing)
               _ -> Left (EvalError "Applying 'cdr' to non-list or empty list")
        XObj (Sym (SymPath [] "cons")) _ _ : x : xs : [] ->
          do evaledX <- eval env x
             evaledXS <- eval env xs
             case evaledXS of
               XObj (Lst lst) _ _ -> return (XObj (Lst (evaledX : lst)) Nothing Nothing)
               _ -> Left (EvalError "Applying 'cons' to non-list or empty list")
        XObj If _ _ : condition : ifTrue : ifFalse : [] ->
          do evaledCondition <- eval env condition
             case obj evaledCondition of
               Bol b -> if b then eval env ifTrue else eval env ifFalse
               _ -> Left (EvalError ("Non-boolean expression in if-statement: " ++ pretty evaledCondition))
        defnExpr@(XObj Defn _ _) : name : args : body : [] ->
          do evaledBody <- eval env body
             Right (XObj (Lst [defnExpr, name, args, evaledBody]) i t)
        defExpr@(XObj Def _ _) : name : expr : [] ->
          do evaledExpr <- expand env expr
             Right (XObj (Lst [defExpr, name, evaledExpr]) i t)
        theExpr@(XObj The _ _) : typeXObj : value : [] ->
          do evaledValue <- expand env value
             Right (XObj (Lst [theExpr, typeXObj, evaledValue]) i t)
        letExpr@(XObj Let _ _) : (XObj (Arr bindings) bindi bindt) : body : [] ->
          if even (length bindings)
          then do bind <- mapM (\(n, x) -> do x' <- eval env x
                                              return [n, x'])
                               (pairwise bindings)
                  let evaledBindings = concat bind
                  evaledBody <- eval env body
                  Right (XObj (Lst [letExpr, XObj (Arr evaledBindings) bindi bindt, evaledBody]) i t)
          else Left (EvalError ("Uneven number of forms in let-statement: " ++ pretty xobj))
        doExpr@(XObj Do _ _) : expressions ->
          do evaledExpressions <- mapM (eval env) expressions
             Right (XObj (Lst (doExpr : evaledExpressions)) i t)
        f:args -> do evaledF <- eval env f
                     case evaledF of
                       XObj (Lst [XObj Dynamic _ _, _, XObj (Arr params) _ _, body]) _ _ ->
                         do evaledArgs <- mapM (eval env) args
                            apply env body params evaledArgs                    
                       XObj (Lst [XObj Macro _ _, _, XObj (Arr params) _ _, body]) _ _ ->
                         apply env body params args
                       _ ->
                         Right xobj
                         --Left (EvalError ("Can't eval non-macro / non-dynamic function: " ++ pretty xobj))
                       
    evalList _ = error "Can't eval non-list in evalList."

    evalSymbol :: XObj -> Either EvalError XObj
    evalSymbol (XObj (Sym path) _ _) =
      case lookupInEnv path env of
        Just (_, Binder (XObj (Lst (XObj External _ _ : _)) _ _)) -> Right xobj
        Just (_, Binder (XObj (Lst (XObj (Instantiate _) _ _ : _)) _ _)) -> Right xobj
        Just (_, Binder (XObj (Lst (XObj (Deftemplate _) _ _ : _)) _ _)) -> Right xobj
        Just (_, Binder (XObj (Lst (XObj Defn _ _ : _)) _ _)) -> Right xobj
        Just (_, Binder (XObj (Lst (XObj Def _ _ : _)) _ _)) -> Right xobj
        Just (_, Binder (XObj (Lst (XObj (Defalias _) _ _ : _)) _ _)) -> Right xobj
        Just (_, Binder found) -> Right found -- use the found value
        Nothing -> Left (EvalError ("Can't find symbol '" ++ show path ++ "'."))
    evalSymbol _ = error "Can't eval non-symbol in evalSymbol."
    
    evalArray :: XObj -> Either EvalError XObj
    evalArray (XObj (Arr xobjs) i t) =
      do evaledXObjs <- mapM (eval env) xobjs
         return (XObj (Arr evaledXObjs) i t)
    evalArray _ = error "Can't eval non-array in evalArray."

apply :: Env -> XObj -> [XObj] -> [XObj] -> Either EvalError XObj
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
expandAll :: Env -> XObj -> Either EvalError XObj
expandAll env xobj =
  case expand env xobj of
    -- | Note: comparing environments is tricky since some things in them can be unconditionally non-equal.
    -- | Therefore the expansion stops when it finds a module.
    Right expanded -> if (isModuleDefinition expanded) || expanded == xobj
                      then Right expanded
                      else expandAll env (trace ("\n\nNOPE\n" ++ (show expanded) ++ "\n\n") expanded)
    err -> err

isModuleDefinition :: XObj -> Bool
isModuleDefinition (XObj (Lst (XObj (Sym _) _ _ : (XObj (Mod _) _ _) : _)) _ _) = True
isModuleDefinition _ = False

expand :: Env -> XObj -> Either EvalError XObj
expand env xobj =
  case obj xobj of 
  --case obj (trace ("Expand: " ++ pretty xobj) xobj) of
    Lst _ -> expandList xobj
    Arr _ -> expandArray xobj
    Sym _ -> expandSymbol xobj
    _     -> Right xobj

  where
    expandList :: XObj -> Either EvalError XObj
    expandList (XObj (Lst xobjs) i t) =
      case xobjs of
        [] -> Right xobj
        XObj External _ _ : _ -> Right xobj
        XObj (Instantiate _) _ _ : _ -> Right xobj
        XObj (Deftemplate _) _ _ : _ -> Right xobj
        XObj (Defalias _) _ _ : _ -> Right xobj
        defnExpr@(XObj Defn _ _) : name : args : body : [] ->
          do expandedBody <- expand env body
             Right (XObj (Lst [defnExpr, name, args, expandedBody]) i t)
        defExpr@(XObj Def _ _) : name : expr : [] ->
          do expandedExpr <- expand env expr
             Right (XObj (Lst [defExpr, name, expandedExpr]) i t)
        theExpr@(XObj The _ _) : typeXObj : value : [] ->
          do expandedValue <- expand env value
             Right (XObj (Lst [theExpr, typeXObj, expandedValue]) i t)
        letExpr@(XObj Let _ _) : (XObj (Arr bindings) bindi bindt) : body : [] ->
          if even (length bindings)
          then do bind <- mapM (\(n, x) -> do x' <- expand env x
                                              return [n, x'])
                            (pairwise bindings)
                  let expandedBindings = concat bind
                  expandedBody <- expand env body
                  Right (XObj (Lst [letExpr, XObj (Arr expandedBindings) bindi bindt, expandedBody]) i t)
          else Left (EvalError ("Uneven number of forms in let-statement: " ++ pretty xobj))
        doExpr@(XObj Do _ _) : expressions ->
          do expandedExpressions <- mapM (expand env) expressions
             Right (XObj (Lst (doExpr : expandedExpressions)) i t)
        (XObj (Mod _) _ _) : _ ->
          Left (EvalError "Can't eval module")
        f:args -> do expandedF <- expand env f
                     expandedArgs <- mapM (expand env) args
                     case expandedF of
                       XObj (Lst [XObj Dynamic _ _, _, XObj (Arr _) _ _, _]) _ _ ->
                         --trace ("Found dynamic: " ++ pretty xobj)
                         eval env xobj
                       XObj (Lst [XObj Macro _ _, _, XObj (Arr _) _ _, _]) _ _ ->
                         --trace ("Found macro: " ++ pretty xobj)
                         eval env xobj
                       _ ->
                         Right (XObj (Lst (expandedF : expandedArgs)) i t)
    expandList _ = error "Can't expand non-list in expandList."

    expandArray :: XObj -> Either EvalError XObj
    expandArray (XObj (Arr xobjs) i t) =
      do evaledXObjs <- mapM (expand env) xobjs
         return (XObj (Arr evaledXObjs) i t)
    expandArray _ = error "Can't expand non-array in expandArray."
    
    expandSymbol :: XObj -> Either a XObj
    expandSymbol (XObj (Sym path) _ _) =
      case lookupInEnv path env of
        Just (_, Binder (XObj (Lst (XObj External _ _ : _)) _ _)) -> Right xobj
        Just (_, Binder (XObj (Lst (XObj (Instantiate _) _ _ : _)) _ _)) -> Right xobj
        Just (_, Binder (XObj (Lst (XObj (Deftemplate _) _ _ : _)) _ _)) -> Right xobj
        Just (_, Binder (XObj (Lst (XObj Defn _ _ : _)) _ _)) -> Right xobj
        Just (_, Binder (XObj (Lst (XObj Def _ _ : _)) _ _)) -> Right xobj
        Just (_, Binder (XObj (Lst (XObj (Defalias _) _ _ : _)) _ _)) -> Right xobj
        Just (_, Binder found) -> Right found -- use the found value
        Nothing -> Right xobj -- symbols that are not found are left as-is
    expandSymbol _ = error "Can't expand non-symbol in expandSymbol."

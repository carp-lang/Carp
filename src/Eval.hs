module Eval where

import qualified Data.Map as Map
import Data.List (foldl', null)
import Data.List.Split (splitWhen)
import Data.Maybe (fromJust, mapMaybe, isJust)
import Control.Monad.State
import Control.Monad.State.Lazy (StateT(..), runStateT, liftIO, modify, get, put)
import System.Exit (exitSuccess, exitFailure, exitWith, ExitCode(..))
import System.IO (hPutStr)
import System.Directory (doesPathExist)
import Control.Concurrent (forkIO)
import qualified Data.Map as Map
import Data.Maybe (fromJust, mapMaybe, isJust)
import Control.Monad
import Control.Exception
import Debug.Trace

import Parsing
import Emit
import Obj
import Types
import Infer
import Deftype
import ColorText
import Template
import Util
import Commands
import Expand

-- | Dynamic (REPL) evaluation of XObj:s (s-expressions)
eval :: Env -> XObj -> StateT Context IO (Either EvalError XObj)
eval env xobj =
  case obj xobj of
  --case obj (trace ("\nEval " ++ pretty xobj ++ ", obj: " ++ show (obj xobj)) xobj) of
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
        XObj Do _ _ : rest ->
          do evaledList <- fmap sequence (mapM (eval env) rest)
             case evaledList of
               Left e -> return (Left e)
               Right ok ->
                 case ok of
                   [] -> return (Left (EvalError "No forms in 'do' statement."))
                   _ -> return (Right (last ok))
        XObj (Sym (SymPath [] "list")) _ _ : rest ->
          do evaledList <- fmap sequence (mapM (eval env) rest)
             return $ do okList <- evaledList
                         Right (XObj (Lst okList) i t)
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
                           (XObj (Str a) _ _, XObj (Str b) _ _) ->
                             if a == b then Right trueXObj else Right falseXObj
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
        [XObj (Sym (SymPath [] "all-but-last")) _ _, target] ->
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
          do ctx <- get
             let pathStrings = contextPath ctx
                 env = contextGlobalEnv ctx
                 typeEnv = contextTypeEnv ctx
                 innerEnv = getEnv env pathStrings
             expansionResult <- expandAll eval env xobj
             ctxAfterExpansion <- get
             case expansionResult of
               Left err -> return (Left (EvalError (show err)))
               Right expanded ->
                 let xobjFullPath = setFullyQualifiedDefn expanded (SymPath pathStrings (getName xobj))
                     xobjFullSymbols = setFullyQualifiedSymbols typeEnv innerEnv xobjFullPath
                 in case annotate typeEnv env xobjFullSymbols of
                      Left err ->
                        return (Left (EvalError (show err)))
                      Right annXObjs ->
                        do ctxWithDefs <- liftIO $ foldM define ctxAfterExpansion annXObjs
                           put ctxWithDefs
                           return dynamicNil
        [defExpr@(XObj Def _ _), name, expr] ->
          -- COMPLETELY DUPLICATED FROM DEFN ABOVE, FIX!
          do ctx <- get
             let pathStrings = contextPath ctx
                 env = contextGlobalEnv ctx
                 typeEnv = contextTypeEnv ctx
                 innerEnv = getEnv env pathStrings
             expansionResult <- expandAll eval env xobj
             ctxAfterExpansion <- get
             case expansionResult of
               Left err -> return (Left (EvalError (show err)))
               Right expanded ->
                 let xobjFullPath = setFullyQualifiedDefn expanded (SymPath pathStrings (getName xobj))
                     xobjFullSymbols = setFullyQualifiedSymbols typeEnv innerEnv xobjFullPath
                 in case annotate typeEnv env xobjFullSymbols of
                      Left err ->
                        return (Left (EvalError (show err)))
                      Right annXObjs ->
                        do ctxWithDefs <- liftIO $ foldM define ctxAfterExpansion annXObjs
                           put ctxWithDefs
                           return dynamicNil

        XObj (Sym (SymPath [] "register-type")) _ _ : XObj (Sym (SymPath _ typeName)) _ _ : rest ->
          do ctx <- get
             let pathStrings = contextPath ctx
                 env = contextGlobalEnv ctx
                 typeEnv = contextTypeEnv ctx
                 innerEnv = getEnv env pathStrings
                 path = SymPath pathStrings typeName
                 typeDefinition = XObj (Lst [XObj ExternalType Nothing Nothing, XObj (Sym path) Nothing Nothing]) Nothing (Just TypeTy)
                 i = Nothing
             case rest of
               [] ->
                 do put (ctx { contextTypeEnv = TypeEnv (extendEnv (getTypeEnv typeEnv) typeName typeDefinition) })
                    return dynamicNil
               members ->
                 case bindingsForRegisteredType typeEnv env pathStrings typeName members i of
                   Left errorMessage ->
                     return (Left (EvalError (show errorMessage)))
                   Right (typeModuleName, typeModuleXObj, deps) ->
                     let ctx' = (ctx { contextGlobalEnv = envInsertAt env (SymPath pathStrings typeModuleName) typeModuleXObj
                                     , contextTypeEnv = TypeEnv (extendEnv (getTypeEnv typeEnv) typeName typeDefinition)
                                     })
                     in do contextWithDefs <- liftIO $ foldM define ctx' deps
                           put contextWithDefs
                           return dynamicNil
        XObj (Sym (SymPath _ "register-type")) _ _ : _ ->
          return (Left (EvalError (show "Invalid ars to 'register-type': " ++ pretty xobj)))

        XObj (Sym (SymPath [] "deftype")) _ _ : nameXObj@(XObj (Sym (SymPath _ typeName)) _ _) : rest ->
          do ctx <- get
             let pathStrings = contextPath ctx
                 env = contextGlobalEnv ctx
                 typeEnv = contextTypeEnv ctx
             case nameXObj of
               XObj (Sym (SymPath _ typeName)) i _ ->
                 case moduleForDeftype typeEnv env pathStrings typeName rest i of
                   Right (typeModuleName, typeModuleXObj, deps) ->
                     let typeDefinition =
                           -- NOTE: The type binding is needed to emit the type definition and all the member functions of the type.
                           XObj (Lst (XObj Typ Nothing Nothing : XObj (Sym (SymPath pathStrings typeName)) Nothing Nothing : rest)) i (Just TypeTy)
                         ctx' = (ctx { contextGlobalEnv = envInsertAt env (SymPath pathStrings typeModuleName) typeModuleXObj
                                     , contextTypeEnv = TypeEnv (extendEnv (getTypeEnv typeEnv) typeName typeDefinition)
                                     })
                     in do ctxWithDeps <- liftIO (foldM define ctx' deps)
                           put $ foldl (\context path -> registerInInterfaceIfNeeded context path) ctxWithDeps
                                       [(SymPath (pathStrings ++ [typeModuleName]) "str")
                                       ,(SymPath (pathStrings ++ [typeModuleName]) "copy")]
                           return dynamicNil
                   Left errorMessage ->
                     return (Left (EvalError ("Invalid type definition for '" ++ pretty nameXObj ++ "'. " ++ errorMessage)))
               _ ->
                 return (Left (EvalError ("Invalid name for type definition: " ++ pretty nameXObj)))

        [XObj (Sym (SymPath [] "register")) _ _, XObj (Sym (SymPath _ name)) _ _, typeXObj] ->
          do ctx <- get
             let pathStrings = contextPath ctx
                 env = contextGlobalEnv ctx
             case xobjToTy typeXObj of
                   Just t -> let path = SymPath pathStrings name
                                 binding = XObj (Lst [XObj External Nothing Nothing,
                                                      XObj (Sym path) Nothing Nothing])
                                           (info typeXObj) (Just t)
                                 env' = envInsertAt env path binding
                                 ctx' = registerInInterfaceIfNeeded ctx path
                             in  do put (ctx' { contextGlobalEnv = env' })
                                    return dynamicNil
                   Nothing ->
                     return (Left (EvalError ("Can't understand type when registering '" ++ name ++ "'")))
        XObj (Sym (SymPath [] "register")) _ _ : _ ->
          return (Left (EvalError ("Invalid args to 'register' command: " ++ pretty xobj)))

        [XObj (Sym (SymPath [] "definterface")) _ _, nameXObj@(XObj (Sym path@(SymPath [] name)) _ _), typeXObj] ->
          do ctx <- get
             let env = contextGlobalEnv ctx
                 typeEnv = getTypeEnv (contextTypeEnv ctx)
             case xobjToTy typeXObj of
               Just t ->
                 case lookupInEnv path typeEnv of
                   Just (_, Binder (XObj (Lst (XObj (Interface foundType _) _ _ : _)) _ _)) ->
                     -- The interface already exists, so it will be left as-is.
                     if foundType == t
                     then return dynamicNil
                     else liftIO $ do putStrLn ("[FORBIDDEN] Tried to change the type of interface '" ++ show path ++ "' from " ++ show foundType ++ " to " ++ show t)
                                      return dynamicNil
                   Nothing ->
                     let interface = defineInterface name t [] (info nameXObj)
                         typeEnv' = TypeEnv (envInsertAt typeEnv (SymPath [] name) interface)
                     in  do put (ctx { contextTypeEnv = typeEnv' })
                            return dynamicNil
               Nothing ->
                 return (Left (EvalError ("Invalid type for interface '" ++ name ++ "': " ++
                                           pretty typeXObj ++ " at " ++ prettyInfoFromXObj typeXObj ++ ".")))
        XObj (Sym (SymPath [] "definterface")) _ _ : _ ->
          return (Left (EvalError ("Invalid args to 'definterface' command: " ++ pretty xobj)))

        [XObj (Sym (SymPath [] "defdynamic")) _ _, (XObj (Sym (SymPath [] name)) _ _), params, body] ->
          do ctx <- get
             let pathStrings = contextPath ctx
                 env = contextGlobalEnv ctx
                 path = SymPath pathStrings name
                 dynamic = XObj (Lst [XObj Dynamic Nothing Nothing, XObj (Sym path) Nothing Nothing, params, body]) (info body) (Just DynamicTy)
             put (ctx { contextGlobalEnv = envInsertAt env path dynamic })
             return dynamicNil
        XObj (Sym (SymPath [] "defdynamic")) _ _ : _ ->
          return (Left (EvalError ("Invalid args to 'defdynamic' command: " ++ pretty xobj)))

        [XObj (Sym (SymPath [] "defmacro")) _ _, (XObj (Sym (SymPath [] name)) _ _), params, body] ->
          do ctx <- get
             let pathStrings = contextPath ctx
                 env = contextGlobalEnv ctx
                 path = SymPath pathStrings name
                 macro = XObj (Lst [XObj Macro Nothing Nothing, XObj (Sym path) Nothing Nothing, params, body]) (info body) (Just MacroTy)
             put (ctx { contextGlobalEnv = envInsertAt env path macro })
             return dynamicNil
        XObj (Sym (SymPath [] "defmacro")) _ _ : _ ->
          return (Left (EvalError ("Invalid args to 'defmacro' command: " ++ pretty xobj)))

        XObj (Sym (SymPath [] "defmodule")) _ _ : (XObj (Sym (SymPath [] moduleName)) _ _) : innerExpressions ->
          do ctx <- get
             let pathStrings = contextPath ctx
                 env = contextGlobalEnv ctx
                 typeEnv = contextTypeEnv ctx
                 lastInput = contextLastInput ctx
                 execMode = contextExecMode ctx
                 proj = contextProj ctx
             result <- case lookupInEnv (SymPath pathStrings moduleName) env of
                         Just (_, Binder (XObj (Mod _) _ _)) ->
                           do let ctx' = (Context env typeEnv (pathStrings ++ [moduleName]) proj lastInput execMode) -- use { = } syntax instead
                              ctxAfterModuleAdditions <- liftIO $ foldM folder ctx' innerExpressions
                              put (popModulePath ctxAfterModuleAdditions)
                              return dynamicNil -- TODO: propagate errors...
                         Just _ ->
                           return (Left (EvalError ("Can't redefine '" ++ moduleName ++ "' as module.")))
                         Nothing ->
                           do let parentEnv = getEnv env pathStrings
                                  innerEnv = Env (Map.fromList []) (Just parentEnv) (Just moduleName) [] ExternalEnv
                                  newModule = XObj (Mod innerEnv) (info xobj) (Just ModuleTy)
                                  globalEnvWithModuleAdded = envInsertAt env (SymPath pathStrings moduleName) newModule
                                  ctx' = Context globalEnvWithModuleAdded typeEnv (pathStrings ++ [moduleName]) proj lastInput execMode -- TODO: also change
                              ctxAfterModuleDef <- liftIO $ foldM folder ctx' innerExpressions
                              put (popModulePath ctxAfterModuleDef)
                              return dynamicNil
             case result of
               Left err -> return (Left err)
               Right _ -> return dynamicNil
        XObj (Sym (SymPath [] "defmodule")) _ _ : _ ->
          return (Left (EvalError ("Invalid args to 'defmodule' command: " ++ pretty xobj)))

        [XObj (Sym (SymPath [] "info")) _ _, target@(XObj (Sym path@(SymPath _ name)) _ _)] ->
          do ctx <- get
             let env = contextGlobalEnv ctx
                 typeEnv = contextTypeEnv ctx
                 proj = contextProj ctx
                 printer allowLookupInALL binderPair =
                   case binderPair of
                     Just (_, binder@(Binder x@(XObj _ (Just i) _))) ->
                       do putStrLnWithColor White (show binder ++ "\nDefined at " ++ prettyInfo i)
                          when (projectPrintTypedAST proj) $ putStrLnWithColor Yellow (prettyTyped x)
                          return ()
                     Just (_, binder@(Binder x)) ->
                       do putStrLnWithColor White (show binder)
                          when (projectPrintTypedAST proj) $ putStrLnWithColor Yellow (prettyTyped x)
                          return ()
                     Nothing ->
                       if allowLookupInALL
                       then case multiLookupALL name env of
                              [] ->
                                do putStrLnWithColor Red ("Can't find '" ++ show path ++ "'")
                                   return ()
                              binders ->
                                do mapM_ (\(env, binder@(Binder (XObj _ i _))) ->
                                            case i of
                                              Just i' -> putStrLnWithColor White (show binder ++ " Defined at " ++ prettyInfo i')
                                              Nothing -> putStrLnWithColor White (show binder))
                                         binders
                                   return ()
                       else return ()
             case path of
               SymPath [] _ ->
                 -- First look in the type env, then in the global env:
                 do case lookupInEnv path (getTypeEnv typeEnv) of
                      Nothing -> liftIO (printer True (lookupInEnv path env))
                      found -> do liftIO (printer True found) -- this will print the interface itself
                                  liftIO (printer True (lookupInEnv path env)) -- this will print the locations of the implementers of the interface
                    return dynamicNil
               qualifiedPath ->
                 do case lookupInEnv path env of
                      Nothing -> notFound path
                      found -> do liftIO (printer False found)
                                  return dynamicNil
        XObj (Sym (SymPath [] "info")) _ _ : _ ->
          return (Left (EvalError ("Invalid args to 'info' command: " ++ pretty xobj)))

        [XObj (Sym (SymPath [] "type")) _ _, target] ->
          do ctx <- get
             let env = contextGlobalEnv ctx
             case target of
                   XObj (Sym path@(SymPath [] name)) _ _ ->
                     case lookupInEnv path env of
                       Just (_, binder) ->
                         found binder
                       Nothing ->
                         case multiLookupALL name env of
                           [] ->
                             notFound path
                           binders ->
                             liftIO $ do mapM_ (\(env, binder) -> putStrLnWithColor White (show binder)) binders
                                         return dynamicNil
                   XObj (Sym qualifiedPath) _ _ ->
                     case lookupInEnv qualifiedPath env of
                       Just (_, binder) ->
                         found binder
                       Nothing ->
                         notFound qualifiedPath
                   _ ->
                     liftIO $ do putStrLnWithColor Red ("Can't get the type of non-symbol: " ++ pretty xobj)
                                 return dynamicNil
        XObj (Sym (SymPath [] "type")) _ _ : _ ->
          return (Left (EvalError ("Invalid args to 'type' command: " ++ pretty xobj)))

        [XObj (Sym (SymPath [] "use")) _ _, xobj@(XObj (Sym path) _ _)] ->
          do ctx <- get
             let pathStrings = contextPath ctx
                 env = contextGlobalEnv ctx
                 e = getEnv env pathStrings
                 useThese = envUseModules e
                 e' = if path `elem` useThese then e else e { envUseModules = path : useThese }
                 innerEnv = getEnv env pathStrings -- Duplication of e?
             case lookupInEnv path innerEnv of
               Just (_, Binder _) ->
                 do put $ ctx { contextGlobalEnv = envReplaceEnvAt env pathStrings e' }
                    return dynamicNil
               Nothing ->
                 return (Left (EvalError ("Can't find a module named '" ++ show path ++ "' at " ++ prettyInfoFromXObj xobj ++ ".")))
        XObj (Sym (SymPath [] "use")) _ _ : _ ->
          return (Left (EvalError ("Invalid args to 'use' command: " ++ pretty xobj)))

        [theExpr@(XObj The _ _), typeXObj, value] ->
          do evaledValue <- expandAll eval env value
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
        f:args -> do evaledF <- eval env f
                     case evaledF of
                       Right (XObj (Lst [XObj Dynamic _ _, _, XObj (Arr params) _ _, body]) _ _) ->
                         do evaledArgs <- fmap sequence (mapM (eval env) args)
                            case evaledArgs of
                              Right okArgs -> apply env body params okArgs
                              Left err -> return (Left err)
                       Right (XObj (Lst [XObj Macro _ _, _, XObj (Arr params) _ _, body]) _ _) ->
                         apply env body params args

                       Right (XObj (Lst [XObj (Command callback) _ _, _]) _ _) ->
                         do evaledArgs <- fmap sequence (mapM (eval env) args)
                            case evaledArgs of
                              Right okArgs -> getCommand callback okArgs
                              Left err -> return (Left err)
                       _ ->
                         return (Right xobj)
                         --Left (EvalError ("Can't eval non-macro / non-dynamic function: " ++ pretty xobj))

    evalList _ = error "Can't eval non-list in evalList."

    evalSymbol :: XObj -> StateT Context IO (Either EvalError XObj)
    evalSymbol xobj@(XObj (Sym path) _ _) =
      case lookupInEnv path env of
        Just (_, Binder found) -> return (Right found) -- use the found value
        Nothing -> return (Left (EvalError ("Can't find symbol '" ++ show path ++ "' at " ++ prettyInfoFromXObj xobj)))
    evalSymbol _ = error "Can't eval non-symbol in evalSymbol."

    evalArray :: XObj -> StateT Context IO (Either EvalError XObj)
    evalArray (XObj (Arr xobjs) i t) =
      do evaledXObjs <- fmap sequence (mapM (eval env) xobjs)
         return $ do okXObjs <- evaledXObjs
                     Right (XObj (Arr okXObjs) i t)
    evalArray _ = error "Can't eval non-array in evalArray."

-- | Apply a function to some arguments. The other half of 'eval'.
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
  in result

-- | Is a string the ':rest' separator for arguments to dynamic functions / macros
isRestArgSeparator :: String -> Bool
isRestArgSeparator ":rest" = True
isRestArgSeparator _ = False

-- | Print a found binder.
found binder =
  liftIO $ do putStrLnWithColor White (show binder)
              return dynamicNil

-- | Print error message for bounder that wasn't found.
notFound path =
  liftIO $ do putStrLnWithColor Red ("Can't find '" ++ show path ++ "'")
              return dynamicNil

-- | A command at the REPL
-- | TODO: Is it possible to remove the error cases?
data ReplCommand = ReplMacroError String
                 | ReplTypeError String
                 | ReplParseError String
                 | ReplCodegenError String
                 | ReplEval XObj
                 | ListOfCallbacks [CommandCallback]

-- | Parses a string and then converts the resulting forms to commands, which are evaluated in order.
executeString :: Context -> String -> String -> IO Context
executeString ctx input fileName = catch exec (catcher ctx)
  where exec = case parse input fileName of
                 Left parseError -> executeCommand ctx (ReplParseError (show parseError))
                 Right xobjs -> foldM folder ctx xobjs

-- | Used by functions that has a series of forms to evaluate and need to fold over them (producing a new Context in the end)
folder :: Context -> XObj -> IO Context
folder context xobj =
  do cmd <- objToCommand context xobj
     executeCommand context cmd

-- | Take a ReplCommand and execute it.
executeCommand :: Context -> ReplCommand -> IO Context
executeCommand ctx@(Context env typeEnv pathStrings proj lastInput execMode) cmd =
  do when (isJust (envModuleName env)) $
       compilerError ("Global env module name is " ++ fromJust (envModuleName env) ++ " (should be Nothing).")
     case cmd of
       ReplEval xobj ->
         do (result, newCtx) <- runStateT (eval env xobj) ctx
            case result of
              Left e ->
                do putStrLnWithColor Red (show e)
                   throw CancelEvaluationException
                   return newCtx
              Right (XObj (Lst []) _ _) ->
                -- Nil result won't print
                do return newCtx
              Right evaled ->
                do -- HACK?! The result after evalution might be a list that
                   -- constitutes a 'def' or 'defn'. So let's evaluate again
                   -- to make it stick in the environment.
                   -- To log the intermediate result:
                   --putStrLnWithColor Yellow ("-> " ++ (pretty evaled))
                   (result', newCtx') <- runStateT (eval env evaled) newCtx
                   case result' of
                     Left e ->
                       do putStrLnWithColor Red (show e)
                          return newCtx'
                     Right (XObj (Lst []) _ _) ->
                       return newCtx' -- Once again, don't print nil result
                     Right okResult' ->
                       do putStrLnWithColor Yellow ("=> " ++ (pretty okResult'))
                          return newCtx'
       ReplParseError e ->
         do putStrLnWithColor Red ("[PARSE ERROR] " ++ e)
            return ctx
       ReplMacroError e ->
         do putStrLnWithColor Red ("[MACRO ERROR] " ++ e)
            return ctx
       ReplTypeError e ->
         do putStrLnWithColor Red ("[TYPE ERROR] " ++ e)
            return ctx
       ReplCodegenError e ->
         do putStrLnWithColor Red ("[CODEGEN ERROR] " ++ e)
            return ctx
       ListOfCallbacks callbacks -> foldM (\ctx' cb -> callCallbackWithArgs ctx' cb []) ctx callbacks

-- | Call a CommandCallback.
callCallbackWithArgs :: Context -> CommandCallback -> [XObj] -> IO Context
callCallbackWithArgs ctx callback args =
  do (_, newCtx) <- runStateT (callback args) ctx
     return newCtx

-- | Convert an XObj to a ReplCommand so that it can be executed dynamically.
-- | TODO: Does this function need the Context?
objToCommand :: Context -> XObj -> IO ReplCommand
objToCommand ctx (XObj (Sym (SymPath [] (':' : text))) _ _) =
  return (ListOfCallbacks (mapMaybe charToCommand text))
objToCommand ctx xobj =
  return (ReplEval xobj)

-- | Generate commands from shortcut characters (i.e. 'b' = build)
charToCommand :: Char -> Maybe CommandCallback
charToCommand 'x' = Just commandRunExe
charToCommand 'r' = Just commandReload
charToCommand 'b' = Just commandBuild
charToCommand 'c' = Just commandCat
charToCommand 'e' = Just commandListBindings
charToCommand 'h' = Just commandHelp
charToCommand 'p' = Just commandProject
charToCommand 'q' = Just commandQuit
charToCommand _   = Just (\_ -> return dynamicNil)

-- | Decides what to do when the evaluation fails for some reason.
catcher :: Context -> CarpException -> IO Context
catcher ctx exception =
  case exception of
    (ShellOutException message returnCode) ->
      do putStrLnWithColor Red ("[RUNTIME ERROR] " ++ message)
         stop returnCode
    CancelEvaluationException ->
      stop 1
  where stop returnCode =
          case contextExecMode ctx of
            Repl -> return ctx
            Build -> exitWith (ExitFailure returnCode)
            BuildAndRun -> exitWith (ExitFailure returnCode)

-- | Sort different kinds of definitions into the globalEnv or the typeEnv.
define :: Context -> XObj -> IO Context
define ctx@(Context globalEnv typeEnv _ proj _ _) annXObj =
  case annXObj of
    XObj (Lst (XObj (Defalias _) _ _ : _)) _ _ ->
      --putStrLnWithColor Yellow (show (getPath annXObj) ++ " : " ++ show annXObj)
      return (ctx { contextTypeEnv = TypeEnv (envInsertAt (getTypeEnv typeEnv) (getPath annXObj) annXObj) })
    _ ->
      do --putStrLnWithColor Blue (show (getPath annXObj) ++ " : " ++ showMaybeTy (ty annXObj))
         when (projectEchoC proj) $
           putStrLn (toC annXObj)
         let ctx' = registerDefnInInterfaceIfNeeded ctx annXObj
         return (ctx' { contextGlobalEnv = envInsertAt globalEnv (getPath annXObj) annXObj })

-- | Ensure that a 'def' / 'defn' has registered with an interface (if they share the same name).
registerDefnInInterfaceIfNeeded :: Context -> XObj -> Context
registerDefnInInterfaceIfNeeded ctx xobj =
  case xobj of
    XObj (Lst [XObj Defn _ _, XObj (Sym path) _ _, _, _]) _ _ ->
      -- This is a function, does it belong to an interface?
      registerInInterfaceIfNeeded ctx path
    XObj (Lst [XObj Def _ _, XObj (Sym path) _ _, _]) _ _ ->
      -- Global variables can also be part of an interface
      registerInInterfaceIfNeeded ctx path
    _ ->
      ctx

-- | Registers a definition with an interface, if it isn't already registerd.
-- | TODO: Make sure the type of the registered definition can unify with the existing interface.
registerInInterfaceIfNeeded :: Context -> SymPath -> Context
registerInInterfaceIfNeeded ctx path@(SymPath _ name) =
  let typeEnv = (getTypeEnv (contextTypeEnv ctx))
  in case lookupInEnv (SymPath [] name) typeEnv of
       Just (_, Binder (XObj (Lst [XObj (Interface interfaceSignature paths) ii it, isym]) i t)) ->
         let updatedInterface = XObj (Lst [XObj (Interface interfaceSignature (addIfNotPresent path paths)) ii it, isym]) i t
         in  ctx { contextTypeEnv = TypeEnv (extendEnv typeEnv name updatedInterface) }
       Just (_, Binder x) ->
         error ("A non-interface named '" ++ name ++ "' was found in the type environment: " ++ show x)
       Nothing ->
         ctx



-- | SPECIAL FORM COMMANDS (needs to get access to unevaluated arguments, which makes them "special forms" in Lisp lingo)





-- | "NORMAL" COMMANDS (just like the ones in Command.hs, but these need access to 'eval', etc.)

-- | Command for changing various project settings.
commandProjectSet :: CommandCallback
commandProjectSet [XObj (Str key) _ _, value] =
  do ctx <- get
     let proj = contextProj ctx
         env = contextGlobalEnv ctx
     evaled <- eval env value
     case evaled of
       Left e -> err (show e) dynamicNil
       Right (XObj (Str valueStr) _ _) -> do
          newCtx <- case key of
                      "cflag" -> return ctx { contextProj = proj { projectCFlags = addIfNotPresent valueStr (projectCFlags proj) } }
                      "libflag" -> return ctx { contextProj = proj { projectCFlags = addIfNotPresent valueStr (projectCFlags proj) } }
                      "prompt" -> return ctx { contextProj = proj { projectPrompt = valueStr } }
                      "search-path" -> return ctx { contextProj = proj { projectCarpSearchPaths = addIfNotPresent valueStr (projectCarpSearchPaths proj) } }
                      "printAST" -> return ctx { contextProj = proj { projectPrintTypedAST = (valueStr == "true") } }
                      "echoC" -> return ctx { contextProj = proj { projectEchoC = (valueStr == "true") } }
                      "echoCompilationCommand" -> return ctx { contextProj = proj { projectEchoCompilationCommand = (valueStr == "true") } }
                      "compiler" -> return ctx { contextProj = proj { projectCompiler = valueStr } }
                      _ -> err ("Unrecognized key: '" ++ key ++ "'") ctx
          put newCtx
          return dynamicNil
       Right val -> err "Argument to project-set! must be a string" dynamicNil
    where err msg ret = liftIO $ do putStrLnWithColor Red msg
                                    return ret
commandProjectSet args =
  liftIO $ do putStrLnWithColor Red ("Invalid args to 'project-set!' command: " ++ joinWithComma (map pretty args))
              return dynamicNil

-- | Command for loading a Carp file.
commandLoad :: CommandCallback
commandLoad [XObj (Str path) _ _] =
  do ctx <- get
     let proj = contextProj ctx
         carpDir = projectCarpDir proj
         fullSearchPaths =
           path :
           ("./" ++ path) :                                      -- the path from the current directory
           map (++ "/" ++ path) (projectCarpSearchPaths proj) ++ -- user defined search paths
           [carpDir ++ "/core/" ++ path]
            -- putStrLn ("Full search paths = " ++ show fullSearchPaths)
     existingPaths <- liftIO (filterM doesPathExist fullSearchPaths)
     case existingPaths of
       [] ->
         liftIO $ do putStrLnWithColor Red ("Invalid path " ++ path)
                     return dynamicNil
       firstPathFound : _ ->
         do contents <- liftIO $ do --putStrLn ("Will load '" ++ firstPathFound ++ "'")
                                    readFile firstPathFound
            let files = projectFiles proj
                files' = if firstPathFound `elem` files
                         then files
                         else firstPathFound : files
                proj' = proj { projectFiles = files' }
            newCtx <- liftIO $ executeString (ctx { contextProj = proj' }) contents firstPathFound
            put newCtx
            return dynamicNil

-- | Load several files in order.
loadFiles :: Context -> [FilePath] -> IO Context
loadFiles ctxStart filesToLoad = foldM folder ctxStart filesToLoad
  where folder :: Context -> FilePath -> IO Context
        folder ctx file =
          callCallbackWithArgs ctx commandLoad [XObj (Str file) Nothing Nothing]

-- | Command for reloading all files in the project (= the files that has been loaded before).
commandReload :: CommandCallback
commandReload args =
  do ctx <- get
     let paths = projectFiles (contextProj ctx)
         f :: Context -> FilePath -> IO Context
         f context filepath = do contents <- readFile filepath
                                 executeString context contents filepath
     newCtx <- liftIO (foldM f ctx paths)
     put newCtx
     return dynamicNil

-- | Command for expanding a form and its macros.
commandExpand :: CommandCallback
commandExpand [xobj] =
  do ctx <- get
     result <- expandAll eval (contextGlobalEnv ctx) xobj
     case result of
       Left e ->
         liftIO $ do putStrLnWithColor Red (show e)
                     return dynamicNil
       Right expanded ->
         liftIO $ do putStrLnWithColor Yellow (pretty expanded)
                     return dynamicNil
commandExpand args =
  liftIO $ do putStrLnWithColor Red ("Invalid args to 'expand' command: " ++ joinWithComma (map pretty args))
              return dynamicNil

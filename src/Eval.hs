module Eval where

import Data.List (foldl', null, isSuffixOf)
import Data.List.Split (splitOn, splitWhen)
import Control.Monad.State
import Control.Monad.State.Lazy (StateT(..), runStateT, liftIO, modify, get, put)
import System.Exit (exitSuccess, exitFailure, exitWith, ExitCode(..))
import qualified System.IO as SysIO
import System.Directory (doesFileExist, canonicalizePath, createDirectoryIfMissing, getCurrentDirectory, getHomeDirectory, setCurrentDirectory)
import System.FilePath (takeDirectory)
import System.Process (readProcess, readProcessWithExitCode)
import Control.Concurrent (forkIO)
import qualified Data.Map as Map
import Data.Maybe (fromJust, mapMaybe, isJust)
import Control.Monad
import Control.Exception
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Error as ParsecError
import Debug.Trace

import Parsing
import Emit
import Obj
import Types
import Infer
import Deftype
import Sumtypes
import ColorText
import Template
import Util
import Commands
import Expand
import Lookup
import Qualify
import TypeError
import Concretize

-- | Dynamic (REPL) evaluation of XObj:s (s-expressions)
eval :: Env -> XObj -> StateT Context IO (Either EvalError XObj)
eval env xobj =
  case obj xobj of
  --case obj (trace ("\nEval " ++ pretty xobj ++ ", obj: " ++ show (obj xobj)) xobj) of
    Lst _   -> evalList xobj
    Arr _   -> evalArray xobj
    Sym _ _ -> evalSymbol xobj
    _       -> return (Right xobj)

  where
    evalList :: XObj -> StateT Context IO (Either EvalError XObj)
    evalList listXObj@(XObj (Lst xobjs) i t) = do
      ctx <- get
      let fppl = projectFilePathPrintLength (contextProj ctx)
      case xobjs of
        [] ->
          return (Right xobj)

        [XObj (Sym (SymPath [] "quote") _) _ _, target] ->
          return (Right target)

        [XObj (Sym (SymPath [] "file") _) _ _] ->
          case i of
            Just info -> return (Right (XObj (Str (infoFile info)) i t))
            Nothing -> return (makeEvalError ctx Nothing ("No information about object " ++ pretty xobj) (info xobj))

        [XObj (Sym (SymPath [] "line") _) _ _] ->
          case i of
            Just info ->
              return (Right (XObj (Num IntTy (fromIntegral (infoLine info))) i t))
            Nothing ->
              return (makeEvalError ctx Nothing ("No information about object " ++ pretty xobj) (info xobj))

        [XObj (Sym (SymPath [] "column") _) _ _] ->
          case i of
            Just info ->
              return (Right (XObj (Num IntTy (fromIntegral (infoColumn info))) i t))
            Nothing ->
              return (makeEvalError ctx Nothing ("No information about object " ++ pretty xobj) (info xobj))

        [XObj (Sym (SymPath [] "file") _) _ _, XObj _ infoToCheck _] ->
          case infoToCheck of
            Just info -> return (Right (XObj (Str (infoFile info)) i t))
            Nothing -> return (makeEvalError ctx Nothing ("No information about object " ++ pretty xobj) (info xobj))

        [XObj (Sym (SymPath [] "line") _) _ _, XObj _ infoToCheck _] ->
          case infoToCheck of
            Just info ->
              return (Right (XObj (Num IntTy (fromIntegral (infoLine info))) i t))
            Nothing ->
              return (makeEvalError ctx Nothing ("No information about object " ++ pretty xobj) (info xobj))

        [XObj (Sym (SymPath [] "column") _) _ _, XObj _ infoToCheck _] ->
          case infoToCheck of
            Just info ->
              return (Right (XObj (Num IntTy (fromIntegral (infoColumn info))) i t))
            Nothing ->
              return (makeEvalError ctx Nothing ("No information about object " ++ pretty xobj) (info xobj))

        XObj Do _ _ : rest ->
          do evaledList <- fmap sequence (mapM (eval env) rest)
             case evaledList of
               Left e -> return (Left e)
               Right ok ->
                 case ok of
                   [] -> return (makeEvalError ctx Nothing "No forms in 'do' statement." (info xobj))
                   _ -> return (Right (last ok))

        XObj (Sym (SymPath [] "list") _) _ _ : rest ->
          do evaledList <- fmap sequence (mapM (eval env) rest)
             return $ do okList <- evaledList
                         Right (XObj (Lst okList) i t)

        XObj (Sym (SymPath [] "array") _) _ _ : rest ->
          do evaledArray <- fmap sequence (mapM (eval env) rest)
             return $ do okEvaledArray <- evaledArray
                         Right (XObj (Arr okEvaledArray) i t)

        -- 'and' and 'or' are defined here because they are expected to short circuit
        [XObj (Sym (SymPath ["Dynamic"] "and") _) _ _, a, b] ->
          do evaledA <- eval env a
             evaledB <- eval env b
             return $ do okA <- evaledA
                         case okA of
                           XObj (Bol ab) _ _ ->
                             if ab
                               then do okB <- evaledB
                                       case okB of
                                         XObj (Bol bb) _ _ ->
                                           if bb then Right trueXObj else Right falseXObj
                                         _ ->
                                           makeEvalError ctx Nothing ("Can't perform logical operation (and) on " ++ pretty okB) (info okB)
                               else Right falseXObj
                           _ ->
                             makeEvalError ctx Nothing ("Can't perform logical operation (and) on " ++ pretty okA) (info okA)

        [XObj (Sym (SymPath ["Dynamic"] "or") _) _ _, a, b] ->
          do evaledA <- eval env a
             evaledB <- eval env b
             return $ do okA <- evaledA
                         case okA of
                           XObj (Bol ab) _ _ ->
                             if ab
                               then Right trueXObj
                               else do okB <- evaledB
                                       case okB of
                                         XObj (Bol bb) _ _ ->
                                           if bb then Right trueXObj else Right falseXObj
                                         _ ->
                                           makeEvalError ctx Nothing ("Can't perform logical operation (or) on " ++ pretty okB) (info okB)
                           _ ->
                             makeEvalError ctx Nothing ("Can't perform logical operation (or) on " ++ pretty okA) (info okA)

        [XObj If _ _, condition, ifTrue, ifFalse] ->
          do evaledCondition <- eval env condition
             case evaledCondition of
               Right okCondition ->
                 case obj okCondition of
                   Bol b -> if b
                            then eval env ifTrue
                            else eval env ifFalse
                   _ -> return (makeEvalError ctx Nothing ("`if` condition contains non-boolean value: " ++ pretty okCondition) (info okCondition))
               Left err -> return (Left err)

        [defnExpr@(XObj Defn _ _), name, args@(XObj (Arr a) _ _), body] ->
            case obj name of
              (Sym (SymPath [] _) _) ->
                  if all isUnqualifiedSym a
                  then specialCommandDefine xobj
                  else return (makeEvalError ctx Nothing ("`defn` requires all arguments to be unqualified symbols, but it got `" ++ pretty args ++ "`") (info xobj))
              _                      -> return (makeEvalError ctx Nothing ("`defn` identifiers must be unqualified symbols, but it got `" ++ pretty name ++ "`") (info xobj))

        [defnExpr@(XObj Defn _ _), name, invalidArgs, _] ->
            return (makeEvalError ctx Nothing ("`defn` requires an array of symbols as argument list, but it got `" ++ pretty invalidArgs ++ "`") (info xobj))

        (defnExpr@(XObj Defn _ _) : _) ->
            return (makeEvalError ctx Nothing ("I didn’t understand the `defn` at " ++ prettyInfoFromXObj xobj ++ ":\n\n" ++ pretty xobj ++ "\n\nIs it valid? Every `defn` needs to follow the form `(defn name [arg] body)`.") Nothing)

        [defExpr@(XObj Def _ _), name, expr] ->
          if isUnqualifiedSym name
          then specialCommandDefine xobj
          else return (makeEvalError ctx Nothing ("`def` identifiers must be unqualified symbols, but it got `" ++ pretty name ++ "`") (info xobj))

        [theExpr@(XObj The _ _), typeXObj, value] ->
          do evaledValue <- expandAll eval env value -- TODO: Why expand all here?
             return $ do okValue <- evaledValue
                         Right (XObj (Lst [theExpr, typeXObj, okValue]) i t)

        (XObj The _ _ : _) ->
            return (makeEvalError ctx Nothing ("I didn’t understand the `the` at " ++ prettyInfoFromXObj xobj ++ ":\n\n" ++ pretty xobj ++ "\n\nIs it valid? Every `the` needs to follow the form `(the type expression)`.") Nothing)

        [letExpr@(XObj Let _ _), XObj (Arr bindings) bindi bindt, body]
          | odd (length bindings) -> return (makeEvalError ctx Nothing ("Uneven number of forms in `let`: " ++ pretty xobj) (info xobj)) -- Unreachable?
          | not (all isSym (evenIndicies bindings)) -> return (makeEvalError ctx Nothing ("`let` identifiers must be symbols, but it got `" ++ joinWithSpace (map pretty bindings) ++ "`") (info xobj))
          | otherwise ->
              do bind <- mapM (\(n, x) -> do x' <- eval env x
                                             return $ do okX <- x'
                                                         Right [n, okX])
                              (pairwise bindings)
                 let innerEnv = Env Map.empty (Just env) (Just "LET") [] InternalEnv 0
                 let okBindings = sequence bind
                 case okBindings of
                   (Left err) -> return (Left err)
                   Right binds ->
                    case getDuplicate [] binds of
                      Just dup -> return (makeEvalError ctx Nothing ("I encountered a duplicate binding `" ++ dup ++ "` inside a `let`") (info xobj))
                      Nothing -> do
                       let envWithBindings = foldl' (\e [XObj (Sym (SymPath _ n) _) _ _, x] -> extendEnv e n x)
                                     innerEnv
                                     binds
                       evaledBody <- eval envWithBindings body
                       return $ do okBody <- evaledBody
                                   Right okBody
          where getDuplicate _ [] = Nothing
                getDuplicate names ([XObj (Sym (SymPath _ x) _) _ _,y]:xs) =
                  if x `elem` names then Just x else getDuplicate (x:names) xs

        XObj (Sym (SymPath [] "register-type") _) _ _ : XObj (Sym (SymPath _ typeName) _) _ _ : rest ->
          specialCommandRegisterType typeName rest
        XObj (Sym (SymPath _ "register-type") _) _ _ : _ ->
          return (makeEvalError ctx Nothing (show "Invalid args to `register-type`: " ++ pretty xobj) (info xobj))

        XObj (Sym (SymPath [] "deftype") _) _ _ : nameXObj : rest ->
          -- We can't as-pattern on the rest as plenty of type definitions (e.g.) Core.Maybe
          -- don't contain arrays of members.
          case rest of
          (XObj (Arr a) _ _ : _) -> if all isUnqualifiedSym (map fst (members a))
                                   then specialCommandDeftype nameXObj rest
                                   else return (makeEvalError ctx Nothing ("Type members must be unqualified symbols, but got `" ++ concatMap pretty rest ++ "`") (info xobj))
                                   where members (binding:val:xs) = (binding, val):members xs
                                         members [] = []
          _ -> specialCommandDeftype nameXObj rest

        [XObj (Sym (SymPath [] "register") _) _ _, XObj (Sym (SymPath _ name) _) _ _, typeXObj] ->
          specialCommandRegister name typeXObj Nothing
        [XObj (Sym (SymPath [] "register") _) _ _, XObj (Sym (SymPath _ name) _) _ _, typeXObj, XObj (Str overrideName) _ _] ->
          specialCommandRegister name typeXObj (Just overrideName)
        XObj (Sym (SymPath [] "register") _) _ _ : _ ->
          return (makeEvalError ctx Nothing ("Invalid args to `register`: " ++ pretty xobj) (info xobj))

        [XObj (Sym (SymPath [] "definterface") _) _ _, nameXObj@(XObj (Sym _ _) _ _), typeXObj] ->
          specialCommandDefinterface nameXObj typeXObj
        XObj (Sym (SymPath [] "definterface") _) _ _ : _ ->
          return (makeEvalError ctx Nothing ("Invalid args to `definterface`: " ++ pretty xobj) (info xobj))

        [XObj (Sym (SymPath [] "defndynamic") _) _ _, XObj (Sym (SymPath [] name) _) _ _, params, body] ->
          specialCommandDefndynamic name params body
        XObj (Sym (SymPath [] "defndynamic") _) _ _ : _ ->
          return (makeEvalError ctx Nothing ("Invalid args to `defndynamic`: " ++ pretty xobj) (info xobj))

        [XObj (Sym (SymPath [] "defdynamic") _) _ _, XObj (Sym (SymPath [] name) _) _ _, body] ->
          specialCommandDefdynamic name body
        [XObj (Sym (SymPath [] "defdynamic") _) _ _, XObj (Sym (SymPath [] name) _) _ _, _, _] ->
          return (makeEvalError ctx Nothing ("Invalid args to `defdynamic`: " ++ pretty xobj ++ " (did you try to define a dynamic function - use 'defndynamic' instead)") (info xobj))
        XObj (Sym (SymPath [] "defdynamic") _) _ _ : _ ->
          return (makeEvalError ctx Nothing ("Invalid args to `defdynamic`: " ++ pretty xobj) (info xobj))

        [XObj (Sym (SymPath [] "defmacro") _) _ _, XObj (Sym (SymPath [] name) _) _ _, params, body] ->
          specialCommandDefmacro name params body
        XObj (Sym (SymPath [] "defmacro") _) _ _ : _ ->
          return (makeEvalError ctx Nothing ("Invalid args to `defmacro`: " ++ pretty xobj) (info xobj))

        XObj (Sym (SymPath [] "defmodule") _) _ _ : XObj (Sym (SymPath [] moduleName) _) _ _ : innerExpressions ->
          specialCommandDefmodule xobj moduleName innerExpressions
        XObj (Sym (SymPath [] "defmodule") _) _ _ : _ ->
          return (makeEvalError ctx Nothing ("Invalid args to `defmodule`: " ++ pretty xobj) (info xobj))

        [XObj (Sym (SymPath [] "info") _) _ _, target@(XObj (Sym path @(SymPath _ name) _) _ _)] ->
          specialCommandInfo target
        XObj (Sym (SymPath [] "info") _) _ _ : _ ->
          return (makeEvalError ctx Nothing ("Invalid args to `info`: " ++ pretty xobj) (info xobj))

        [XObj (Sym (SymPath [] "type") _) _ _, target] ->
          specialCommandType target
        XObj (Sym (SymPath [] "type") _) _ _ : _ ->
          return (makeEvalError ctx Nothing ("Invalid args to `type`: " ++ pretty xobj) (info xobj))

        [XObj (Sym (SymPath [] "meta-set!") _) _ _, target@(XObj (Sym path @(SymPath _ name) _) _ _), XObj (Str key) _ _, value] -> do
            specialCommandMetaSet path key value
        XObj (Sym (SymPath [] "meta-set!") _) _ _ : _ ->
          return (makeEvalError ctx Nothing ("Invalid args to `meta-set!`: " ++ pretty xobj) (info xobj))

        [XObj (Sym (SymPath [] "meta") _) _ _, target@(XObj (Sym path @(SymPath _ name) _) _ _), XObj (Str key) _ _] -> do
            -- make sure we resolve all variables
            p <- eval env target
            case p of
              Right (XObj (Sym newPath _) _ _) -> specialCommandMetaGet newPath key
              _ -> specialCommandMetaGet path key
        XObj (Sym (SymPath [] "meta") _) _ _ : _ ->
          return (makeEvalError ctx Nothing ("Invalid args to `meta`: " ++ pretty xobj) (info xobj))

        [XObj (Sym (SymPath [] "members") _) _ _, target] ->
          specialCommandMembers target env
        XObj (Sym (SymPath [] "members") _) _ _ : _ ->
          return (makeEvalError ctx Nothing ("Invalid args to `members`: " ++ pretty xobj) (info xobj))

        [XObj (Sym (SymPath [] "use") _) _ _, xobj@(XObj (Sym path _) _ _)] ->
          specialCommandUse xobj path
        XObj (Sym (SymPath [] "use") _) _ _ : _ ->
          return (makeEvalError ctx Nothing ("Invalid args to `use`: " ++ pretty xobj) (info xobj))

        XObj With _ _ : xobj@(XObj (Sym path _) _ _) : forms ->
          specialCommandWith xobj path forms
        XObj With _ _ : _ ->
          return (makeEvalError ctx Nothing ("Invalid args to `with`: " ++ pretty xobj) (info xobj))

        f:args -> do evaledF <- eval env f
                     case evaledF of
                       Right (XObj (Lst [XObj Dynamic _ _, _, XObj (Arr params) _ _, body]) _ _) ->
                         case checkMatchingNrOfArgs ctx fppl f params args of
                           Left err -> return (Left err)
                           Right () ->
                             do evaledArgs <- fmap sequence (mapM (eval env) args)
                                case evaledArgs of
                                  Right okArgs -> apply env body params okArgs
                                  Left err -> return (Left err)

                       Right (XObj (Lst [XObj Macro _ _, _, XObj (Arr params) _ _, body]) _ _) ->
                         case checkMatchingNrOfArgs ctx fppl f params args of
                           Left err ->
                             return (Left err)
                           Right () ->
                             -- Replace info so that the macro which is called gets the source location info of the expansion site.
                             let replacedBody = replaceSourceInfoOnXObj (info xobj) body
                             in  apply env replacedBody params args

                       Right (XObj (Lst [XObj (Command callback) _ _, _]) _ _) ->
                         do evaledArgs <- fmap sequence (mapM (eval env) args)
                            case evaledArgs of
                              Right okArgs -> getCommand callback okArgs
                              Left err -> return (Left err)

                       Right (XObj (Lst (XObj _ _ _ : _)) _ _) ->
                         executeFunctionAsMain ctx listXObj

                       Right x ->
                         return (makeEvalError ctx Nothing ("Can't evaluate " ++ show x) (info x))

                       Left err ->
                         return (Left err)


    evalList _ = error "Can't eval non-list in evalList."

    evalSymbol :: XObj -> StateT Context IO (Either EvalError XObj)
    evalSymbol xobj@(XObj (Sym path@(SymPath pathStrings name) _) _ _) = do
      ctx <- get
      let fppl = projectFilePathPrintLength (contextProj ctx)
      case lookupInEnv (SymPath ("Dynamic" : pathStrings) name) env of -- A slight hack!
        Just (_, Binder _ found) -> return (Right (resolveDef found)) -- use the found value
        Nothing ->
          case lookupInEnv path env of
            Just (_, Binder _ found) -> return (Right (resolveDef found))
            Nothing -> return (makeEvalError ctx Nothing ("Can't find symbol '" ++ show path ++ "'") (info xobj))
    evalSymbol _ = error "Can't eval non-symbol in evalSymbol."

    evalArray :: XObj -> StateT Context IO (Either EvalError XObj)
    evalArray (XObj (Arr xobjs) i t) =
      do evaledXObjs <- fmap sequence (mapM (eval env) xobjs)
         return $ do okXObjs <- evaledXObjs
                     Right (XObj (Arr okXObjs) i t)
    evalArray _ = error "Can't eval non-array in evalArray."

    resolveDef :: XObj -> XObj
    resolveDef (XObj (Lst [XObj DefDynamic _ _, _, value]) _ _) =
      value
    resolveDef x =
      x

-- | Make sure the arg list is the same length as the parameter list
checkMatchingNrOfArgs :: Context -> FilePathPrintLength -> XObj -> [XObj] -> [XObj] -> Either EvalError ()
checkMatchingNrOfArgs ctx fppl xobj params args =
  let usesRestArgs = any (isRestArgSeparator . getName) params
      paramLen = if usesRestArgs then length params - 2 else length params
      argsLen = length args
      expected =
        if usesRestArgs
        then "at least " ++ show paramLen
        else show paramLen
  in  if (usesRestArgs && argsLen > paramLen) || (paramLen == argsLen)
      then Right ()
      else case makeEvalError ctx Nothing ("Wrong number of arguments in call to '" ++ pretty xobj ++ "', expected " ++ expected ++ " but got " ++ show argsLen) (info xobj) of
             Left e -> Left e
             Right _ -> Right ()

-- | Apply a function to some arguments. The other half of 'eval'.
apply :: Env -> XObj -> [XObj] -> [XObj] -> StateT Context IO (Either EvalError XObj)
apply env body params args =
  let insideEnv = Env Map.empty (Just env) Nothing [] InternalEnv 0
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

-- | Is a string the 'rest' separator for arguments to dynamic functions / macros
isRestArgSeparator :: String -> Bool
isRestArgSeparator ":rest" = True
isRestArgSeparator _ = False

-- | Print a found binder.
found binder =
  liftIO $ do putStrLnWithColor White (show binder)
              return dynamicNil

-- | Print error message for binder that wasn't found.
notFound :: ExecutionMode -> XObj -> SymPath -> StateT Context IO (Either EvalError XObj)
notFound execMode xobj path =
  do fppl <- gets (projectFilePathPrintLength . contextProj)
     return $ Left $ EvalError (case execMode of
                                   Check -> machineReadableInfoFromXObj fppl xobj ++ (" Can't find '" ++ show path ++ "'")
                                   _ -> "Can't find '" ++ show path ++ "'") (info xobj) fppl

-- | A command at the REPL
-- | TODO: Is it possible to remove the error cases?
data ReplCommand = ReplParseError String XObj
                 | ReplEval XObj
                 | ListOfCallbacks [CommandCallback]

-- | Parses a string and then converts the resulting forms to commands, which are evaluated in order.
executeString :: Bool -> Context -> String -> String -> IO Context
executeString doCatch ctx input fileName = if doCatch then catch exec (catcher ctx) else exec
  where exec = case parse input fileName of
                 Left parseError ->
                   let sourcePos = Parsec.errorPos parseError
                       parseErrorXObj = XObj (Lst []) (Just dummyInfo { infoFile = fileName
                                                                      , infoLine = Parsec.sourceLine sourcePos
                                                                      , infoColumn = Parsec.sourceColumn sourcePos
                                                                      }) Nothing
                   in  executeCommand ctx (ReplParseError (replaceChars (Map.fromList [('\n', " ")]) (show parseError)) parseErrorXObj)
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
       error ("Global env module name is " ++ fromJust (envModuleName env) ++ " (should be Nothing).")
     case cmd of
       ReplEval xobj ->
         do (result, newCtx) <- runStateT (eval env xobj) ctx
            case result of
              Left e ->
                reportExecutionError newCtx (show e)
              Right (XObj (Lst []) _ _) ->
                -- Nil result won't print
                return newCtx
              Right result@(XObj (Arr _) _ _) ->
                do putStrLnWithColor Yellow ("=> " ++ pretty result)
                   return newCtx
              Right evaled ->
                do -- HACK?! The result after evalution might be a list that
                   -- constitutes a 'def' or 'defn'. So let's evaluate again
                   -- to make it stick in the environment.
                   -- To log the intermediate result:
                   -- putStrLnWithColor Yellow ("-> " ++ (pretty evaled))
                   (result', newCtx') <- runStateT (eval env evaled) newCtx
                   case result' of
                     Left e ->
                       reportExecutionError newCtx' (show e)
                     Right (XObj (Lst []) _ _) ->
                       return newCtx' -- Once again, don't print nil result
                     Right okResult' ->
                       do putStrLnWithColor Yellow ("=> " ++ pretty okResult')
                          return newCtx'
       -- TODO: This is a weird case:
       ReplParseError e xobj ->
         do let msg =  "[PARSE ERROR] " ++ e
                fppl = projectFilePathPrintLength (contextProj ctx)
            case contextExecMode ctx of
              Check -> putStrLn (machineReadableInfoFromXObj fppl xobj ++ " " ++ msg)
              _ -> putStrLnWithColor Red msg
            throw CancelEvaluationException
       ListOfCallbacks callbacks -> foldM (\ctx' cb -> callCallbackWithArgs ctx' cb []) ctx callbacks

reportExecutionError :: Context -> String -> IO Context
reportExecutionError ctx errorMessage =
  case contextExecMode ctx of
    Check ->
      do putStrLn errorMessage
         return ctx
    _ ->
      do putStrLnWithColor Red errorMessage
         throw CancelEvaluationException

-- | Call a CommandCallback.
callCallbackWithArgs :: Context -> CommandCallback -> [XObj] -> IO Context
callCallbackWithArgs ctx callback args =
  do (ret, newCtx) <- runStateT (callback args) ctx
     let fppl = projectFilePathPrintLength (contextProj newCtx)
     case ret of
       Left err -> throw (EvalException (err fppl))
       Right _ -> return newCtx

-- | Convert an XObj to a ReplCommand so that it can be executed dynamically.
-- | TODO: Does this function need the Context?
objToCommand :: Context -> XObj -> IO ReplCommand
objToCommand ctx (XObj (Sym (SymPath [] (':' : text)) _) _ _) =
  return (ListOfCallbacks (mapMaybe charToCommand text))
objToCommand ctx xobj =
  return (ReplEval xobj)

-- | Generate commands from shortcut characters (i.e. 'b' = build)
charToCommand :: Char -> Maybe CommandCallback
charToCommand 'x' = Just commandRunExe
charToCommand 'r' = Just commandReload
charToCommand 'b' = Just (commandBuild False)
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
    EvalException evalError ->
      do putStrLnWithColor Red (show evalError)
         stop 1
  where stop returnCode =
          case contextExecMode ctx of
            Repl -> return ctx
            Build -> exitWith (ExitFailure returnCode)
            Install _ -> exitWith (ExitFailure returnCode)
            BuildAndRun -> exitWith (ExitFailure returnCode)
            Check -> exitSuccess

existingMeta :: Env -> XObj -> MetaData
existingMeta globalEnv xobj =
  case lookupInEnv (getPath xobj) globalEnv of
    Just (_, Binder meta _) -> meta
    Nothing -> emptyMeta

-- | Sort different kinds of definitions into the globalEnv or the typeEnv.
define :: Bool -> Context -> XObj -> IO Context
define hidden ctx@(Context globalEnv typeEnv _ proj _ _) annXObj =
  let previousType =
        case lookupInEnv (getPath annXObj) globalEnv of
          Just (_, Binder _ found) -> ty found
          Nothing -> Nothing
      previousMeta = existingMeta globalEnv annXObj
      adjustedMeta = if hidden
                     then previousMeta { getMeta = Map.insert "hidden" trueXObj (getMeta previousMeta) }
                     else previousMeta
      fppl = projectFilePathPrintLength proj
  in case annXObj of
       XObj (Lst (XObj (Defalias _) _ _ : _)) _ _ ->
         return (ctx { contextTypeEnv = TypeEnv (envInsertAt (getTypeEnv typeEnv) (getPath annXObj) (Binder adjustedMeta annXObj)) })
       XObj (Lst (XObj (Typ _) _ _ : _)) _ _ ->
         return (ctx { contextTypeEnv = TypeEnv (envInsertAt (getTypeEnv typeEnv) (getPath annXObj) (Binder adjustedMeta annXObj)) })
       XObj (Lst (XObj (DefSumtype _) _ _ : _)) _ _ ->
         return (ctx { contextTypeEnv = TypeEnv (envInsertAt (getTypeEnv typeEnv) (getPath annXObj) (Binder adjustedMeta annXObj)) })
       _ ->
         do case Map.lookup "sig" (getMeta adjustedMeta) of
              Just foundSignature ->
                do let Just sigTy = xobjToTy foundSignature
                   unless (areUnifiable (forceTy annXObj) sigTy) $
                     throw $ EvalException (EvalError ("Definition at " ++ prettyInfoFromXObj annXObj ++ " does not match `sig` annotation " ++
                              show sigTy ++ ", actual type is `" ++ show (forceTy annXObj) ++ "`.") Nothing fppl)
              Nothing ->
                return ()
            when (projectEchoC proj) $
              putStrLn (toC All (Binder emptyMeta annXObj))
            case previousType of
              Just previousTypeUnwrapped ->
                unless (areUnifiable (forceTy annXObj) previousTypeUnwrapped) $
                  do putStrWithColor Blue ("[WARNING] Definition at " ++ prettyInfoFromXObj annXObj ++ " changed type of '" ++ show (getPath annXObj) ++
                                           "' from " ++ show previousTypeUnwrapped ++ " to " ++ show (forceTy annXObj))
                     putStrLnWithColor White "" -- To restore color for sure.
              Nothing -> return ()
            case registerDefnOrDefInInterfaceIfNeeded ctx annXObj of
              Left err ->
                do case contextExecMode ctx of
                     Check -> let fppl = projectFilePathPrintLength (contextProj ctx)
                              in  putStrLn (machineReadableInfoFromXObj fppl annXObj ++ " " ++ err)
                     _ -> putStrLnWithColor Red err
                   return ctx
              Right ctx' ->
                return (ctx' { contextGlobalEnv = envInsertAt globalEnv (getPath annXObj) (Binder adjustedMeta annXObj) })

-- | Ensure that a 'def' / 'defn' has registered with an interface (if they share the same name).
registerDefnOrDefInInterfaceIfNeeded :: Context -> XObj -> Either String Context
registerDefnOrDefInInterfaceIfNeeded ctx xobj =
  case xobj of
    XObj (Lst [XObj Defn _ _, XObj (Sym path _) _ _, _, _]) _ (Just t) ->
      -- This is a function, does it belong to an interface?
      registerInInterfaceIfNeeded ctx path t
    XObj (Lst [XObj Def _ _, XObj (Sym path _) _ _, _]) _ (Just t) ->
      -- Global variables can also be part of an interface
      registerInInterfaceIfNeeded ctx path t
    _ ->
      return ctx

-- | Registers a definition with an interface, if it isn't already registerd.
registerInInterfaceIfNeeded :: Context -> SymPath -> Ty -> Either String Context
registerInInterfaceIfNeeded ctx path@(SymPath _ name) definitionSignature =
  let typeEnv = getTypeEnv (contextTypeEnv ctx)
  in case lookupInEnv (SymPath [] name) typeEnv of
       Just (_, Binder _ (XObj (Lst [XObj (Interface interfaceSignature paths) ii it, isym]) i t)) ->
         if areUnifiable interfaceSignature definitionSignature
         then let updatedInterface = XObj (Lst [XObj (Interface interfaceSignature (addIfNotPresent path paths)) ii it, isym]) i t
              in  return $ ctx { contextTypeEnv = TypeEnv (extendEnv typeEnv name updatedInterface) }
         else Left ("[INTERFACE ERROR] " ++ show path ++ " : " ++ show definitionSignature ++
                    " doesn't match the interface signature " ++ show interfaceSignature)
       Just (_, Binder _ x) ->
         error ("A non-interface named '" ++ name ++ "' was found in the type environment: " ++ show x)
       Nothing ->
         return ctx

annotateWithinContext :: Bool -> XObj -> StateT Context IO (Either EvalError (XObj, [XObj]))
annotateWithinContext qualifyDefn xobj =
  do ctx <- get
     let pathStrings = contextPath ctx
         fppl = projectFilePathPrintLength (contextProj ctx)
         globalEnv = contextGlobalEnv ctx
         typeEnv = contextTypeEnv ctx
         innerEnv = getEnv globalEnv pathStrings
     expansionResult <- expandAll eval globalEnv xobj
     ctxAfterExpansion <- get
     case expansionResult of
       Left err -> return (makeEvalError ctx Nothing (show err) Nothing)
       Right expanded ->
         let xobjFullPath = if qualifyDefn then setFullyQualifiedDefn expanded (SymPath pathStrings (getName xobj)) else expanded
             xobjFullSymbols = setFullyQualifiedSymbols typeEnv globalEnv innerEnv xobjFullPath
         in case annotate typeEnv globalEnv xobjFullSymbols of
              Left err ->
                case contextExecMode ctx of
                  Check ->
                    let fppl = projectFilePathPrintLength (contextProj ctx)
                    in  return (Left (EvalError (joinWith "\n" (machineReadableErrorStrings fppl err)) Nothing fppl))
                  _ ->
                    return (Left (EvalError (show err) Nothing fppl))
              Right ok ->
                return (Right ok)

-- | SPECIAL FORM COMMANDS (needs to get access to unevaluated arguments, which makes them "special forms" in Lisp lingo)

specialCommandDefine :: XObj -> StateT Context IO (Either EvalError XObj)
specialCommandDefine xobj =
  do result <- annotateWithinContext True xobj
     case result of
       Right (annXObj, annDeps) ->
         do ctxAfterExpansion <- get
            ctxWithDeps <- liftIO $ foldM (define True) ctxAfterExpansion annDeps
            ctxWithDef <- liftIO $ define False ctxWithDeps annXObj
            put ctxWithDef
            return dynamicNil
       Left err ->
         return (Left err)

specialCommandRegisterType :: String -> [XObj] -> StateT Context IO (Either EvalError XObj)
specialCommandRegisterType typeName rest =
  do ctx <- get
     let pathStrings = contextPath ctx
         fppl = projectFilePathPrintLength (contextProj ctx)
         globalEnv = contextGlobalEnv ctx
         typeEnv = contextTypeEnv ctx
         innerEnv = getEnv globalEnv pathStrings
         path = SymPath pathStrings typeName
         typeDefinition = XObj (Lst [XObj ExternalType Nothing Nothing, XObj (Sym path Symbol) Nothing Nothing]) Nothing (Just TypeTy)
         i = Nothing
         preExistingModule = case lookupInEnv (SymPath pathStrings typeName) globalEnv of
                               Just (_, Binder _ (XObj (Mod found) _ _)) -> Just found
                               _ -> Nothing
     case rest of
       [] ->
         do put (ctx { contextTypeEnv = TypeEnv (extendEnv (getTypeEnv typeEnv) typeName typeDefinition) })
            return dynamicNil
       members ->
         case bindingsForRegisteredType typeEnv globalEnv pathStrings typeName members i preExistingModule of
           Left err ->
             return (makeEvalError ctx (Just err) (show err) Nothing)
           Right (typeModuleName, typeModuleXObj, deps) ->
             let ctx' = (ctx { contextGlobalEnv = envInsertAt globalEnv (SymPath pathStrings typeModuleName) (Binder emptyMeta typeModuleXObj)
                             , contextTypeEnv = TypeEnv (extendEnv (getTypeEnv typeEnv) typeName typeDefinition)
                             })
             in do contextWithDefs <- liftIO $ foldM (define True) ctx' deps
                   put contextWithDefs
                   return dynamicNil

specialCommandDeftype :: XObj -> [XObj] -> StateT Context IO (Either EvalError XObj)
specialCommandDeftype nameXObj@(XObj (Sym (SymPath _ typeName) _) _ _) rest =
  deftypeInternal nameXObj typeName [] rest
specialCommandDeftype (XObj (Lst (nameXObj@(XObj (Sym (SymPath _ typeName) _) _ _) : typeVariables)) _ _) rest =
  deftypeInternal nameXObj typeName typeVariables rest
specialCommandDeftype nameXObj _ =
  do ctx <- get
     return (makeEvalError ctx Nothing ("Invalid name for type definition: " ++ pretty nameXObj) (info nameXObj))

deftypeInternal :: XObj -> String -> [XObj] -> [XObj] -> StateT Context IO (Either EvalError XObj)
deftypeInternal nameXObj typeName typeVariableXObjs rest =
  do ctx <- get
     let pathStrings = contextPath ctx
         fppl = projectFilePathPrintLength (contextProj ctx)
         env = contextGlobalEnv ctx
         typeEnv = contextTypeEnv ctx
         typeVariables = mapM xobjToTy typeVariableXObjs
         preExistingModule = case lookupInEnv (SymPath pathStrings typeName) env of
                               Just (_, Binder _ (XObj (Mod found) _ _)) -> Just found
                               _ -> Nothing
         (creatorFunction, typeConstructor) =
            if length rest == 1 && isArray (head rest)
            then (moduleForDeftype, Typ)
            else (moduleForSumtype, DefSumtype)
     case (nameXObj, typeVariables) of
       (XObj (Sym (SymPath _ typeName) _) i _, Just okTypeVariables) ->
         case creatorFunction typeEnv env pathStrings typeName okTypeVariables rest i preExistingModule of
           Right (typeModuleName, typeModuleXObj, deps) ->
             let structTy = StructTy typeName okTypeVariables
                 typeDefinition =
                   -- NOTE: The type binding is needed to emit the type definition and all the member functions of the type.
                   XObj (Lst (XObj (typeConstructor structTy) Nothing Nothing :
                              XObj (Sym (SymPath pathStrings typeName) Symbol) Nothing Nothing :
                              rest)
                        ) i (Just TypeTy)
                 ctx' = (ctx { contextGlobalEnv = envInsertAt env (SymPath pathStrings typeModuleName) (Binder emptyMeta typeModuleXObj)
                             , contextTypeEnv = TypeEnv (extendEnv (getTypeEnv typeEnv) typeName typeDefinition)
                             })
             in do ctxWithDeps <- liftIO (foldM (define True) ctx' deps)
                   let ctxWithInterfaceRegistrations =
                         foldM (\context (path, sig) -> registerInInterfaceIfNeeded context path sig) ctxWithDeps
                               [(SymPath (pathStrings ++ [typeModuleName]) "str", FuncTy [RefTy structTy] StringTy)
                               ,(SymPath (pathStrings ++ [typeModuleName]) "copy", FuncTy [RefTy structTy] structTy)]
                   case ctxWithInterfaceRegistrations of
                     Left err -> liftIO (putStrLnWithColor Red err)
                     Right ok -> put ok
                   return dynamicNil
           Left err ->
             return (makeEvalError ctx (Just err) ("Invalid type definition for '" ++ pretty nameXObj ++ "':\n\n" ++ show err) Nothing)
       (_, Nothing) ->
         return (makeEvalError ctx Nothing ("Invalid type variables for type definition: " ++ pretty nameXObj) (info nameXObj))

specialCommandRegister :: String -> XObj -> Maybe String -> StateT Context IO (Either EvalError XObj)
specialCommandRegister name typeXObj overrideName =
  do ctx <- get
     let pathStrings = contextPath ctx
         fppl = projectFilePathPrintLength (contextProj ctx)
         globalEnv = contextGlobalEnv ctx
     case xobjToTy typeXObj of
           Just t -> let path = SymPath pathStrings name
                         registration = XObj (Lst [XObj (External overrideName) Nothing Nothing,
                                                   XObj (Sym path Symbol) Nothing Nothing])
                                        (info typeXObj) (Just t)
                         meta = existingMeta globalEnv registration
                         env' = envInsertAt globalEnv path (Binder meta registration)
                     in  case registerInInterfaceIfNeeded ctx path t of
                           Left errorMessage ->
                             return (makeEvalError ctx Nothing errorMessage (info typeXObj))
                           Right ctx' ->
                             do put (ctx' { contextGlobalEnv = env' })
                                return dynamicNil
           Nothing ->
             return (makeEvalError ctx Nothing ("Can't understand type when registering '" ++ name ++ "'") (info typeXObj))

specialCommandDefinterface :: XObj -> XObj -> StateT Context IO (Either EvalError XObj)
specialCommandDefinterface nameXObj@(XObj (Sym path@(SymPath [] name) _) _ _) typeXObj =
  do ctx <- get
     let env = contextGlobalEnv ctx
         fppl = projectFilePathPrintLength (contextProj ctx)
         typeEnv = getTypeEnv (contextTypeEnv ctx)
     case xobjToTy typeXObj of
       Just t ->
         case lookupInEnv path typeEnv of
           Just (_, Binder _ (XObj (Lst (XObj (Interface foundType _) _ _ : _)) _ _)) ->
             -- The interface already exists, so it will be left as-is.
             if foundType == t
             then return dynamicNil
             else liftIO $ do putStrLn ("[FORBIDDEN] Tried to change the type of interface '" ++ show path ++ "' from " ++ show foundType ++ " to " ++ show t)
                              return dynamicNil
           Nothing ->
             let interface = defineInterface name t [] (info nameXObj)
                 typeEnv' = TypeEnv (envInsertAt typeEnv (SymPath [] name) (Binder emptyMeta interface))
             in  do put (ctx { contextTypeEnv = typeEnv' })
                    return dynamicNil
       Nothing ->
         return (makeEvalError ctx Nothing ("Invalid type for interface '" ++ name ++ "': " ++ pretty typeXObj) (info typeXObj))

dynamicOrMacroWith :: (SymPath -> [XObj]) -> Ty -> String -> XObj -> StateT Context IO (Either EvalError XObj)
dynamicOrMacroWith producer ty name body =
  do ctx <- get
     let pathStrings = contextPath ctx
         globalEnv = contextGlobalEnv ctx
         path = SymPath pathStrings name
         elem = XObj (Lst (producer path)) (info body) (Just ty)
         meta = existingMeta globalEnv elem
     put (ctx { contextGlobalEnv = envInsertAt globalEnv path (Binder meta elem) })
     return dynamicNil

dynamicOrMacro :: Obj -> Ty -> String -> XObj -> XObj -> StateT Context IO (Either EvalError XObj)
dynamicOrMacro pat ty name params body =
  dynamicOrMacroWith (\path -> [XObj pat Nothing Nothing, XObj (Sym path Symbol) Nothing Nothing, params, body]) ty name body

specialCommandDefndynamic :: String -> XObj -> XObj -> StateT Context IO (Either EvalError XObj)
specialCommandDefndynamic = dynamicOrMacro Dynamic DynamicTy

specialCommandDefdynamic :: String -> XObj -> StateT Context IO (Either EvalError XObj)
specialCommandDefdynamic name body =
  do env <- gets contextGlobalEnv
     result <- eval env body
     case result of
       Left err -> return (Left err)
       Right evaledBody ->
         dynamicOrMacroWith (\path -> [XObj DefDynamic Nothing Nothing, XObj (Sym path Symbol) Nothing Nothing, evaledBody]) DynamicTy name body

specialCommandDefmacro :: String -> XObj -> XObj -> StateT Context IO (Either EvalError XObj)
specialCommandDefmacro = dynamicOrMacro Macro MacroTy

specialCommandDefmodule :: XObj -> String -> [XObj] -> StateT Context IO (Either EvalError XObj)
specialCommandDefmodule xobj moduleName innerExpressions =
  do ctx <- get
     let pathStrings = contextPath ctx
         fppl = projectFilePathPrintLength (contextProj ctx)
         env = contextGlobalEnv ctx
         typeEnv = contextTypeEnv ctx
         lastInput = contextLastInput ctx
         execMode = contextExecMode ctx
         proj = contextProj ctx

         defineIt :: MetaData -> StateT Context IO (Either EvalError XObj)
         defineIt meta = do let parentEnv = getEnv env pathStrings
                                innerEnv = Env (Map.fromList []) (Just parentEnv) (Just moduleName) [] ExternalEnv 0
                                newModule = XObj (Mod innerEnv) (info xobj) (Just ModuleTy)
                                globalEnvWithModuleAdded = envInsertAt env (SymPath pathStrings moduleName) (Binder meta newModule)
                                ctx' = Context globalEnvWithModuleAdded typeEnv (pathStrings ++ [moduleName]) proj lastInput execMode
                            ctxAfterModuleDef <- liftIO $ foldM folder ctx' innerExpressions
                            put (popModulePath ctxAfterModuleDef)
                            return dynamicNil

     result <- case lookupInEnv (SymPath pathStrings moduleName) env of
                 Just (_, Binder _ (XObj (Mod _) _ _)) ->
                   do let ctx' = Context env typeEnv (pathStrings ++ [moduleName]) proj lastInput execMode -- use { = } syntax instead
                      ctxAfterModuleAdditions <- liftIO $ foldM folder ctx' innerExpressions
                      put (popModulePath ctxAfterModuleAdditions)
                      return dynamicNil -- TODO: propagate errors...
                 Just (_, Binder existingMeta (XObj (Lst [(XObj DocStub _ _), _]) _ _)) ->
                   defineIt existingMeta
                 Just (_, Binder _ x) ->
                   return (makeEvalError ctx Nothing ("Can't redefine '" ++ moduleName ++ "' as module") (info xobj))
                 Nothing ->
                   defineIt emptyMeta

     case result of
       Left err -> return (Left err)
       Right _ -> return dynamicNil

specialCommandInfo :: XObj -> StateT Context IO (Either EvalError XObj)
specialCommandInfo target@(XObj (Sym path@(SymPath _ name) _) _ _) =
  do ctx <- get
     let env = contextGlobalEnv ctx
         typeEnv = contextTypeEnv ctx
         proj = contextProj ctx
         execMode = contextExecMode ctx
         printer allowLookupInALL binderPair itIsAnErrorNotToFindIt =
           case binderPair of
             Just (_, binder@(Binder metaData x@(XObj _ (Just i) _))) ->
               do putStrLn (show binder ++ "\nDefined at " ++ prettyInfo i)
                  case Map.lookup "doc" (getMeta metaData) of
                    Just (XObj (Str val) _ _) -> putStrLn ("Documentation: " ++ val)
                    Nothing -> return ()
                  when (projectPrintTypedAST proj) $ putStrLnWithColor Yellow (prettyTyped x)
             Just (_, binder@(Binder metaData x)) ->
               do print binder
                  case Map.lookup "doc" (getMeta metaData) of
                    Just (XObj (Str val) _ _) -> putStrLn ("Documentation: " ++ val)
                    Nothing -> return ()
                  when (projectPrintTypedAST proj) $ putStrLnWithColor Yellow (prettyTyped x)
             Nothing ->
               when allowLookupInALL
               (case multiLookupALL name env of
                 [] ->
                   when itIsAnErrorNotToFindIt $
                     putStrLn $
                       case execMode of
                         Check -> let fppl = projectFilePathPrintLength (contextProj ctx)
                                  in  machineReadableInfoFromXObj fppl target ++ (" Can't find '" ++ show path ++ "'")
                         _ -> strWithColor Red ("Can't find '" ++ show path ++ "'")
                 binders ->
                   mapM_ (\(env, binder@(Binder _ (XObj _ i _))) ->
                            case i of
                              Just i' -> putStrLnWithColor White (show binder ++ " Defined at " ++ prettyInfo i')
                              Nothing -> putStrLnWithColor White (show binder))
                         binders)
     case path of
       SymPath [] _ ->
         -- First look in the type env, then in the global env:
         do case lookupInEnv path (getTypeEnv typeEnv) of
              Nothing -> liftIO (printer True (lookupInEnv path env) True)
              found -> do liftIO (printer True found True) -- this will print the interface itself
                          liftIO (printer True (lookupInEnv path env) False) -- this will print the locations of the implementers of the interface
            return dynamicNil
       qualifiedPath ->
         case lookupInEnv path env of
           Nothing -> notFound execMode target path
           found -> do liftIO (printer False found True)
                       return dynamicNil

specialCommandType :: XObj -> StateT Context IO (Either EvalError XObj)
specialCommandType target =
  do ctx <- get
     let env = contextGlobalEnv ctx
         execMode = contextExecMode ctx
     case target of
           XObj (Sym path@(SymPath [] name) _) _ _ ->
             case lookupInEnv path env of
               Just (_, binder) ->
                 found binder
               Nothing ->
                 case multiLookupALL name env of
                   [] ->
                     notFound execMode target path
                   binders ->
                     liftIO $ do mapM_ (\(env, binder) -> putStrLnWithColor White (show binder)) binders
                                 return dynamicNil
           XObj (Sym qualifiedPath _) _ _ ->
             case lookupInEnv qualifiedPath env of
               Just (_, binder) ->
                 found binder
               Nothing ->
                 notFound execMode target qualifiedPath
           _ ->
             liftIO $ do putStrLnWithColor Red ("Can't get the type of non-symbol: " ++ pretty target)
                         return dynamicNil

specialCommandMembers :: XObj -> Env -> StateT Context IO (Either EvalError XObj)
specialCommandMembers target env =
  do ctx <- get
     let typeEnv = contextTypeEnv ctx
         fppl = projectFilePathPrintLength (contextProj ctx)
     case bottomedTarget target of
           XObj (Sym path@(SymPath _ name) _) _ _ ->
              case lookupInEnv path (getTypeEnv typeEnv) of
                Just (_, Binder _ (XObj (Lst [
                  XObj (Typ structTy) Nothing Nothing,
                  XObj (Sym (SymPath pathStrings typeName) Symbol) Nothing Nothing,
                  XObj (Arr members) _ _]) _ _))
                  ->
                    return (Right (XObj (Arr (map (\(a, b) -> XObj (Lst [a, b]) Nothing Nothing) (pairwise members))) Nothing Nothing))
                _ ->
                  return (makeEvalError ctx Nothing ("Can't find a struct type named '" ++ name ++ "' in type environment") (info target))
           _ -> return (makeEvalError ctx Nothing ("Can't get the members of non-symbol: " ++ pretty target) (info target))
  where bottomedTarget target =
          case target of
            XObj (Sym targetPath _) _ _ ->
              case lookupInEnv targetPath env of
                -- this is a trick: every type generates a module in the env;
                -- we’re special-casing here because we need the parent of the
                -- module
                Just (_, Binder _ (XObj (Mod _) _ _)) -> target
                -- if we’re recursing into a non-sym, we’ll stop one level down
                Just (_, Binder _ x) -> bottomedTarget x
                _ -> target
            _ -> target

specialCommandUse :: XObj -> SymPath -> StateT Context IO (Either EvalError XObj)
specialCommandUse xobj path =
  do ctx <- get
     let pathStrings = contextPath ctx
         fppl = projectFilePathPrintLength (contextProj ctx)
         env = contextGlobalEnv ctx
         e = getEnv env pathStrings
         useThese = envUseModules e
         e' = if path `elem` useThese then e else e { envUseModules = path : useThese }
         innerEnv = getEnv env pathStrings -- Duplication of e?
     case lookupInEnv path innerEnv of
       Just (_, Binder _ _) ->
         do put $ ctx { contextGlobalEnv = envReplaceEnvAt env pathStrings e' }
            return dynamicNil
       Nothing ->
         return (makeEvalError ctx Nothing ("Can't find a module named '" ++ show path ++ "'") (info xobj))

specialCommandWith :: XObj -> SymPath -> [XObj] -> StateT Context IO (Either EvalError XObj)
specialCommandWith xobj path forms =
  do ctx <- get
     let pathStrings = contextPath ctx
         env = contextGlobalEnv ctx
         typeEnv = contextTypeEnv ctx
         useThese = envUseModules env
         env' = if path `elem` useThese then env else env { envUseModules = path : useThese }
         ctx' = ctx { contextGlobalEnv = env' }
     ctxAfter <- liftIO $ foldM folder ctx' forms
     let envAfter = contextGlobalEnv ctxAfter
         ctxAfter' = ctx { contextGlobalEnv = envAfter { envUseModules = useThese } } -- This will undo ALL use:s made inside the 'with'.
     put ctxAfter'
     return dynamicNil

-- | Set meta data for a Binder
specialCommandMetaSet :: SymPath -> String -> XObj -> StateT Context IO (Either EvalError XObj)
specialCommandMetaSet path key value =
  do ctx <- get
     let pathStrings = contextPath ctx
         fppl = projectFilePathPrintLength (contextProj ctx)
         globalEnv = contextGlobalEnv ctx
     case lookupInEnv (consPath pathStrings path) globalEnv of
       Just (_, binder@(Binder _ xobj)) ->
         -- | Set meta on existing binder
         setMetaOn ctx binder
       Nothing ->
         case path of
           -- | If the path is unqualified, create a binder and set the meta on that one. This enables docstrings before function exists.
           (SymPath [] name) ->
             setMetaOn ctx (Binder emptyMeta (XObj (Lst [XObj DocStub Nothing Nothing,
                                                         XObj (Sym (SymPath pathStrings name) Symbol) Nothing Nothing])
                                              (Just dummyInfo)
                                              (Just (VarTy "a"))))
           (SymPath _ _) ->
             return (makeEvalError ctx Nothing ("Special command 'meta-set!' failed, can't find '" ++ show path ++ "'") (info value))
       where
         setMetaOn :: Context -> Binder -> StateT Context IO (Either EvalError XObj)
         setMetaOn ctx binder@(Binder metaData xobj) =
           do let globalEnv = contextGlobalEnv ctx
                  newMetaData = MetaData (Map.insert key value (getMeta metaData))
                  xobjPath = getPath xobj
                  newBinder = binder { binderMeta = newMetaData }
                  newEnv = envInsertAt globalEnv xobjPath newBinder
              put (ctx { contextGlobalEnv = newEnv })
              return dynamicNil

-- | Get meta data for a Binder
specialCommandMetaGet :: SymPath -> String -> StateT Context IO (Either EvalError XObj)
specialCommandMetaGet path key =
  do ctx <- get
     let pathStrings = contextPath ctx
         fppl = projectFilePathPrintLength (contextProj ctx)
         globalEnv = contextGlobalEnv ctx
     case lookupInEnv (consPath pathStrings path) globalEnv of
       Just (_, Binder metaData _) ->
           case Map.lookup key (getMeta metaData) of
             Just foundValue ->
               return (Right foundValue)
             Nothing ->
               return dynamicNil
       Nothing ->
         return (makeEvalError ctx Nothing ("Special command 'meta' failed, can't find '" ++ show path ++ "'") Nothing)



-- | "NORMAL" COMMANDS (just like the ones in Command.hs, but these need access to 'eval', etc.)

-- | Command for loading a Carp file.
commandLoad :: CommandCallback
commandLoad [xobj@(XObj (Str path) i _)] =
  do ctx <- get
     home <- liftIO getHomeDirectory
     let relativeTo = case i of
                        Just ii ->
                          case infoFile ii of
                            "REPL" -> "."
                            file -> takeDirectory file
                        Nothing -> "."
         proj = contextProj ctx
         libDir = home ++ "/" ++ projectLibDir proj
         carpDir = projectCarpDir proj
         fullSearchPaths =
           path :
           (relativeTo ++ "/" ++ path) :                         -- the path from the file that contains the '(load)', or the current directory if not loading from a file (e.g. the repl)
           map (++ "/" ++ path) (projectCarpSearchPaths proj) ++ -- user defined search paths
           [carpDir ++ "/core/" ++ path] ++
           [libDir ++ "/" ++ path]
     existingPaths <- liftIO (filterM doesFileExist fullSearchPaths)
     case existingPaths of
       [] ->
        if '@' `elem` path
          then tryInstall path
          else return $ invalidPath ctx path
       firstPathFound : _ ->
         do canonicalPath <- liftIO (canonicalizePath firstPathFound)
            fileThatLoads <- liftIO (canonicalizePath (case i of
                                                         Just ii -> infoFile ii
                                                         Nothing -> ""))
            if canonicalPath == fileThatLoads
              then return $ cantLoadSelf ctx path
              else do let alreadyLoaded = projectAlreadyLoaded proj
                      if canonicalPath `elem` alreadyLoaded
                        then
                             return ()
                        else do contents <- liftIO $ do
                                                        handle <- SysIO.openFile canonicalPath SysIO.ReadMode
                                                        SysIO.hSetEncoding handle SysIO.utf8
                                                        SysIO.hGetContents handle
                                let files = projectFiles proj
                                    files' = if canonicalPath `elem` files
                                             then files
                                             else files ++ [canonicalPath]
                                    proj' = proj { projectFiles = files', projectAlreadyLoaded = canonicalPath : alreadyLoaded }
                                newCtx <- liftIO $ executeString True (ctx { contextProj = proj' }) contents canonicalPath
                                put newCtx
                      return dynamicNil
  where
    fppl ctx =
      projectFilePathPrintLength (contextProj ctx)
    invalidPath ctx path =
      Left $ EvalError
        ((case contextExecMode ctx of
          Check ->
            machineReadableInfoFromXObj (fppl ctx) xobj ++ " I can't find a file named: '" ++ path ++ "'"
          _ -> "I can't find a file named: '" ++ path ++ "'") ++
        "\n\nIf you tried loading an external package, try appending a version string (like `@master`)") (info xobj)
    invalidPathWith ctx path stderr =
      Left $ EvalError
        ((case contextExecMode ctx of
          Check ->
            machineReadableInfoFromXObj (fppl ctx) xobj ++ " I can't find a file named: '" ++ path ++ "'"
          _ -> "I can't find a file named: '" ++ path ++ "'") ++
        "\n\nI tried interpreting the statement as a git import, but got: " ++ stderr) (info xobj)
    cantLoadSelf ctx path =
      case contextExecMode ctx of
        Check ->
          Left (EvalError (machineReadableInfoFromXObj (fppl ctx) xobj ++ " A file can't load itself: '" ++ path ++ "'") Nothing)
        _ ->
          Left (EvalError ("A file can't load itself: '" ++ path ++ "'") (info xobj))
    tryInstall path =
      let split = splitOn "@" path
      in tryInstallWithCheckout (joinWith "@" (init split)) (last split)
    fromURL url =
      let split = splitOn "/" url
          fst = head split
      in if fst `elem` ["https:", "http:"]
        then joinWith "/" (tail split)
        else
          if '@' `elem` fst
            then joinWith "/" (joinWith "@" (tail (splitOn "@" fst)) : tail split)
            else url
    tryInstallWithCheckout path toCheckout = do
      ctx <- get
      home <- liftIO getHomeDirectory
      let proj = contextProj ctx
      let libDir = home ++ "/" ++ projectLibDir proj
      let fpath = libDir ++ "/" ++ fromURL path ++ "/" ++ toCheckout
      cur <- liftIO getCurrentDirectory
      _ <- liftIO $ createDirectoryIfMissing True fpath
      _ <- liftIO $ setCurrentDirectory fpath
      _ <- liftIO $ readProcessWithExitCode "git" ["init"] ""
      _ <- liftIO $ readProcessWithExitCode "git" ["remote", "add", "origin", path] ""
      (x0, _, stderr0) <- liftIO $ readProcessWithExitCode "git" ["fetch", "--all", "--tags"] ""
      case x0 of
        ExitFailure _ -> do
          _ <- liftIO $ setCurrentDirectory cur
          return $ invalidPathWith ctx path stderr0
        ExitSuccess -> do
          (x1, _, stderr1) <- liftIO $ readProcessWithExitCode "git" ["checkout", toCheckout] ""
          _ <- liftIO $ setCurrentDirectory cur
          case x1 of
            ExitSuccess ->
              let fName = last (splitOn "/" path)
                  realName' = if ".git" `isSuffixOf` fName
                               then take (length fName - 4) fName
                               else fName
                  realName = if ".carp" `isSuffixOf` realName'
                              then realName'
                              else realName' ++ ".carp"
                  fileToLoad = fpath ++ "/" ++ realName
                  mainToLoad = fpath ++ "/main.carp"
              in do
                res <- commandLoad [XObj (Str fileToLoad) Nothing Nothing]
                case res of
                  ret@(Right _) -> return ret
                  Left _ ->  commandLoad [XObj (Str mainToLoad) Nothing Nothing]
            ExitFailure _ ->
                return $ invalidPathWith ctx path stderr1
commandLoad [x] =
  return $ Left (EvalError ("Invalid args to `load`: " ++ pretty x) (info x))


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
         f context filepath = do let proj = contextProj context
                                     alreadyLoaded = projectAlreadyLoaded proj
                                 if filepath `elem` alreadyLoaded
                                   then
                                        return context
                                   else do
                                           contents <- readFile filepath
                                           let proj' = proj { projectAlreadyLoaded = filepath : alreadyLoaded }
                                           executeString False (context { contextProj = proj' }) contents filepath
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
         return (Left (removeFppl e))
       Right expanded ->
         liftIO $ do putStrLnWithColor Yellow (pretty expanded)
                     return dynamicNil
  where removeFppl (EvalError msg info fppl) = EvalError msg info

-- | This function will show the resulting C code from an expression.
-- | i.e. (Int.+ 2 3) => "_0 = 2 + 3"
commandC :: CommandCallback
commandC [xobj] =
  do ctx <- get
     let globalEnv = contextGlobalEnv ctx
         typeEnv = contextTypeEnv ctx
     result <- expandAll eval globalEnv xobj
     case result of
       Left err -> return (Left (EvalError (show err) (info xobj)))
       Right expanded ->
         case annotate typeEnv globalEnv (setFullyQualifiedSymbols typeEnv globalEnv globalEnv expanded) of
           Left err -> return (Left (EvalError (show err) (info xobj)))
           Right (annXObj, annDeps) ->
             do liftIO (printC annXObj)
                liftIO (mapM printC annDeps)
                return dynamicNil

-- | Helper function for commandC
printC :: XObj -> IO ()
printC xobj =
  case checkForUnresolvedSymbols xobj of
    Left e ->
      putStrLnWithColor Red (show e ++ ", can't print resulting code.\n")
    Right _ ->
      putStrLnWithColor Green (toC All (Binder emptyMeta xobj))

-- | This allows execution of calls to non-dynamic functions (defined with 'defn') to be run from the REPL
executeFunctionAsMain :: Context -> XObj -> StateT Context IO (Either EvalError XObj)
executeFunctionAsMain ctx expression =
  let fppl = projectFilePathPrintLength (contextProj ctx)
      tempMainFunction x = XObj (Lst [XObj Defn (Just dummyInfo) Nothing
                                     ,XObj (Sym (SymPath [] "main") Symbol) (Just dummyInfo) Nothing
                                     ,XObj (Arr []) (Just dummyInfo) Nothing
                                     ,case ty x of
                                        Just UnitTy -> x
                                        Just (RefTy _) -> XObj (Lst [XObj (Sym (SymPath [] "println*") Symbol) (Just dummyInfo) Nothing, x])
                                                               (Just dummyInfo) (Just UnitTy)
                                        Just _ -> XObj (Lst [XObj (Sym (SymPath [] "println*") Symbol) (Just dummyInfo) Nothing,
                                                             XObj (Lst [XObj Ref (Just dummyInfo) Nothing, x])
                                                                   (Just dummyInfo) (Just UnitTy)])
                                                       (Just dummyInfo) (Just UnitTy)
                                     ]) (Just dummyInfo) (Just (FuncTy [] UnitTy))
  in  do r <- annotateWithinContext False expression
         case r of
           Right (annXObj, annDeps) ->
             do let m = tempMainFunction annXObj

                ctxAfterExpansion <- get
                ctxWithDeps <- liftIO $ foldM (define True) ctxAfterExpansion annDeps
                put ctxWithDeps

                defineResult <- specialCommandDefine m
                case defineResult of
                  Left e -> return (Left e)
                  Right _ ->
                    do buildResult <- commandBuild True []
                       case buildResult of
                         Left e -> return (Left (e fppl))
                         Right _ ->
                           do executionResult <- commandRunExe []
                              case executionResult of
                                Left e -> return (Left (e fppl))
                                Right _ -> return dynamicNil
           Left err ->
             return (Left err)

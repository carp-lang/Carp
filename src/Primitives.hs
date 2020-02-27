module Primitives where

import Control.Monad.State.Lazy (StateT(..), get, put, liftIO, foldM, when, unless)
import qualified Data.Map as Map

import ColorText
import Commands
import Deftype
import Emit
import Lookup
import Obj
import TypeError
import Types
import Util

type Primitive = XObj -> Env -> [XObj] -> StateT Context IO (Either EvalError XObj)

makePrim :: String -> Int -> String -> Primitive -> (SymPath, Primitive)
makePrim name arity example callback =
  makePrim' name (Just arity) example callback

makeVarPrim :: String -> String -> Primitive -> (SymPath, Primitive)
makeVarPrim name example callback =
  makePrim' name Nothing example callback

makePrim' :: String -> Maybe Int -> String -> Primitive -> (SymPath, Primitive)
makePrim' name maybeArity example callback =
  let path = SymPath [] name
  in (path, wrapped)
  where wrapped =
          case maybeArity of
            Just a ->
              \x e l ->
                let ll = length l
                in (if ll /= a then err x a ll else callback x e l)
            Nothing -> callback
        err :: XObj -> Int -> Int -> StateT Context IO (Either EvalError XObj)
        err x a l = do
          ctx <- get
          return (makeEvalError ctx Nothing (
            "The primitive '" ++ name ++ "' expected " ++ show a ++
            " arguments, but got " ++ show l ++ ".\n\nExample Usage:\n```\n" ++
            example ++ "\n```\n") (info x))

primitiveFile :: Primitive
primitiveFile x@(XObj _ i t) _ [XObj _ mi _] = do
  ctx <- get
  case mi of
    Just info -> return (Right (XObj (Str (infoFile info)) i t))
    Nothing ->
      return (makeEvalError ctx Nothing ("No information about object " ++ pretty x) (info x))

primitiveLine :: Primitive
primitiveLine x@(XObj _ i t) _ [XObj _ mi _] = do
  ctx <- get
  case mi of
    Just info -> return (Right (XObj (Num IntTy (fromIntegral (infoLine info))) i t))
    Nothing ->
      return (makeEvalError ctx Nothing ("No information about object " ++ pretty x) (info x))

primitiveColumn :: Primitive
primitiveColumn x@(XObj _ i t) _ [XObj _ mi _] = do
  ctx <- get
  case mi of
    Just info -> return (Right (XObj (Num IntTy (fromIntegral (infoColumn info))) i t))
    Nothing ->
      return (makeEvalError ctx Nothing ("No information about object " ++ pretty x) (info x))

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
       Nothing -> return ctx

-- | Ensure that a 'def' / 'defn' has registered with an interface (if they share the same name).
registerDefnOrDefInInterfaceIfNeeded :: Context -> XObj -> Either String Context
registerDefnOrDefInInterfaceIfNeeded ctx xobj =
  case xobj of
    XObj (Lst [XObj (Defn _) _ _, XObj (Sym path _) _ _, _, _]) _ (Just t) ->
      -- This is a function, does it belong to an interface?
      registerInInterfaceIfNeeded ctx path t
    XObj (Lst [XObj Def _ _, XObj (Sym path _) _ _, _]) _ (Just t) ->
      -- Global variables can also be part of an interface
      registerInInterfaceIfNeeded ctx path t
    _ -> return ctx

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
       XObj (Lst (XObj (Deftype _) _ _ : _)) _ _ ->
         return (ctx { contextTypeEnv = TypeEnv (envInsertAt (getTypeEnv typeEnv) (getPath annXObj) (Binder adjustedMeta annXObj)) })
       XObj (Lst (XObj (DefSumtype _) _ _ : _)) _ _ ->
         return (ctx { contextTypeEnv = TypeEnv (envInsertAt (getTypeEnv typeEnv) (getPath annXObj) (Binder adjustedMeta annXObj)) })
       _ ->
         do when (projectEchoC proj) $
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

primitiveRegisterType :: Primitive
primitiveRegisterType _ e [XObj (Sym (SymPath [] t) _) _ _] = do
  ctx <- get
  let pathStrings = contextPath ctx
      typeEnv = contextTypeEnv ctx
      path = SymPath pathStrings t
      typeDefinition = XObj (Lst [XObj ExternalType Nothing Nothing, XObj (Sym path Symbol) Nothing Nothing]) Nothing (Just TypeTy)
  put (ctx { contextTypeEnv = TypeEnv (extendEnv (getTypeEnv typeEnv) t typeDefinition) })
  return dynamicNil
primitiveRegisterType _ _ [x] = do
  ctx <- get
  return (makeEvalError ctx Nothing ("`register-type` takes a symbol, but it got " ++ pretty x) (info x))
primitiveRegisterType _ _ (XObj (Sym (SymPath [] t) _) _ _:members) = do
  ctx <- get
  let pathStrings = contextPath ctx
      globalEnv = contextGlobalEnv ctx
      typeEnv = contextTypeEnv ctx
      path = SymPath pathStrings t
      preExistingModule = case lookupInEnv (SymPath pathStrings t) globalEnv of
                            Just (_, Binder _ (XObj (Mod found) _ _)) -> Just found
                            _ -> Nothing
  case bindingsForRegisteredType typeEnv globalEnv pathStrings t members Nothing preExistingModule of
    Left err -> return (makeEvalError ctx (Just err) (show err) Nothing)
    Right (typeModuleName, typeModuleXObj, deps) -> do
      let typeDefinition = XObj (Lst [XObj ExternalType Nothing Nothing, XObj (Sym path Symbol) Nothing Nothing]) Nothing (Just TypeTy)
          ctx' = (ctx { contextGlobalEnv = envInsertAt globalEnv (SymPath pathStrings typeModuleName) (Binder emptyMeta typeModuleXObj)
                      , contextTypeEnv = TypeEnv (extendEnv (getTypeEnv typeEnv) t typeDefinition)
                      })
      contextWithDefs <- liftIO $ foldM (define True) ctx' deps
      put contextWithDefs
      return dynamicNil

notFound :: XObj -> SymPath -> StateT Context IO (Either EvalError XObj)
notFound x path = do
  ctx <- get
  return (makeEvalError ctx Nothing ("I can’t find the symbol `" ++ show path ++ "`") (info x))

primitiveInfo :: Primitive
primitiveInfo _ env [target@(XObj (Sym path@(SymPath _ name) _) _ _)] = do
  ctx <- get
  let typeEnv = contextTypeEnv ctx
  case path of
    SymPath [] _ ->
      -- First look in the type env, then in the global env:
      case lookupInEnv path (getTypeEnv typeEnv) of
        Nothing -> printer True True (lookupInEnv path env)
        found -> do printer True True found -- this will print the interface itself
                    printer True False (lookupInEnv path env)-- this will print the locations of the implementers of the interface
    qualifiedPath ->
      case lookupInEnv path env of
        Nothing -> notFound target path
        found -> printer False True found
  where printer allowLookupInALL errNotFound binderPair = do
          ctx <- get
          let proj = contextProj ctx
          case binderPair of
           Just (_, binder@(Binder metaData x@(XObj _ (Just i) _))) ->
             do liftIO $ putStrLn (show binder ++ "\nDefined at " ++ prettyInfo i)
                case Map.lookup "doc" (getMeta metaData) of
                  Just (XObj (Str val) _ _) -> liftIO $ putStrLn ("Documentation: " ++ val)
                  Nothing -> return ()
                liftIO $ when (projectPrintTypedAST proj) $ putStrLnWithColor Yellow (prettyTyped x)
                return dynamicNil
           Just (_, binder@(Binder metaData x)) ->
             do liftIO $ print binder
                case Map.lookup "doc" (getMeta metaData) of
                  Just (XObj (Str val) _ _) -> liftIO $ putStrLn ("Documentation: " ++ val)
                  Nothing -> return ()
                liftIO $ when (projectPrintTypedAST proj) $ putStrLnWithColor Yellow (prettyTyped x)
                return dynamicNil
           Nothing ->
             if allowLookupInALL
             then case multiLookupALL name env of
                   [] -> if errNotFound then notFound target path else return dynamicNil
                   binders -> do
                     liftIO$ mapM_ (\(env, binder@(Binder _ (XObj _ i _))) ->
                              case i of
                                Just i' -> putStrLnWithColor White (show binder ++ " Defined at " ++ prettyInfo i')
                                Nothing -> putStrLnWithColor White (show binder))
                           binders
                     return dynamicNil
            else if errNotFound then notFound target path else return dynamicNil

primitives :: Map.Map SymPath Primitive
primitives = Map.fromList
  [ makePrim "quote" 1 "(quote x) ; where x is an actual symbol" (\_ _ [x] -> return (Right x))
  , makePrim "file" 1 "(file mysymbol)" primitiveFile
  , makePrim "line" 1 "(line mysymbol)" primitiveLine
  , makePrim "column" 1 "(column mysymbol)" primitiveColumn
  , makePrim "info" 1 "(info mysymbol)" primitiveInfo
  , makeVarPrim "register-type" "(register-type Name <optional: members>)" primitiveRegisterType
  ]

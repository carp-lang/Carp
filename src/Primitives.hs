module Primitives where

import Control.Applicative
import Control.Monad (unless, when, foldM)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.List (union)
import Data.Maybe (fromMaybe)
import Data.Either (rights)
import Web.Browser (openBrowser)

import ColorText
import Commands
import Deftype
import Emit
import Lookup
import Obj
import Path (takeFileName)
import Project
import Sumtypes
import TypeError
import Types
import Util
import Template
import ToTemplate
import Info
import qualified Meta as Meta
import Interfaces
import Infer
import Reify


-- found :: (MonadIO m, Show a1) => a2 -> a1 -> m (a2, Either a3 XObj)
-- found ctx binder =
--   liftIO $ do putStrLnWithColor White (show binder)
--               pure (ctx, dynamicNil)

makePrim :: String -> Int -> String -> String -> Primitive -> (String, Binder)
makePrim name arity doc example callback =
  makePrim' name (Just arity) doc example callback

makeVarPrim :: String -> String -> String -> Primitive -> (String, Binder)
makeVarPrim name doc example callback =
  makePrim' name Nothing doc example callback

argumentErr :: Context -> String -> String -> String -> XObj -> IO (Context, Either EvalError XObj)
argumentErr ctx fun ty number actual =
  pure (evalError ctx (
            "`" ++ fun ++ "` expected " ++ ty ++ " as its " ++ number ++
            " argument, but got `" ++ pretty actual ++ "`") (info actual))

makePrim' :: String -> Maybe Int -> String -> String -> Primitive -> (String, Binder)
makePrim' name maybeArity docString example callback =
  let path = SymPath [] name
      prim = XObj (Lst [ XObj (Primitive (PrimitiveFunction wrapped)) (Just dummyInfo) Nothing
                       , XObj (Sym path Symbol) Nothing Nothing
                       , unfoldArgs
                       ])
            (Just dummyInfo) (Just DynamicTy)
      meta = Meta.set "doc" (XObj (Str doc) Nothing Nothing) emptyMeta
  in (name, Binder meta prim)
  where wrapped =
          case maybeArity of
            Just a ->
              \x c l ->
                let ll = length l
                in (if ll /= a then err x c a ll else callback x c l)
            Nothing -> callback
        err :: XObj -> Context -> Int -> Int -> IO (Context, Either EvalError XObj)
        err x ctx a l =
          pure (evalError ctx (
            "The primitive `" ++ name ++ "` expected " ++ show a ++
            " arguments, but got " ++ show l ++ ".\n\n" ++ exampleUsage) (info x))
        doc = docString ++ "\n\n" ++ exampleUsage
        exampleUsage = "Example Usage:\n```\n" ++ example ++ "\n```\n"
        unfoldArgs =
          case maybeArity of
            Just arity ->
              let tosym x = (XObj (Sym (SymPath [] x) Symbol) Nothing Nothing)
              in  XObj (Arr (map (tosym . intToArgName) [1..arity])) Nothing Nothing
            Nothing -> XObj (Arr [(XObj (Sym (SymPath [] "") Symbol) Nothing Nothing)]) Nothing Nothing

primitiveFile :: Primitive
primitiveFile x@(XObj _ i t) ctx args =
  pure $ case args of
             [] -> go i
             [XObj _ mi _] -> go mi
             _ -> evalError ctx
                            ("`file` expected 0 or 1 arguments, but got " ++ show (length args))
                            (info x)
  where err = evalError ctx ("No information about object " ++ pretty x) (info x)
        go  = maybe err (\info ->
          let fppl = projectFilePathPrintLength (contextProj ctx)
              file = infoFile info
              file' = case fppl of
                        FullPath -> file
                        ShortPath -> takeFileName file
          in (ctx, Right (XObj (Str file') i t)))

primitiveLine :: Primitive
primitiveLine x@(XObj _ i t) ctx args =
  pure $ case args of
             [] ->  go i
             [XObj _ mi _] -> go mi
             _ -> evalError ctx
                            ("`line` expected 0 or 1 arguments, but got " ++ show (length args))
                            (info x)
  where err = evalError ctx ("No information about object " ++ pretty x) (info x)
        go  = maybe err (\info -> (ctx, Right (XObj (Num IntTy (fromIntegral (infoLine info))) i t)))

primitiveColumn :: Primitive
primitiveColumn x@(XObj _ i t) ctx args =
  pure $ case args of
             [] -> go i
             [XObj _ mi _] -> go mi
             _ -> evalError ctx
                 ("`column` expected 0 or 1 arguments, but got " ++ show (length args))
                 (info x)
  where err = evalError ctx ("No information about object " ++ pretty x) (info x)
        go  = maybe err (\info -> (ctx, Right (XObj (Num IntTy (fromIntegral (infoColumn info))) i t)))

primitiveImplements :: Primitive
primitiveImplements xobj ctx [x@(XObj (Sym interface@(SymPath _ _) _) _ _), inner@(XObj (Sym impl@(SymPath prefixes name) _) inf _)] =
  let global = contextGlobalEnv ctx
      def = lookupInEnv impl global
  in  maybe notFound found def
  where fullPath@(SymPath modules name') = consPath (union (contextPath ctx) prefixes) (SymPath [] name)
        checkInterface = let warn = do putStrWithColor Blue ("[WARNING] The interface " ++ show interface ++ " implemented by " ++ show impl ++
                                                              " at " ++ prettyInfoFromXObj xobj ++ " is not defined." ++
                                                              " Did you define it using `definterface`?")
                                       putStrLnWithColor White "" -- To restore color for sure.
                             tyEnv = getTypeEnv . contextTypeEnv $ ctx
                         in maybe warn (\_ -> pure ()) (lookupInEnv interface tyEnv)
        -- If the implementation binding doesn't exist yet, set the implements
        -- meta. This enables specifying a function as an implementation before
        -- defining it.
        --
        -- This is only allowed for qualified bindings. Allowing forward declarations on global bindings would cause a loop in
        -- primitiveMetaSet's lookup which is generic.
        notFound = if null modules
                   then pure $ evalError ctx "Can't set the `implements` meta on a global definition before it is declared." inf
                   else (checkInterface >>
                         primitiveMetaSet xobj ctx [inner, XObj (Str "implements") (Just dummyInfo) (Just StringTy), XObj (Lst [x]) (Just dummyInfo) (Just DynamicTy)])
        found (_, Binder meta defobj) = checkInterface >>
                                        either registerError updateImpls (registerInInterface ctx defobj interface)
              where registerError e = do case contextExecMode ctx of
                                           Check -> let fppl = projectFilePathPrintLength (contextProj ctx)
                                                    in  putStrLn (machineReadableInfoFromXObj fppl defobj ++ " " ++ e)
                                           _ -> putStrLnWithColor Red e
                                         pure $ evalError ctx e (info x)
                    updateImpls ctx' = do currentImplementations <- primitiveMeta xobj ctx [inner, XObj (Str "implements") (Just dummyInfo) (Just StringTy)]
                                          pure $ either metaError existingImpls (snd currentImplementations)
                      where metaError e = (ctx, Left e)
                            existingImpls is = case is of
                                                 old@(XObj (Lst impls) inf ty) ->
                                                   let newImpls = if x `elem` impls
                                                                  then old
                                                                  else XObj (Lst (x : impls)) inf ty
                                                       newMeta = Meta.set "implements" newImpls meta
                                                   in (ctx' {contextGlobalEnv = envInsertAt global (getPath defobj) (Binder newMeta defobj)}, dynamicNil)
                                                 _ -> let impls = XObj (Lst [x]) (Just dummyInfo) (Just DynamicTy)
                                                          newMeta = Meta.set "implements" impls meta
                                                      in (ctx' {contextGlobalEnv = envInsertAt global (getPath defobj) (Binder newMeta defobj)}, dynamicNil)
                            global = contextGlobalEnv ctx
primitiveImplements xobj ctx [x, y] =
  pure $ evalError ctx ("`implements` expects symbol arguments.") (info x)
primitiveImplements x@(XObj _ i t) ctx args =
  pure $ evalError
    ctx ("`implements` expected 2 arguments, but got " ++ show (length args)) (info x)


define :: Bool -> Context -> XObj -> IO Context
define hidden ctx@(Context globalEnv _ typeEnv _ proj _ _ _) annXObj =
  let previousType =
        case lookupInEnv (getPath annXObj) globalEnv of
          Just (_, Binder _ found) -> ty found
          Nothing -> Nothing
      previousMeta = existingMeta globalEnv annXObj
      adjustedMeta = if hidden
                     then Meta.set "hidden" trueXObj previousMeta
                     else previousMeta
      fppl = projectFilePathPrintLength proj
  in case annXObj of
       XObj (Lst (XObj (Defalias _) _ _ : _)) _ _ ->
         pure (ctx { contextTypeEnv = TypeEnv (envInsertAt (getTypeEnv typeEnv) (getPath annXObj) (Binder adjustedMeta annXObj)) })
       XObj (Lst (XObj (Deftype _) _ _ : _)) _ _ ->
         pure (ctx { contextTypeEnv = TypeEnv (envInsertAt (getTypeEnv typeEnv) (getPath annXObj) (Binder adjustedMeta annXObj)) })
       XObj (Lst (XObj (DefSumtype _) _ _ : _)) _ _ ->
         pure (ctx { contextTypeEnv = TypeEnv (envInsertAt (getTypeEnv typeEnv) (getPath annXObj) (Binder adjustedMeta annXObj)) })
       _ ->
         do when (projectEchoC proj) $
              putStrLn (toC All (Binder emptyMeta annXObj))
            case previousType of
              Just previousTypeUnwrapped ->
                unless (areUnifiable (forceTy annXObj) previousTypeUnwrapped) $
                  do putStrWithColor Blue ("[WARNING] Definition at " ++ prettyInfoFromXObj annXObj ++ " changed type of '" ++ show (getPath annXObj) ++
                                           "' from " ++ show previousTypeUnwrapped ++ " to " ++ show (forceTy annXObj))
                     putStrLnWithColor White "" -- To restore color for sure.
              Nothing -> pure ()
            case Meta.get "implements" previousMeta of
              Just (XObj (Lst interfaces) _ _) ->
                do let result = foldM (\ctx (xobj, interface) -> registerInInterface ctx xobj interface) ctx (zip (cycle [annXObj]) (map getPath interfaces))
                   case result of
                     Left err ->
                       do case contextExecMode ctx of
                            Check ->
                              let fppl = projectFilePathPrintLength (contextProj ctx)
                              in putStrLn (machineReadableInfoFromXObj fppl annXObj ++ " " ++ err)
                            _ -> putStrLnWithColor Red err
                          pure ctx
                     Right ctx' -> pure (ctx' {contextGlobalEnv = envInsertAt globalEnv (getPath annXObj) (Binder adjustedMeta annXObj)})
              _ -> pure (ctx {contextGlobalEnv = envInsertAt globalEnv (getPath annXObj) (Binder adjustedMeta annXObj)})

primitiveRegisterType :: Primitive
primitiveRegisterType _ ctx [XObj (Sym (SymPath [] t) _) _ _] =
  primitiveRegisterTypeWithoutFields ctx t Nothing
primitiveRegisterType _ ctx [x] =
  pure (evalError ctx ("`register-type` takes a symbol, but it got " ++ pretty x) (info x))
primitiveRegisterType _ ctx [XObj (Sym (SymPath [] t) _) _ _, XObj (Str override) _ _] =
  primitiveRegisterTypeWithoutFields ctx t (Just override)
primitiveRegisterType _ ctx [x@(XObj (Sym (SymPath [] t) _) _ _), (XObj (Str override) _ _), members] =
  primitiveRegisterTypeWithFields ctx x t (Just override) members
primitiveRegisterType _ ctx [x@(XObj (Sym (SymPath [] t) _) _ _), members] =
  primitiveRegisterTypeWithFields ctx x t Nothing members
primitiveRegisterType _ ctx _ =
  pure (evalError ctx (
    "I don't understand this usage of `register-type`.\n\n" ++
    "Valid usages :\n" ++
    "  (register-type Name)\n" ++
    "  (register-type Name [field0 Type, ...])\n" ++
    "  (register-type Name c-name)\n" ++
    "  (register-type Name c-name [field0 Type, ...]") Nothing)


primitiveRegisterTypeWithoutFields :: Context -> String -> (Maybe String) -> IO (Context, Either EvalError XObj)
primitiveRegisterTypeWithoutFields ctx t override = do
  let pathStrings = contextPath ctx
      typeEnv = contextTypeEnv ctx
      path = SymPath pathStrings t
      typeDefinition = XObj (Lst [XObj (ExternalType override) Nothing Nothing, XObj (Sym path Symbol) Nothing Nothing]) Nothing (Just TypeTy)
  pure (ctx { contextTypeEnv = TypeEnv (extendEnv (getTypeEnv typeEnv) t typeDefinition) }, dynamicNil)

primitiveRegisterTypeWithFields :: Context -> XObj -> String -> (Maybe String) -> XObj -> IO (Context, Either EvalError XObj)
primitiveRegisterTypeWithFields ctx x t override members =
  either handleErr updateContext
    (bindingsForRegisteredType typeEnv globalEnv pathStrings t [members] Nothing preExistingModule)
  where handleErr e = pure $ makeEvalError ctx (Just e) (show e) (info x)
        updateContext (typeModuleName, typeModuleXObj, deps) =
          do let typeDefinition = XObj (Lst [XObj (ExternalType override) Nothing Nothing, XObj (Sym path Symbol) Nothing Nothing]) Nothing (Just TypeTy)
                 ctx' = (ctx { contextGlobalEnv = envInsertAt globalEnv (SymPath pathStrings typeModuleName) (Binder emptyMeta typeModuleXObj)
                             , contextTypeEnv = TypeEnv (extendEnv (getTypeEnv typeEnv) t typeDefinition)
                             })
             contextWithDefs <- liftIO $ foldM (define True) ctx' deps
             pure (contextWithDefs, dynamicNil)
        pathStrings = contextPath ctx
        globalEnv = contextGlobalEnv ctx
        typeEnv = contextTypeEnv ctx
        path = SymPath pathStrings t
        preExistingModule = case lookupInEnv (SymPath pathStrings t) globalEnv of
                              Just (_, Binder _ (XObj (Mod found) _ _)) -> Just found
                              _ -> Nothing

notFound :: Context -> XObj -> SymPath -> IO (Context, Either EvalError XObj)
notFound ctx x path =
  pure (evalError ctx ("I can’t find the symbol `" ++ show path ++ "`") (info x))

primitiveInfo :: Primitive
primitiveInfo _ ctx [target@(XObj (Sym path@(SymPath _ name) _) _ _)] = do
  let env = contextEnv ctx
      typeEnv = contextTypeEnv ctx
  case path of
    SymPath [] _ ->
      -- First look in the type env, then in the global env:
      case lookupInEnv path (getTypeEnv typeEnv) of
        Nothing -> printer env True True (lookupInEnv path env)
        found -> do _ <- printer env True True found -- this will print the interface itself
                    printer env True False (lookupInEnv path env)-- this will print the locations of the implementers of the interface
    qualifiedPath ->
      case lookupInEnv path env of
        Nothing -> notFound ctx target path
        found -> printer env False True found
  where printer env allowLookupInALL errNotFound binderPair = do
          let proj = contextProj ctx
          case binderPair of
           Just (_, binder@(Binder metaData x@(XObj _ (Just i) _))) ->
             do liftIO $ putStrLn (show binder ++ "\nDefined at " ++ prettyInfo i)
                printDoc metaData proj x
           Just (_, binder@(Binder metaData x)) ->
             do liftIO $ print binder
                printDoc metaData proj x
           Nothing | allowLookupInALL ->
            case multiLookupALL name env of
                [] -> if errNotFound then notFound ctx target path else
                        pure (ctx,  dynamicNil)
                binders -> do liftIO $
                                mapM_
                                  (\ (env, binder@(Binder metaData x@(XObj _ i _))) ->
                                     case i of
                                         Just i' -> do
                                          putStrLnWithColor White
                                                    (show binder ++ "\nDefined at " ++ prettyInfo i')
                                          _ <- printDoc metaData proj x
                                          pure ()
                                         Nothing -> putStrLnWithColor White (show binder))
                                  binders
                              pure (ctx, dynamicNil)
                   | errNotFound -> notFound ctx target path
                   | otherwise -> pure (ctx, dynamicNil)
        printDoc metaData proj x = do
          case Meta.get "doc" metaData of
            Just (XObj (Str val) _ _) -> liftIO $ putStrLn ("Documentation: " ++ val)
            Nothing -> pure ()
          case Meta.get "implements" metaData of
            Just xobj@(XObj object info _) -> do
              case info of
                Just info' -> putStrLn $ "Implementing: " ++ getName xobj
                Nothing -> pure ()
            Nothing -> pure ()
          liftIO $ when (projectPrintTypedAST proj) $ putStrLnWithColor Yellow (prettyTyped x)
          pure (ctx, dynamicNil)
primitiveInfo _ ctx [notName] =
  argumentErr ctx "info" "a name" "first" notName

dynamicOrMacroWith :: Context -> (SymPath -> [XObj]) -> Ty -> String -> XObj -> IO (Context, Either EvalError XObj)
dynamicOrMacroWith ctx producer ty name body = do
  let pathStrings = contextPath ctx
      globalEnv = contextGlobalEnv ctx
      path = SymPath pathStrings name
      elem = XObj (Lst (producer path)) (info body) (Just ty)
      meta = existingMeta globalEnv elem
  pure (ctx { contextGlobalEnv = envInsertAt globalEnv path (Binder meta elem) }, dynamicNil)

primitiveMembers :: Primitive
primitiveMembers _ ctx [target] = do
  let env = contextEnv ctx
      typeEnv = contextTypeEnv ctx
      fppl = projectFilePathPrintLength (contextProj ctx)
  case bottomedTarget env target of
        XObj (Sym path@(SymPath _ name) _) _ _ ->
           case lookupInEnv path (getTypeEnv typeEnv) of
             Just (_, Binder _ (XObj (Lst [
               XObj (Deftype structTy) Nothing Nothing,
               XObj (Sym (SymPath pathStrings typeName) Symbol) Nothing Nothing,
               XObj (Arr members) _ _]) _ _))
               ->
                 pure (ctx, Right (XObj (Arr (map (\(a, b) -> XObj (Lst [a, b]) Nothing Nothing) (pairwise members))) Nothing Nothing))
             Just (_, Binder _ (XObj (Lst (
               XObj (DefSumtype structTy) Nothing Nothing :
               XObj (Sym (SymPath pathStrings typeName) Symbol) Nothing Nothing :
               sumtypeCases)) _ _))
               ->
                 pure (ctx, Right (XObj (Arr (concatMap getMembersFromCase sumtypeCases)) Nothing Nothing))
               where getMembersFromCase :: XObj -> [XObj]
                     getMembersFromCase (XObj (Lst members) _ _) =
                       map (\(a, b) -> XObj (Lst [a, b]) Nothing Nothing) (pairwise members)
                     getMembersFromCase x@(XObj (Sym sym _) _ _) =
                       [XObj (Lst [x, XObj (Arr []) Nothing Nothing]) Nothing Nothing]
                     getMembersFromCase (XObj x _ _) =
                       error ("Can't handle case " ++ show x)
             _ ->
               pure (evalError ctx ("Can't find a struct type named '" ++ name ++ "' in type environment") (info target))
        _ -> pure (evalError ctx ("Can't get the members of non-symbol: " ++ pretty target) (info target))
  where bottomedTarget env target =
          case target of
            XObj (Sym targetPath _) _ _ ->
              case lookupInEnv targetPath env of
                -- this is a trick: every type generates a module in the env;
                -- we’re special-casing here because we need the parent of the
                -- module
                Just (_, Binder _ (XObj (Mod _) _ _)) -> target
                -- if we’re recursing into a non-sym, we’ll stop one level down
                Just (_, Binder _ x) -> bottomedTarget env x
                _ -> target
            _ -> target

-- | Set meta data for a Binder
primitiveMetaSet :: Primitive
primitiveMetaSet _ ctx [target@(XObj (Sym path@(SymPath prefixes name) _) _ _), XObj (Str key) _ _, value] =
  pure $ maybe create (\newCtx -> (newCtx, dynamicNil)) lookupAndUpdate

  where fullPath@(SymPath modules name') = consPath (union (contextPath ctx) prefixes) (SymPath [] name)
        dynamicPath = (consPath ["Dynamic"] fullPath)
        global = contextGlobalEnv ctx
        types = (getTypeEnv (contextTypeEnv ctx))

        lookupAndUpdate :: Maybe Context
        lookupAndUpdate = ((lookupInEnv dynamicPath global)
                            >>= \(e, binder) -> (pure (Meta.updateBinderMeta binder key value))
                            >>= \b -> (pure (envInsertAt global dynamicPath b))
                            >>= \env -> pure (ctx {contextGlobalEnv = env}))
                          <|> ((lookupInEnv fullPath global)
                               >>= \(e, binder) -> (pure (Meta.updateBinderMeta binder key value))
                               >>= \b -> (pure (envInsertAt global fullPath b))
                               >>= \env -> pure (ctx {contextGlobalEnv = env}))
                          -- This is a global name but it doesn't exist in the global env
                          -- Before creating a new binder, check that it doesn't denote an existing type or interface.
                          <|> (if (null modules)
                              then ((lookupInEnv fullPath types)
                                   >>= \(_, binder) -> (pure (Meta.updateBinderMeta binder key value))
                                   >>= \b -> (pure (envInsertAt types fullPath b))
                                   >>= \env -> pure (ctx {contextTypeEnv = (TypeEnv env)}))
                              else Nothing)

        create :: (Context, Either EvalError XObj)
        create =
          if null prefixes
          then let updated = Meta.updateBinderMeta (Meta.stub fullPath) key value
                   newEnv  = envInsertAt global fullPath updated
               in  (ctx {contextGlobalEnv = newEnv}, dynamicNil)
          else evalError ctx ("`meta-set!` failed, I can't find the symbol `" ++ pretty target ++ "`") (info target)
primitiveMetaSet _ ctx [XObj (Sym _ _) _ _, key, _] =
  argumentErr ctx "meta-set!" "a string" "second" key
primitiveMetaSet _ ctx [target, _, _] =
  argumentErr ctx "meta-set!" "a symbol" "first" target


primitiveDefinterface :: Primitive
primitiveDefinterface xobj ctx [nameXObj@(XObj (Sym path@(SymPath [] name) _) _ _), ty] =
  pure $ maybe invalidType validType (xobjToTy ty)
  where fppl = projectFilePathPrintLength (contextProj ctx)
        typeEnv = getTypeEnv (contextTypeEnv ctx)
        invalidType = evalError ctx ("Invalid type for interface `" ++ name ++ "`: " ++ pretty ty) (info ty)
        validType t = maybe defInterface (updateInterface . snd) (lookupInEnv path typeEnv)
          where defInterface = let interface = defineInterface name t [] (info nameXObj)
                                   typeEnv' = TypeEnv (envInsertAt typeEnv (SymPath [] name) (Binder emptyMeta interface))
                                   newCtx = retroactivelyRegisterInInterface (ctx { contextTypeEnv = typeEnv' }) path
                               in  (newCtx, dynamicNil)
                updateInterface binder = case binder of
                                           Binder _ (XObj (Lst (XObj (Interface foundType _) _ _ : _)) _ _) ->
                                             if foundType == t
                                             then (ctx, dynamicNil)
                                             else evalError ctx ("Tried to change the type of interface `" ++
                                                                 show path ++ "` from `" ++ show foundType ++
                                                                 "` to `" ++ show t ++ "`") (info xobj)
primitiveDefinterface _ ctx [name, _] =
  pure (evalError ctx ("`definterface` expects a name as first argument, but got `" ++ pretty name ++ "`") (info name))

registerInternal :: Context -> String -> XObj -> Maybe String -> IO (Context, Either EvalError XObj)
registerInternal ctx name ty override =
  pure $ maybe invalidType validType (xobjToTy ty)
  where pathStrings = contextPath ctx
        fppl = projectFilePathPrintLength (contextProj ctx)
        globalEnv = contextGlobalEnv ctx
        invalidType = evalError ctx
                                ("Can't understand type when registering '" ++ name ++
                                 "'") (info ty)
        -- TODO: Retroactively register in interface if implements metadata is present.
        validType t = let path = SymPath pathStrings name
                          registration = XObj (Lst [XObj (External override) Nothing Nothing
                                                   ,XObj (Sym path Symbol) Nothing Nothing
                                                   ,ty
                                                   ]) (info ty) (Just t)
                          meta = existingMeta globalEnv registration
                          env' = envInsertAt globalEnv path (Binder meta registration)
                      in  (ctx { contextGlobalEnv = env' }, dynamicNil)

primitiveRegister :: Primitive
primitiveRegister _ ctx [XObj (Sym (SymPath _ name) _) _ _, ty] =
  registerInternal ctx name ty Nothing
primitiveRegister _ ctx [name, _] =
  pure (evalError ctx
    ("`register` expects a name as first argument, but got `" ++ pretty name ++ "`")
    (info name))
primitiveRegister _ ctx [XObj (Sym (SymPath _ name) _) _ _, ty, XObj (Str override) _ _] =
  registerInternal ctx name ty (Just override)
primitiveRegister _ ctx [XObj (Sym (SymPath _ name) _) _ _, _, override] =
  pure (evalError ctx
    ("`register` expects a string as third argument, but got `" ++ pretty override ++ "`")
    (info override))
primitiveRegister _ ctx [name, _, _] =
  pure (evalError ctx
    ("`register` expects a name as first argument, but got `" ++ pretty name ++ "`")
    (info name))
primitiveRegister x ctx _ =
  pure (evalError ctx
    ("I didn’t understand the form `" ++ pretty x ++
     "`.\n\nIs it valid? Every `register` needs to follow the form `(register name <signature> <optional: override>)`.")
    (info x))



primitiveDeftype :: Primitive
primitiveDeftype xobj ctx (name:rest) =
  case rest of
    (XObj (Arr a) _ _ : _) ->
      case members a of
        Nothing ->
          pure $
                makeEvalError
                  ctx
                  Nothing
                  ("All fields must have a name and a type." ++
                   "Example:\n" ++
                   "```(deftype Name [field1 Type1, field2 Type2, field3 Type3])```\n")
                  (info xobj)
        Just a ->
          ensureUnqualified $ map fst a
      where members :: [XObj] -> Maybe [(XObj, XObj)]
            members (binding:val:xs) = do
                xs' <- members xs
                Just $ (binding, val) : xs'
            members (x:[]) = Nothing
            members   [] = Just []

            ensureUnqualified :: [XObj] -> IO (Context, Either EvalError XObj)
            ensureUnqualified objs =
              if all isUnqualifiedSym objs
                then deftype name
                else pure $
                       makeEvalError
                          ctx
                          Nothing
                          ("Type members must be unqualified symbols, but got `" ++
                           concatMap pretty rest ++ "`")
                          (info xobj)
    _ -> deftype name
  where deftype name@(XObj (Sym (SymPath _ ty) _) _ _) = deftype' name ty []
        deftype (XObj (Lst (name@(XObj (Sym (SymPath _ ty) _) _ _) : tyvars)) _ _) =
          deftype' name ty tyvars
        deftype name =
          pure (evalError ctx
                   ("Invalid name for type definition: " ++ pretty name)
                   (info name))
        deftype' :: XObj -> String -> [XObj] -> IO (Context, Either EvalError XObj)
        deftype' nameXObj typeName typeVariableXObjs = do
         let pathStrings = contextPath ctx
             fppl = projectFilePathPrintLength (contextProj ctx)
             env = contextGlobalEnv ctx
             innerEnv = fromMaybe env (contextInternalEnv ctx)
             typeEnv = contextTypeEnv ctx
             typeVariables = mapM xobjToTy typeVariableXObjs
             (preExistingModule, existingMeta) =
               case lookupInEnv (SymPath pathStrings typeName) env of
                 Just (_, Binder existingMeta (XObj (Mod found) _ _)) -> (Just found, existingMeta)
                 Just (_, Binder existingMeta _) -> (Nothing, existingMeta)
                 _ -> (Nothing, emptyMeta)
             (creatorFunction, typeConstructor) =
                if length rest == 1 && isArray (head rest)
                then (moduleForDeftype, Deftype)
                else (moduleForSumtype, DefSumtype)
         case (nameXObj, typeVariables) of
           (XObj (Sym (SymPath _ typeName) _) i _, Just okTypeVariables) ->
             case creatorFunction innerEnv typeEnv env pathStrings typeName okTypeVariables rest i preExistingModule of
               Right (typeModuleName, typeModuleXObj, deps) ->
                 let structTy = StructTy (ConcreteNameTy typeName) okTypeVariables
                     typeDefinition =
                       -- NOTE: The type binding is needed to emit the type definition and all the member functions of the type.
                       XObj (Lst (XObj (typeConstructor structTy) Nothing Nothing :
                                  XObj (Sym (SymPath pathStrings typeName) Symbol) Nothing Nothing :
                                  rest)
                            ) i (Just TypeTy)
                     ctx' = (ctx { contextGlobalEnv = envInsertAt env (SymPath pathStrings typeModuleName) (Binder existingMeta typeModuleXObj)
                                 , contextTypeEnv = TypeEnv (extendEnv (getTypeEnv typeEnv) typeName typeDefinition)
                                 })
                 in do ctxWithDeps <- liftIO (foldM (define True) ctx' deps)
                       let ctxWithInterfaceRegistrations =
                             -- Since these functions are autogenerated, we treat them as a special case and automatically implement the interfaces.
                             foldM (\context (path, sig, interface) -> registerInInterfaceIfNeeded context path interface sig) ctxWithDeps
                                   [(SymPath (pathStrings ++ [typeModuleName]) "str", FuncTy [RefTy structTy (VarTy "q")] StringTy StaticLifetimeTy, (SymPath [] "str"))
                                   ,(SymPath (pathStrings ++ [typeModuleName]) "copy", FuncTy [RefTy structTy (VarTy "q")] structTy StaticLifetimeTy, (SymPath [] "copy"))]
                       case ctxWithInterfaceRegistrations of
                         Left err -> do
                          liftIO (putStrLnWithColor Red err)
                          pure (ctx, dynamicNil)
                         Right ok -> pure (ok, dynamicNil)
               Left err ->
                 pure (makeEvalError ctx (Just err) ("Invalid type definition for '" ++ pretty nameXObj ++ "':\n\n" ++ show err) Nothing)
           (_, Nothing) ->
             pure (makeEvalError ctx Nothing ("Invalid type variables for type definition: " ++ pretty nameXObj) (info nameXObj))

primitiveUse :: Primitive
primitiveUse xobj ctx [XObj (Sym path _) _ _] =
  pure $ maybe lookupInGlobal useModule (lookupInEnv path e)
  where pathStrings = contextPath ctx
        fppl = projectFilePathPrintLength (contextProj ctx)
        env = contextGlobalEnv ctx
        e = getEnv env pathStrings
        useThese = envUseModules e
        e' = if path `elem` useThese then e else e { envUseModules = path : useThese }
        lookupInGlobal = maybe missing useModule (lookupInEnv path env)
          where missing = evalError ctx ("Can't find a module named '" ++ show path ++ "'") (info xobj)
        useModule _ = (ctx { contextGlobalEnv = envReplaceEnvAt env pathStrings e' }, dynamicNil)
primitiveUse xobj ctx [x] =
  argumentErr ctx "use" "a symbol" "first" x

-- | Get meta data for a Binder
primitiveMeta :: Primitive
primitiveMeta (XObj _ i _) ctx [XObj (Sym path@(SymPath prefixes name) _) _ _, XObj (Str key) _ _] = do
  pure $ maybe notFound foundBinder lookup

  where global = contextGlobalEnv ctx
        types  = getTypeEnv (contextTypeEnv ctx)
        fullPath = consPath (union (contextPath ctx) prefixes) (SymPath [] name)

        lookup :: Maybe Binder
        lookup = ((lookupInEnv fullPath global)
                 >>= pure . snd)
                 <|>
                 ((lookupInEnv fullPath types)
                 >>= pure . snd)

        foundBinder :: Binder -> (Context, Either EvalError XObj)
        foundBinder binder = (ctx, maybe dynamicNil Right (Meta.getBinderMetaValue key binder))

        notFound :: (Context, Either EvalError XObj)
        notFound = evalError ctx  ("`meta` failed, I can’t find `" ++ show fullPath ++ "`") i
primitiveMeta _ ctx [XObj (Sym path _) _ _, key] =
  argumentErr ctx "meta" "a string" "second" key
primitiveMeta _ ctx [path, _] =
  argumentErr ctx "meta" "a symbol" "first" path

primitiveDefined :: Primitive
primitiveDefined _ ctx [XObj (Sym path _) _ _] = do
  let env = contextEnv ctx
  pure $ maybe (ctx, Right falseXObj) (\_ -> (ctx, Right trueXObj)) (lookupInEnv path env)
primitiveDefined _ ctx [arg] =
  argumentErr ctx "defined" "a symbol" "first" arg

primitiveDeftemplate :: Primitive
-- deftemplate can't receive a dependency function, as Ty aren't exposed in Carp
primitiveDeftemplate _ ctx [XObj (Sym (SymPath [] name) _) pinfo _, ty, XObj (Str declTempl) _ _, XObj (Str defTempl) _ _] =
  pure $ maybe invalidType validType (xobjToTy ty)
  where pathStrings = contextPath ctx
        typeEnv = contextTypeEnv ctx
        globalEnv = contextGlobalEnv ctx
        p = SymPath pathStrings name
        invalidType = evalError ctx ("I do not understand the type form in " ++ pretty ty) (info ty)
        validType t =  case defineTemplate p t "" (toTemplate declTempl) (toTemplate defTempl) (const []) of
                         (_, b@(Binder _ (XObj (Lst (XObj (Deftemplate template) _ _ : _)) _ _))) ->
                           if isTypeGeneric t
                           then
                             let (Binder _ registration) = b
                                 meta = existingMeta globalEnv registration
                                 env' = envInsertAt globalEnv p (Binder meta registration)
                             in (ctx { contextGlobalEnv = env' }, dynamicNil)
                           else
                             let templateCreator = getTemplateCreator template
                                 (registration, _) = instantiateTemplate p t (templateCreator typeEnv globalEnv)
                                 meta = existingMeta globalEnv registration
                                 env' = envInsertAt globalEnv p (Binder meta registration)
                             in (ctx { contextGlobalEnv = env' }, dynamicNil)
primitiveDeftemplate _ ctx [XObj (Sym (SymPath [] _) _) _ _, _, XObj (Str _) _ _, x] =
  argumentErr ctx "deftemplate" "a string" "fourth" x
primitiveDeftemplate _ ctx [XObj (Sym (SymPath [] _) _) _ _, _, x, _] =
  argumentErr ctx "deftemplate" "a string" "third" x
primitiveDeftemplate _ ctx [s@(XObj (Sym (SymPath _ _) _) _ _), _, _, _] = do
  argumentErr ctx "deftemplate" "a symbol without prefix" "first" s
primitiveDeftemplate _ ctx [x, _, _, _] =
  argumentErr ctx "deftemplate" "a symbol" "first" x

noTypeError :: Context -> XObj -> IO (Context, Either EvalError XObj)
noTypeError ctx x = pure $ evalError ctx ("Can't get the type of: " ++ pretty x) (info x)

primitiveType :: Primitive
-- A special case, the type of the type of types (type (type (type 1))) => ()
primitiveType _ ctx [x@(XObj _ _ (Just Universe))] =
  pure (ctx, Right (XObj (Lst []) Nothing Nothing))
primitiveType _ ctx [x@(XObj _ _ (Just TypeTy))] = liftIO $ pure (ctx, Right $ reify TypeTy)
primitiveType _ ctx [x@(XObj (Sym path@(SymPath [] name) _) _ _)] =
  (maybe otherDefs (go ctx . snd) (lookupInEnv path env))
  where env = contextGlobalEnv ctx
        otherDefs = case multiLookupALL name env of
                      [] ->
                        notFound ctx x path
                      binders ->
                        (sequence (map (go ctx . snd) binders))
                        >>= pure . Lst . rights . map snd
                        >>= \obj -> pure (ctx, Right $ (XObj obj Nothing Nothing))
        go ctx binder =
          case (ty (binderXObj binder))of
            Nothing -> noTypeError ctx x
            Just t -> pure (ctx, Right (reify t))
primitiveType _ ctx [x@(XObj (Sym qualifiedPath _) _ _)] =
  maybe (notFound ctx x qualifiedPath) (go ctx . snd) (lookupInEnv qualifiedPath env)
  where env = contextGlobalEnv ctx
        go ctx binder =
          case (ty (binderXObj binder)) of
            Nothing -> noTypeError ctx x
            Just t -> pure (ctx, Right $ reify t)
-- As a special case, we force evaluation on sequences such as (type (type 1))
-- Because primitives don't evaluate their arguments, passing (type 1) to type would result in an error
-- However, such an invocation *is* meaningful, and returns Type (the type of types). (type (type 1)) => Type
-- Note that simply making type a command as an alternative leads to inconsistent behaviors whereby
-- (type 1) => Int
-- (type '1) => Int
-- (type (Pair.init 1 1)) => Error can't find symbol "type"
-- (type '(Pair.init 1 1)) => (Pair Int Int)
-- Contrarily the behavior is far more consistent as a primitive if we simply add this case, and from a user perspective, it makes more sense
-- that this function would be one that *doesn't* evaluate its arguments.
primitiveType any ctx [x@(XObj (Lst (XObj (Sym (SymPath [] "type") _) _ _: rest)) _ _)] =
  primitiveType any ctx rest
  >>= \result -> case snd result of
                 Right xobj -> primitiveType any (fst result) [xobj]
                 Left e -> pure (ctx, Left e)
primitiveType _ ctx [x@(XObj _ _ _)] =
  let tenv  = contextTypeEnv ctx
      typed = annotate tenv (contextGlobalEnv ctx) x Nothing
  in  liftIO $ either fail ok typed
  where fail e = pure (evalError ctx ("Can't get the type of: " ++ pretty x) (info x))
        ok ((XObj _ _ (Just t)),_) = pure (ctx, Right $ reify t)
        ok (_,_) = pure (evalError ctx ("Can't get the type of: " ++ pretty x) (info x))

primitiveKind :: Primitive
primitiveKind _ ctx [x@(XObj _ _ _)] =
  let tenv = contextTypeEnv ctx
      typed = annotate tenv (contextGlobalEnv ctx) x Nothing
  in  pure (either fail ok typed)
  where fail e = (evalError ctx ("Can't get the kind of: " ++ pretty x) (info x))
        ok (XObj _ _ (Just t), _) = (ctx, Right $ reify (tyToKind t))
        ok (_, _) = (evalError ctx ("Can't get the kind of: " ++ pretty x) (info x))

-- | Primitive for printing help.
primitiveHelp :: Primitive
primitiveHelp _ ctx [XObj (Sym (SymPath [] "about") _) _ _] =
  liftIO $ do putStrLn "Carp is an ongoing research project by Erik Svedäng, et al."
              putStrLn ""
              putStrLn "Licensed under the Apache License, Version 2.0 (the \"License\"); \n\
                       \you may not use this file except in compliance with the License. \n\
                       \You may obtain a copy of the License at \n\
                       \http://www.apache.org/licenses/LICENSE-2.0"
              putStrLn ""
              putStrLn "THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS ``AS IS'' AND ANY \n\
                       \EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE \n\
                       \IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR \n\
                       \PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS BE \n\
                       \LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR \n\
                       \CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF \n\
                       \SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR \n\
                       \BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, \n\
                       \WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE \n\
                       \OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN\n\
                       \IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE."
              putStrLn ""
              return (ctx, dynamicNil)

primitiveHelp _ ctx [XObj (Sym (SymPath [] "compiler") _) _ _] =
  openBrowserHelper ctx "https://github.com/carp-lang/Carp/blob/master/docs/Manual.md"

primitiveHelp _ ctx [XObj (Sym (SymPath [] "language") _) _ _] =
  openBrowserHelper ctx "https://github.com/carp-lang/Carp/blob/master/docs/LanguageGuide.md"

primitiveHelp _ ctx [XObj (Sym (SymPath [] "core") _) _ _] =
  openBrowserHelper ctx "https://carp-lang.github.io/carp-docs/core/core_index.html"

primitiveHelp _ ctx [XObj (Sym (SymPath [] "gitter") _) _ _] =
  openBrowserHelper ctx "https://gitter.im/carp-lang/Carp"

primitiveHelp _ ctx [XObj (Sym (SymPath [] "shortcuts") _) _ _] =
  liftIO $ do putStrLn ""
              putStrLn "(reload)       :r"
              putStrLn "(build)        :b"
              putStrLn "(run)          :x"
              putStrLn "(cat)          :c"
              putStrLn "(env)          :e"
              putStrLn "(help)         :h"
              putStrLn "(project)      :p"
              putStrLn "(quit)         :q"
              putStrLn "(type <arg>)   :t"
              putStrLn "(expand <arg>) :m"
              putStrLn "(info <arg>)   :i"
              putStrLn ""
              putStrLn "The shortcuts can be combined like this: \":rbx\""
              putStrLn ""
              return (ctx, dynamicNil)

primitiveHelp _ ctx [] =
  liftIO $ do putStrLn "Don't panic - we can solve this!"
              putStrLn ""
              putStrLn "Evaluate (help <chapter>) to get to the relevant subchapter:"
              putStrLn ""
              putStrLn "compiler  - Learn how to use the compiler / repl        (web)"
              putStrLn "language  - Syntax and semantics of the language        (web)"
              putStrLn "core      - Core library API documentation              (web)"
              putStrLn "gitter    - Get help from the community                 (web)"
              putStrLn "shortcuts - Useful shortcuts in the REPL                     "
              putStrLn "about     - Print some information about this program        "
              putStrLn ""
              return (ctx, dynamicNil)

primitiveHelp _ ctx args =
  return (evalError ctx ("Invalid args to `help`: " ++ joinWithComma (map pretty args)) Nothing)

openBrowserHelper :: MonadIO m => Context -> String -> m (Context, Either EvalError XObj)
openBrowserHelper ctx url =
  liftIO $ do _ <- openBrowser url
              return (ctx, dynamicNil)

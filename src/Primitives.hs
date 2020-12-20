module Primitives where

import ColorText
import Commands
import Context
import Control.Applicative
import Control.Monad (foldM, unless, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Either (rights)
import Data.List (union)
import Data.Maybe (fromJust, fromMaybe, mapMaybe, maybeToList)
import Deftype
import Emit
import Env
import Infer
import Info
import Interfaces
import Lookup
import qualified Map as Map
import qualified Meta as Meta
import Obj
import PrimitiveError
import Project
import Reify
import Sumtypes
import Template
import ToTemplate
import TypeError
import TypePredicates
import Types
import Util
import Web.Browser (openBrowser)

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
  pure (toEvalError ctx actual (ArgumentTypeError fun ty number actual))

makePrim' :: String -> Maybe Int -> String -> String -> Primitive -> (String, Binder)
makePrim' name maybeArity docString example callback =
  let path = SymPath [] name
      prim =
        XObj
          ( Lst
              [ XObj (Primitive (PrimitiveFunction wrapped)) (Just dummyInfo) Nothing,
                XObj (Sym path Symbol) Nothing Nothing,
                unfoldArgs
              ]
          )
          (Just dummyInfo)
          (Just DynamicTy)
      meta = Meta.set "doc" (XObj (Str doc) Nothing Nothing) emptyMeta
   in (name, Binder meta prim)
  where
    wrapped =
      case maybeArity of
        Just a ->
          \x c l ->
            let ll = length l
             in (if ll /= a then err x c a ll else callback x c l)
        Nothing -> callback
    err :: XObj -> Context -> Int -> Int -> IO (Context, Either EvalError XObj)
    err x ctx a l =
      pure
        ( evalError
            ctx
            ( "The primitive `" ++ name ++ "` expected " ++ show a
                ++ " arguments, but got "
                ++ show l
                ++ ".\n\n"
                ++ exampleUsage
            )
            (xobjInfo x)
        )
    doc = docString ++ "\n\n" ++ exampleUsage
    exampleUsage = "Example Usage:\n```\n" ++ example ++ "\n```\n"
    unfoldArgs =
      case maybeArity of
        Just arity ->
          let tosym x = (XObj (Sym (SymPath [] x) Symbol) Nothing Nothing)
           in XObj (Arr (map (tosym . intToArgName) [1 .. arity])) Nothing Nothing
        Nothing -> XObj (Arr [(XObj (Sym (SymPath [] "") Symbol) Nothing Nothing)]) Nothing Nothing

infoXObjOrError :: Context -> (Context, Either EvalError XObj) -> Maybe Info -> Maybe XObj -> (Context, Either EvalError XObj)
infoXObjOrError ctx err i = maybe err (\xobj -> (ctx, Right xobj {xobjInfo = i}))

primitiveFile :: Primitive
primitiveFile x@(XObj _ i _) ctx args =
  pure $ case args of
    [] -> infoXObjOrError ctx err i (getFileAsXObj fppl i)
    [XObj _ mi _] -> infoXObjOrError ctx err i (getFileAsXObj fppl mi)
    _ -> toEvalError ctx x (ArgumentArityError x "0 or 1" args)
  where
    fppl = projectFilePathPrintLength (contextProj ctx)
    err = toEvalError ctx x (MissingInfo x)

primitiveLine :: Primitive
primitiveLine x@(XObj _ i _) ctx args =
  pure $ case args of
    [] -> infoXObjOrError ctx err i (getLineAsXObj i)
    [XObj _ mi _] -> infoXObjOrError ctx err i (getLineAsXObj mi)
    _ -> toEvalError ctx x (ArgumentArityError x "0 or 1" args)
  where
    err = toEvalError ctx x (MissingInfo x)

primitiveColumn :: Primitive
primitiveColumn x@(XObj _ i _) ctx args =
  pure $ case args of
    [] -> infoXObjOrError ctx err i (getColumnAsXObj i)
    [XObj _ mi _] -> infoXObjOrError ctx err i (getColumnAsXObj mi)
    _ -> toEvalError ctx x (ArgumentArityError x "0 or 1" args)
  where
    err = toEvalError ctx x (MissingInfo x)

primitiveImplements :: Primitive
primitiveImplements call ctx [x@(XObj (Sym interface@(SymPath _ _) _) _ _), (XObj (Sym (SymPath prefixes name) _) _ _)] =
  do
    (maybeInterface, maybeImpl) <- pure ((lookupBinder interface tyEnv), (lookupBinder (SymPath modules name) global))
    case (maybeInterface, maybeImpl) of
      (_, Nothing) ->
        if null modules
          then pure (toEvalError ctx call ForewardImplementsMeta)
          else updateMeta (Meta.stub (SymPath modules name)) ctx
      (Nothing, Just implBinder) ->
        (warn >> updateMeta implBinder ctx)
      (Just interfaceBinder, Just implBinder) ->
        (addToInterface interfaceBinder implBinder)
  where
    global = contextGlobalEnv ctx
    tyEnv = getTypeEnv . contextTypeEnv $ ctx
    (SymPath modules _) = consPath (union (contextPath ctx) prefixes) (SymPath [] name)
    warn :: IO ()
    warn = emitWarning (show (NonExistentInterfaceWarning x))
    addToInterface :: Binder -> Binder -> IO (Context, Either EvalError XObj)
    addToInterface inter impl =
      either
        (addToInterfaceError (contextExecMode ctx))
        (updateMeta impl)
        (registerInInterface ctx impl inter)
    addToInterfaceError :: ExecutionMode -> String -> IO (Context, Either EvalError XObj)
    addToInterfaceError Check e =
      putStrLn (machineReadableInfoFromXObj fppl x ++ " " ++ e)
        >> pure (evalError ctx e (xobjInfo x))
      where
        fppl = projectFilePathPrintLength (contextProj ctx)
    addToInterfaceError _ e =
      putStrLnWithColor Red e
        >> pure (evalError ctx e (xobjInfo x))
    updateMeta :: Binder -> Context -> IO (Context, Either EvalError XObj)
    updateMeta binder context =
      pure (fromJust update, dynamicNil)
      where
        update =
          ( ( Meta.getBinderMetaValue "implements" binder
                >>= pure . updateImplementations binder
            )
              <|> Just (updateImplementations binder (XObj (Lst []) (Just dummyInfo) (Just DynamicTy)))
          )
            >>= \newBinder -> pure (context {contextGlobalEnv = envInsertAt global (getBinderPath binder) newBinder})
        updateImplementations :: Binder -> XObj -> Binder
        updateImplementations implBinder (XObj (Lst impls) inf ty) =
          if x `elem` impls
            then binder
            else Meta.updateBinderMeta implBinder "implements" (XObj (Lst (x : impls)) inf ty)
        updateImplementations implBinder _ =
          Meta.updateBinderMeta implBinder "implements" (XObj (Lst [x]) (Just dummyInfo) (Just DynamicTy))
primitiveImplements x ctx [(XObj (Sym _ _) _ _), y] =
  pure $ toEvalError ctx x (ArgumentTypeError "implements" "a symbol" "second" y)
primitiveImplements _ ctx [x, _] =
  pure $ toEvalError ctx x (ArgumentTypeError "implements" "a symbol" "first" x)
primitiveImplements x@(XObj _ _ _) ctx args =
  pure $ toEvalError ctx x (ArgumentArityError x "2" args)

define :: Bool -> Context -> XObj -> IO Context
define hidden ctx@(Context globalEnv _ typeEnv _ proj _ _ _) annXObj =
  pure (hideIt freshBinder)
    >>= \newBinder ->
      if isTypeDef annXObj
        then defineInTypeEnv newBinder
        else defineInGlobalEnv newBinder
  where
    freshBinder = (Binder emptyMeta annXObj)
    defineInTypeEnv :: Binder -> IO Context
    defineInTypeEnv binder = pure (insertInTypeEnv ctx (getPath annXObj) binder)
    defineInGlobalEnv :: Binder -> IO Context
    defineInGlobalEnv fallbackBinder =
      do
        maybeExistingBinder <- pure (lookupBinder (getPath annXObj) globalEnv)
        when (projectEchoC proj) (putStrLn (toC All (Binder emptyMeta annXObj)))
        case maybeExistingBinder of
          Nothing -> pure (insertInGlobalEnv ctx (getPath annXObj) fallbackBinder)
          Just binder -> redefineExistingBinder binder
    redefineExistingBinder :: Binder -> IO Context
    redefineExistingBinder old@(Binder meta _) =
      do
        updatedBinder <- pure (hideIt (Binder meta annXObj))
        warnTypeChange old
        updatedContext <- implementInterfaces updatedBinder
        pure (insertInGlobalEnv updatedContext (getPath annXObj) updatedBinder)
    hideIt :: Binder -> Binder
    hideIt binder =
      if hidden
        then Meta.updateBinderMeta binder "hidden" trueXObj
        else binder
    warnTypeChange :: Binder -> IO ()
    warnTypeChange binder =
      unless (areUnifiable (forceTy annXObj) previousType) warn
      where
        previousType = forceTy (binderXObj binder)
        warn :: IO ()
        warn =
          emitWarning (show (DefinitionTypeChangeWarning annXObj previousType))
    implementInterfaces :: Binder -> IO Context
    implementInterfaces binder =
      pure
        ( Meta.getBinderMetaValue "implements" binder
            >>= \(XObj (Lst interfaces) _ _) -> pure (map getPath interfaces)
        )
        >>= \maybeinterfaces ->
          pure (mapMaybe ((flip lookupBinder) (getTypeEnv typeEnv)) (fromMaybe [] maybeinterfaces))
            >>= \interfaceBinders ->
              pure (foldM (\ctx' interface -> registerInInterface ctx' binder interface) ctx interfaceBinders)
                >>= \result -> case result of
                  Left e -> ((printError (contextExecMode ctx) e) >> pure ctx)
                  Right newCtx -> (pure newCtx)
    printError :: ExecutionMode -> String -> IO ()
    printError Check e =
      let fppl = projectFilePathPrintLength (contextProj ctx)
       in putStrLn (machineReadableInfoFromXObj fppl annXObj ++ " " ++ e)
    printError _ e = putStrLnWithColor Red e

primitiveRegisterType :: Primitive
primitiveRegisterType _ ctx [XObj (Sym (SymPath [] t) _) _ _] =
  primitiveRegisterTypeWithoutFields ctx t Nothing
primitiveRegisterType _ ctx [x] =
  pure (evalError ctx ("`register-type` takes a symbol, but it got " ++ pretty x) (xobjInfo x))
primitiveRegisterType _ ctx [XObj (Sym (SymPath [] t) _) _ _, XObj (Str override) _ _] =
  primitiveRegisterTypeWithoutFields ctx t (Just override)
primitiveRegisterType _ ctx [x@(XObj (Sym (SymPath [] t) _) _ _), (XObj (Str override) _ _), members] =
  primitiveRegisterTypeWithFields ctx x t (Just override) members
primitiveRegisterType _ ctx [x@(XObj (Sym (SymPath [] t) _) _ _), members] =
  primitiveRegisterTypeWithFields ctx x t Nothing members
primitiveRegisterType x ctx _ = pure (toEvalError ctx x RegisterTypeError)

primitiveRegisterTypeWithoutFields :: Context -> String -> (Maybe String) -> IO (Context, Either EvalError XObj)
primitiveRegisterTypeWithoutFields ctx t override = do
  let pathStrings = contextPath ctx
      typeEnv = contextTypeEnv ctx
      path = SymPath pathStrings t
      typeDefinition = XObj (Lst [XObj (ExternalType override) Nothing Nothing, XObj (Sym path Symbol) Nothing Nothing]) Nothing (Just TypeTy)
  pure (ctx {contextTypeEnv = TypeEnv (extendEnv (getTypeEnv typeEnv) t typeDefinition)}, dynamicNil)

primitiveRegisterTypeWithFields :: Context -> XObj -> String -> (Maybe String) -> XObj -> IO (Context, Either EvalError XObj)
primitiveRegisterTypeWithFields ctx x t override members =
  either
    handleErr
    updateContext
    (bindingsForRegisteredType typeEnv globalEnv pathStrings t [members] Nothing preExistingModule)
  where
    handleErr e = pure $ makeEvalError ctx (Just e) (show e) (xobjInfo x)
    updateContext (typeModuleName, typeModuleXObj, deps) =
      do
        let typeDefinition = XObj (Lst [XObj (ExternalType override) Nothing Nothing, XObj (Sym path Symbol) Nothing Nothing]) Nothing (Just TypeTy)
            ctx' =
              ( ctx
                  { contextGlobalEnv = envInsertAt globalEnv (SymPath pathStrings typeModuleName) (Binder emptyMeta typeModuleXObj),
                    contextTypeEnv = TypeEnv (extendEnv (getTypeEnv typeEnv) t typeDefinition)
                  }
              )
        contextWithDefs <- liftIO $ foldM (define True) ctx' deps
        pure (contextWithDefs, dynamicNil)
    pathStrings = contextPath ctx
    globalEnv = contextGlobalEnv ctx
    typeEnv = contextTypeEnv ctx
    path = SymPath pathStrings t
    preExistingModule = case lookupBinder (SymPath pathStrings t) globalEnv of
      Just (Binder _ (XObj (Mod found) _ _)) -> Just found
      _ -> Nothing

notFound :: Context -> XObj -> SymPath -> IO (Context, Either EvalError XObj)
notFound ctx x path = pure (toEvalError ctx x (SymbolNotFoundError path))

primitiveInfo :: Primitive
primitiveInfo _ ctx [target@(XObj (Sym path@(SymPath _ _) _) _ _)] = do
  case path of
    SymPath [] _ ->
      (printIfFound (lookupBinderInTypeEnv ctx path))
        >> maybe
          (notFound ctx target path)
          (\binders -> foldM (\_ binder -> printer binder) (ctx, dynamicNil) binders)
          ( (fmap (: []) (lookupBinderInContextEnv ctx path))
              <|> (multiLookupBinderEverywhere ctx path)
          )
    _ ->
      (printIfFound (lookupBinderInTypeEnv ctx path))
        >> ( case lookupBinderInContextEnv ctx path of
               Nothing -> notFound ctx target path
               Just found -> printer found
           )
  where
    printIfFound :: Maybe Binder -> IO (Context, Either EvalError XObj)
    printIfFound binder = maybe (pure (ctx, dynamicNil)) printer binder

    printer (binder@(Binder metaData x@(XObj _ (Just i) _))) =
      (putStrLnWithColor Blue (forceShowBinder binder))
        >> putStrLn ("  Defined at " ++ prettyInfo i)
        >> printMeta metaData (contextProj ctx) x
        >> pure (ctx, dynamicNil)
    printer (binder@(Binder metaData x)) =
      (print binder)
        >> printMeta metaData (contextProj ctx) x
        >> pure (ctx, dynamicNil)
    printMeta :: MetaData -> Project -> XObj -> IO ()
    printMeta metaData proj x =
      (maybe (pure ()) (printMetaVal "Documentation" ((either (const "") id) . unwrapStringXObj)) (Meta.get "doc" metaData))
        >> maybe (pure ()) (printMetaVal "Implements" getName) (Meta.get "implements" metaData)
        >> maybe (pure ()) (printMetaVal "Private" pretty) (Meta.get "private" metaData)
        >> maybe (pure ()) (printMetaVal "Hidden" pretty) (Meta.get "hidden" metaData)
        >> maybe (pure ()) (printMetaVal "Signature" pretty) (Meta.get "sig" metaData)
        >> when (projectPrintTypedAST proj) (putStrLnWithColor Yellow (prettyTyped x))

    printMetaVal :: String -> (XObj -> String) -> XObj -> IO ()
    printMetaVal s f xobj = putStrLn ("  " ++ s ++ ": " ++ (f xobj))
primitiveInfo _ ctx [notName] =
  argumentErr ctx "info" "a name" "first" notName
primitiveInfo x ctx xs = pure $ toEvalError ctx x (ArgumentArityError x "1" xs)

dynamicOrMacroWith :: Context -> (SymPath -> [XObj]) -> Ty -> String -> XObj -> IO (Context, Either EvalError XObj)
dynamicOrMacroWith ctx producer ty name body = do
  let pathStrings = contextPath ctx
      globalEnv = contextGlobalEnv ctx
      path = SymPath pathStrings name
      elt = XObj (Lst (producer path)) (xobjInfo body) (Just ty)
      meta = lookupMeta (getPath elt) globalEnv
  pure (ctx {contextGlobalEnv = envInsertAt globalEnv path (Binder meta elt)}, dynamicNil)

primitiveMembers :: Primitive
primitiveMembers _ ctx [target] = do
  let typeEnv = contextTypeEnv ctx
  case bottomedTarget of
    XObj (Sym path@(SymPath _ name) _) _ _ ->
      case lookupBinder path (getTypeEnv typeEnv) of
        Just
          ( Binder
              _
              ( XObj
                  ( Lst
                      [ XObj (Deftype _) Nothing Nothing,
                        XObj (Sym (SymPath _ _) Symbol) Nothing Nothing,
                        XObj (Arr members) _ _
                        ]
                    )
                  _
                  _
                )
            ) ->
            pure (ctx, Right (XObj (Arr (map (\(a, b) -> XObj (Lst [a, b]) Nothing Nothing) (pairwise members))) Nothing Nothing))
        Just
          ( Binder
              _
              ( XObj
                  ( Lst
                      ( XObj (DefSumtype _) Nothing Nothing
                          : XObj (Sym (SymPath _ _) Symbol) Nothing Nothing
                          : sumtypeCases
                        )
                    )
                  _
                  _
                )
            ) ->
            pure (ctx, Right (XObj (Arr (concatMap getMembersFromCase sumtypeCases)) Nothing Nothing))
            where
              getMembersFromCase :: XObj -> [XObj]
              getMembersFromCase (XObj (Lst members) _ _) =
                map (\(a, b) -> XObj (Lst [a, b]) Nothing Nothing) (pairwise members)
              getMembersFromCase x@(XObj (Sym _ _) _ _) =
                [XObj (Lst [x, XObj (Arr []) Nothing Nothing]) Nothing Nothing]
              getMembersFromCase (XObj x _ _) =
                error ("Can't handle case " ++ show x)
        _ ->
          pure (evalError ctx ("Can't find a struct type named '" ++ name ++ "' in type environment") (xobjInfo target))
    _ -> pure (evalError ctx ("Can't get the members of non-symbol: " ++ pretty target) (xobjInfo target))
  where
    env = contextEnv ctx
    bottomedTarget =
      case target of
        XObj (Sym targetPath _) _ _ ->
          case lookupBinder targetPath env of
            -- this is a trick: every type generates a module in the env;
            -- we’re special-casing here because we need the parent of the
            -- module
            Just (Binder _ (XObj (Mod _) _ _)) -> target
            -- if we’re recursing into a non-sym, we’ll stop one level down
            Just (Binder _ _) -> bottomedTarget
            _ -> target
        _ -> target
primitiveMembers _ _ _ = error "primitivemembers"

-- | Set meta data for a Binder
primitiveMetaSet :: Primitive
primitiveMetaSet _ ctx [target@(XObj (Sym (SymPath prefixes name) _) _ _), XObj (Str key) _ _, value] =
  pure $ maybe create (\newCtx -> (newCtx, dynamicNil)) lookupAndUpdate
  where
    fullPath@(SymPath modules _) = consPath (union (contextPath ctx) prefixes) (SymPath [] name)
    dynamicPath = (consPath ["Dynamic"] fullPath)
    global = contextGlobalEnv ctx
    types = (getTypeEnv (contextTypeEnv ctx))
    lookupAndUpdate :: Maybe Context
    lookupAndUpdate =
      ( (lookupBinder dynamicPath global)
          >>= \binder ->
            (pure (Meta.updateBinderMeta binder key value))
              >>= \b ->
                (pure (envInsertAt global dynamicPath b))
                  >>= \env -> pure (ctx {contextGlobalEnv = env})
      )
        <|> ( (lookupBinder fullPath global)
                >>= \binder ->
                  (pure (Meta.updateBinderMeta binder key value))
                    >>= \b ->
                      (pure (envInsertAt global fullPath b))
                        >>= \env -> pure (ctx {contextGlobalEnv = env})
            )
        -- This is a global name but it doesn't exist in the global env
        -- Before creating a new binder, check that it doesn't denote an existing type or interface.
        <|> ( if (null modules)
                then
                  ( (lookupBinder fullPath types)
                      >>= \binder ->
                        (pure (Meta.updateBinderMeta binder key value))
                          >>= \b ->
                            (pure (envInsertAt types fullPath b))
                              >>= \env -> pure (ctx {contextTypeEnv = (TypeEnv env)})
                  )
                else Nothing
            )
    create :: (Context, Either EvalError XObj)
    create =
      if null prefixes
        then
          let updated = Meta.updateBinderMeta (Meta.stub fullPath) key value
              newEnv = envInsertAt global fullPath updated
           in (ctx {contextGlobalEnv = newEnv}, dynamicNil)
        else evalError ctx ("`meta-set!` failed, I can't find the symbol `" ++ pretty target ++ "`") (xobjInfo target)
primitiveMetaSet _ ctx [XObj (Sym _ _) _ _, key, _] =
  argumentErr ctx "meta-set!" "a string" "second" key
primitiveMetaSet _ ctx [target, _, _] =
  argumentErr ctx "meta-set!" "a symbol" "first" target
primitiveMetaSet _ _ _ = error "primitivemetaset"

primitiveDefinterface :: Primitive
primitiveDefinterface xobj ctx [nameXObj@(XObj (Sym path@(SymPath [] name) _) _ _), ty] =
  pure $ maybe invalidType validType (xobjToTy ty)
  where
    typeEnv = getTypeEnv (contextTypeEnv ctx)
    invalidType = evalError ctx ("Invalid type for interface `" ++ name ++ "`: " ++ pretty ty) (xobjInfo ty)
    validType t = maybe defInterface updateInterface (lookupBinder path typeEnv)
      where
        defInterface =
          let interface = defineInterface name t [] (xobjInfo nameXObj)
              typeEnv' = TypeEnv (envInsertAt typeEnv (SymPath [] name) (Binder emptyMeta interface))
              newCtx = retroactivelyRegisterInInterface (ctx {contextTypeEnv = typeEnv'}) (Binder emptyMeta interface)
           in (newCtx, dynamicNil)
        updateInterface binder = case binder of
          Binder _ (XObj (Lst (XObj (Interface foundType _) _ _ : _)) _ _) ->
            if foundType == t
              then (ctx, dynamicNil)
              else
                evalError
                  ctx
                  ( "Tried to change the type of interface `"
                      ++ show path
                      ++ "` from `"
                      ++ show foundType
                      ++ "` to `"
                      ++ show t
                      ++ "`"
                  )
                  (xobjInfo xobj)
          _ -> error "updateinterface"
primitiveDefinterface _ ctx [name, _] =
  pure (evalError ctx ("`definterface` expects a name as first argument, but got `" ++ pretty name ++ "`") (xobjInfo name))
primitiveDefinterface _ _ _ = error "primitivedefinterface"

registerInternal :: Context -> String -> XObj -> Maybe String -> IO (Context, Either EvalError XObj)
registerInternal ctx name ty override =
  pure $ maybe invalidType validType (xobjToTy ty)
  where
    pathStrings = contextPath ctx
    globalEnv = contextGlobalEnv ctx
    invalidType =
      evalError
        ctx
        ( "Can't understand type when registering '" ++ name
            ++ "'"
        )
        (xobjInfo ty)
    -- TODO: Retroactively register in interface if implements metadata is present.
    validType t =
      let path = SymPath pathStrings name
          registration =
            XObj
              ( Lst
                  [ XObj (External override) Nothing Nothing,
                    XObj (Sym path Symbol) Nothing Nothing,
                    ty
                  ]
              )
              (xobjInfo ty)
              (Just t)
          meta = lookupMeta (getPath registration) globalEnv
          env' = envInsertAt globalEnv path (Binder meta registration)
       in (ctx {contextGlobalEnv = env'}, dynamicNil)

primitiveRegister :: Primitive
primitiveRegister _ ctx [XObj (Sym (SymPath _ name) _) _ _, ty] =
  registerInternal ctx name ty Nothing
primitiveRegister _ ctx [name, _] =
  pure
    ( evalError
        ctx
        ("`register` expects a name as first argument, but got `" ++ pretty name ++ "`")
        (xobjInfo name)
    )
primitiveRegister _ ctx [XObj (Sym (SymPath _ name) _) _ _, ty, XObj (Str override) _ _] =
  registerInternal ctx name ty (Just override)
primitiveRegister _ ctx [XObj (Sym (SymPath _ _) _) _ _, _, override] =
  pure
    ( evalError
        ctx
        ("`register` expects a string as third argument, but got `" ++ pretty override ++ "`")
        (xobjInfo override)
    )
primitiveRegister _ ctx [name, _, _] =
  pure
    ( evalError
        ctx
        ("`register` expects a name as first argument, but got `" ++ pretty name ++ "`")
        (xobjInfo name)
    )
primitiveRegister x ctx _ =
  pure
    ( evalError
        ctx
        ( "I didn’t understand the form `" ++ pretty x
            ++ "`.\n\nIs it valid? Every `register` needs to follow the form `(register name <signature> <optional: override>)`."
        )
        (xobjInfo x)
    )

primitiveDeftype :: Primitive
primitiveDeftype xobj ctx (name : rest) =
  case rest of
    (XObj (Arr a) _ _ : _) ->
      case members a of
        Nothing ->
          pure $
            makeEvalError
              ctx
              Nothing
              ( "All fields must have a name and a type."
                  ++ "Example:\n"
                  ++ "```(deftype Name [field1 Type1, field2 Type2, field3 Type3])```\n"
              )
              (xobjInfo xobj)
        Just ms ->
          ensureUnqualified $ map fst ms
      where
        members :: [XObj] -> Maybe [(XObj, XObj)]
        members (binding : val : xs) = do
          xs' <- members xs
          Just $ (binding, val) : xs'
        members (_ : []) = Nothing
        members [] = Just []
        ensureUnqualified :: [XObj] -> IO (Context, Either EvalError XObj)
        ensureUnqualified objs =
          if all isUnqualifiedSym objs
            then deftype name
            else
              pure $
                makeEvalError
                  ctx
                  Nothing
                  ( "Type members must be unqualified symbols, but got `"
                      ++ concatMap pretty rest
                      ++ "`"
                  )
                  (xobjInfo xobj)
    _ -> deftype name
  where
    deftype nm@(XObj (Sym (SymPath _ ty) _) _ _) = deftype' nm ty []
    deftype (XObj (Lst (nm@(XObj (Sym (SymPath _ ty) _) _ _) : tyvars)) _ _) =
      deftype' nm ty tyvars
    deftype nm =
      pure
        ( evalError
            ctx
            ("Invalid name for type definition: " ++ pretty nm)
            (xobjInfo nm)
        )
    deftype' :: XObj -> String -> [XObj] -> IO (Context, Either EvalError XObj)
    deftype' nameXObj typeName typeVariableXObjs = do
      let pathStrings = contextPath ctx
          env = contextGlobalEnv ctx
          innerEnv = (contextInternalEnv ctx)
          typeEnv = contextTypeEnv ctx
          typeVariables = mapM xobjToTy typeVariableXObjs
          (preExistingModule, preExistingMeta) =
            case lookupBinder (SymPath pathStrings typeName) (fromMaybe env innerEnv) {envParent = Nothing} of
              Just (Binder meta (XObj (Mod found) _ _)) -> (Just found, meta)
              Just (Binder meta _) -> (Nothing, meta)
              _ -> (Nothing, emptyMeta)
          (creatorFunction, typeConstructor) =
            if length rest == 1 && isArray (head rest)
              then (moduleForDeftype, Deftype)
              else (moduleForSumtype, DefSumtype)
      case (nameXObj, typeVariables) of
        (XObj (Sym (SymPath _ tyName) _) i _, Just okTypeVariables) ->
          case creatorFunction (Just (getEnv env pathStrings)) typeEnv env pathStrings tyName okTypeVariables rest i preExistingModule of
            Right (typeModuleName, typeModuleXObj, deps) ->
              let structTy = StructTy (ConcreteNameTy (createStructName pathStrings tyName)) okTypeVariables
                  updatedGlobal = envInsertAt env (SymPath pathStrings typeModuleName) (Binder preExistingMeta typeModuleXObj)
                  typeDefinition =
                    -- NOTE: The type binding is needed to emit the type definition and all the member functions of the type.
                    XObj
                      ( Lst
                          ( XObj (typeConstructor structTy) Nothing Nothing :
                            XObj (Sym (SymPath pathStrings tyName) Symbol) Nothing Nothing :
                            rest
                          )
                      )
                      i
                      (Just TypeTy)
                  holderEnv = \name' prev -> Env (Map.fromList []) (Just prev) (Just name') [] ExternalEnv 0
                  holderModule = \name'' prevEnv -> (Binder emptyMeta (XObj (Mod (holderEnv name'' prevEnv)) (Just dummyInfo) (Just ModuleTy)))
                  folder = \(contx, prev) pathstring -> (contx {contextTypeEnv = TypeEnv $ envInsertAt (getTypeEnv typeEnv) (SymPath (maybeToList (envModuleName prev)) pathstring) (holderModule pathstring prev)}, (holderEnv pathstring prev))
                  wHolders = (fst (foldl folder (ctx, (getTypeEnv typeEnv)) pathStrings))
                  ctx' =
                    ( (fst (foldl folder (ctx, (getTypeEnv typeEnv)) pathStrings))
                        { contextGlobalEnv = updatedGlobal,
                          contextTypeEnv = TypeEnv (envInsertAt (getTypeEnv (contextTypeEnv wHolders)) (SymPath pathStrings tyName) (Binder emptyMeta typeDefinition))
                        }
                    )
               in do
                    ctxWithDeps <- liftIO (foldM (define True) ctx' deps)
                    let fakeImplBinder sympath t = (Binder emptyMeta (XObj (Sym sympath Symbol) (Just dummyInfo) (Just t)))
                        strSig = FuncTy [RefTy structTy (VarTy "q")] StringTy StaticLifetimeTy
                        copySig = FuncTy [RefTy structTy (VarTy "q")] structTy StaticLifetimeTy
                        Just strInterface = lookupBinder (SymPath [] "str") (getTypeEnv typeEnv)
                        Just copyInterface = lookupBinder (SymPath [] "copy") (getTypeEnv typeEnv)
                        ctxWithInterfaceRegistrations =
                          -- Since these functions are autogenerated, we treat them as a special case and automatically implement the interfaces.
                          foldM
                            (\context (path, sig, interface) -> registerInInterfaceIfNeeded context path interface sig)
                            ctxWithDeps
                            [ ((fakeImplBinder (SymPath (pathStrings ++ [typeModuleName]) "str") strSig), strSig, strInterface),
                              ((fakeImplBinder (SymPath (pathStrings ++ [typeModuleName]) "copy") copySig), copySig, copyInterface)
                            ]
                    case ctxWithInterfaceRegistrations of
                      Left err -> do
                        liftIO (putStrLnWithColor Red err)
                        pure (ctx, dynamicNil)
                      Right ok -> pure (ok, dynamicNil)
            Left err ->
              pure (makeEvalError ctx (Just err) ("Invalid type definition for '" ++ pretty nameXObj ++ "':\n\n" ++ show err) Nothing)
        (_, Nothing) ->
          pure (makeEvalError ctx Nothing ("Invalid type variables for type definition: " ++ pretty nameXObj) (xobjInfo nameXObj))
        _ -> error "primitiveDeftype1"
primitiveDeftype _ _ _ = error "primitivedeftype"

primitiveUse :: Primitive
primitiveUse xobj ctx [XObj (Sym path _) _ _] =
  (pure $ maybe lookupInGlobal useModule (lookupInEnv path e))
  where
    pathStrings = contextPath ctx
    env = contextGlobalEnv ctx
    e = getEnv env pathStrings
    useThese = envUseModules e
    e' = if path `elem` useThese then e else e {envUseModules = path : useThese}
    lookupInGlobal = maybe missing useModule (lookupInEnv path env)
      where
        missing = evalError ctx ("Can't find a module named '" ++ show path ++ "'") (xobjInfo xobj)
    useModule _ = (ctx {contextGlobalEnv = envReplaceEnvAt env pathStrings e'}, dynamicNil)
primitiveUse _ ctx [x] =
  argumentErr ctx "use" "a symbol" "first" x
primitiveUse _ _ _ = error "primitiveuse"

-- | Get meta data for a Binder
primitiveMeta :: Primitive
primitiveMeta (XObj _ i _) ctx [XObj (Sym (SymPath prefixes name) _) _ _, XObj (Str key) _ _] = do
  pure $ maybe errNotFound foundBinder lookup'
  where
    global = contextGlobalEnv ctx
    types = getTypeEnv (contextTypeEnv ctx)
    fullPath = consPath (union (contextPath ctx) prefixes) (SymPath [] name)
    lookup' :: Maybe Binder
    lookup' = (lookupBinder fullPath global <|> lookupBinder fullPath types) >>= pure
    foundBinder :: Binder -> (Context, Either EvalError XObj)
    foundBinder binder = (ctx, maybe dynamicNil Right (Meta.getBinderMetaValue key binder))
    errNotFound :: (Context, Either EvalError XObj)
    errNotFound = evalError ctx ("`meta` failed, I can’t find `" ++ show fullPath ++ "`") i
primitiveMeta _ ctx [XObj (Sym _ _) _ _, key] =
  argumentErr ctx "meta" "a string" "second" key
primitiveMeta _ ctx [path, _] =
  argumentErr ctx "meta" "a symbol" "first" path
primitiveMeta _ _ _ = error "primitivemeta"

primitiveDefined :: Primitive
primitiveDefined _ ctx [XObj (Sym path _) _ _] = do
  let env = contextEnv ctx
  pure $ maybe (ctx, Right falseXObj) (\_ -> (ctx, Right trueXObj)) (lookupInEnv path env)
primitiveDefined _ ctx [arg] =
  argumentErr ctx "defined" "a symbol" "first" arg
primitiveDefined _ _ _ = error "primitivedefined"

primitiveDeftemplate :: Primitive
-- deftemplate can't receive a dependency function, as Ty aren't exposed in Carp
primitiveDeftemplate _ ctx [XObj (Sym (SymPath [] name) _) _ _, ty, XObj (Str declTempl) _ _, XObj (Str defTempl) _ _] =
  pure $ maybe invalidType validType (xobjToTy ty)
  where
    pathStrings = contextPath ctx
    typeEnv = contextTypeEnv ctx
    globalEnv = contextGlobalEnv ctx
    p = SymPath pathStrings name
    invalidType = evalError ctx ("I do not understand the type form in " ++ pretty ty) (xobjInfo ty)
    validType t = case defineTemplate p t "" (toTemplate declTempl) (toTemplate defTempl) (const []) of
      (_, b@(Binder _ (XObj (Lst (XObj (Deftemplate template) _ _ : _)) _ _))) ->
        if isTypeGeneric t
          then
            let (Binder _ registration) = b
                meta = lookupMeta (getPath registration) globalEnv
                env' = envInsertAt globalEnv p (Binder meta registration)
             in (ctx {contextGlobalEnv = env'}, dynamicNil)
          else
            let templateCreator = getTemplateCreator template
                (registration, _) = instantiateTemplate p t (templateCreator typeEnv globalEnv)
                meta = lookupMeta (getPath registration) globalEnv
                env' = envInsertAt globalEnv p (Binder meta registration)
             in (ctx {contextGlobalEnv = env'}, dynamicNil)
      _ -> error "primitivedeftemplate1"
primitiveDeftemplate _ ctx [XObj (Sym (SymPath [] _) _) _ _, _, XObj (Str _) _ _, x] =
  argumentErr ctx "deftemplate" "a string" "fourth" x
primitiveDeftemplate _ ctx [XObj (Sym (SymPath [] _) _) _ _, _, x, _] =
  argumentErr ctx "deftemplate" "a string" "third" x
primitiveDeftemplate _ ctx [s@(XObj (Sym (SymPath _ _) _) _ _), _, _, _] = do
  argumentErr ctx "deftemplate" "a symbol without prefix" "first" s
primitiveDeftemplate _ ctx [x, _, _, _] =
  argumentErr ctx "deftemplate" "a symbol" "first" x
primitiveDeftemplate _ _ _ = error "primitivedeftemplate"

noTypeError :: Context -> XObj -> IO (Context, Either EvalError XObj)
noTypeError ctx x = pure $ evalError ctx ("Can't get the type of: " ++ pretty x) (xobjInfo x)

primitiveType :: Primitive
-- A special case, the type of the type of types (type (type (type 1))) => ()
primitiveType _ ctx [(XObj _ _ (Just Universe))] =
  pure (ctx, Right (XObj (Lst []) Nothing Nothing))
primitiveType _ ctx [(XObj _ _ (Just TypeTy))] = liftIO $ pure (ctx, Right $ reify TypeTy)
primitiveType _ ctx [x@(XObj (Sym path@(SymPath [] name) _) _ _)] =
  (maybe otherDefs go (lookupBinder path env))
  where
    env = contextGlobalEnv ctx
    otherDefs = case multiLookupEverywhere name env of
      [] ->
        notFound ctx x path
      binders ->
        (sequence (map (go . snd) binders))
          >>= pure . Lst . rights . map snd
          >>= \obj -> pure (ctx, Right $ (XObj obj Nothing Nothing))
    go binder =
      case (xobjTy (binderXObj binder)) of
        Nothing -> noTypeError ctx x
        Just t -> pure (ctx, Right (reify t))
primitiveType _ ctx [x@(XObj (Sym qualifiedPath _) _ _)] =
  maybe (notFound ctx x qualifiedPath) (go . snd) (lookupInEnv qualifiedPath env)
  where
    env = contextGlobalEnv ctx
    go binder =
      case (xobjTy (binderXObj binder)) of
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
primitiveType any' ctx [(XObj (Lst (XObj (Sym (SymPath [] "type") _) _ _ : rest)) _ _)] =
  primitiveType any' ctx rest
    >>= \result -> case snd result of
      Right xobj -> primitiveType any' (fst result) [xobj]
      Left e -> pure (ctx, Left e)
primitiveType _ ctx [x@(XObj _ _ _)] =
  let tenv = contextTypeEnv ctx
      typed = annotate tenv (contextGlobalEnv ctx) x Nothing
   in liftIO $ either fail' ok typed
  where
    fail' _ = pure (evalError ctx ("Can't get the type of: " ++ pretty x) (xobjInfo x))
    ok ((XObj _ _ (Just t)), _) = pure (ctx, Right $ reify t)
    ok (_, _) = pure (evalError ctx ("Can't get the type of: " ++ pretty x) (xobjInfo x))
primitiveType _ _ _ = error "primitivetype"

primitiveKind :: Primitive
primitiveKind _ ctx [x@(XObj _ _ _)] =
  let tenv = contextTypeEnv ctx
      typed = annotate tenv (contextGlobalEnv ctx) x Nothing
   in pure (either fail' ok typed)
  where
    fail' _ = (evalError ctx ("Can't get the kind of: " ++ pretty x) (xobjInfo x))
    ok (XObj _ _ (Just t), _) = (ctx, Right $ reify (tyToKind t))
    ok (_, _) = (evalError ctx ("Can't get the kind of: " ++ pretty x) (xobjInfo x))
primitiveKind _ _ _ = error "primitivekind"

-- | Primitive for printing help.
primitiveHelp :: Primitive
primitiveHelp _ ctx [XObj (Sym (SymPath [] "about") _) _ _] =
  liftIO $ do
    putStrLn "Carp is an ongoing research project by Erik Svedäng, et al."
    putStrLn ""
    putStrLn
      "Licensed under the Apache License, Version 2.0 (the \"License\"); \n\
      \you may not use this file except in compliance with the License. \n\
      \You may obtain a copy of the License at \n\
      \http://www.apache.org/licenses/LICENSE-2.0"
    putStrLn ""
    putStrLn
      "THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS ``AS IS'' AND ANY \n\
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
  liftIO $ do
    putStrLn ""
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
  liftIO $ do
    putStrLn "Don't panic - we can solve this!"
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
  liftIO $ do
    _ <- openBrowser url
    return (ctx, dynamicNil)

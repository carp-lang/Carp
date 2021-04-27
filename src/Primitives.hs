{-# LANGUAGE TupleSections #-}

module Primitives where

import ColorText
import Commands
import Context
import Control.Applicative
import Control.Monad (foldM, unless, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bifunctor
import Data.Either (rights, fromRight)
import Data.Functor ((<&>))
import Data.List (foldl')
import Data.Maybe (fromJust, fromMaybe)
import Deftype
import Emit
import Env (contextEnv, lookupEverywhere, lookupMeta, lookupBinder, lookupBinderEverywhere, addUsePath, insert)
import Infer
import Info
import Interfaces
import Managed
import qualified Meta
import Obj
import PrimitiveError
import Project
import Qualify (Qualified (..), getQualifiedPath, markQualified, qualify, qualifyNull, qualifyPath, unqualify, QualifiedPath)
import Reify
import Sumtypes
import Template
import ToTemplate
import TypeError
import TypePredicates
import Types
import Util
import Web.Browser (openBrowser)

makeNullaryPrim :: SymPath -> NullaryPrimitiveCallback -> String -> String -> (String, Binder)
makeNullaryPrim p = makePrim p . NullaryPrimitive

makeUnaryPrim :: SymPath -> UnaryPrimitiveCallback -> String -> String -> (String, Binder)
makeUnaryPrim p = makePrim p . UnaryPrimitive

makeBinaryPrim :: SymPath -> BinaryPrimitiveCallback -> String -> String -> (String, Binder)
makeBinaryPrim p = makePrim p . BinaryPrimitive

makeTernaryPrim :: SymPath -> TernaryPrimitiveCallback -> String -> String -> (String, Binder)
makeTernaryPrim p = makePrim p . TernaryPrimitive

makeQuaternaryPrim :: SymPath -> QuaternaryPrimitiveCallback -> String -> String -> (String, Binder)
makeQuaternaryPrim p = makePrim p . QuaternaryPrimitive

makeVariadicPrim :: SymPath -> VariadicPrimitiveCallback -> String -> String -> (String, Binder)
makeVariadicPrim p = makePrim p . VariadicPrimitive

argumentErr :: Context -> String -> String -> String -> XObj -> IO (Context, Either EvalError XObj)
argumentErr ctx fun ty number actual =
  pure (toEvalError ctx actual (ArgumentTypeError fun ty number actual))

makePrim :: SymPath -> PrimitiveFunctionType -> String -> String -> (String, Binder)
makePrim path callback doc example =
  (name, Binder meta prim)
  where
    SymPath _ name = path
    exampleUsage = "Example Usage:\n```\n" ++ example ++ "\n```\n"
    docString = doc ++ "\n\n" ++ exampleUsage
    meta = Meta.set "doc" (XObj (Str docString) Nothing Nothing) emptyMeta
    prim =
      XObj
        ( Lst
            [ XObj (Primitive callback) (Just dummyInfo) Nothing,
              XObj (Sym path Symbol) Nothing Nothing,
              XObj (Arr args) Nothing Nothing
            ]
        )
        (Just dummyInfo)
        (Just DynamicTy)
    args = (\x -> XObj (Sym (SymPath [] x) Symbol) Nothing Nothing) <$> argnames
    argnames = case callback of
      NullaryPrimitive _ -> []
      UnaryPrimitive _ -> ["x"]
      BinaryPrimitive _ -> ["x", "y"]
      TernaryPrimitive _ -> ["x", "y", "z"]
      QuaternaryPrimitive _ -> ["x", "y", "z", "w"]
      VariadicPrimitive _ -> []

infoXObjOrError :: Context -> (Context, Either EvalError XObj) -> Maybe Info -> Maybe XObj -> (Context, Either EvalError XObj)
infoXObjOrError ctx err i = maybe err (\xobj -> (ctx, Right xobj {xobjInfo = i}))

primitiveFile :: VariadicPrimitiveCallback
primitiveFile x@(XObj _ i _) ctx args =
  pure $ case args of
    [] -> infoXObjOrError ctx err i (getFileAsXObj fppl i)
    [XObj _ mi _] -> infoXObjOrError ctx err i (getFileAsXObj fppl mi)
    _ -> toEvalError ctx x (ArgumentArityError x "0 or 1" args)
  where
    fppl = projectFilePathPrintLength (contextProj ctx)
    err = toEvalError ctx x (MissingInfo x)

primitiveLine :: VariadicPrimitiveCallback
primitiveLine x@(XObj _ i _) ctx args =
  pure $ case args of
    [] -> infoXObjOrError ctx err i (getLineAsXObj i)
    [XObj _ mi _] -> infoXObjOrError ctx err i (getLineAsXObj mi)
    _ -> toEvalError ctx x (ArgumentArityError x "0 or 1" args)
  where
    err = toEvalError ctx x (MissingInfo x)

primitiveColumn :: VariadicPrimitiveCallback
primitiveColumn x@(XObj _ i _) ctx args =
  pure $ case args of
    [] -> infoXObjOrError ctx err i (getColumnAsXObj i)
    [XObj _ mi _] -> infoXObjOrError ctx err i (getColumnAsXObj mi)
    _ -> toEvalError ctx x (ArgumentArityError x "0 or 1" args)
  where
    err = toEvalError ctx x (MissingInfo x)

primitiveImplements :: BinaryPrimitiveCallback
primitiveImplements _ ctx x@(XObj (Sym interface@(SymPath _ _) _) _ _) (XObj (Sym path _) _ _) =
  do
    (maybeInterface, maybeImpl) <- pure (lookupInterface ctx interface, lookupBinderInGlobalEnv ctx qpath)
    case (maybeInterface, maybeImpl) of
      (_, Left _) -> updateMeta (Meta.stub (contextualize path ctx)) ctx
      (Left _, Right implBinder) ->
        warn >> updateMeta implBinder ctx
      (Right interfaceBinder, Right implBinder) ->
        -- N.B. The found binding will be fully qualified!
        addToInterface interfaceBinder implBinder
  where
    qpath = qualifyNull ctx path
    warn :: IO ()
    warn = emitWarning (show (NonExistentInterfaceWarning x))
    addToInterface :: Binder -> Binder -> IO (Context, Either EvalError XObj)
    addToInterface inter impl =
      let (Right newCtx, maybeErr) = registerInInterface ctx impl inter
       in maybe (updateMeta impl newCtx) (handleError newCtx impl) maybeErr
    handleError :: Context -> Binder -> InterfaceError -> IO (Context, Either EvalError XObj)
    handleError context impl e@(AlreadyImplemented _ oldImplPath _ _) =
      emitWarning (show e) >> pure (removeInterfaceFromImplements oldImplPath x context) >>= updateMeta impl
    handleError context _ e =
      emitError (show e) >> pure (evalError context (show e) (xobjInfo x))
    updateMeta :: Binder -> Context -> IO (Context, Either EvalError XObj)
    updateMeta binder context =
      pure (fromRight (error "Couldn't insert updated meta!!") (fromJust updater), dynamicNil)
      where
        updater =
          ( ( Meta.getBinderMetaValue "implements" binder
                <&> updateImplementations binder
            )
              <|> Just (updateImplementations binder (XObj (Lst []) (Just dummyInfo) (Just DynamicTy)))
          )
            >>= pure . (insertInGlobalEnv context qpath)
        updateImplementations :: Binder -> XObj -> Binder
        updateImplementations implBinder (XObj (Lst impls) inf ty) =
          if x `elem` impls
            then binder
            else Meta.updateBinderMeta implBinder "implements" (XObj (Lst (x : impls)) inf ty)
        updateImplementations implBinder _ =
          Meta.updateBinderMeta implBinder "implements" (XObj (Lst [x]) (Just dummyInfo) (Just DynamicTy))
primitiveImplements x ctx (XObj (Sym _ _) _ _) y =
  pure $ toEvalError ctx x (ArgumentTypeError "implements" "a symbol" "second" y)
primitiveImplements _ ctx x _ =
  pure $ toEvalError ctx x (ArgumentTypeError "implements" "a symbol" "first" x)

-- N.B. Symbols come into this function FULLY QUALIFIED!
-- see Eval.hs annotateWithinContext
define :: Bool -> Context -> Qualified -> IO Context
define hidden ctx qualifiedXObj =
  pure (if hidden then (Meta.hide freshBinder) else freshBinder)
    >>= \newBinder ->
      if isTypeDef annXObj
        then defineInTypeEnv newBinder
        else defineInGlobalEnv newBinder
  where
    annXObj = unQualified qualifiedXObj
    freshBinder = toBinder annXObj
    qpath = getQualifiedPath qualifiedXObj
    defineInTypeEnv :: Binder -> IO Context
    defineInTypeEnv = pure . fromRight ctx . (insertTypeBinder ctx qpath)
    defineInGlobalEnv :: Binder -> IO Context
    defineInGlobalEnv newBinder =
      when (projectEchoC (contextProj ctx)) (putStrLn (toC All (Binder emptyMeta annXObj)))
        >> case (lookupBinderInGlobalEnv ctx qpath) of
          Left _ -> pure (fromRight ctx (insertInGlobalEnv ctx qpath newBinder))
          Right oldBinder -> redefineExistingBinder oldBinder newBinder
    redefineExistingBinder :: Binder -> Binder -> IO Context
    redefineExistingBinder old@(Binder meta _) (Binder _ x) =
      do
        unless (isInstantiation (binderXObj old)) (warnTypeChange old)
        -- TODO: Merge meta more elegantly.
        updatedContext <- (implementInterfaces (Binder meta x))
        pure (fromRight (error ("Failed to insert " ++ show qpath)) (insertInGlobalEnv updatedContext qpath (Binder meta x)))
    -- | Templates have varying types that change often depending on context, but the template form itself is initially untyped.
    -- This can lead to odd type change warnings where the actual types *are* in fact the same.
    -- TODO: Find a way to remove this case.
    isInstantiation (XObj (Lst (XObj (Instantiate _) _ _ : _)) _ _) = True
    isInstantiation _ = False
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
            -- TODO: Direct qualification!
            >>= \(XObj (Lst interfaces) _ _) -> pure (map Qualified interfaces)
        )
        >>= \maybeinterfaces ->
          pure (rights (fmap (lookupBinderInTypeEnv ctx . getQualifiedPath) (fromMaybe [] maybeinterfaces)))
            >>= \interfaceBinders ->
              pure (foldl' (\(ctx', _) interface -> first (fromRight ctx') (registerInInterface ctx' binder interface)) (ctx, Nothing) interfaceBinders)
                >>= \(newCtx, err) -> case err of
                  Just e -> printError (contextExecMode ctx) (show e) >> pure ctx
                  Nothing -> pure newCtx
    printError :: ExecutionMode -> String -> IO ()
    printError Check e =
      let fppl = projectFilePathPrintLength (contextProj ctx)
       in putStrLn (machineReadableInfoFromXObj fppl annXObj ++ " " ++ e)
    printError _ e = putStrLnWithColor Red e

primitiveRegisterType :: VariadicPrimitiveCallback
primitiveRegisterType _ ctx [XObj (Sym (SymPath [] t) _) _ _] =
  primitiveRegisterTypeWithoutFields ctx t Nothing
primitiveRegisterType _ ctx [x] =
  pure (evalError ctx ("`register-type` takes a symbol, but it got " ++ pretty x) (xobjInfo x))
primitiveRegisterType _ ctx [XObj (Sym (SymPath [] t) _) _ _, XObj (Str override) _ _] =
  primitiveRegisterTypeWithoutFields ctx t (Just override)
primitiveRegisterType _ ctx [x@(XObj (Sym (SymPath [] t) _) _ _), XObj (Str override) _ _, members] =
  primitiveRegisterTypeWithFields ctx x t (Just override) members
primitiveRegisterType _ ctx [x@(XObj (Sym (SymPath [] t) _) _ _), members] =
  primitiveRegisterTypeWithFields ctx x t Nothing members
primitiveRegisterType x ctx _ = pure (toEvalError ctx x RegisterTypeError)

-- | Register an external type that has no fields.
primitiveRegisterTypeWithoutFields :: Context -> String -> Maybe String -> IO (Context, Either EvalError XObj)
primitiveRegisterTypeWithoutFields ctx t override = do
  let path = SymPath [] t
      typeDefinition = XObj (Lst [XObj (ExternalType override) Nothing Nothing, XObj (Sym path Symbol) Nothing Nothing]) Nothing (Just TypeTy)
  -- TODO: Support registering types in modules
  case insertTypeBinder ctx (markQualified path) (toBinder typeDefinition) of
    Left  e -> pure (evalError ctx (show e) Nothing)
    Right c -> pure (c, dynamicNil)

-- | Register an external type that has fields.
primitiveRegisterTypeWithFields :: Context -> XObj -> String -> Maybe String -> XObj -> IO (Context, Either EvalError XObj)
primitiveRegisterTypeWithFields ctx x t override members =
  either
    handleErr
    updateContext
    (bindingsForRegisteredType (contextTypeEnv ctx) (contextGlobalEnv ctx) (contextPath ctx) t [members] Nothing preExistingModule)
  where
    handleErr e = pure $ makeEvalError ctx (Just e) (show e) (xobjInfo x)
    updateContext (typeModuleName, typeModuleXObj, deps) =
      do
        let typeDefinition = XObj (Lst [XObj (ExternalType override) Nothing Nothing, XObj (Sym path Symbol) Nothing Nothing]) Nothing (Just TypeTy)
            path' = (qualifyPath ctx (SymPath [] typeModuleName))
            update = \c -> insertInGlobalEnv' path' (toBinder typeModuleXObj) c >>= insertTypeBinder' path' (toBinder typeDefinition)
            Right ctx' = update ctx
        -- TODO: Another case where define does not get formally qualified deps!
        contextWithDefs <- liftIO $ foldM (define True) ctx' (map Qualified deps)
        pure (contextWithDefs, dynamicNil)
    path = SymPath [] t
    preExistingModule = case lookupBinderInGlobalEnv ctx path of
      Right (Binder _ (XObj (Mod found _) _ _)) -> Just found
      _ -> Nothing

notFound :: Context -> XObj -> SymPath -> IO (Context, Either EvalError XObj)
notFound ctx x path = pure (toEvalError ctx x (SymbolNotFoundError path))

-- | Get information about a binding.
primitiveInfo :: UnaryPrimitiveCallback
primitiveInfo _ ctx target@(XObj (Sym path@(SymPath _ name) _) _ _) =
  case path of
    SymPath [] _ ->
      do
        let found = lookupBinderInTypeEnv ctx path
        _ <- printIfFound found
        _ <- printInterfaceImplementationsOrAll found otherBindings
        either (const (notFound ctx target path)) (const ok) (found <> fmap head otherBindings)
      where
        otherBindings =
          fmap (: []) (lookupBinderInContextEnv ctx path)
            <> (Right (lookupBinderEverywhere (contextGlobalEnv ctx) name))
    _ ->
      do
        let found  = lookupBinderInTypeEnv ctx path
        let others = lookupBinderInContextEnv ctx path
        _ <- printIfFound found
        _ <- either (const (pure ())) printer others
        either (const (notFound ctx target path)) (const ok) (found <> others)
  where
    ok :: IO (Context, Either EvalError XObj)
    ok = pure (ctx, dynamicNil)
    printInterfaceImplementationsOrAll :: Either ContextError Binder -> Either ContextError [Binder] -> IO ()
    printInterfaceImplementationsOrAll interface impls =
      either
        (const (pure ()))
        (foldM (\_ binder -> printer binder) ())
        ( ( interface
              >>= \binder ->
                pure (xobjObj (binderXObj binder))
                  >>= \obj ->
                    case obj of
                      (Lst [XObj (Interface _ _) _ _, _]) ->
                        fmap (filter (implementsInterface binder)) impls
                      _ -> impls
          )
            <> impls
        )
    implementsInterface :: Binder -> Binder -> Bool
    implementsInterface binder binder' =
      maybe
        False
        (\(XObj (Lst impls) _ _) -> getBinderPath binder `elem` map getPath impls)
        (Meta.getBinderMetaValue "implements" binder')
    printIfFound :: Either ContextError Binder -> IO ()
    printIfFound = either (const (pure ())) printer
    printer :: Binder -> IO ()
    printer binder@(Binder metaData x@(XObj _ (Just i) _)) =
      putStrLnWithColor Blue (forceShowBinder binder)
        >> putStrLn ("  Defined at " ++ prettyInfo i)
        >> printMeta metaData (contextProj ctx) x
    printer binder@(Binder metaData x) =
      print binder
        >> printMeta metaData (contextProj ctx) x
    printMeta :: MetaData -> Project -> XObj -> IO ()
    printMeta metaData proj x =
      maybe (pure ()) (printMetaVal "Documentation" (either (const "") id . unwrapStringXObj)) (Meta.get "doc" metaData)
        >> maybe (pure ()) (printMetaVal "Implements" getName) (Meta.get "implements" metaData)
        >> maybe (pure ()) (printMetaBool "Private") (Meta.get "private" metaData)
        >> maybe (pure ()) (printMetaBool "Hidden") (Meta.get "hidden" metaData)
        >> maybe (pure ()) (printMetaVal "Signature" pretty) (Meta.get "sig" metaData)
        >> maybe (pure ()) printDeprecated (Meta.get "deprecated" metaData)
        >> when (projectPrintTypedAST proj) (putStrLnWithColor Yellow (prettyTyped x))
    printMetaBool :: String -> XObj -> IO ()
    printMetaBool s (XObj (Bol True) _ _) = putStrLn ("  " ++ s)
    printMetaBool _ _ = return ()
    printDeprecated :: XObj -> IO ()
    printDeprecated (XObj (Bol True) _ _) = putStrLn "  Deprecated"
    printDeprecated (XObj (Str v) _ _) = putStrLn ("  Deprecated: " ++ v)
    printDeprecated _ = return ()
    printMetaVal :: String -> (XObj -> String) -> XObj -> IO ()
    printMetaVal s f xobj = putStrLn ("  " ++ s ++ ": " ++ f xobj)
primitiveInfo _ ctx notName =
  argumentErr ctx "info" "a name" "first" notName

dynamicOrMacroWith :: Context -> (SymPath -> [XObj]) -> Ty -> String -> XObj -> IO (Context, Either EvalError XObj)
dynamicOrMacroWith ctx producer ty name body = do
  let qpath = qualifyPath ctx (SymPath [] name)
      elt = XObj (Lst (producer (unqualify qpath))) (xobjInfo body) (Just ty)
      meta = fromRight emptyMeta (lookupMeta (contextGlobalEnv ctx) (getPath elt))
  pure (case (insertInGlobalEnv ctx qpath (Binder meta elt)) of
         Left e -> evalError ctx (show e) (xobjInfo body)
         Right c -> (c, dynamicNil))

-- | Get the members of a type declaration.
primitiveMembers :: UnaryPrimitiveCallback
primitiveMembers _ ctx xobj@(XObj (Sym path _) _ _) =
  case (lookupBinderInTypeEnv ctx path) of
    Left _  -> pure $ toEvalError ctx xobj (StructNotFound xobj)
    Right b -> go (binderXObj b)
  where go :: XObj -> IO (Context, Either EvalError XObj)
        go (XObj (Lst [(XObj (Deftype _) _ _), _, (XObj (Arr members) _ _)]) _ _) =
          pure (ctx, Right (XObj (Arr (map (\(a, b) -> XObj (Lst [a, b]) Nothing Nothing) (pairwise members))) Nothing Nothing))
        go (XObj (Lst ((XObj (DefSumtype _) _ _) : _ :  cases)) _ _) =
          pure $ (ctx, (either Left (\a -> Right (XObj (Arr (concat a)) Nothing Nothing)) (mapM getMembersFromCase cases)))
        go x = pure (toEvalError ctx x (NonTypeInTypeEnv path x))

        getMembersFromCase :: XObj -> Either EvalError [XObj]
        getMembersFromCase (XObj (Lst members) _ _) =
          Right (map (\(a, b) -> XObj (Lst [a, b]) Nothing Nothing) (pairwise members))
        getMembersFromCase x@(XObj (Sym _ _) _ _) =
          Right [XObj (Lst [x, XObj (Arr []) Nothing Nothing]) Nothing Nothing]
        getMembersFromCase x =
          second (:[]) (snd (toEvalError ctx x (PrimitiveError.InvalidSumtypeCase x)))
primitiveMembers _ ctx x = argumentErr ctx "members" "a symbol" "first" x

-- | Set meta data for a Binder
--
-- Permits "forward-declaration": if the binder doesn't exist, it creates a
-- "meta stub" for the binder with the meta information.
primitiveMetaSet :: TernaryPrimitiveCallback
primitiveMetaSet _ ctx target@(XObj (Sym path@(SymPath _ _) _) _ _) (XObj (Str key) _ _) value =
  pure $ either (const create) (,dynamicNil) (lookupGlobal <> lookupType)
  where
    qpath = qualifyPath ctx path
    lookupGlobal :: Either ContextError Context
    lookupGlobal = lookupBinderInGlobalEnv ctx path
                     >>= \binder -> pure (Meta.updateBinderMeta binder key value)
                     >>= insertInGlobalEnv ctx qpath
    lookupType :: Either ContextError Context
    lookupType = lookupBinderInTypeEnv ctx qpath
                   >>= \binder -> pure (Meta.updateBinderMeta binder key value)
                   >>= insertTypeBinder ctx qpath
    create :: (Context, Either EvalError XObj)
    create =
      let updated = Meta.updateBinderMeta (Meta.stub (unqualify qpath)) key value
       in case (insertInGlobalEnv ctx qpath updated) of
            Left e -> toEvalError ctx target (MetaSetFailed target (show e))
            Right c -> (c, dynamicNil)
primitiveMetaSet _ ctx (XObj (Sym (SymPath _ _) _) _ _) key _ =
  argumentErr ctx "meta-set!" "a string" "second" key
primitiveMetaSet _ ctx target _ _ =
  argumentErr ctx "meta-set!" "a symbol" "first" target

primitiveDefinterface :: BinaryPrimitiveCallback
primitiveDefinterface xobj ctx nameXObj@(XObj (Sym path@(SymPath [] name) _) _ _) ty =
  pure $ maybe invalidType validType (xobjToTy ty)
  where
    invalidType = evalError ctx ("Invalid type for interface `" ++ name ++ "`: " ++ pretty ty) (xobjInfo ty)
    validType t = either (const defInterface) updateInterface (lookupBinderInTypeEnv ctx path)
      where
        defInterface =
          let interface = defineInterface name t [] (xobjInfo nameXObj)
              binder = toBinder interface
              Right ctx' = insertTypeBinder ctx (markQualified (SymPath [] name)) binder
              Right newCtx = retroactivelyRegisterInInterface ctx' binder
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
primitiveDefinterface _ ctx name _ =
  pure (evalError ctx ("`definterface` expects a name as first argument, but got `" ++ pretty name ++ "`") (xobjInfo name))

registerInternal :: Context -> String -> XObj -> Maybe String -> IO (Context, Either EvalError XObj)
registerInternal ctx name ty override =
  pure $ maybe invalidType validType (xobjToTy ty)
  where
    invalidType =
      evalError
        ctx
        ( "Can't understand type when registering '" ++ name
            ++ "'"
        )
        (xobjInfo ty)
    -- TODO: Retroactively register in interface if implements metadata is present.
    validType t =
      let qpath = qualifyPath ctx (SymPath [] name)
          registration =
            XObj
              ( Lst
                  [ XObj (External override) Nothing Nothing,
                    XObj (Sym (unqualify qpath) Symbol) Nothing Nothing,
                    ty
                  ]
              )
              (xobjInfo ty)
              (Just t)
          meta = fromRight emptyMeta (lookupMeta (contextGlobalEnv ctx) (getPath registration))
       in case (insertInGlobalEnv ctx qpath (Binder meta registration)) of
            Left err -> evalError ctx (show err) (xobjInfo ty)
            Right c -> (c, dynamicNil)

primitiveRegister :: VariadicPrimitiveCallback
primitiveRegister _ ctx [XObj (Sym (SymPath [] name) _) _ _, ty] =
  registerInternal ctx name ty Nothing
primitiveRegister _ ctx [invalid@(XObj (Sym _ _) _ _), _] =
  pure
    ( evalError
        ctx
        ("`register` expects an unqualified name as first argument, but got `" ++ pretty invalid ++ "`")
        (xobjInfo invalid)
    )
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

primitiveDeftype :: VariadicPrimitiveCallback
primitiveDeftype xobj ctx (name : rest@(XObj (Arr a) _ _ : _)) =
  case members a of
      Nothing -> pure (toEvalError ctx xobj BadDeftypeMembers)
      Just ms -> ensureUnqualified (map fst ms)
  where
    -- | Get the members of the type
    members :: [XObj] -> Maybe [(XObj, XObj)]
    members [] = Just []
    members [_] = Nothing
    members (binding : val : xs) = members xs >>= \xs' -> pure $ (binding, val) : xs'
    -- | Ensure all the type members are unqualified symbols.
    ensureUnqualified :: [XObj] -> IO (Context, Either EvalError XObj)
    ensureUnqualified objs =
      if all isUnqualifiedSym objs
        then deftype ctx name (selectConstructor rest)
        else pure (toEvalError ctx xobj (QualifiedTypeMember rest))
primitiveDeftype _ ctx (name : rest) =
  deftype ctx name (selectConstructor rest)
primitiveDeftype _ _ _ = error "primitivedeftype"

type ModuleCreator = Context -> String -> [Ty] -> [XObj] -> Maybe Info -> Either TypeError (String, XObj, [XObj])

-- | Build an XObj representing the constructor of a type in Carp.
selectConstructor :: [XObj] -> (Ty -> (XObj, [XObj], ModuleCreator))
selectConstructor xs =
  let
      (constructor, creator, mems) = if length xs == 1 && isArray (head xs)
                                       then (Deftype, moduleForDeftypeInContext, xs)
                                       else (DefSumtype, moduleForSumtypeInContext, xs)
   in \t -> (XObj (Lst (XObj (constructor t) Nothing Nothing
                      : XObj (Sym (getStructPath t) Symbol) Nothing Nothing
                      : mems
                      )
                  )
                  Nothing
                  (Just TypeTy), mems, creator)

deftype :: Context -> XObj -> (Ty -> (XObj, [XObj], ModuleCreator)) -> IO (Context, Either EvalError XObj)
deftype ctx x@(XObj (Sym (SymPath [] name) _) _ _) constructor =
  do (ctxWithType, e) <- (makeType ctx name [] constructor)
     case e of
       Left err -> pure (evalError ctx (show err) (xobjInfo x))
       Right t -> autoDerive ctxWithType t
deftype ctx x@(XObj (Lst ((XObj (Sym (SymPath [] name) _) _ _) : tyvars)) _ _) constructor =
  do (ctxWithType, e) <- (either (\s -> pure (evalError ctx s Nothing))
                                 (\vars -> makeType ctx name vars constructor)
                                 (maybe (Left (show (InvalidTypeVariables x))) Right (checkVariables tyvars)))
     case e of
       Left err -> pure (evalError ctx (show err) (xobjInfo x))
       Right t -> autoDerive ctxWithType t
deftype ctx name _ = pure $ toEvalError ctx name (InvalidTypeName name)

checkVariables :: [XObj] -> Maybe [Ty]
checkVariables vars = mapM xobjToTy vars

makeType :: Context -> String -> [Ty] -> (Ty -> (XObj, [XObj], ModuleCreator)) -> IO (Context, Either EvalError Ty)
makeType ctx name vars constructor =
  let qpath  = (qualifyPath ctx (SymPath [] name))
      ty    = StructTy (ConcreteNameTy (unqualify qpath)) vars
      (typeX, members, creator) = constructor ty
   in case (unwrapErr (creator ctx name vars members Nothing)
             >>= \(_, modx, deps) -> pure (existingOr ctx qpath modx)
             >>= \mod' -> unwrapErr (insertType ctx qpath (toBinder typeX) mod')
             >>= \c -> pure (foldM (define True) c (map Qualified deps))) of
        Left e  -> pure (evalError ctx e (xobjInfo typeX))
        Right result -> (result >>= \ctx' -> pure (ctx', pure ty))
   where existingOr :: Context -> QualifiedPath -> XObj -> Binder
         existingOr c q x@(XObj (Mod e _) _ _) =
           case ((lookupBinderInInternalEnv c q) <> (lookupBinderInGlobalEnv c q)) of
             Right (Binder meta (XObj (Mod ve te) ii tt)) ->
               (Binder meta (XObj (Mod (e <> ve) te) ii tt))
             _ -> (toBinder x)
         existingOr _ _ x = (toBinder x)

-- | Automatically derive implementations of interfaces.
autoDerive :: Context -> Ty -> IO (Context, Either EvalError XObj)
autoDerive c ty =
  let (SymPath mods tyname) = (getStructPath ty)
      implBinder :: String -> Ty -> Binder
      implBinder name t = Binder emptyMeta (XObj (Sym (SymPath (mods ++ [tyname]) name) Symbol) (Just dummyInfo) (Just t))
      getSig :: String -> Ty
      getSig "delete" = FuncTy [ty] UnitTy StaticLifetimeTy
      getSig "str"    = FuncTy [RefTy ty (VarTy "q")] StringTy StaticLifetimeTy
      getSig "copy"   = FuncTy [RefTy ty (VarTy "q")] ty StaticLifetimeTy
      getSig _        = VarTy "z"
      interfaces = [lookupBinderInTypeEnv c (markQualified (SymPath [] "delete")),
                    lookupBinderInTypeEnv c (markQualified (SymPath [] "str")),
                    lookupBinderInTypeEnv c (markQualified (SymPath [] "copy"))]
      registration interface =
        let name = getSimpleName (binderXObj interface)
            sig  = getSig name
         in (implBinder name sig, sig, interface)
      derives = (sequence interfaces)
                  >>= \binders -> pure (fmap registration binders)
   in case derives of
        Left _ -> pure (evalError c "Couldn't derive interfaces." Nothing)
        Right regs ->
          case foldl' (\(context, _) (path, sig, interface) -> first (fromRight (error "COULDNT DERIVE!")) (registerInInterfaceIfNeeded context path interface sig)) (c, Nothing) regs of
            (ci, Just err@AlreadyImplemented {}) -> emitWarning (show err) >> pure (ci, dynamicNil::Either EvalError XObj)
            (_, Just err) -> pure $ evalError c (show err) Nothing
            (ci, Nothing) -> pure (ci, dynamicNil::Either EvalError XObj)

-- | Add a module to the list of implicitly imported modules.
primitiveUse :: UnaryPrimitiveCallback
primitiveUse xobj ctx (XObj (Sym path _) _ _) =
 let modulePath = (contextPath ctx)
     global     = (contextGlobalEnv ctx)
  in pure (case modulePath of
             [] -> updateGlobalUsePaths global
             _ -> case (lookupBinder global (SymPath (init modulePath) (last modulePath))) of
                    Left err -> (evalError ctx (show err) (xobjInfo xobj))
                    Right binder ->
                      updateModuleUsePaths global (SymPath (init modulePath) (last modulePath)) binder)
  where updateGlobalUsePaths :: Env -> (Context, Either EvalError XObj)
        updateGlobalUsePaths e =
          ((replaceGlobalEnv ctx (addUsePath e path)), dynamicNil)

        updateModuleUsePaths :: Env -> SymPath -> Binder -> (Context, Either EvalError XObj)
        updateModuleUsePaths e p (Binder meta (XObj (Mod ev et) i t)) =
          either
            (\err -> (evalError ctx err (xobjInfo xobj)))
            (\newCtx -> (newCtx, dynamicNil))
            ((unwrapErr (insert e p (Binder meta (XObj (Mod (addUsePath ev path) et) i t))))
              >>= pure . replaceGlobalEnv ctx)
        updateModuleUsePaths _ _ _ =
          (evalError ctx "Context path pointed to non-module!" (xobjInfo xobj))
primitiveUse _ ctx x =
  argumentErr ctx "use" "a symbol" "first" x

-- | Get meta data for a Binder
primitiveMeta :: BinaryPrimitiveCallback
primitiveMeta (XObj _ i _) ctx (XObj (Sym path _) _ _) (XObj (Str key) _ _) =
  pure $ maybe errNotFound foundBinder lookup'
  where
    lookup' :: Maybe Binder
    lookup' = either (const Nothing) Just (lookupBinderInGlobalEnv ctx path <> lookupBinderInTypeEnv ctx path)
    foundBinder :: Binder -> (Context, Either EvalError XObj)
    foundBinder binder = (ctx, maybe dynamicNil Right (Meta.getBinderMetaValue key binder))
    errNotFound :: (Context, Either EvalError XObj)
    errNotFound = evalError ctx ("`meta` failed, I can’t find `" ++ show path ++ "`") i
primitiveMeta _ ctx (XObj (Sym _ _) _ _) key =
  argumentErr ctx "meta" "a string" "second" key
primitiveMeta _ ctx path _ =
  argumentErr ctx "meta" "a symbol" "first" path

primitiveDefined :: UnaryPrimitiveCallback
primitiveDefined _ ctx (XObj (Sym path _) _ _) =
  pure $ either (const (ctx, Right falseXObj)) (const (ctx, Right trueXObj)) (lookupBinderInContextEnv ctx path)
primitiveDefined _ ctx arg =
  argumentErr ctx "defined" "a symbol" "first" arg

primitiveDeftemplate :: QuaternaryPrimitiveCallback
-- deftemplate can't receive a dependency function, as Ty aren't exposed in Carp
primitiveDeftemplate _ ctx (XObj (Sym p@(SymPath [] _) _) _ _) ty (XObj (Str declTempl) _ _) (XObj (Str defTempl) _ _) =
  pure $ maybe invalidType (fromRight invalidType . fmap (\x -> (x, dynamicNil)) . validType) (xobjToTy ty)
  where
    typeEnv = contextTypeEnv ctx
    globalEnv = contextGlobalEnv ctx
    invalidType = evalError ctx ("I do not understand the type form in " ++ pretty ty) (xobjInfo ty)
    validType t = case defineTemplate (contextualize p ctx) t "" (toTemplate declTempl) (toTemplate defTempl) (const []) of
      (_, b@(Binder _ (XObj (Lst (XObj (Deftemplate template) _ _ : _)) _ _))) ->
        if isTypeGeneric t
          then
            let (Binder _ registration) = b
                meta = fromRight emptyMeta (lookupMeta globalEnv (getPath registration))
             in insertInGlobalEnv ctx (qualifyPath ctx p) (Binder meta registration)
          else
            let templateCreator = getTemplateCreator template
                (registration, _) = instantiateTemplate (contextualize p ctx) t (templateCreator typeEnv globalEnv)
                meta = fromRight emptyMeta (lookupMeta globalEnv (getPath registration))
             in insertInGlobalEnv ctx (qualifyPath ctx p) (Binder meta registration)
      _ -> error "primitivedeftemplate1"
primitiveDeftemplate _ ctx (XObj (Sym (SymPath [] _) _) _ _) _ (XObj (Str _) _ _) x =
  argumentErr ctx "deftemplate" "a string" "fourth" x
primitiveDeftemplate _ ctx (XObj (Sym (SymPath [] _) _) _ _) _ x _ =
  argumentErr ctx "deftemplate" "a string" "third" x
primitiveDeftemplate _ ctx s@(XObj (Sym (SymPath _ _) _) _ _) _ _ _ =
  argumentErr ctx "deftemplate" "a symbol without prefix" "first" s
primitiveDeftemplate _ ctx x _ _ _ =
  argumentErr ctx "deftemplate" "a symbol" "first" x

noTypeError :: Context -> XObj -> IO (Context, Either EvalError XObj)
noTypeError ctx x = pure $ evalError ctx ("Can't get the type of: " ++ pretty x) (xobjInfo x)

primitiveType :: UnaryPrimitiveCallback
-- A special case, the type of the type of types (type (type (type 1))) => ()
primitiveType _ ctx (XObj _ _ (Just Universe)) =
  pure (ctx, Right (XObj (Lst []) Nothing Nothing))
primitiveType _ ctx (XObj _ _ (Just TypeTy)) = liftIO $ pure (ctx, Right $ reify TypeTy)
primitiveType _ ctx x@(XObj (Sym path@(SymPath [] name) _) _ _) =
  fromRight otherDefs (second go (lookupBinderInGlobalEnv ctx path))
  where
    env = contextGlobalEnv ctx
    otherDefs = case lookupEverywhere env name of
      [] ->
        notFound ctx x path
      binders ->
        mapM (go . snd) binders
          >>= (\obj -> pure (ctx, Right (XObj obj Nothing Nothing)))
            . Lst
            . rights
            . map snd
    go binder =
      case xobjTy (binderXObj binder) of
        Nothing -> noTypeError ctx x
        Just t -> pure (ctx, Right (reify t))
primitiveType _ ctx x@(XObj (Sym qualifiedPath _) _ _) =
  fromRight (notFound ctx x qualifiedPath) (second go (lookupBinderInGlobalEnv ctx qualifiedPath))
  where
    go binder =
      case xobjTy (binderXObj binder) of
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
primitiveType any' ctx (XObj (Lst [XObj (Sym (SymPath [] "type") _) _ _, rest]) _ _) =
  primitiveType any' ctx rest
    >>= \result -> case snd result of
      Right xobj -> primitiveType any' (fst result) xobj
      Left e -> pure (ctx, Left e)
primitiveType _ ctx x@XObj {} =
  let tenv = contextTypeEnv ctx
      typed = either (\_ -> annotate tenv (contextGlobalEnv ctx) (Qualified x) Nothing) (\q -> annotate tenv (contextGlobalEnv ctx) q Nothing) $ qualify ctx x
   in liftIO $ either fail' ok typed
  where
    fail' _ = pure (evalError ctx ("Can't get the type of: " ++ pretty x) (xobjInfo x))
    ok (XObj _ _ (Just t), _) = pure (ctx, Right $ reify t)
    ok (_, _) = pure (evalError ctx ("Can't get the type of: " ++ pretty x) (xobjInfo x))

primitiveKind :: UnaryPrimitiveCallback
primitiveKind _ ctx x@XObj {} =
  let tenv = contextTypeEnv ctx
      typed = either (\_ -> annotate tenv (contextGlobalEnv ctx) (Qualified x) Nothing) (\q -> annotate tenv (contextGlobalEnv ctx) q Nothing) $ qualify ctx x
   in pure (either fail' ok typed)
  where
    fail' _ = evalError ctx ("Can't get the kind of: " ++ pretty x) (xobjInfo x)
    ok (XObj _ _ (Just t), _) = (ctx, Right $ reify (tyToKind t))
    ok (_, _) = evalError ctx ("Can't get the kind of: " ++ pretty x) (xobjInfo x)

-- | Primitive for printing help.
primitiveHelp :: VariadicPrimitiveCallback
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

-- | Checks if a type is managed. Note that it probably could be implemented in terms
-- of `(implements? <sym>)` but to be 100% sure that we use the same lookup as the
-- type system, I've done it this way -- at least for now.
primitiveIsManaged :: UnaryPrimitiveCallback
primitiveIsManaged _ ctx xobj@(XObj (Sym _ _) i _) =
  let tenv = contextTypeEnv ctx
      genv = contextEnv ctx
   in case xobjToTy xobj of
        Just ty ->
          if isManaged tenv genv ty
            then pure (ctx, Right trueXObj)
            else pure (ctx, Right falseXObj)
        Nothing -> pure (evalError ctx ("Can't take type of " ++ pretty xobj) i)
primitiveIsManaged _ _ _ = error "primitiveismanaged"

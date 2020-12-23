-- | This module contains interface registration functions.
-- Interface registration involves associating some concrete form, e.g. a defn with an interface.
-- Registered forms may be used wherever the interface is called.
-- Registrations are stored w/ the interface in the context type environment.
module Interfaces
  ( registerInInterfaceIfNeeded,
    registerInInterface,
    retroactivelyRegisterInInterface,
    interfaceImplementedForTy,
    removeInterfaceFromImplements,
    InterfaceError (..),
  )
where

import ColorText
import Constraints
import Data.List (delete, deleteBy)
import Data.Maybe (fromMaybe, mapMaybe)
import Env
import Lookup
import qualified Meta
import Obj
import Types
import Util

data InterfaceError
  = KindMismatch SymPath Ty Ty
  | TypeMismatch SymPath Ty Ty
  | NonInterface SymPath
  | AlreadyImplemented SymPath SymPath SymPath Ty

instance Show InterfaceError where
  show (KindMismatch path definitionSignature interfaceSignature) =
    labelStr
      "INTERFACE ERROR"
      ( show path ++ ":" ++ " One or more types in the interface implementation "
          ++ show definitionSignature
          ++ " have kinds that do not match the kinds of the types in the interface signature "
          ++ show interfaceSignature
          ++ "\n"
          ++ "Types of the form (f a) must be matched by constructor types such as (Maybe a)"
      )
  show (TypeMismatch path definitionSignature interfaceSignature) =
    labelStr
      "INTERFACE ERROR"
      ( show path ++ " : " ++ show definitionSignature
          ++ " doesn't match the interface signature "
          ++ show interfaceSignature
      )
  show (NonInterface path) =
    labelStr
      "INTERFACE ERROR"
      (show path ++ "Cant' implement the non-interface `" ++ show path ++ "`")
  show (AlreadyImplemented interfacePath implementationPath replacementPath ty) =
    "An implementation of the interface " ++ show interfacePath
      ++ " with type "
      ++ show ty
      ++ " already exists: "
      ++ show implementationPath
      ++ ". "
      ++ "It will be replaced by the implementation: "
      ++ show replacementPath
      ++ "."
      ++ "\n"
      ++ "This may break a bunch of upstream code!"

-- | Get the first path of an interface implementation that matches a given type signature
getFirstMatchingImplementation :: Context -> [SymPath] -> Ty -> Maybe SymPath
getFirstMatchingImplementation ctx paths ty =
  case filter predicate (mapMaybe (`lookupBinder` global) paths) of
    [] -> Nothing
    (x : _) -> Just ((getPath . binderXObj) x)
  where
    predicate = (== Just ty) . (xobjTy . binderXObj)
    global = contextGlobalEnv ctx

-- | Remove an interface from a binder's list of implemented interfaces
removeInterfaceFromImplements :: SymPath -> XObj -> Context -> Context
removeInterfaceFromImplements oldImplPath interface ctx =
  fromMaybe
    ctx
    ( lookupBinder oldImplPath (contextGlobalEnv ctx)
        >>= \binder ->
          Meta.getBinderMetaValue "implements" binder
            >>= ( \x ->
                    case x of
                      (XObj (Lst impls) i t) ->
                        pure $ Meta.updateBinderMeta binder "implements" (XObj (Lst (deleteBy matchPath interface impls)) i t)
                      _ -> Nothing
                )
            >>= (\b -> pure $ ctx {contextGlobalEnv = envInsertAt (contextGlobalEnv ctx) oldImplPath b})
    )
  where
    matchPath xobj xobj' = getPath xobj == getPath xobj'

-- TODO: This is currently called once outside of this module--try to remove that call and make this internal.
-- Checks whether a given form's type matches an interface, and if so, registers the form with the interface.
registerInInterfaceIfNeeded :: Context -> Binder -> Binder -> Ty -> (Context, Maybe InterfaceError)
registerInInterfaceIfNeeded ctx implementation interface definitionSignature =
  case interface of
    Binder _ (XObj (Lst [inter@(XObj (Interface interfaceSignature paths) ii it), isym]) i t) ->
      if checkKinds interfaceSignature definitionSignature
        then case solve [Constraint interfaceSignature definitionSignature inter inter inter OrdInterfaceImpl] of
          Left _ -> (ctx, Just (TypeMismatch implPath definitionSignature interfaceSignature))
          Right _ -> case getFirstMatchingImplementation ctx paths definitionSignature of
            Nothing -> (updatedCtx, Nothing)
            Just x ->
              if x == implPath
                then (updatedCtx, Nothing)
                else (implReplacedCtx x, Just (AlreadyImplemented ipath x implPath definitionSignature))
        else (ctx, Just (KindMismatch implPath definitionSignature interfaceSignature))
      where
        updatedInterface = XObj (Lst [XObj (Interface interfaceSignature (addIfNotPresent implPath paths)) ii it, isym]) i t
        updatedCtx = ctx {contextTypeEnv = TypeEnv (extendEnv typeEnv name updatedInterface)}
        implReplacedInterface x = XObj (Lst [XObj (Interface interfaceSignature (addIfNotPresent implPath (delete x paths))) ii it, isym]) i t
        implReplacedCtx x = ctx {contextTypeEnv = TypeEnv (extendEnv typeEnv name (implReplacedInterface x))}
    _ ->
      (ctx, Just (NonInterface (getBinderPath interface)))
  where
    implPath = getBinderPath implementation
    typeEnv = getTypeEnv (contextTypeEnv ctx)
    ipath@(SymPath _ name) = getBinderPath interface

-- | Given a binder and an interface path, ensure that the form is
-- registered with the interface.
registerInInterface :: Context -> Binder -> Binder -> (Context, Maybe InterfaceError)
registerInInterface ctx implementation interface =
  case binderXObj implementation of
    XObj (Lst [XObj (Defn _) _ _, _, _, _]) _ (Just t) ->
      -- This is a function, does it belong to an interface?
      registerInInterfaceIfNeeded ctx implementation interface t
    XObj (Lst [XObj (Deftemplate _) _ _, _]) _ (Just t) ->
      -- Templates should also be registered.
      registerInInterfaceIfNeeded ctx implementation interface t
    XObj (Lst [XObj Def _ _, _, _]) _ (Just t) ->
      -- Global variables can also be part of an interface
      registerInInterfaceIfNeeded ctx implementation interface t
    -- So can externals!
    XObj (Lst [XObj (External _) _ _, _, _]) _ (Just t) ->
      registerInInterfaceIfNeeded ctx implementation interface t
    -- And instantiated/auto-derived type functions! (e.g. Pair.a)
    XObj (Lst [XObj (Instantiate _) _ _, _]) _ (Just t) ->
      registerInInterfaceIfNeeded ctx implementation interface t
    _ -> (ctx, Nothing)

-- | For forms that were declared as implementations of interfaces that didn't exist,
-- retroactively register those forms with the interface once its defined.
retroactivelyRegisterInInterface :: Context -> Binder -> Context
retroactivelyRegisterInInterface ctx interface =
  -- TODO: Propagate error
  maybe resultCtx (error . show) err
  where
    env = contextGlobalEnv ctx
    impls = lookupMany Everywhere lookupImplementations (getPath (binderXObj interface)) env
    (resultCtx, err) = foldl (\(context, _) binder -> registerInInterface context binder interface) (ctx, Nothing) impls

-- | Checks whether an interface is implemented for a certain type signature,
-- | e.g. Is "delete" implemented for `(Fn [String] ())` ?
interfaceImplementedForTy :: TypeEnv -> Env -> String -> Ty -> Bool
interfaceImplementedForTy (TypeEnv typeEnv) globalEnv interfaceName matchingTy =
  case lookupBinder (SymPath [] interfaceName) typeEnv of
    Just (Binder _ (XObj (Lst (XObj (Interface _ paths) _ _ : _)) _ _)) ->
      let lookupType path = forceTy . binderXObj <$> lookupBinder path globalEnv
          matches = filter (areUnifiable matchingTy) (mapMaybe lookupType paths)
       in not . null $ matches
    _ -> False

-- | This module contains interface registration functions.
-- Interface registration involves associating some concrete form, e.g. a defn with an interface.
-- Registered forms may be used wherever the interface is called.
-- Registrations are stored w/ the interface in the context type environment.
module Interfaces
  ( registerInInterfaceIfNeeded,
    registerInInterface,
    retroactivelyRegisterInInterface,
  )
where

import ColorText
import Constraints
import Control.Monad (foldM)
import Lookup
import Obj
import Types
import Util

data InterfaceError
  = KindMismatch SymPath Ty Ty
  | TypeMismatch SymPath Ty Ty
  | NonInterface SymPath

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

-- TODO: This is currently called once outside of this module--try to remove that call and make this internal.
-- Checks whether a given form's type matches an interface, and if so, registers the form with the interface.
registerInInterfaceIfNeeded :: Context -> Binder -> Binder -> Ty -> Either String Context
registerInInterfaceIfNeeded ctx implementation interface definitionSignature =
  case interface of
    Binder _ (XObj (Lst [inter@(XObj (Interface interfaceSignature paths) ii it), isym]) i t) ->
      if checkKinds interfaceSignature definitionSignature
        then case solve [Constraint interfaceSignature definitionSignature inter inter inter OrdInterfaceImpl] of
          Left _ -> Left (show $ TypeMismatch implPath definitionSignature interfaceSignature)
          Right _ -> Right (ctx {contextTypeEnv = TypeEnv (extendEnv typeEnv name updatedInterface)})
        else Left (show $ KindMismatch implPath definitionSignature interfaceSignature)
      where
        updatedInterface = XObj (Lst [XObj (Interface interfaceSignature (addIfNotPresent implPath paths)) ii it, isym]) i t
    _ ->
      Left (show $ NonInterface (getBinderPath interface))
  where
    implPath = (getBinderPath implementation)
    typeEnv = getTypeEnv (contextTypeEnv ctx)
    (SymPath _ name) = getBinderPath interface

-- | Given a binder and an interface path, ensure that the form is
-- registered with the interface.
registerInInterface :: Context -> Binder -> Binder -> Either String Context
registerInInterface ctx implementation interface =
  case (binderXObj implementation) of
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
    _ -> pure ctx

-- | For forms that were declared as implementations of interfaces that didn't exist,
-- retroactively register those forms with the interface once its defined.
retroactivelyRegisterInInterface :: Context -> Binder -> Context
retroactivelyRegisterInInterface ctx interface =
  -- TODO: Don't use error here?
  either (\e -> error e) id resultCtx
  where
    env = contextGlobalEnv ctx
    impls = recursiveLookupAll (getPath (binderXObj interface)) lookupImplementations env
    resultCtx = foldM (\context binder -> registerInInterface context binder interface) ctx impls

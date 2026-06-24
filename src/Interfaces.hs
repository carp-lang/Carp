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
    verifyProtocolImplementation,
    checkOrphanRule,
    InterfaceError (..),
  )
where

import ColorText
import Constraints
import Context
import Data.Either (fromRight, isRight, rights)
import Data.List (delete, deleteBy, foldl')
import qualified Env
import qualified Map
import qualified Meta
import Obj
import qualified Qualify
import Types
import Util

data InterfaceError
  = KindMismatch SymPath Ty Ty
  | TypeMismatch SymPath Ty Ty
  | NonInterface SymPath
  | AlreadyImplemented SymPath SymPath SymPath Ty
  | AmbiguousInterfaceMatch SymPath Ty [Ty]

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
      (show path ++ ": Can't implement the non-interface `" ++ show path ++ "`")
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
      ++ "This may result in unexpected behavior."
  show (AmbiguousInterfaceMatch path definitionSignature matches) =
    labelStr
      "INTERFACE ERROR"
      ( show path ++ " : " ++ show definitionSignature
          ++ " matches multiple signatures for the same interface: "
          ++ joinWithComma (map show matches)
      )

-- | Get the first path of an interface implementation that matches a given type signature
getFirstMatchingImplementation :: Context -> [SymPath] -> Ty -> Maybe SymPath
getFirstMatchingImplementation ctx paths ty =
  case filter predicate (rights (map (global `Env.searchBinder`) paths)) of
    [] -> Nothing
    (x : _) -> Just ((getPath . binderXObj) x)
  where
    predicate = (== Just ty) . (xobjTy . binderXObj)
    global = contextGlobalEnv ctx

-- | Get the first path of an interface implementation that satisfies a given type signature
getFirstSatisfyingImplementation :: Context -> [SymPath] -> Ty -> Maybe SymPath
getFirstSatisfyingImplementation ctx paths ty =
  case filter predicate (rights (map (global `Env.searchBinder`) paths)) of
    [] -> Nothing
    (x : _) -> Just ((getPath . binderXObj) x)
  where
    global = contextGlobalEnv ctx
    dummy = XObj (Lst []) Nothing Nothing
    predicate binder =
      case xobjTy (binderXObj binder) of
        Just implTy -> isRight (solve [Constraint ty implTy dummy dummy dummy OrdInterfaceImpl])
        Nothing -> False


-- | Remove an interface from a binder's list of implemented interfaces
removeInterfaceFromImplements :: SymPath -> XObj -> Context -> Context
removeInterfaceFromImplements oldImplPath interface ctx =
  fromRight
    ctx
    ( lookupBinderInGlobalEnv ctx (Qualify.markQualified oldImplPath)
        >>= \binder ->
          pure
            ( case Meta.getBinderMetaValue "implements" binder of
                Just (XObj (Lst impls) i t) -> Meta.updateBinderMeta binder "implements" (XObj (Lst (deleteBy matchPath interface impls)) i t)
                _ -> binder
            )
            >>= insertInGlobalEnv ctx (Qualify.markQualified oldImplPath)
    )
  where
    matchPath xobj xobj' = getPath xobj == getPath xobj'

-- TODO: This is currently called once outside of this module--try to remove that call and make this internal.
-- Checks whether a given form's type matches an interface, and if so, registers the form with the interface.
registerInInterfaceIfNeeded :: Context -> Binder -> Binder -> Ty -> (Either ContextError Context, Maybe InterfaceError)
registerInInterfaceIfNeeded ctx implementation interface definitionSignature =
  case interface of
    Binder _ (XObj (Lst [inter@(XObj (Interface interfaceSignatures@(firstSig : _) paths) ii it), isym]) i t) ->
      let matches = filter (\sig -> checkKinds sig definitionSignature && isRight (solve [Constraint sig definitionSignature inter inter inter OrdInterfaceImpl])) interfaceSignatures
          qpath = Qualify.markQualified (SymPath [] name)
          updatedInterface = XObj (Lst [XObj (Interface interfaceSignatures (addIfNotPresent implPath paths)) ii it, isym]) i t
          updatedCtx = replaceTypeBinder ctx qpath (toBinder updatedInterface)
          implReplacedInterface x = XObj (Lst [XObj (Interface interfaceSignatures (addIfNotPresent implPath (delete x paths))) ii it, isym]) i t
          implReplacedCtx x = replaceTypeBinder ctx qpath (toBinder (implReplacedInterface x))
       in case matches of
            [] -> (Right ctx, Just (TypeMismatch implPath definitionSignature firstSig))
            [_matchingSig] ->
              case getFirstMatchingImplementation ctx paths definitionSignature of
                Nothing -> (updatedCtx, Nothing)
                Just x ->
                  if x == implPath
                    then (updatedCtx, Nothing)
                    else (implReplacedCtx x, Just (AlreadyImplemented ipath x implPath definitionSignature))
            _ -> (Right ctx, Just (AmbiguousInterfaceMatch implPath definitionSignature matches))
    _ ->
      (Right ctx, Just (NonInterface (getBinderPath interface)))
  where
    implPath = getBinderPath implementation
    ipath@(SymPath _ name) = getBinderPath interface

-- | Given a binder and an interface path, ensure that the form is
-- registered with the interface.
registerInInterface :: Context -> Binder -> Binder -> (Either ContextError Context, Maybe InterfaceError)
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
    -- Protocol support: if implementation is a type and interface is a protocol
    XObj (Lst (XObj (Deftype t) _ _ : _)) _ _ ->
      case binderXObj interface of
        XObj (Lst [XObj (Protocol _ _) _ _, _]) _ _ ->
          case verifyProtocolImplementation ctx t (binderXObj interface) of
            Right (ctx', memberImplPairs) ->
              let registerWithInterface context (mPath, implPath) =
                    case Env.getBinder (contextGlobalEnv context) (show implPath) of
                      Right implBinder ->
                        case Env.getBinder (contextTypeEnv context) (show mPath) of
                          Right interBinder ->
                            let (nc, _) = registerInInterface context implBinder interBinder
                             in fromRight context nc
                          _ -> context
                      _ -> context
                  finalCtx = foldl' registerWithInterface ctx' memberImplPairs
               in (Right finalCtx, Nothing)
            Left (err : _) -> (Right ctx, Just err)
            Left [] -> (Right ctx, Nothing)
        _ -> (Right ctx, Nothing)
    _ -> (Right ctx, Nothing)

-- | Verify that a type fully implements all members of a protocol.
verifyProtocolImplementation :: Context -> Ty -> XObj -> Either [InterfaceError] (Context, [(SymPath, SymPath)])
verifyProtocolImplementation ctx ty protocol@(XObj (Lst [XObj (Protocol memberPaths _) _info (Just protocolTy), _sym]) _ _) =
  let typeEnv = contextTypeEnv ctx
      -- Unify the target type 'ty' with the protocol's type variables.
      -- This finds the mapping between the protocol's generic parameters and 'ty'.
      dummy = XObj (Lst []) Nothing Nothing
      mappings = case solve [Constraint protocolTy (ProtocolTy (getPath protocol) [ty]) dummy dummy dummy OrdInterfaceImpl] of
        Right m -> m
        Left _ -> Map.empty
      checkMember (errors, paths) mPath@(SymPath _ mName) =
        case Env.getBinder typeEnv mName of
          Right (Binder _ (XObj (Lst (XObj (Interface (sig : _) iPaths) _ _ : _)) _ _)) ->
            -- Specialize the member signature using the mappings derived from the protocol.
            let specializedSig = replaceTyVars mappings sig
             in case getFirstSatisfyingImplementation ctx iPaths specializedSig of
                  Just implPath -> (errors, (mPath, implPath) : paths)
                  Nothing -> ((NonInterface (SymPath [] mName)) : errors, paths)
          _ -> ((NonInterface (SymPath [] mName)) : errors, paths)
      (allErrors, foundPaths) = foldl' checkMember ([], []) memberPaths
   in if null allErrors
        then Right (ctx, foundPaths)
        else Left allErrors
verifyProtocolImplementation ctx ty protocol@(XObj (Lst [XObj (Protocol memberPaths _) info Nothing, sym]) i t) =
  -- Fallback for older protocols without type information, assume 'a'
  let protocolTy = ProtocolTy (getPath protocol) [VarTy "a"]
      updatedProtocol = XObj (Lst [XObj (Protocol memberPaths []) info (Just protocolTy), sym]) i t
   in verifyProtocolImplementation ctx ty updatedProtocol
verifyProtocolImplementation _ _ protocol = Left [NonInterface (getPath protocol)]

-- | Enforce the orphan instance rule for protocols.
-- TODO: Protocols should only be implemented in the same module as the type or the protocol.
checkOrphanRule :: Context -> SymPath -> Ty -> Either InterfaceError ()
checkOrphanRule _ctx _protocol _ty = Right ()

-- | For forms that were declared as implementations of interfaces that didn't exist,
-- retroactively register those forms with the interface once its defined.
retroactivelyRegisterInInterface :: Context -> Binder -> Either ContextError Context
retroactivelyRegisterInInterface ctx interface =
  -- TODO: Propagate error
  maybe resultCtx (error . show) err
  where
    env = contextGlobalEnv ctx
    tenv = contextTypeEnv ctx
    searchEnv e = concat (rights (fmap ((flip Env.findImplementations) (getPath (binderXObj interface))) (e : (Env.lookupChildren e))))
    impls = searchEnv env ++ searchEnv tenv
    (resultCtx, err) = foldl' go (Right ctx, Nothing) impls
    go (Right context, _) binder = registerInInterface context binder interface
    go e _ = e

-- | Checks whether an interface is implemented for a certain type signature,
-- | e.g. Is "delete" implemented for `(Fn [String] ())` ?
interfaceImplementedForTy :: TypeEnv -> Env -> String -> Ty -> Bool
interfaceImplementedForTy typeEnv globalEnv interfaceName matchingTy =
  let lookupType' path = forceTy . binderXObj <$> (Env.searchBinder globalEnv path)
   in case Env.getBinder typeEnv interfaceName of
    Right (Binder _ (XObj (Lst (XObj (Interface _ paths) _ _ : _)) _ _)) ->
      let matches = filter (areUnifiable matchingTy) (rights (map lookupType' paths))
       in not . null $ matches
    Right (Binder _ (XObj (Lst (XObj (Protocol _ instances) _ _ : _)) _ _)) ->
      let matches = filter (areUnifiable matchingTy) instances
       in not . null $ matches
    _ -> False

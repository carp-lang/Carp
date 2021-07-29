-- | Defines functions for manipulating protocols.
--
-- Protocols are bundles of interfaces that can be used for forming type
-- hierarchies.
module Protocol
  (registerInstance)
where

import Data.Either (rights)
import Debug.Trace

import Context
import Interfaces
import Obj
import Qualify
import Types
import Util
import Env
import Forms

--------------------------------------------------------------------------------
-- Data

-- | The type of protocol errors.
data ProtocolError =
  NotAType SymPath
  | NotAProtocol SymPath
  | NotImplemented Ty [SymPath]

instance Show ProtocolError where
  show (NotAType path) =
    show path ++ "is not a type. Only types may be instances of protocols."
  show (NotAProtocol path) =
    show path ++ "is not a protocol."
  show (NotImplemented ty paths) =
    "The type " ++ show ty ++ " does not implement the following interfaces: " ++ joinWithComma (map show paths) ++ " which are required by the protocol."

--------------------------------------------------------------------------------
-- Protocol management functions

-- | Add a type as a new instance of a protocol, returning an updated context.
--
-- Types will be rejected if there are no implementations of the protocol's
-- interfaces that include the type.
registerInstance :: Context -> SymPath -> SymPath -> Either ProtocolError Context
registerInstance ctx protocol inst =
  let qprotocol = markQualified protocol
   in getProtocol ctx qprotocol
      >>= \proto@(ProtocolPat _ interfaces _) -> getTypeFromPath inst
      >>= \ty -> checkImplementations ctx interfaces ty
      >> (updateProtocol proto inst ty)
      >>= \newProto -> replaceLeft (NotAProtocol protocol) (replaceTypeBinder ctx qprotocol newProto)

  where updateProtocol :: XObj -> SymPath -> Ty -> Either ProtocolError Binder
        updateProtocol p@(ProtocolPat name interfaces instances) i iTy =
          let  info   = xobjInfo p
               newTy  = fmap (addInstance iTy) (xobjTy p)
               newX   = XObj (Lst [XObj (Protocol interfaces (addIfNotPresent i instances)) info newTy, name]) info newTy
           in pure $ toBinder newX
        updateProtocol x _ _ = Left (NotAProtocol (getPath x))
        addInstance :: Ty -> Ty -> Ty
        addInstance i (ProtocolTy is) = (ProtocolTy (addIfNotPresent i is))
        addInstance _ t = t      

--------------------------------------------------------------------------------
-- Private utilities

-- | Given a context and path, try to retrieve an associated protocol.
getProtocol :: Context -> QualifiedPath -> Either ProtocolError XObj 
getProtocol ctx protocol = 
  case lookupBinderInTypeEnv ctx protocol of 
    Right (Binder _ x@(ProtocolPat _ _ _)) -> pure x
    _ -> Left $ NotAProtocol (unqualify protocol)   

-- | Just a wrapper around xobjToTy.
getTypeFromPath :: SymPath -> Either ProtocolError Ty
getTypeFromPath typath =
  let x = XObj (Sym typath Symbol) Nothing Nothing
   in maybe (Left (NotAType typath)) Right (xobjToTy x)
  
-- | Given a list of interfaces and a type, verify that the type appears in at
-- least one implementation of each interface.
checkImplementations :: Context -> [SymPath] -> Ty -> Either ProtocolError ()
checkImplementations ctx interfaces t =
  let actual  = traceShowId $ map binderXObj (rights (map (lookupBinderInTypeEnv ctx . markQualified) interfaces))
      impls   = traceShowId $ map ((map (typeFromPath (contextGlobalEnv ctx))) . getImplementations) actual
      matches = map (any ((flip isSubType) t)) impls
   in if (all (==True) matches)
        then pure ()
        else Left (NotImplemented t interfaces) 

-- | Get the type of a symbol at a given path.
--
-- TODO: Duplicated from Concretize to prevent inclusion loops. Fix.
typeFromPath :: Env -> SymPath -> Ty
typeFromPath env p =
  case searchValue env p of
    Right (e, Binder _ found)
      | envIsExternal e -> forceTy found
      | otherwise -> error "Local bindings shouldn't be ambiguous."
    _ -> error ("Couldn't find " ++ show p ++ " in env:\n" ++ prettyEnvironmentChain env)

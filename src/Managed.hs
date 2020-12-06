module Managed where

import Lookup
import Obj
import Types

-- | Find out if a type is "external", meaning it is not defined by the user
--   in this program but instead imported from another C library or similar.
-- NOTE: Quite possibly this function should be removed and we should rely on 'isManaged' instead?
isExternalType :: TypeEnv -> Ty -> Bool
isExternalType typeEnv (PointerTy p) =
  isExternalType typeEnv p
isExternalType typeEnv (StructTy (ConcreteNameTy name) _) =
  case lookupBinder (SymPath [] name) (getTypeEnv typeEnv) of
    Just (Binder _ (XObj (Lst (XObj (ExternalType _) _ _ : _)) _ _)) -> True
    Just _ -> False
    Nothing -> False
isExternalType _ _ =
  False

-- | Is this type managed - does it need to be freed?
isManaged :: TypeEnv -> Ty -> Bool
isManaged typeEnv (StructTy (ConcreteNameTy name) _) =
  (name == "Array") || (name == "StaticArray") || (name == "Dictionary")
    || ( case lookupBinder (SymPath [] name) (getTypeEnv typeEnv) of
           Just (Binder _ (XObj (Lst (XObj (ExternalType _) _ _ : _)) _ _)) -> False
           Just (Binder _ (XObj (Lst (XObj (Deftype _) _ _ : _)) _ _)) -> True
           Just (Binder _ (XObj (Lst (XObj (DefSumtype _) _ _ : _)) _ _)) -> True
           Just (Binder _ (XObj wrong _ _)) -> error ("Invalid XObj in type env: " ++ show wrong)
           Nothing -> error ("Can't find " ++ name ++ " in type env.") -- TODO: Please don't crash here!
       )
isManaged _ StringTy = True
isManaged _ PatternTy = True
isManaged _ FuncTy {} = True
isManaged _ _ = False

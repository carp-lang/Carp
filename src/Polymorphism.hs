module Polymorphism
  ( nameOfPolymorphicFunction,
  )
where

import Env as E
import Obj
import Types

-- | Calculate the full, mangled name of a concretized polymorphic function.
-- For example, The 'id' in "(id 3)" will become 'id__int'.
--
-- This function uses findPoly, which gives it access to *all* possible
-- environments in the given input environment (children, (modules) parents,
-- and use modules). This allows it to derive the correct name for functions
-- that may be defined in a different environment.
--
-- TODO: Environments are passed in different order here!!!
nameOfPolymorphicFunction :: TypeEnv -> Env -> Ty -> String -> Maybe SymPath
nameOfPolymorphicFunction _ env functionType functionName =
  let foundBinder =
        (E.findPoly env functionName functionType)
          <> (E.findPoly (progenitor env) functionName functionType)
   in case foundBinder of
        Right (_, (Binder _ (XObj (Lst (XObj (External (Just name)) _ _ : _)) _ _))) ->
          Just (SymPath [] name)
        Right (_, (Binder _ single)) ->
          let Just t' = xobjTy single
              (SymPath pathStrings name) = getPath single
              suffix = polymorphicSuffix t' functionType
              concretizedPath = SymPath pathStrings (name ++ suffix)
           in Just concretizedPath
        _ -> Nothing

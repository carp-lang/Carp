module Polymorphism where

import Lookup
import Obj
import Types

-- | Calculate the full, mangled name of a concretized polymorphic function.
-- | For example, The 'id' in "(id 3)" will become 'id__int'.
-- | This function uses the 'multiLookupALL' function which gives it acces to
-- | modules that are not imported. This allows it to access 'delete' functions
-- | and similar for internal use.

-- | TODO: Environments are passed in different order here!!!
nameOfPolymorphicFunction :: TypeEnv -> Env -> Ty -> String -> Maybe SymPath
nameOfPolymorphicFunction _ env functionType functionName =
  let foundBinders = multiLookupEverywhere functionName env
   in case filter ((\(Just t') -> areUnifiable functionType t') . xobjTy . binderXObj . snd) foundBinders of
        [] -> Nothing
        [(_, Binder _ (XObj (Lst (XObj (External (Just name)) _ _ : _)) _ _))] ->
          Just (SymPath [] name)
        [(_, Binder _ single)] ->
          let Just t' = xobjTy single
              (SymPath pathStrings name) = getPath single
              suffix = polymorphicSuffix t' functionType
              concretizedPath = SymPath pathStrings (name ++ suffix)
           in Just concretizedPath
        _ -> Nothing

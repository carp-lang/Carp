module Polymorphism where

import Debug.Trace
import Lookup
import Obj
import TypeError
import Types
import Util

-- | Calculate the full, mangled name of a concretized polymorphic function.
-- | For example, The 'id' in "(id 3)" will become 'id__int'.
-- | This function uses the 'multiLookupALL' function which gives it acces to
-- | modules that are not imported. This allows it to access 'delete' functions
-- | and similar for internal use.

-- | TODO: Environments are passed in different order here!!!
nameOfPolymorphicFunction :: TypeEnv -> Env -> Ty -> String -> Maybe SymPath
nameOfPolymorphicFunction typeEnv env functionType functionName =
  let foundBinders = multiLookupALL functionName env
   in case filter ((\(Just t') -> areUnifiable functionType t') . ty . binderXObj . snd) foundBinders of
        [] -> Nothing
        [(_, Binder _ single)] ->
          let Just t' = ty single
              (SymPath pathStrings name) = getPath single
              suffix = polymorphicSuffix t' functionType
              concretizedPath = SymPath pathStrings (name ++ suffix)
           in Just concretizedPath
        _ -> Nothing

module Polymorphism where

import Obj
import Types
import Util
import TypeError

nameOfPolymorphicFunction :: Env -> Ty -> String -> Maybe SymPath
nameOfPolymorphicFunction env t lookupName
  | isManaged t =
    case filter ((\(Just t') -> areUnifiable (FuncTy [t] UnitTy) t') . ty . binderXObj . snd) (multiLookupALL lookupName env) of
      [] -> Nothing
      [(_, Binder single)] ->
        let Just t' = ty single
            (SymPath pathStrings name) = getPath single
            suffix = polymorphicSuffix t' (FuncTy [t] UnitTy)
            concretizedPath = SymPath pathStrings (name ++ suffix)
        in  Just concretizedPath
      _ -> Nothing
  | otherwise   = Nothing

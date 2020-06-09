module Meta (get,
             set,
             fromBinder,
             getBinderMetaValue,
             updateBinderMeta,
            ) where

import Data.Map as Map
import Obj

get :: String -> MetaData -> Maybe XObj
get key meta = Map.lookup key $ getMeta meta

set :: String -> XObj -> MetaData -> MetaData
set key value meta = MetaData $ Map.insert key value $ getMeta meta

fromBinder :: Binder -> MetaData
fromBinder binder = binderMeta binder

getBinderMetaValue :: String -> Binder -> Maybe XObj
getBinderMetaValue key binder = 
  get key $ fromBinder binder

updateBinderMeta :: Binder -> String -> XObj -> Binder
updateBinderMeta binder key value = 
  binder { binderMeta = set key value $ fromBinder binder }



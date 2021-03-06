module Meta
  ( stub,
    get,
    set,
    fromBinder,
    getBinderMetaValue,
    updateBinderMeta,
    Meta.member,
    binderMember,
    hide,
  )
where

import Info
import qualified Map
import Obj
import SymPath
import Types

-- | A temporary binder for meta calls on symbols that haven't been declared yet.
-- Used in situations such as:
--   (doc foo "A foo.") <- foo hasn't been declared yet.
--   (def foo 0)
stub :: SymPath -> Binder
stub path =
  Binder
    emptyMeta
    ( XObj
        ( Lst
            [ XObj MetaStub Nothing Nothing,
              XObj (Sym path Symbol) Nothing Nothing
            ]
        )
        (Just dummyInfo)
        (Just (VarTy "a"))
    )

get :: String -> MetaData -> Maybe XObj
get key meta = Map.lookup key $ getMeta meta

set :: String -> XObj -> MetaData -> MetaData
set key value meta = MetaData $ Map.insert key value $ getMeta meta

fromBinder :: Binder -> MetaData
fromBinder = binderMeta

getBinderMetaValue :: String -> Binder -> Maybe XObj
getBinderMetaValue key binder =
  get key $ fromBinder binder

updateBinderMeta :: Binder -> String -> XObj -> Binder
updateBinderMeta binder key value =
  binder {binderMeta = set key value $ fromBinder binder}

member :: String -> MetaData -> Bool
member key meta = Map.member key $ getMeta meta

binderMember :: String -> Binder -> Bool
binderMember key binder = Meta.member key $ fromBinder binder

hide :: Binder -> Binder
hide binder =
  updateBinderMeta binder "hidden" trueXObj

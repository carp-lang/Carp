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
    getString,
    getCompilerKey,
    validateAndSet,
    CompilerKey (..),
  )
where

import Data.Either (fromRight)
import Data.Maybe (fromMaybe)
import Info
import qualified Map
import Obj
import SymPath
import Types

--------------------------------------------------------------------------------
-- builtin special meta key values
-- These keys, when set, alter the compiler's behavior.

data CompilerKey = CNAME

-- Given a compiler key, returns the key name as a string along with a default value.
toKeyValue :: CompilerKey -> (String, XObj)
toKeyValue CNAME = ("c-name", (XObj (Str "") Nothing Nothing))

-- | Get the key associated with a compiler Meta key as a string.
getCompilerKey :: CompilerKey -> String
getCompilerKey = fst . toKeyValue

-- | Special meta KV pairs expect values of a certain XObj form.
--
-- Returns True for valid values for the given compiler key, False otherwise.
validateCompilerKeyValue :: CompilerKey -> Obj -> Bool
validateCompilerKeyValue CNAME (Str _) = True
validateCompilerKeyValue CNAME _ = False

-- | Validate and set a compiler key for a given MetaData object.
--
-- If the key or value is invalid, returns Left containing the original metadata.
-- If the key and value is valid, return Right containing the updated metadata.
validateAndSet :: MetaData -> CompilerKey -> XObj -> Either MetaData MetaData
validateAndSet meta key val
  | validateCompilerKeyValue key (xobjObj val) =
    Right (set (getCompilerKey key) val meta)
  | otherwise = Left meta

--------------------------------------------------------------------------------

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

-- | Get the value of a string valued meta key.
getString :: String -> MetaData -> String
getString key meta = fromMaybe "" $ fmap (fromRight "" . unwrapStringXObj) (Meta.get key meta)

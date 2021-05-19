{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Map where

import Data.Hashable
import qualified Data.Map as M

newtype Map k v = Map {unMap :: M.Map k v} deriving (Show, Eq, Foldable, Functor, Traversable)

instance (Hashable k, Hashable v) => Hashable (Map k v) where
  hashWithSalt s = hashWithSalt s . Map.toList

toList :: Map k v -> [(k, v)]
toList (Map m) = M.toList m

fromList :: Ord k => [(k, v)] -> Map k v
fromList = Map . M.fromList

lookup :: Ord k => k -> Map k v -> Maybe v
lookup !k (Map !m) = M.lookup k m

member :: Ord k => k -> Map k v -> Bool
member k (Map m) = M.member k m

insert :: Ord k => k -> v -> Map k v -> Map k v
insert k v (Map m) = Map $ M.insert k v m

empty :: Map k v
empty = Map M.empty

filterWithKey :: Ord k => (k -> v -> Bool) -> Map k v -> Map k v
filterWithKey f (Map m) = Map $ M.filterWithKey f m

filter :: Ord k => (v -> Bool) -> Map k v -> Map k v
filter f (Map m) = Map $ M.filter f m

keys :: Map k v -> [k]
keys (Map m) = M.keys m

map :: (a -> b) -> Map k a -> Map k b
map f (Map m) = Map $ M.map f m

union :: Ord k => Map k v -> Map k v -> Map k v
union (Map m) (Map m') = (Map (M.union m m'))

assocs :: Map k a -> [(k, a)]
assocs (Map m) = M.assocs m

elems :: Map k a -> [a]
elems (Map m) = M.elems m

adjust :: Ord k => (a -> a) -> k -> Map k a -> Map k a
adjust f k (Map m) = (Map (M.adjust f k m))

delete :: Ord k => k -> Map k a -> Map k a
delete k (Map m) = (Map (M.delete k m))

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Set where

import Data.Hashable
import qualified Data.Set as S

newtype Set v = Set {unSet :: S.Set v} deriving (Show, Eq, Foldable, Ord)

instance Hashable v => Hashable (Set v) where
  hashWithSalt s = hashWithSalt s . Set.toList

toList :: Set v -> [v]
toList (Set s) = S.toList s

fromList :: Ord v => [v] -> Set v
fromList = Set . S.fromList

empty :: Set v
empty = Set S.empty

intersection :: Ord v => Set v -> Set v -> Set v
intersection (Set a) (Set b) = Set (S.intersection a b)

union :: Ord v => Set v -> Set v -> Set v
union (Set a) (Set b) = Set (S.union a b)

member :: Ord v => v -> Set v -> Bool
member k (Set s) = S.member k s

notMember :: Ord v => v -> Set v -> Bool
notMember k (Set s) = S.notMember k s

insert :: Ord v => v -> Set v -> Set v
insert v (Set s) = Set $ S.insert v s

(\\) :: Ord v => Set v -> Set v -> Set v
(\\) (Set a) (Set b) = Set ((S.\\) a b)

delete :: Ord v => v -> Set v -> Set v
delete v (Set s) = Set $ S.delete v s

filter :: Ord v => (v -> Bool) -> Set v -> Set v
filter f (Set s) = Set $ S.filter f s

size :: Set v -> Int
size (Set s) = S.size s

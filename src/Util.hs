module Util where

import Data.List
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

joinWith :: String -> [String] -> String
joinWith = intercalate

joinWithSpace :: [String] -> String
joinWithSpace = unwords

joinWithComma :: [String] -> String
joinWithComma = intercalate", "

joinWithUnderscore :: [String] -> String
joinWithUnderscore = intercalate"_"

joinWithPeriod :: [String] -> String
joinWithPeriod = intercalate"."

pairwise :: Show a => [a] -> [(a, a)]
pairwise [] = []
pairwise (x : y : xs) = (x, y) : pairwise xs
pairwise leftover = error ("An uneven number of forms sent to pairwise: " ++ show leftover)

compilerError :: String -> a
compilerError msg = error ("Internal compiler error: " ++ msg)

-- | Unwraps a Maybe value a to Right a, or returns a default value (Left b) if it was Nothing.
toEither :: Maybe a -> b -> Either b a
toEither a b = case a of
                 Just ok -> Right ok
                 Nothing -> Left b

replaceChars :: Map.Map Char String -> String -> String
replaceChars dict = concatMap replacer
  where replacer c = fromMaybe [c] (Map.lookup c dict)

addIfNotPresent :: Eq a => a -> [a] -> [a]
addIfNotPresent x xs =
  if x `elem` xs
  then xs
  else x : xs

remove :: (a -> Bool) -> [a] -> [a]
remove f = filter (not . f)

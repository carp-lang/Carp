module Util where

import Data.List
import qualified Data.Map as Map

joinWith :: String -> [String] -> String
joinWith s = concat . (intersperse s)

joinWithSpace :: [String] -> String
joinWithSpace = joinWith " "

joinWithComma :: [String] -> String
joinWithComma = joinWith ", "

joinWithUnderscore :: [String] -> String
joinWithUnderscore = joinWith "_"

joinWithPeriod :: [String] -> String
joinWithPeriod = joinWith "."

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
replaceChars dict input = concat (map replacer input)
  where replacer c = case Map.lookup c dict of
                       Just s -> s
                       Nothing -> [c]

addIfNotPresent :: Eq a => a -> [a] -> [a]
addIfNotPresent x xs =
  if x `elem` xs
  then xs
  else x : xs

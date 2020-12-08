module Util where

import Data.List
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import System.Info (os)

joinWith :: String -> [String] -> String
joinWith = intercalate

joinLines :: [String] -> String
joinLines = joinWith "\n"

joinWithSpace :: [String] -> String
joinWithSpace = unwords

joinWithComma :: [String] -> String
joinWithComma = intercalate ", "

joinWithUnderscore :: [String] -> String
joinWithUnderscore = intercalate "_"

joinWithPeriod :: [String] -> String
joinWithPeriod = intercalate "."

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
  where
    replacer c = fromMaybe [c] (Map.lookup c dict)

replaceStrings :: Map.Map String String -> String -> String
replaceStrings dict s = fromMaybe s (Map.lookup s dict)

addIfNotPresent :: Eq a => a -> [a] -> [a]
addIfNotPresent x xs =
  if x `elem` xs
    then xs
    else xs ++ [x]

remove :: (a -> Bool) -> [a] -> [a]
remove f = filter (not . f)

enumerate :: Int -> String
enumerate 0 = "first"
enumerate 1 = "second"
enumerate 2 = "third"
enumerate 3 = "fourth"
enumerate 4 = "fifth"
enumerate 5 = "sixth"
enumerate 6 = "seventh"
enumerate n = show n ++ ":th"

data Platform = Linux | MacOS | Windows | FreeBSD deriving (Show, Eq)

platform :: Platform
platform =
  case os of
    "linux" -> Linux
    "darwin" -> MacOS
    "mingw32" -> Windows
    "freebsd" -> FreeBSD
    p -> error ("Unknown platform: " ++ p)

unionOfSetsInList :: Ord a => [Set.Set a] -> Set.Set a
unionOfSetsInList (x : xs) =
  foldl' Set.union x xs
unionOfSetsInList [] =
  Set.empty

intersectionOfSetsInList :: Ord a => [Set.Set a] -> Set.Set a
intersectionOfSetsInList (x : xs) =
  foldl' Set.intersection x xs
intersectionOfSetsInList [] =
  Set.empty

evenIndices :: [a] -> [a]
evenIndices = map snd . filter (even . fst) . zip ([0 ..] :: [Int])

-- 'Naked' Lmabdas declared at the top level have their own s-expression forms
-- as names, e.g. (fn <> [] ()). This can result in invalid c code. This
-- function checks a lambda's nesting level. If the lambda is declared at the
-- top level it returns a constant string, otherwise it returns the provided
-- name (usually the name of the function in which the lambda is defined).
lambdaToCName :: String -> Int -> String
lambdaToCName name nestLevel =
  if nestLevel > 0
    then name
    else "NAKED_LAMBDA"

-- Given an integer, create a dummy argument name for it.
-- Called by XObj producing functions such as addCommand.
intToArgName :: Int -> String
intToArgName 1 = "x"
intToArgName 2 = "y"
intToArgName 3 = "z"
intToArgName 4 = "w"
intToArgName 5 = "v"
intToArgName 6 = "u"
intToArgName 7 = "t"
intToArgName 8 = "s"
intToArgName 9 = "r"
intToArgName n = intToArgName 1 ++ intToArgName (n `div` 10)

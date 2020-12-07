module Main where

import Test.HUnit
import TestConstraints
import TestLookup

main :: IO ()
main = do
  _ <- runTestTT (groupTests "Constraints" testConstraints)
  _ <- runTestTT (groupTests "Lookup" testLookup)
  return ()

groupTests :: String -> [Test] -> Test
groupTests label testCases =
  TestList (zipWith TestLabel (map ((\s -> label ++ " Test " ++ s) . show) [1 ..]) testCases)

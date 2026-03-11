module Main where

import Test.HUnit
import TestConstraints
import TestEvalIR
import TestEvalSlotLowering
import TestEvalVM
import TestEvalVMCore
import TestLookup

main :: IO ()
main = do
  _ <- runTestTT (groupTests "Constraints" testConstraints)
  _ <- runTestTT (groupTests "Lookup" testLookup)
  _ <- runTestTT (groupTests "EvalIR" testEvalIR)
  _ <- runTestTT (groupTests "EvalSlotLowering" testEvalSlotLowering)
  _ <- runTestTT (groupTests "EvalVMCore" testEvalVMCore)
  _ <- runTestTT (groupTests "EvalVM" testEvalVM)
  return ()

groupTests :: String -> [Test] -> Test
groupTests label testCases =
  TestList (zipWith TestLabel (map ((\s -> label ++ " Test " ++ s) . show) [1 ..]) testCases)

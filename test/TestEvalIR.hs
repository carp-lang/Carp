module TestEvalIR where

import EvalIR
import Obj
import Project (Project)
import StartingEnv (startingGlobalEnv, startingTypeEnv)
import SymPath
import Test.HUnit
import Types

testEvalIR :: [Test]
testEvalIR =
  [ roundTripIf,
    roundTripLet,
    roundTripGenericCall,
    lowersIfToIRIf,
    lowersGenericCallToIRCall
  ]

roundTripIf :: Test
roundTripIf =
  TestCase $
    assertBool "if round-trip should be equivalent" (lowerRoundTripEquivalent testContext ifExpr)

roundTripLet :: Test
roundTripLet =
  TestCase $
    assertBool "let round-trip should be equivalent" (lowerRoundTripEquivalent testContext letExpr)

roundTripGenericCall :: Test
roundTripGenericCall =
  TestCase $
    assertBool "generic call round-trip should be equivalent" (lowerRoundTripEquivalent testContext genericCallExpr)

lowersIfToIRIf :: Test
lowersIfToIRIf =
  TestCase $
    case lowerExpr testContext ifExpr of
      IRIf {} -> assertBool "if should lower to IRIf" True
      other -> assertFailure ("Expected IRIf, got " ++ show other)

lowersGenericCallToIRCall :: Test
lowersGenericCallToIRCall =
  TestCase $
    case lowerExpr testContext genericCallExpr of
      IRCall {} -> assertBool "generic call should lower to IRCall" True
      other -> assertFailure ("Expected IRCall, got " ++ show other)

testContext :: Context
testContext =
  Context
    (startingGlobalEnv False)
    Nothing
    (TypeEnv startingTypeEnv)
    []
    0
    (error "unused project value for EvalIR tests" :: Project)
    ""
    Repl
    []

ifExpr :: XObj
ifExpr = lst [sym "if", trueXObj, int 1, int 2]

letExpr :: XObj
letExpr = lst [sym "let", arr [sym "x", int 1], sym "x"]

genericCallExpr :: XObj
genericCallExpr = lst [sym "map", sym "inc", arr [int 1, int 2]]

sym :: String -> XObj
sym name = XObj (Sym (SymPath [] name) Symbol) Nothing Nothing

int :: Int -> XObj
int n = XObj (Num IntTy (Integral n)) Nothing (Just IntTy)

lst :: [XObj] -> XObj
lst xs = XObj (Lst xs) Nothing Nothing

arr :: [XObj] -> XObj
arr xs = XObj (Arr xs) Nothing Nothing

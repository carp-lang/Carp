module TestEvalVM where

import EvalIR
import EvalTypes
import EvalVM
import Obj
import Project (Project)
import StartingEnv (startingGlobalEnv, startingTypeEnv)
import SymPath
import Test.HUnit
import Types

testEvalVM :: [Test]
testEvalVM =
  [ vmLiteral,
    vmIfTrue,
    vmIfFalse,
    vmDoLastValue,
    vmArrayBuild,
    vmUnsupportedCallTraps
  ]

vmLiteral :: Test
vmLiteral =
  TestCase $ do
    (_, result) <- runEvalIRVM testContext (IRLiteral (int 42)) PreferDynamic
    assertEqual "literal should execute on EvalVM" (Right (int 42)) result

vmIfTrue :: Test
vmIfTrue =
  TestCase $ do
    (_, result) <- runEvalIRVM testContext (IRIf (IRLiteral trueXObj) (IRLiteral (int 1)) (IRLiteral (int 2)) Nothing Nothing) PreferDynamic
    assertEqual "if true branch should run on EvalVM" (Right (int 1)) result

vmIfFalse :: Test
vmIfFalse =
  TestCase $ do
    (_, result) <- runEvalIRVM testContext (IRIf (IRLiteral falseXObj) (IRLiteral (int 1)) (IRLiteral (int 2)) Nothing Nothing) PreferDynamic
    assertEqual "if false branch should run on EvalVM" (Right (int 2)) result

vmDoLastValue :: Test
vmDoLastValue =
  TestCase $ do
    (_, result) <- runEvalIRVM testContext (IRDo [IRLiteral (int 10), IRLiteral (int 20), IRLiteral (int 30)] Nothing Nothing) PreferDynamic
    assertEqual "do should produce the last value on EvalVM" (Right (int 30)) result

vmArrayBuild :: Test
vmArrayBuild =
  TestCase $ do
    let expected = XObj (Arr [int 1, int 2, int 3]) Nothing Nothing
    (_, result) <- runEvalIRVM testContext (IRArray [IRLiteral (int 1), IRLiteral (int 2), IRLiteral (int 3)] Nothing Nothing) PreferDynamic
    assertEqual "array should build from stack items in source order" (Right expected) result

vmUnsupportedCallTraps :: Test
vmUnsupportedCallTraps =
  TestCase $ do
    let call = IRCall (IRLiteral (sym "inc")) [IRLiteral (int 1)] Nothing Nothing
    (_, result) <- runEvalIRVM testContext call PreferDynamic
    case result of
      Left _ -> pure ()
      Right ok -> assertFailure ("unsupported call should trap, got: " ++ show ok)

testContext :: Context
testContext =
  Context
    (startingGlobalEnv False)
    Nothing
    (TypeEnv startingTypeEnv)
    []
    0
    (error "unused project value for EvalVM tests" :: Project)
    ""
    Repl
    []

sym :: String -> XObj
sym name = XObj (Sym (SymPath [] name) Symbol) Nothing Nothing

int :: Int -> XObj
int n = XObj (Num IntTy (Integral n)) Nothing (Just IntTy)

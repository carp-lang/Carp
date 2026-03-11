module TestEvalSlotLowering where

import EvalBound (BoundRef (..))
import EvalIR (EvalIR (..))
import EvalSlotLowering (findUnresolvedLocalRefs, slotifyCallableLocals)
import Obj
import SymPath
import Test.HUnit
import Types (Ty (IntTy))

testEvalSlotLowering :: [Test]
testEvalSlotLowering =
  [ slotifiesParamSymbol,
    slotifiesRestSymbol,
    slotifiesLetLocalInBody,
    slotifiesMutableLocalReadsButNotSetTarget,
    slotifiedCallableHasNoUnresolvedLocals
  ]

slotifiesParamSymbol :: Test
slotifiesParamSymbol =
  TestCase $ do
    let input = IRSymbol (BoundUnresolved (SymPath [] "x")) Symbol Nothing Nothing
        output = slotifyCallableLocals ["x"] Nothing input
    assertEqual "param should lower to local slot 0" (IRSymbol (BoundLocalSlot 0) Symbol Nothing Nothing) output

slotifiesRestSymbol :: Test
slotifiesRestSymbol =
  TestCase $ do
    let input = IRSymbol (BoundUnresolved (SymPath [] "rest")) Symbol Nothing Nothing
        output = slotifyCallableLocals ["a", "b"] (Just "rest") input
    assertEqual "rest should lower to slot after fixed args" (IRSymbol (BoundLocalSlot 2) Symbol Nothing Nothing) output

slotifiesLetLocalInBody :: Test
slotifiesLetLocalInBody =
  TestCase $ do
    let letBindings =
          IRArray
            [ IRSymbol (BoundUnresolved (SymPath [] "x")) Symbol Nothing Nothing,
              IRLiteral (XObj (Num IntTy (Integral 1)) Nothing (Just IntTy))
            ]
            Nothing
            Nothing
        body = IRSymbol (BoundUnresolved (SymPath [] "x")) Symbol Nothing Nothing
        input = IRLet letBindings body Nothing Nothing
        expected = IRLet letBindings (IRSymbol (BoundLocalSlot 1) Symbol Nothing Nothing) Nothing Nothing
        output = slotifyCallableLocals ["x"] Nothing input
    assertEqual "let-local in let body should lower to a local slot" expected output

slotifiesMutableLocalReadsButNotSetTarget :: Test
slotifiesMutableLocalReadsButNotSetTarget =
  TestCase $ do
    let symX = IRSymbol (BoundUnresolved (SymPath [] "x")) Symbol Nothing Nothing
        input = IRDo [IRSet symX (IRLiteral (XObj (Num IntTy (Integral 3)) Nothing (Just IntTy))) Nothing Nothing, symX] Nothing Nothing
        expected =
          IRDo
            [ IRSet symX (IRLiteral (XObj (Num IntTy (Integral 3)) Nothing (Just IntTy))) Nothing Nothing,
              IRSymbol (BoundLocalSlot 0) Symbol Nothing Nothing
            ]
            Nothing
            Nothing
        output = slotifyCallableLocals ["x"] Nothing input
    assertEqual "set! target stays unresolved, non-target reads use slots" expected output

slotifiedCallableHasNoUnresolvedLocals :: Test
slotifiedCallableHasNoUnresolvedLocals =
  TestCase $ do
    let symX = IRSymbol (BoundUnresolved (SymPath [] "x")) Symbol Nothing Nothing
        letBindings =
          IRArray
            [ IRSymbol (BoundUnresolved (SymPath [] "y")) Symbol Nothing Nothing,
              IRLiteral (XObj (Num IntTy (Integral 2)) Nothing (Just IntTy))
            ]
            Nothing
            Nothing
        input = IRDo [symX, IRLet letBindings (IRSymbol (BoundUnresolved (SymPath [] "y")) Symbol Nothing Nothing) Nothing Nothing] Nothing Nothing
        lowered = slotifyCallableLocals ["x"] Nothing input
        unresolved = findUnresolvedLocalRefs ["x"] Nothing lowered
    assertEqual "function-mode slotified callable should have no unresolved local refs" [] unresolved

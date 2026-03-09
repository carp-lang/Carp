module TestEvalVMCore where

import EvalBound (BoundRef (..))
import EvalCode (EvalInstr (..), ResolverHandle (..), mkEvalCode)
import EvalVMCore
import qualified Map
import Obj
import Test.HUnit
import Types

testEvalVMCore :: [Test]
testEvalVMCore =
  [ frameCanLoadLocalSlot,
    registeredCallableUsesSlot0Arg
  ]

frameCanLoadLocalSlot :: Test
frameCanLoadLocalSlot =
  TestCase $ do
    let code = mkEvalCode [IResolveSymbol 0 (RHLocalSlot 0) (BoundLocalSlot 0) Symbol Nothing Nothing, IHalt]
        frame = mkFrame code (Map.fromList [(0, int 42)])
    assertEqual "frame should resolve local slot 0" (Right (int 42)) (runFrame frame)

registeredCallableUsesSlot0Arg :: Test
registeredCallableUsesSlot0Arg =
  TestCase $ do
    let code = mkEvalCode [IResolveSymbol 0 (RHLocalSlot 0) (BoundLocalSlot 0) Symbol Nothing Nothing, IHalt]
    store <- newCodeStore
    cid <- registerCode store code
    result <- runRegisteredCallable store cid [int 99]
    assertEqual "registered callable should read arg from slot 0" (Right (int 99)) result

int :: Int -> XObj
int n = XObj (Num IntTy (Integral n)) Nothing (Just IntTy)

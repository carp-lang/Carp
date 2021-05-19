module TestLookup where

import Env as E
import qualified Map
import Obj
import qualified Set
import Test.HUnit
import Types

testLookup :: [Test]
testLookup =
  [ basicLookup
  ]

b1 = Binder emptyMeta (XObj (Str "b1") Nothing (Just StringTy))

emptyRootEnv = Env (Map.fromList []) Nothing Nothing Set.empty ExternalEnv 0

assertNotFound :: Either EnvironmentError Binder -> Test
assertNotFound (Left _) = TestCase (assertBool "assertNotFound" True) -- Better way?
assertNotFound _ = TestCase (assertBool "assertNotFound" False)

basicLookup :: Test
basicLookup = assertNotFound (fmap snd (E.searchValue emptyRootEnv (SymPath [] "nonexisting")))

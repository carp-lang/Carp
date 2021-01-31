module TestLookup where

import qualified Lookup as Lookup
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

assertNotFound :: Maybe Binder -> Test
assertNotFound Nothing = TestCase (assertBool "assertNotFound" True) -- Better way?
assertNotFound _ = TestCase (assertBool "assertNotFound" False)

basicLookup :: Test
basicLookup = assertNotFound (fmap snd (Lookup.lookupInEnv (SymPath [] "nonexisting") emptyRootEnv))

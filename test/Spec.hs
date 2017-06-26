import Test.HUnit
import qualified Data.Map as Map
import qualified Data.Set as Set
import Constraints
import Types
import Obj
import Parsing
import Infer
import Eval

main :: IO ()
main = do _ <- runTestTT (groupTests "Constraints" testConstraints)
          _ <- runTestTT (groupTests "Memory Management" testMemoryManagement)
          return ()

groupTests :: String -> [Test] -> Test
groupTests label testCases =
  TestList (zipWith TestLabel (map ((\s -> label ++ " Test " ++ s) . show) [1..]) testCases)

-- | Helper functions for testing unification of Constraints
isUnificationFailure :: Either UnificationFailure TypeMappings -> Bool
isUnificationFailure (Left _)  = True
isUnificationFailure (Right _) = False

assertUnificationFailure :: [Constraint] -> Test
assertUnificationFailure constraints = TestCase $
  assertBool "Failure" (isUnificationFailure (solve constraints))

assertSolution :: [Constraint] -> [(String, Ty)] -> Test
assertSolution constraints solution = TestCase $
  assertEqual "Solution" (Right (Map.fromList solution)) (solve constraints)

-- | A dummy XObj
x = XObj External Nothing Nothing

-- | Some type variables
t0 = VarTy "t0"
t1 = VarTy "t1"
t2 = VarTy "t2"
t3 = VarTy "t3"

-- | Test constraints
testConstraints = [testConstr1, testConstr2, testConstr3, testConstr4, testConstr5
                  ,testConstr6, testConstr7, testConstr8, testConstr9, testConstr10
                  ,testConstr11, testConstr12, testConstr13
                  ,testConstr20, testConstr21, testConstr22, testConstr23, testConstr24
                  ,testConstr30, testConstr31, testConstr32, testConstr33
                  ]

testConstr1 = assertUnificationFailure
  [Constraint FloatTy IntTy x x]

testConstr2 = assertSolution
  [Constraint IntTy t0 x x]
  [("t0", IntTy)]

testConstr3 = assertSolution
  [Constraint t0 IntTy x x]
  [("t0", IntTy)]

testConstr4 = assertSolution
  [Constraint t0 t1 x x, Constraint t0 IntTy x x]
  [("t0", IntTy), ("t1", IntTy)]

testConstr5 = assertSolution
  [Constraint t0 t1 x x, Constraint t1 IntTy x x]
  [("t0", IntTy), ("t1", IntTy)]

testConstr6 = assertSolution
  [Constraint t0 t1 x x, Constraint t1 t3 x x, Constraint t2 IntTy x x, Constraint t3 IntTy x x]
  [("t0", IntTy), ("t1", IntTy), ("t2", IntTy), ("t3", IntTy)]

testConstr7 = assertUnificationFailure
  [Constraint t0 IntTy x x, Constraint t0 FloatTy x x]

testConstr8 = assertSolution
   [Constraint t0 IntTy x x, Constraint t0 t0 x x]
   [("t0", IntTy)]

testConstr9 = assertSolution
  [Constraint t0 IntTy x x, Constraint t0 t1 x x]
  [("t0", IntTy), ("t1", IntTy)]

testConstr10 = assertSolution
  [Constraint (PointerTy (VarTy "a")) (PointerTy (VarTy "b")) x x]
  [("a", (VarTy "a")), ("b", (VarTy "a"))]

testConstr11 = assertSolution
  [Constraint (PointerTy (VarTy "a")) (PointerTy (StructTy "Monkey" [])) x x]
  [("a", (StructTy "Monkey" []))]

testConstr12 = assertSolution
  [Constraint t1 (PointerTy (StructTy "Array" [IntTy])) x x
  ,Constraint t1 (PointerTy t2) x x]
  [("t1", (PointerTy (StructTy "Array" [IntTy])))
  ,("t2", (StructTy "Array" [IntTy]))]

testConstr13 = assertSolution
  [Constraint t1 CharTy x x
  ,Constraint t1 CharTy x x]
  [("t1", CharTy)]
  
-- -- Should collapse type variables into minimal set:
-- testConstr10 = assertSolution
--   [Constraint t0 t1 x x, Constraint t1 t2 x x, Constraint t2 t3 x x]
--   [("t0", VarTy "t0"), ("t1", VarTy "t0"), ("t2", VarTy "t0")]
-- m7 = solve ([Constraint t1 t2 x x, Constraint t0 t1 x x])

-- Struct types
testConstr20 = assertSolution
  [Constraint t0 (StructTy "Vector" [t1]) x x
  ,Constraint t0 (StructTy "Vector" [IntTy]) x x]
  [("t0", (StructTy "Vector" [IntTy])), ("t1", IntTy)]

testConstr21 = assertSolution
  [Constraint t1 (StructTy "Array" [t2]) x x
  ,Constraint t1 (StructTy "Array" [t3]) x x
  ,Constraint t3 BoolTy x x]
  [("t1", (StructTy "Array" [BoolTy]))
  ,("t2", BoolTy)
  ,("t3", BoolTy)]

testConstr22 = assertSolution
  [Constraint t1 (StructTy "Array" [t2]) x x
  ,Constraint t2 (StructTy "Array" [t3]) x x
  ,Constraint t3 FloatTy x x]
  [("t1", (StructTy "Array" [(StructTy "Array" [FloatTy])]))
  ,("t2", (StructTy "Array" [FloatTy]))
  ,("t3", FloatTy)]

testConstr23 = assertUnificationFailure
  [Constraint (StructTy "Array" [t1]) (StructTy "Array" [t2]) x x
  ,Constraint t1 IntTy  x x
  ,Constraint t2 FloatTy x x]

testConstr24 = assertUnificationFailure
  [Constraint t2 FloatTy x x
  ,Constraint t1 IntTy  x x
  ,Constraint (StructTy "Array" [t1]) (StructTy "Array" [t2]) x x]

-- m9 = solve [Constraint (StructTy "Vector" [IntTy]) (StructTy "Vector" [t1]) x x]
-- m10 = solve [Constraint (StructTy "Vector" [t1]) (StructTy "Vector" [t2]) x x]

-- Func types
testConstr30 = assertSolution
  [Constraint t2 (FuncTy [t0] t1) x x
  ,Constraint t2 (FuncTy [IntTy] BoolTy) x x]
  [("t0", IntTy), ("t1", BoolTy), ("t2", (FuncTy [IntTy] BoolTy))]

testConstr31 = assertSolution
  [Constraint (FuncTy [t0] t1) (FuncTy [IntTy] BoolTy) x x]
  [("t0", IntTy), ("t1", BoolTy)]

testConstr32 = assertSolution
  [Constraint t0 (FuncTy [IntTy] BoolTy) x x]
  [("t0", (FuncTy [IntTy] BoolTy))]

testConstr33 = assertSolution
  [Constraint t1 (FuncTy [t2] IntTy) x x
  ,Constraint t1 (FuncTy [t3] IntTy) x x
  ,Constraint t3 BoolTy x x]
  [("t1", (FuncTy [BoolTy] IntTy))
  ,("t2", BoolTy)
  ,("t3", BoolTy)]



-- | Test memory management

testMemoryManagement = [testMem1]

testEnv :: Env
testEnv = Env { envBindings = bs, envParent = Nothing, envModuleName = Nothing, envImports = [], envMode = ExternalEnv }
  where bs = Map.fromList []

assertMem :: String -> [Deleter] -> Test
assertMem code deleters =
  let Right [parsed] = parse code
      Right expanded = expandAll testEnv parsed
      xobjFullSymbols = setFullyQualifiedSymbols testEnv expanded
      Right (ann : _) = annotate testEnv xobjFullSymbols
      Just i = info ann
  in  TestCase $ assertEqual "Memory" (infoDelete i) (Set.fromList deleters)

testMem1 = assertMem "(defn f [] (let [s \"HELLO\"] 12345))" []


-- case expandAll env xobj of
--                Left err -> executeCommand ctx (ReplMacroError (show err))
--                Right expanded ->
--                  let xobjFullPath = setFullyQualifiedDefn expanded (SymPath pathStrings (getName xobj))
--                      xobjFullSymbols = setFullyQualifiedSymbols innerEnv xobjFullPath
--                  in case annotate env xobjFullSymbols of
             

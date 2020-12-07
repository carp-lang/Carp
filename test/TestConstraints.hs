module TestConstraints where

import Constraints
import qualified Data.Map as Map
import qualified Data.Set as Set
import Eval
import Infer
import Obj
import Parsing
import Test.HUnit
import TestLookup
import Types

-- | Helper functions for testing unification of Constraints
isUnificationFailure :: Either UnificationFailure TypeMappings -> Bool
isUnificationFailure (Left _) = True
isUnificationFailure (Right _) = False

assertUnificationFailure :: [Constraint] -> Test
assertUnificationFailure constraints =
  TestCase $
    assertBool "Failure" (isUnificationFailure (solve constraints))

assertSolution :: [Constraint] -> [(String, Ty)] -> Test
assertSolution constraints solution =
  TestCase $
    assertEqual "Solution" (Right (Map.fromList solution)) (solve constraints)

-- | A dummy XObj
x = XObj (External Nothing) Nothing Nothing

-- | Some type variables
t0 = VarTy "t0"

t1 = VarTy "t1"

t2 = VarTy "t2"

t3 = VarTy "t3"

-- | Test constraints
testConstraints =
  [ testConstr1,
    testConstr2,
    testConstr3,
    testConstr4,
    testConstr5,
    testConstr6,
    testConstr7,
    testConstr8,
    testConstr9,
    testConstr10,
    testConstr11,
    testConstr12,
    testConstr13,
    testConstr20,
    testConstr21,
    testConstr22,
    testConstr23,
    testConstr24,
    -- ,testConstr30 DISABLED FOR NOW, started failing when lifetimes were added to function types TODO: Fix!
    testConstr31,
    testConstr32,
    testConstr33,
    testConstr34,
    testConstr35
  ]

testConstr1 =
  assertUnificationFailure
    [Constraint FloatTy IntTy x x x OrdNo]

testConstr2 =
  assertSolution
    [Constraint IntTy t0 x x x OrdNo]
    [("t0", IntTy)]

testConstr3 =
  assertSolution
    [Constraint t0 IntTy x x x OrdNo]
    [("t0", IntTy)]

testConstr4 =
  assertSolution
    [Constraint t0 t1 x x x OrdNo, Constraint t0 IntTy x x x OrdNo]
    [("t0", IntTy), ("t1", IntTy)]

testConstr5 =
  assertSolution
    [Constraint t0 t1 x x x OrdNo, Constraint t1 IntTy x x x OrdNo]
    [("t0", IntTy), ("t1", IntTy)]

testConstr6 =
  assertSolution
    [Constraint t0 t1 x x x OrdNo, Constraint t1 t3 x x x OrdNo, Constraint t2 IntTy x x x OrdNo, Constraint t3 IntTy x x x OrdNo]
    [("t0", IntTy), ("t1", IntTy), ("t2", IntTy), ("t3", IntTy)]

testConstr7 =
  assertUnificationFailure
    [Constraint t0 IntTy x x x OrdNo, Constraint t0 FloatTy x x x OrdNo]

testConstr8 =
  assertSolution
    [Constraint t0 IntTy x x x OrdNo, Constraint t0 t0 x x x OrdNo]
    [("t0", IntTy)]

testConstr9 =
  assertSolution
    [Constraint t0 IntTy x x x OrdNo, Constraint t0 t1 x x x OrdNo]
    [("t0", IntTy), ("t1", IntTy)]

testConstr10 =
  assertSolution
    [Constraint (PointerTy (VarTy "a")) (PointerTy (VarTy "b")) x x x OrdNo]
    [("a", (VarTy "a")), ("b", (VarTy "a"))]

testConstr11 =
  assertSolution
    [Constraint (PointerTy (VarTy "a")) (PointerTy (StructTy (ConcreteNameTy "Monkey") [])) x x x OrdNo]
    [("a", (StructTy (ConcreteNameTy "Monkey") []))]

testConstr12 =
  assertSolution
    [ Constraint t1 (PointerTy (StructTy (ConcreteNameTy "Array") [IntTy])) x x x OrdNo,
      Constraint t1 (PointerTy t2) x x x OrdNo
    ]
    [ ("t1", (PointerTy (StructTy (ConcreteNameTy "Array") [IntTy]))),
      ("t2", (StructTy (ConcreteNameTy "Array") [IntTy]))
    ]

testConstr13 =
  assertSolution
    [ Constraint t1 CharTy x x x OrdNo,
      Constraint t1 CharTy x x x OrdNo
    ]
    [("t1", CharTy)]

-- -- Should collapse type variables into minimal set:
-- testConstr10 = assertSolution
--   [Constraint t0 t1 x x x, Constraint t1 t2 x x x, Constraint t2 t3 x x x OrdNo]
--   [("t0", VarTy "t0"), ("t1", VarTy "t0"), ("t2", VarTy "t0")]
-- m7 = solve ([Constraint t1 t2 x x x, Constraint t0 t1 x x x OrdNo])

-- Struct types
testConstr20 =
  assertSolution
    [ Constraint t0 (StructTy (ConcreteNameTy "Vector") [t1]) x x x OrdNo,
      Constraint t0 (StructTy (ConcreteNameTy "Vector") [IntTy]) x x x OrdNo
    ]
    [("t0", (StructTy (ConcreteNameTy "Vector") [IntTy])), ("t1", IntTy)]

testConstr21 =
  assertSolution
    [ Constraint t1 (StructTy (ConcreteNameTy "Array") [t2]) x x x OrdNo,
      Constraint t1 (StructTy (ConcreteNameTy "Array") [t3]) x x x OrdNo,
      Constraint t3 BoolTy x x x OrdNo
    ]
    [ ("t1", (StructTy (ConcreteNameTy "Array") [BoolTy])),
      ("t2", BoolTy),
      ("t3", BoolTy)
    ]

testConstr22 =
  assertSolution
    [ Constraint t1 (StructTy (ConcreteNameTy "Array") [t2]) x x x OrdNo,
      Constraint t2 (StructTy (ConcreteNameTy "Array") [t3]) x x x OrdNo,
      Constraint t3 FloatTy x x x OrdNo
    ]
    [ ("t1", (StructTy (ConcreteNameTy "Array") [(StructTy (ConcreteNameTy "Array") [FloatTy])])),
      ("t2", (StructTy (ConcreteNameTy "Array") [FloatTy])),
      ("t3", FloatTy)
    ]

testConstr23 =
  assertUnificationFailure
    [ Constraint (StructTy (ConcreteNameTy "Array") [t1]) (StructTy (ConcreteNameTy "Array") [t2]) x x x OrdNo,
      Constraint t1 IntTy x x x OrdNo,
      Constraint t2 FloatTy x x x OrdNo
    ]

testConstr24 =
  assertUnificationFailure
    [ Constraint t2 FloatTy x x x OrdNo,
      Constraint t1 IntTy x x x OrdNo,
      Constraint (StructTy (ConcreteNameTy "Array") [t1]) (StructTy (ConcreteNameTy "Array") [t2]) x x x OrdNo
    ]

-- m9 = solve [Constraint (StructTy "Vector" [IntTy]) (StructTy "Vector" [t1]) x x x OrdNo]
-- m10 = solve [Constraint (StructTy "Vector" [t1]) (StructTy "Vector" [t2]) x x x OrdNo]

-- Func types
testConstr30 =
  assertSolution
    [ Constraint t2 (FuncTy [t0] t1 StaticLifetimeTy) x x x OrdNo,
      Constraint t2 (FuncTy [IntTy] BoolTy StaticLifetimeTy) x x x OrdNo
    ]
    [("t0", IntTy), ("t1", BoolTy), ("t2", (FuncTy [IntTy] BoolTy StaticLifetimeTy))]

testConstr31 =
  assertSolution
    [Constraint (FuncTy [t0] t1 StaticLifetimeTy) (FuncTy [IntTy] BoolTy StaticLifetimeTy) x x x OrdNo]
    [("t0", IntTy), ("t1", BoolTy)]

testConstr32 =
  assertSolution
    [Constraint t0 (FuncTy [IntTy] BoolTy StaticLifetimeTy) x x x OrdNo]
    [("t0", (FuncTy [IntTy] BoolTy StaticLifetimeTy))]

testConstr33 =
  assertSolution
    [ Constraint t1 (FuncTy [t2] IntTy StaticLifetimeTy) x x x OrdNo,
      Constraint t1 (FuncTy [t3] IntTy StaticLifetimeTy) x x x OrdNo,
      Constraint t3 BoolTy x x x OrdNo
    ]
    [ ("t1", (FuncTy [BoolTy] IntTy StaticLifetimeTy)),
      ("t2", BoolTy),
      ("t3", BoolTy)
    ]

testConstr34 =
  assertSolution
    [ Constraint (VarTy "a") (StructTy (ConcreteNameTy "Pair") [(VarTy "x0"), (VarTy "y0")]) x x x OrdNo,
      Constraint (StructTy (ConcreteNameTy "Array") [(VarTy "a")]) (StructTy (ConcreteNameTy "Array") [(StructTy (ConcreteNameTy "Pair") [(VarTy "x1"), (VarTy "y1")])]) x x x OrdNo
    ]
    [ ("a", (StructTy (ConcreteNameTy "Pair") [(VarTy "x0"), (VarTy "y0")])),
      ("x0", (VarTy "x0")),
      ("y0", (VarTy "y0")),
      ("x1", (VarTy "x0")),
      ("y1", (VarTy "y0"))
    ]

-- Same as testConstr34, except everything is wrapped in refs
testConstr35 =
  assertSolution
    [ Constraint (RefTy (VarTy "a") (VarTy "lt0")) (RefTy (StructTy (ConcreteNameTy "Pair") [(VarTy "x0"), (VarTy "y0")]) (VarTy "lt1")) x x x OrdNo,
      Constraint (RefTy (StructTy (ConcreteNameTy "Array") [(VarTy "a")]) (VarTy "lt2")) (RefTy (StructTy (ConcreteNameTy "Array") [(StructTy (ConcreteNameTy "Pair") [(VarTy "x1"), (VarTy "y1")])]) (VarTy "lt3")) x x x OrdNo
    ]
    [ ("a", (StructTy (ConcreteNameTy "Pair") [(VarTy "x0"), (VarTy "y0")])),
      ("x0", (VarTy "x0")),
      ("y0", (VarTy "y0")),
      ("x1", (VarTy "x0")),
      ("y1", (VarTy "y0")),
      ("lt0", (VarTy "lt0")),
      ("lt1", (VarTy "lt0")),
      ("lt2", (VarTy "lt2")),
      ("lt3", (VarTy "lt2"))
    ]

-- Ref types with lifetimes
-- testConstr36 = assertSolution
--   [Constraint (RefTy (VarTy "a")) (RefTy (StructTy "Pair" [(VarTy "x0"), (VarTy "y0")])) x x x OrdNo
--   ,Constraint (RefTy (StructTy "Array" [(VarTy "a")])) (RefTy (StructTy "Array" [(StructTy "Pair" [(VarTy "x1"), (VarTy "y1")])])) x x x OrdNo]
--   [("a", (StructTy "Pair" [(VarTy "x0"), (VarTy "y0")]))
--   ,("x0", (VarTy "x0"))
--   ,("y0", (VarTy "y0"))
--   ,("x1", (VarTy "x0"))
--   ,("y1", (VarTy "y0"))
--   ]

module Workbench where

import ColorText
import Obj
import Types
import Commands
import Template
import Parsing
import Infer
import Constraints
import Emit
import qualified Data.Map as Map

-- | Not part of the cabal file, just for interactive repl sessions:

pt :: XObj -> IO ()
pt = putStrLn . prettyTyped 

-- hmm

-- Testing

-- (Right [p]) = parse "(defn f (x) (str (add x 10)))"
-- (Right [p]) = parse "(add 2 \"hej\")"
-- e = Env (Map.fromList [("f", (Binder "f" p3)), ("g", (Binder "g" p3))]) Nothing

--                   (defn f [] (println (id "hej")))
--                   (defn f [] (println (ID \"hej\")))
--                   (array-str (replicate 5 true))

a :: Env
a = Env { envBindings = bs, envParent = Nothing, envModuleName = Nothing, envImports = [], envMode = ExternalEnv }
  where bs = Map.fromList [("str", Binder (XObj (Lst [XObj External Nothing Nothing,
                                                       XObj (Sym (SymPath ["A"] "str")) Nothing Nothing])
                                            Nothing (Just (FuncTy [(StructTy "Array" [(VarTy "t")])] StringTy)))),
                           ("fmap", Binder (XObj (Lst [XObj External Nothing Nothing,
                                                       XObj (Sym (SymPath ["A"] "fmap")) Nothing Nothing])
                                            Nothing (Just (FuncTy [] (VarTy "t")))))]

b :: Env
b = Env { envBindings = bs, envParent = Nothing, envModuleName = Nothing, envImports = [], envMode = ExternalEnv }
  where bs = Map.fromList [("fmap", Binder (XObj (Lst [XObj External Nothing Nothing,
                                                       XObj (Sym (SymPath ["B"] "fmap")) Nothing Nothing])
                                            Nothing (Just (FuncTy [(FuncTy [(VarTy "t")] (VarTy "t")), (StructTy "Array" [(VarTy "t")])]
                                                           (StructTy "Array" [(VarTy "t")])))))]

arrayModule :: Env
arrayModule = Env { envBindings = bindings, envParent = Nothing, envModuleName = (Just "Array"), envImports = [], envMode = ExternalEnv }
  where bindings = Map.fromList [ templateNth
                                , templateReplicate
                                , templateMap
                                , templateRaw
                                ]

startingGlobalEnv :: Env
startingGlobalEnv = Env { envBindings = bs, envParent = Nothing, envModuleName = Nothing, envImports = [(SymPath [] "A"),
                                                                                                        (SymPath [] "B")],
                          envMode = ExternalEnv }
  where bs = Map.fromList [-- (register "fmap" (FuncTy [(VarTy "t")] (VarTy "s")))
                            (register "str" (FuncTy [IntTy] StringTy))
                          , (register "inc" (FuncTy [IntTy] IntTy))
                          , (register "println" (FuncTy [StringTy] UnitTy))
                          , (register "id" (FuncTy [(VarTy "t")] (VarTy "t")))
                          , (register "SDL_RenderDrawLines" (FuncTy [(PointerTy (StructTy "Array" [IntTy])), IntTy] UnitTy))
                          , ("A", Binder (XObj (Mod a) Nothing Nothing))
                          , ("B", Binder (XObj (Mod b) Nothing Nothing))
                          , ("Array", Binder (XObj (Mod arrayModule) Nothing Nothing))
                          ]

-- [t0 == t1,t2 == (λ [Float] t1)]
-- (defn main [] (SDL_RenderDrawLines (Array.raw [1 2 3]) 0))
-- defn main [] (let [x \"HELLO\"] 12345)

(Right [p]) = parse "(defn f [] (ref 123))"
-- xobjFullPath = setFullyQualifiedDefn p (SymPath [] (getName p))
-- p' = setFullyQualifiedSymbols startingGlobalEnv xobjFullPath

(Right p2) = initialTypes startingGlobalEnv p
(Right c) = genConstraints p2
Right m = solve c
m0 = solve c
p3 = assignTypes m p2
conc = concretizeXObj True startingGlobalEnv p3
Right (p4, deps) = conc

cc = putStrLn $ toC p4

(Right c') = genConstraints p4
Right m' = solve c'
p3' = assignTypes m' p4
conc2 = concretizeXObj True startingGlobalEnv p3'
Right (p5, deps2) = conc2

Right mem = manageMemory startingGlobalEnv p5

ccc = putStrLn $ toC mem
co = [Constraint (StructTy "Array" [t1]) (StructTy "Array" [t2]) x x
     ,Constraint t1 IntTy  x x
     ,Constraint t2 FloatTy x x]


x = XObj External Nothing Nothing
t0 = VarTy "t0"
t1 = VarTy "t1"
t2 = VarTy "t2"
t3 = VarTy "t3"
t4 = VarTy "t4"
t5 = VarTy "t5"
t6 = VarTy "t6"

-- con = [Constraint t1 (StructTy "Array" [t2]) x x
--       ,Constraint t2 (StructTy "Array" [t3]) x x
--       ,Constraint t3 FloatTy x x]


-- f' = setFullyQualifiedDefn p (SymPath [] (getName p))
-- f'' = setFullyQualifiedSymbols startingGlobalEnv f'
-- f''' = annotate startingGlobalEnv f''





-- b :: Env
-- b = Env (Map.fromList [("f", Binder (XObj (Sym (SymPath [] "f")) Nothing Nothing))]) Nothing Nothing

-- a :: Env
-- a = Env (Map.fromList [("B", Binder (XObj (Mod b) Nothing Nothing))]) Nothing Nothing

-- glob :: Env
-- glob = Env (Map.fromList [("A", Binder (XObj (Mod a) Nothing Nothing))]) Nothing Nothing

-- p = putStrLn (prettyEnvironment glob)
-- p' = putStrLn (prettyEnvironment (envInsertAt glob (SymPath ["A", "B"] "g") (XObj (Sym (SymPath [] "g")) Nothing Nothing)))




-- Matching types


blub = (FuncTy [(FuncTy [IntTy] IntTy), (StructTy "Array" [IntTy])] (StructTy "Array" [t3]))
flub = (FuncTy [(FuncTy [t1]    t2),    (StructTy "Array" [t1])]    (StructTy "Array" [t2]))

are = areUnifiable blub flub






aaa = (FuncTy [t1] t1)
bbb = (FuncTy [IntTy] t0)
solution = solve [Constraint aaa bbb x x]










Right (Just pointerTy) = fmap (xobjToTy . head) (parse "(Ptr a)")
otherTy = (PointerTy (VarTy "t"))

u = solve [(Constraint pointerTy otherTy x x)]


q = solve [Constraint t1 (PointerTy (StructTy "Array" [IntTy])) x x
          ,Constraint t1 (PointerTy t2) x x]

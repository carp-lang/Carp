<!-- This presentation is made to be run in the Deckset app. -->

# Introduction to Carp

---
# Slogan

> Carp is a statically typed Lisp without a GC
> for high performance applications.

---
# Motivation
## (why make a language, apart from the fun)

* Lisp is great
* Most lisps are garbage collected, won't work well for games, etc.
* But I want to make games!

---
# Preliminary use cases

1. Games
2. Audio and video generation

---
# Main ideas of Carp

* Static type system (subset of ML)
* No garbage collection (a la Rust)
* Lisp (Clojure) syntax
* A helpful and interactive programming environment

---
# Workflow

* REPL = project
* (load <file>) and (reload)
* (build) and (run)
* GHCI-style shortcuts, i.e. ":rbx"
* A build system

---
# The prompt
## 鲮

---
# Language basics

1. Functions, values & types
2. Memory management
3. Modules
4. Defining data types
5. Array processing

---
# [fit] 1. Functions, values & types

---
# Defining things
```
(defn circle-area [r] (* π (* r r))

(defn id [x] x)

(def gravity 9.8)
```

---
# Type inference

```
(defn circle-area [r] (* π (* r r)) : (λ [Float] Float)

(defn id [x] x) : (λ [a] a)

(def gravity 9.8) : Double
```

---
# [fit] Getting the type of a function (at the REPL)

```
鲮 (type println)

=> IO.println : (λ [(Ref String)] ())
```

---
# [fit] Enforcing types using the 'the' special form
An idea stolen from Idris.

```

(defn id-for-strings-only [x]
  (the String x))
```

---
# [fit] Holes can help you figure out the type of an expression

```
鲮 (+ 1.0f ?w00t)

=> ?w00t : Float

```

---
# 2. Memory management

---
# [fit] Values are deleted at the end of the scope

```
(let [s (get-line)]
  (println "OK")
  <here>)

get-line : (λ [] String)
```

---
# [fit] Unless they are sent to another function or returned

```
(let [s (get-line)]
  (validate-string s))

(let [s (get-line)]
  s)

validate-string : (λ [String] Bool)
```

---
# [fit] That means that passing a value repeateadly is forbidden

```
(let [s (get-line)]
  (do (validate s)
      (validate s)
      (validate s)))

=> Using a given-away value 's'
```

---
# [fit] To temporarily borrow a value, use 'ref'

```
(let [s (get-line)]
  (do (println (ref s))
      (println (ref s))
      (println (ref s))))

s : String
println : (λ [(Ref String)] ())
```

---
# [fit] Another name for 'ref' is '&'

A reader macro.

```

(let [s (get-line)]
  (do (println &s)
      (println &s)
      (println &s)))
```

---
# [fit] String literals have the type (Ref String)

```
(println "Hi!")
```

---
# [fit] Functions are not allowed to return Ref:s

Would lead to dangling pointers.

```

(defn invalid []
  (let [s (get-line)]
    &s
    <delete s here>))


invalid : (λ [] (Ref String))
```

---
# Sometimes you need a copy

The 'copy' functions take Ref:s and return non-Ref:s.

```

(defn name []
  (copy "John McCarthy"))


name : (λ [] String)
String.copy : (λ [(Ref String)] String)
```

---
# Syntactic sugar for 'copy' is '@'

```
(defn name []
  @"John McCarthy")
```

---
# 3. Modules

---
# Purpose
- Allow sharing of names
- Clarify which function or value that is used in the code

---
# Defining and using a module

```
(defmodule Goods
  (defn cost [] (random-between 100 200))
  (def amount 75600))

(defn total-cost []
  (* (Goods.cost) Goods.amount))
```

---
# Not tied to files

```
;; stuff.carp

(defmodule A
  (defn f [] ...)
  (defn g [] ...))

(defmodule B
  (defn f [] ...)
  (defn g [] ...)
  (defn h [] ...))
```

---
# Open to extension

```
(defmodule A
  (defn f [] ...))

(defmodule A
  (defn g [] ...))
```

---
# [fit] To avoid qualifying the module name: 'use'

```
(use Int)

(defn twice [x] (+ x x))

twice : (λ [Int] Int)
```

---
# Multiple modules can contain a function with the same name

<!-- The type checker tries to figure out the correct one to use based on other types. -->

```
(use Int)
(use Double)
(use Float)

(defn inc [x] (+ x 1))

1 : Int
inc : (λ [Int] Int)

'+' resolves to 'Int.+'
```

---
# The Expression Problem

```
(defmodule Ur
  (defn f [x]
    (foo x)))

(defmodule A
  (defn foo [x]
    (+ x 1)))

(defmodule B
  (defn foo [x]
    (+ x 1.0f)))

(defn tester []
  (Ur.f 42))
```

---
# Interfaces to the rescue

```
(definterface foo (λ [a] a))

(defmodule Ur
  (defn f [x]
    (foo x)))

(defmodule A
  (defn foo [x]
    (+ x 1)))

(defmodule B
  (defn foo [x]
    (+ x 1.0f)))

(defn tester []
  (Ur.f 42))
```

---
# Some built-in interfaces

```
copy : (λ [&a] a)
str : (λ [a] String)
= : (λ [a, a] Bool)
```

---
# Inspecting modules at the REPL

```
鲮 (type Double)

Double : Module = {
    * : (λ [Double Double] Double)
    + : (λ [Double Double] Double)
    - : (λ [Double Double] Double)
    / : (λ [Double Double] Double)
    cos : (λ [Double] Double)
    from-int : (λ [Int] Double)
    sin : (λ [Double] Double)
    str : (λ [Double] String)
    to-int : (λ [Double] Int)
    π : Double
}
```

---
# 4. Defining data types

---
# Structs

```
(deftype Point
  [x Int
   y Int])
```

---
# [fit] Defining a struct will create a module with the same name

```
(deftype Point
  [x Int
   y Int])

Point : Module = {
    init : (λ [Int Int] Point)
    new : (λ [Int Int] (Ptr Point))
    copy : (λ [(Ref Point)] Point)
    delete : (λ [Point] ())
    str : (λ [(Ref Point)] String)
    x : (λ [(Ref Point)] Int)
    y : (λ [(Ref Point)] Int)
    set-x : (λ [Point Int] Point)
    set-y : (λ [Point Int] Point)
    update-x : (λ [Point (λ [Int] Int)] Point)
    update-y : (λ [Point (λ [Int] Int)] Point)
}
```

---
# Allows structs to share member names

```
(deftype Vec2
  [x Int
   y Int])

(deftype Vec3
  [x Int
   y Int
   z Int])

(let [v (Vec2.init 11 35)]
  (Vec2.x &v))

(let [v (Vec3.init 300 400 500)]
  (Vec3.set-y v 0))
```

---
# Possible extensions to the type system

* Tagged unions
* Generic data types

---
# 5. Array processing

---
# Creating arrays

```
[1 2 3 4 5]
```

---
# [fit] Arrays can also be generated using various functions

```
Array.range : (λ [t t t] (Array t))
Array.repeat : (λ [Int (λ [] t)] (Array t))
Array.replicate : (λ [Int (Ref t)] (Array t))
```

Yes, the 't' in 'range' is too general.

---
# Mapping functions over arrays is useful

---
# Transforming from one type to another

Will allocate new memory and leave the old data as-is.

```
(let [xs [1 2 3 4 5]]
  (copy-map Int.str &xs))

copy-map : (λ [(λ [&a] b), &(Array a)] (Array b))
```

---
# When mapping from one type to itself, the copying is unnecessary

```
(let [xs [1 2 3 4 5]]
  (endo-map square xs))
```

Ownership of 'xs' is passed to the 'endo-map' function, which will mutate the array and return it.

---
# Behind the scenes

---
# Written in Haskell

```
-------------------------------------------------------------------------------
Language                     files          blank        comment           code
-------------------------------------------------------------------------------
Haskell                         23            607            330           4350
-------------------------------------------------------------------------------
SUM:                            23            607            330           4350
-------------------------------------------------------------------------------
```

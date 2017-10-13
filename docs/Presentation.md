# Introduction to Carp
## A slide show presentation by Erik Svedäng

---
# Slogan

> Carp is a statically typed Lisp without a GC
> for high performance applications.

---
# Motivation
## (why make a language, apart from that it's fun)

* Lisp is fun (and has the best syntax!)
* I make games
* Most lisps are garbage collected, won't work well for games

---
# Preliminary use cases

1. Games
2. Audio and video generation

---
# Main ideas of Carp

* A simple ML-like type system
* Linear types, very similar to Rust
* Clojure-like syntax + macros
* An interactive environment where you can work efficiently using a REPL
* High performance functional programming

---
# [fit] 鲮

---
# Language basics

1. Functions, values & types
2. Memory management
3. Modules
4. Defining data types
5. Array processing

---
# 1. Functions, values & types

---
# Defining things
```
(defn tenfold [x] (* x 10))

(defn id [x] x)

(def pi 3.14)
```

---
# Type inference

```
(defn tenfold [x] (* x 10)) : (λ [Int] Int)

(defn id [x] x) : (λ [a] a)

(def pi 3.14) : Double
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
# [fit] Modules


---
# Defining data types


---
# Array processing


---

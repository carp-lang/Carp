# Dynamic Semantics

This document will help us rewrite Carp's dynamic evaluator (defined in Eval.hs) to make it behave in a more principled and non-surprising way.

## Goals of the rewrite
* Fix the various bugs related to dynamic evaluation that has been found (see "Relevant issues" below).
* Add missing features that are to be expected in a dynamic Lisp (see below).
* Make it easy to extend the dynamic evaluator with new features in the future.

## Relevant issues
* https://github.com/carp-lang/Carp/issues/560
* https://github.com/carp-lang/Carp/issues/555
* https://github.com/carp-lang/Carp/issues/545
* https://github.com/carp-lang/Carp/issues/476
* https://github.com/carp-lang/Carp/issues/556
* https://github.com/carp-lang/Carp/issues/659
* https://github.com/carp-lang/Carp/issues/660
* https://github.com/carp-lang/Carp/issues/453

## Desired features (currently missing)
* Documentation on how to use the dynamic language and the macro system
* Complete macro facilities (quasiquoting, splicing, complete error reporting, etc)
* Dynamic stack traces
* Auto completion of user-defined names

<hr>

# Semantics

## Index
[TODO]

## 0. Terms used in this document
* form : Any valid Carp data struture as represented in text.
* top level : Any form that isn't embedded in another form.
* Static Carp : The compiled version of the Carp language.
* Dynamic Carp : The interpreted, functional, GC'ed version of the Carp language.

## 1. Scoping Rules
Related issues:
* https://github.com/carp-lang/Carp/issues/659

Questions:
#### How does Carp figure out what the value of the symbol X is?
Lexical scoping (look in the current scope, then any enclosing scope, up until global scope).
Things that create scopes:
- function definitions (defn, defndynamic, fn)
- let
- modules

#### How do you set the value for symbol X?

`(set! <symbol> <value>)`

#### Are there any reserved names?
Yes (see the Parsing module for more info)

- defn
- def
- do
- while
- fn
- let
- break
- if
- match
- true
- false
- address
- set!
- the
- ref
- deref
- with

More things should be moved to the reserved list, actually.
The `:rest` token in defmacro is also reserved.

#### What is a keyword?
There are no keywords. Maybe will be in the macros, see this implementation https://gist.github.com/sdilts/73a811a633bb0ef3dd7e31b84a138a5a.

#### Are there different namespaces for dynamic and static Carp?
They use the same modules but dynamic lookup will only find dynamic functions, and static lookup will only find static functions.

### 1.1 Global Variables
Questions:
#### Are global variables mutable?
Yes.

#### How are they mutated? When do these mutations come into affect?
Using `set!`. The mutation comes into effect immedately (using IORefs internally).

#### Do global variables have lexical or dynamic scope?
Lexical (no dynamic scope for anything).

### 1.2 Local variables
Questions:
#### Are local variables mutable?
Yup.

#### When do local variables come in and out of scope?
Lexical scoping rules, functions and let create new variables.

#### What is a closure? What are the important rules for variables inside closures?
No captured variables are mutable.
The dynamic lambdas captures the whole environment at the time the closure is created (when the `(fn ...)` form is evaluated).

### 1.3. Namespace Rules
Questions:
#### Given symbols `a` in the `Foo` module and `a` in the `Bar` module, how do I refer to each of them?
Using `.`, Foo.a and Bar.a.
By using `(use <module name>)` you can avoid having to specify the module.

#### What happens if multiple modules are imported and they contain the same symbol?
Runtime error when looking up the symbol.

#### Given the symbols`Foo.a` and `Bar.a`, exist, which symbol does `a` refer to?
Neither, unless any single one of the modules (Foo/Bar) is imported with `use`. If both are imported the lookup is an error since it can't be resolved to a single value.

#### Do functions and variables live in the same namespace?
Yes. Types live in a different namespace.

### 1.4 Definitions
Questions:
#### What kinds of definitions are there and how are they created?

Dynamic context:
- defndynamic (creates dynamic functions)
- defdynamic (creates dynamic global variables)
- defmacro (creates macros)

Static context:
- defn (creates static functions)
- def (creates static global variables)
- deftype (for defining product- and sumtypes)
- register (for making external functions available)

All contexts:
- defmodule

## 2. Evaluation Rules
Related issues:
* https://github.com/carp-lang/Carp/issues/555

Questions:
#### When are macros evaluated?
#### When are symbols evaluated?
#### When are forms evaluated?
#### Are forms evaluated left-to-right or right-to-left?
#### How does error reporting work?

### 2.1 Macros
Questions:
* What is a macro?
* What functions are available at macro-expansion time?
* What is quasi-quoting and what is its syntax?
* What is splicing, and what is its syntax?

### 2.2 REPL
Questions:
* How does the REPL know when to evalutate something in the dynamic or static context?
* When does it decide to run the given code in the dynamic or static context?

## 3. Types
Issues:
* [#560 Add Reflection Module Proposal](https://github.com/carp-lang/Carp/issues/560)

Questions:
* What types are available?
* When is a form typechecked?
* How do you refer to a specific type? Are types [first class citizens](https://en.wikipedia.org/wiki/First-class_citizen)?

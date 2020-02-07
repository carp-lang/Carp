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
* Static Carp : The compiled version of the Carp langauge
* Dynamic Carp : The interpreted, functional, GC'ed version of the Carp langauge

## 1. Scoping Rules
Related issues:
* https://github.com/carp-lang/Carp/issues/659

Questions:
* How does Carp figure out what the definition of the symbol X is?
* How do you set the value for symbol X?
* Are there any reserved names?
* What is a keyword?
### 1.1 Global Variables
Questions:
* Are global variables mutable?
* If they are mutable, how are they mutated? When do these mutations come into affect?
* Do global variables have lexical or dynamic scope?
### 1.2 Local variables
Questions:
* Are local variables mutable?
* When do local variables come in and out of scope?
* What is a closure?
### 1.3. Namespace Rules
Questions:
* Given symbols `a` in the `foo` module and `a` in the `bar` module, how do I refer to each of them?
* Given the symbols`Foo.a` and `Bar.a`, exist, which symbol does `a` refer to?
* Do functions and variables live in the same namespace?

## 2. Evaluation Rules
Related issues:
* https://github.com/carp-lang/Carp/issues/555

Questions:
* When are macros evaluated?
* When are symbols evaluated?
* When are forms evaluated?
* Are forms evaluated left-to-right or right-to-left?
* How does error reporting work?

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

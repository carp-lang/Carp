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

## 1. [TODO]

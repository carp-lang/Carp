# Todo

## Critical Bugs
* Not a bug, but to prevent bugs phantom types could be used for the environments to prevent mixing up the global Env and the type Env

## Big Language Features
* Generic data types (apart from Array, which already is)
* Tagged unions (also known as "sum types" or "enums")
* Lambdas (anonymous functions)
* Automatic generation of 'str'-function for Arrays (and structs containing Arrays)

## Smaller Language Features ("niceties")

## Language Design Considerations
* The type of the variable in a set!-form, i.e. (set! &x 10)
* 'copy' should probably be a special form, just like 'ref'?
* Is some kind of interface/typeclass construct worthwhile?
* How should passing primitive types (that do not care about being referenced) as ref:ed parameters be handled?

## Code generation
* LLVM backend
* Emit #LINE macros in the generated C code

## Tooling
* Allow checking type of a function from the REPL
* Store line numbers with forms and present them in error messages
* Proper error handling when defining invalid struct types (right now it crashes)
* Stop evalutaion of forms after errors to avoid "Trying to refer to undefined symbol" error
* Built in REPL history (without using rlwrap)
* Preserve whitespace to allow saving forms back to disk

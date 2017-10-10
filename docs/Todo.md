# Todo

## Critical Bugs
* Let-forms can return references

## Big Language Features
* Generic data types (apart from Array, which already is)
* Tagged unions (also known as "sum types" or "enums")
* Lambdas (anonymous functions)
* Automatic generation of 'str'-function for struct types and Arrays

## Smaller Language Features ("niceties")

## Language Design Considerations
* The type of the variable in a set!-form, i.e. (set! &x 10)
* 'copy' should probably be a special form, just like 'ref'?
* Is some kind of interface/typeclass construct worthwhile?

## Code generation
* LLVM backend
* Emit #LINE macros in the generated C code

## Tooling
* Built in REPL history (without using rlwrap)
* Stop evalutaion of forms after errors to avoid "Trying to refer to undefined symbol" error

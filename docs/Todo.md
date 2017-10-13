# Todo

## Critical Bugs
* The 'range' function is fully generic (for all 'a') but only compiles when 'a' is numeric type
* Go over all the Array functions and make sure they are memory safe
* Can't define globals of type String or String-ref.
* Just entering '=' at the REPL leads ot strange type error.
* The lambda sent to 'transform' should probably have type (λ [(Ref a)] b) to prevent it from touching the contents of the source array

## Big Language Features
* Generic data types (apart from Array, which already is)
* Tagged unions (also known as "sum types" or "enums")
* Lambdas (anonymous functions)

## Smaller Language Features ("niceties")
* Good string functions
* Being able to use 'the' in function parameter declarations, i.e. (defn f [(the Int x)] x) to enforce a type
* Allow lambda ("λ") as an alias for Fn when defining types

## Language Design Considerations
* What's the correct type of the variable in a set!-form, i.e. (set! &x value) or (set! x value)
* Is some kind of interface/typeclass construct worthwhile?
* How should passing primitive types (that do not care about being referenced) as ref:ed parameters be handled?
* How to handle heap allocated values? Box type with reference count?
* Fixed-size stack allocated arrays would be useful (also as members of structs)
* Look over how many times the function 'annotateOne' in Infer.hs actually needs to be applied to a form

## Code generation
* LLVM backend
* Emit #LINE macros in the generated C code

## Tooling
* Enable printing of typed AST:s at the REPL to help debug unresolved type variables etc.
* Proper error handling when defining invalid struct types (right now it crashes)
* Stop evalutaion of forms after errors to avoid "Trying to refer to undefined symbol" error
* Built in REPL history (without using rlwrap)
* Preserve whitespace to allow saving forms back to disk
* Refactorings at the REPL. Rename, extract function, add/remove parameter?
* Hide instances of templates/generic functions when printing the environment (by default, allow it as a setting)
* Somehow make it possible to enter ":t foo" at the REPL (can't be done now because each atom is evaluated separately)
* Rename type variables from t0, t1, t2 to a, b, c, etc.

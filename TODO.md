# Compiler
  - Track dependencies between functions
  - Change :a and :b in binop and if to :left and :right
  - Compilation of generic functions
  - lambdas / lambda lifting
  - compile a whole file to a single dylib
  - nicer names for compiler generated variables
  - speed up some passes by mutating a single variable instead of copying immutable versions around
  - Clean up unifier even more

# Lisp Core Libs
  - -> and ->>
  - shuffle
  - conversions between a list of pairs and dictionaries

# Dynamic Runtime
  - register/register-builtin should use the lisp name, not the C name 
  - call user.carp file if it exists in ~/.carp or project.car if it's in the folder where you run the carp command
  - meta data on Objs: Line nr, line pos, source file,
  - add array as its own tag for Obj, [] syntax, etc
  - jump table in evaluator, use a 'dispatch' member with a label adress in Obj
  - primops should have signatures
  - remove globals to enable several instances of the runner in parallel
  - nicer pretty printing of lists of lists
  - better error handling and input validation for primops, clean up the C macros
  - lambdas should be able to have their signature set/get
  - profile the evaluator
  - namespaces

# Maybes
  - polymorphic math operators?
  - matching/destructuring in let statements and function arguments too?
  - reading of dotted pairs?


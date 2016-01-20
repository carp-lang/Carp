# Compiler
  - Handle when a let binding refers to an earlier let binding
  - Compilation of generic functions
  - Compiling arrays
  - Prevent uses of a variable when it has been sent to another function
  - Avoid problems with name shadowing when freeing a local variable
  - Handle global variables referenced inside functions, in regards to the lifetime checker
  - Track dependencies between functions
  - Change :a and :b in binop and if to :left and :right
  - lambdas / lambda lifting
  - defstruct
  - deftype
  - compile a whole file to a single dylib
  - nicer names for compiler generated variables
  - speed up some passes by mutating a single variable instead of copying immutable versions around
  - Clean up unifier even more
  - Option for turning off lifetime checker
  - Change type of functions from (:arrow ...) to (:fn ...)

# Lisp Core Libs
  - assert-eq shows wrong result when the assertion fails? (in ffi situations...)
  - -> and ->>
  - shuffle
  - conversions between a list of pairs and dictionaries

# Bugs
  - Didn't show error when registering non-existing C function?!
  
# Dynamic Runtime
  - no need to handle (:ptr ...) as a special case?
  - add array as its own tag for Obj, [] syntax, etc
  - use [] in parameter list for function definitions
  - register/register-builtin should use the lisp name, not the C name 
  - meta data on Objs: Line nr, line pos, source file,
  - jump table in evaluator, use a 'dispatch' member with a label adress in Obj
  - remove globals to enable several instances of the runner in parallel
  - primops should have signatures, right?
  - nicer pretty printing of lists of lists
  - better error handling and input validation for primops, clean up the C macros
  - lambdas should be able to have their signature set/get
  - profile the evaluator
  - namespaces

# Maybes
  - polymorphic math operators?
  - matching/destructuring in let statements and function arguments too?
  - reading of dotted pairs?


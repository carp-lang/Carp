# Compiler
  - Compilation of generic functions
  - Compiling arrays
  - Avoid problems with name shadowing when freeing a local variable
  - Handle global variables referenced inside functions, in regards to the lifetime checker
  - Track dependencies between functions
  - Change :a and :b in binop and if to :left and :right
  - lambdas / lambda lifting
  - defstruct
  - deftype
  - compile a whole file to a single dylib
  - speed up some passes by mutating a single variable instead of copying immutable versions around
  - Clean up unifier even more
  - Clean up lifetime checker code
  - Compiler doesn't catch when a let-binding refers to a variable that's defined later (in the same let binding)
  - :result-name not needed in literals
  - add proper no-op :node for ()

# Lisp Core Libs
  - assert-eq shows wrong result when the assertion fails? (in ffi situations...)
  - -> and ->>
  - shuffle
  - conversions between a list of pairs and dictionaries

# Bugs
  - Didn't show error when registering non-existing C function?!
  
# Dynamic Runtime
  - no need to handle (:ptr ...) as a special case?
  - nil matches anything in a match statement?!
  - read crashes when given data of the wrong type, should check for string type
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


# Compiler
  - Add void constraints for (do ...) statements 
  - Ownership in while loops
  - Ownership tracking to enable returning refs from functions (it's forbidden at the moment)
  - Handle Function pointers as values (re-activate the tests)
  - Compilation of generic functions
  - Compiling arrays
  - Handle global variables referenced inside functions, in regards to the lifetime checker
  - Avoid problems with name shadowing when freeing a local variable (is this possible? disallow shadowing instead?)
  - Track dependencies between functions to enable automatic recompilation when a function changes 
  - lambdas / lambda lifting
  - defstruct
  - deftype
  - compile a whole file to a single dylib
  - speed up some passes by mutating a single variable instead of copying immutable versions around
  - Compiler doesn't catch when a let-binding refers to a variable that's defined later (in the same let binding)
  - add proper no-op :node for ()
  - self recuring function doens't check argument count/types in the actual call to itself
  - rewrite a bunch of functions in the compiler passes using pipe operator and update-in

# Lisp Core Libs
  - assert-eq shows wrong result when the assertion fails? (in ffi situations...)
  - -> and ->>
  - shuffle
  - conversions between a list of pairs and dictionaries
  - 'for' macro with multiple bindings (i, j, etc...)

# Bugs
  
  
# Dynamic Runtime
  - make line numbers and position be actually correct
  - use only one name for env/dict, consolidate the functions manipulating them to one file
  - add one stack frame to the printout that's actually at the location of the error, if possible
  - call stack isn't properly popped when errors occur inside (load-lisp ...) at startup!
  - use [] in parameter list for function definitions
  - register/register-builtin should use the lisp name, not the C name 
  - jump table in evaluator, use a 'dispatch' member with a label adress in Obj
  - remove globals to enable several instances of the runner in parallel
  - nicer pretty printing of lists of lists
  - namespaces
  - don't allow sending compiled functions of wrong type to ffi functions (check their types with 'signature')
  - create pretty printed stack when needed, not always when calling functions
  - profile the evaluator
  - better error handling and input validation for primops, clean up the C macros
  - Fix problem with "No meta data." for some calls in stack trace

# Maybes
  - polymorphic math operators?
  - matching/destructuring in let statements and function arguments too?
  - reading of dotted pairs?
  - primops should have signatures, right?
  - lambdas should be able to have their signature set/get?


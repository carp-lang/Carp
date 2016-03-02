# Compiler 
  - Handle Function pointers as values, must typedef the correct function type
  - Automatically implement for structs and arrays:
     - str
	 - copy
	 - delete
  - Use 'delete' instead of free for memory management
  - Can't declare array literals inside array literals (works with temp variables inside though)
  - Ownership in while loops
  - Handle global variables referenced inside functions
  - Track dependencies between functions to enable automatic recompilation when a function changes 
  - Make let-polymorphism work
  - Ownership tracking to enable returning refs from functions (it's forbidden at the moment)
  - lambdas / lambda lifting
  - compile a whole file to a single dylib
  - Allow map/filter/reduce to take arguments that are boxed void pointers to arrays
  - speed up some passes by mutating a single variable instead of copying immutable versions around
  - Compiler doesn't catch when a let-binding refers to a variable that's defined later (in the same let binding)
  - self recuring function doens't check argument count/types in the actual call to itself
  - rewrite a bunch of functions in the compiler passes using pipe operator and update-in
  - Avoid problems with name shadowing when freeing a local variable (is this possible? disallow shadowing instead?)
  - deftype (tagged unions)
  - clean up the awful 'list-to-ast' function

# Lisp Core Libs
  - assert-eq shows wrong result when the assertion fails? (in ffi situations, the wrong type is produced and compared to something else)
  - -> and ->>
  - shuffle
  - conversions between a list of pairs and dictionaries
  - 'for' macro with multiple bindings (i, j, etc...)

# Bugs
  
  
# Dynamic Runtime
  - Valgrind finds error with strdup in eval.c:312 ('apply' function)
  - Valgrind finds error with realloc in obj_string.c line 17
  - Xcode finds strange error in primops.c 1271
  - Get inferior lisp to work
  - Change () to [] in defn:s
  - be able to mark symbols as "frozen" (with meta data) so that they can't be overriden by user
  - make line numbers and position be actually correct
  - quasiquoting and splicing for easier macro writing
  - add one stack frame to the printout that's actually at the location of the error, if possible
  - call stack isn't properly popped when errors occur inside (load-lisp ...) at startup!
  - modules
  - add 'case'/'cond' macro
  - only allow [] in parameter list for function definitions
  - register/register-builtin should use the lisp name, not the C name 
  - jump table in evaluator, use a 'dispatch' member with a label adress in Obj
  - remove globals to enable several instances of the runner in parallel
  - nicer pretty printing of lists of lists
  - don't leak values returned from calling ffi functions at the repl (but how..?)
  - don't allow sending compiled functions of wrong type to ffi functions (check their types with 'signature')
  - create pretty printed stack when needed, not always when calling functions
  - profile the evaluator
  - better error handling and input validation for primops, clean up the C macros
  - Fix problem with "No meta data." for some calls in stack trace
  - Reader syntax for refs: &
  - The paren_balance function in repl.c can be tricked by parens in strings and unmatched (), [], {}, etc.
  - use modules to solve problem of using same name for members in different structs
  - how to not leak memory in the repl
  - ensure correctness of GC

# Maybes
  - Add void constraints for (do ...) statements ?
  - add proper no-op :node for () ?
  - polymorphic math operators?
  - matching/destructuring in let statements and function arguments too?
  - reading of dotted pairs?
  - primops should have signatures, right?
  - lambdas should be able to have their signature set/get?
  - not possible to write an 'eat-void' function: (register-builtin "eat_void" '(:void) :void), need a proper unit type for that
  - :when clauses in match?

# Niceties
  - Built in tutorial for the language
  - Built in manual

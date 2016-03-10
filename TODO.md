# Compiler
  - Allow dashes in struct member names
  - Need let polymorphism before array literals can be nested
  - Better (shorter, using < and >) names for generic functions
  - Automatically implement for structs and arrays:
	 - copy
	 - delete
  - Use 'delete' instead of free for memory management
  - Can't declare array literals inside array literals (works with temp variables inside though)
  - Ownership in while loops
  - Handle global variables referenced inside functions
  - Make let-polymorphism work
  - Ownership tracking to enable returning refs from functions (it's forbidden at the moment)
  - Lambdas / lambda lifting
  - Compile a whole file to a single dylib
  - Speed up some passes by mutating a single variable instead of copying immutable versions around
  - Compiler doesn't catch when a let-binding refers to a variable that's defined later (in the same let binding)
  - self recuring function doens't check argument count/types in the actual call to itself
  - rewrite a bunch of functions in the compiler passes using pipe operator and update-in
  - Avoid problems with name shadowing when freeing a local variable (is this possible? disallow shadowing instead?)
  - deftype (tagged unions)
  - Clean up the awful 'list-to-ast' function
  - Complete type constraints for binops, check for "numeric" types (use a union type of some sort?)

# Lisp Core Libs
  - assert-eq shows wrong result when the assertion fails? (in ffi situations, the wrong type is produced and compared to something else)
  - -> and ->>
  - shuffle
  - Conversions between a list of pairs and dictionaries
  - 'for' macro with multiple bindings (i, j, etc...)
  - 'case'/'cond' macro

# Bugs
  
  
# Dynamic Runtime
  - Allow map/filter/reduce to take arguments that are boxed void pointers to arrays
  - glfw-demo closes repl second time
  - Should be error when ptr of wrong type is sent to baked function
  - Valgrind finds error with strdup in eval.c:312 ('apply' function)
  - Valgrind finds error with realloc in obj_string.c line 17
  - ^syntax for meta data
  - Get inferior lisp to work
  - Be able to mark symbols/modules as "frozen" (with meta data) so that they can't be overriden by user
  - Quasiquoting and splicing for easier macro writing
  - Modules
  - ONLY allow [] in parameter list for function definitions
  - register/register-builtin should use the lisp name, not the C name 
  - Jump table in evaluator, use a 'dispatch' member with a label adress in Obj
  - Remove globals to enable several instances of the runner in parallel
  - Nicer pretty printing of lists of lists
  - Don't allow sending compiled functions of wrong type to ffi functions (check their types with 'signature')
  - Better error handling and input validation for primops, clean up the C error/assertion macros
  - Fix problem with "No meta data." for some calls in stack trace
  - Reader syntax for refs: &
  - Match rest of list can use '...' instead
  - The paren_balance function in repl.c can be tricked by parens in strings and unmatched (), [], {}, etc.
  - Use modules to solve problem of using same name for members in different structs
  
# Sanity checks
  - Ensure correctness of GC (run at every step)
  - Don't leak values returned from calling ffi functions at the repl (but how..?)
  - Profile the evaluator

# Maybes
  - Add void constraints for (do ...) statements ?
  - Add proper no-op :node for () ?
  - Polymorphic math operators?
  - Matching/destructuring in let statements and function arguments too?
  - Reading of dotted pairs?
  - Not possible to write an 'eat-void' function: (register-builtin "eat_void" '(:void) :void), need a proper unit type for that
  - :when clauses in match?
  - Use a more generalized method for generating 'str' function when inspecting ptr:s at the REPL (some kind of "hook" system)

# Niceties
  - Built in tutorial for the language
  - Built in manual

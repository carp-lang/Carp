# Compiler 
  - Add constraints for struct constructor arguments based on the types in the struct
  - Don't apply borrowing rules to primitive (auto-copyable) types like int/float/bool/char
  - Compile struct member lookups properly
  - Automatically implement for structs and arrays:
     - str
	 - copy
	 - delete
  - Use 'delete' instead of free for memory management
  - Bake can't handle when a function changes signature
  - Can't declare array literals inside array literals (works with temp variables inside though)
  - Ownership in while loops
  - Handle global variables referenced inside functions
  - Make let-polymorphism work
  - Ownership tracking to enable returning refs from functions (it's forbidden at the moment)
  - Lambdas / lambda lifting
  - Compile a whole file to a single dylib
  - Allow map/filter/reduce to take arguments that are boxed void pointers to arrays
  - Speed up some passes by mutating a single variable instead of copying immutable versions around
  - Compiler doesn't catch when a let-binding refers to a variable that's defined later (in the same let binding)
  - self recuring function doens't check argument count/types in the actual call to itself
  - rewrite a bunch of functions in the compiler passes using pipe operator and update-in
  - Avoid problems with name shadowing when freeing a local variable (is this possible? disallow shadowing instead?)
  - deftype (tagged unions)
  - Clean up the awful 'list-to-ast' function

# Lisp Core Libs
  - assert-eq shows wrong result when the assertion fails? (in ffi situations, the wrong type is produced and compared to something else)
  - -> and ->>
  - shuffle
  - Conversions between a list of pairs and dictionaries
  - 'for' macro with multiple bindings (i, j, etc...)
  - 'case'/'cond' macro

# Bugs
  
  
# Dynamic Runtime
  - Valgrind finds error with strdup in eval.c:312 ('apply' function)
  - Valgrind finds error with realloc in obj_string.c line 17
  - Xcode finds strange error in primops.c 1271
  - Binding to a function call in 'let' crashes
  - ^syntax for meta data
  - Get inferior lisp to work
  - Be able to mark symbols/modules as "frozen" (with meta data) so that they can't be overriden by user
  - Quasiquoting and splicing for easier macro writing
  - Modules
  - Only allow [] in parameter list for function definitions
  - register/register-builtin should use the lisp name, not the C name 
  - Jump table in evaluator, use a 'dispatch' member with a label adress in Obj
  - Remove globals to enable several instances of the runner in parallel
  - Nicer pretty printing of lists of lists
  - Don't leak values returned from calling ffi functions at the repl (but how..?)
  - Don't allow sending compiled functions of wrong type to ffi functions (check their types with 'signature')
  - Profile the evaluator
  - Better error handling and input validation for primops, clean up the C error/assertion macros
  - Fix problem with "No meta data." for some calls in stack trace
  - Reader syntax for refs: &
  - The paren_balance function in repl.c can be tricked by parens in strings and unmatched (), [], {}, etc.
  - Use modules to solve problem of using same name for members in different structs
  - How to not leak memory in the repl
  - Ensure correctness of GC

# Maybes
  - Add void constraints for (do ...) statements ?
  - Add proper no-op :node for () ?
  - Polymorphic math operators?
  - Matching/destructuring in let statements and function arguments too?
  - Reading of dotted pairs?
  - Not possible to write an 'eat-void' function: (register-builtin "eat_void" '(:void) :void), need a proper unit type for that
  - :when clauses in match?

# Niceties
  - Built in tutorial for the language
  - Built in manual

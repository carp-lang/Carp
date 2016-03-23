# Compiler Big Features
  - Have priorities for different constraints, sort them by prio
  - Don't compile a function if there's already an adequate build artifact
  - Compile globals so that they can be mutated (and all references refer to the same storage)
  - Collect all unification errors and present them in the end
  - Allow recompiling changed defstructs
  - Structs refering to structs
  - Special handling of POD structs (stack allocated, not sent by pointer)
  - Compile a whole file to a single dylib
  - Lambdas / lambda lifting
  - Compile match statements
  - Generic structs
  - Equality
  - deftype (tagged unions)
  - Option Type (Maybe in Haskell)
  - Err Type (Either in Haskell)
  - Compile global variables with correct initialization and any kind of type
  - Reorder arguements to "set"/"update"-lens to make them less problematic for borrow checking (the main structure is given away to the first argument)

# Compiler Small Features
  - Shorter names for concrete versions of generic functions
  - Be able to compare C-array in pointer to Obj array
  - A deref function that can remove the ref from primitive types?

# Compiler Correctness
  - Disallow "returning" of ref in let-form
  - Ownership in while loops
  - Self recuring function doesn't check argument count/types in the actual call to itself
  - Compiler doesn't catch when a let-binding refers to a variable that's defined later (in the same let binding)
  - Avoid problems with name shadowing when freeing a local variable (is this possible? disallow shadowing instead?)
  - Complete type constraints for binops, check for "numeric" types (use a union type of some sort?). Turn binops into normal funcs?
  - Automatic recompilation doesn't work when depending on concrete instantiation of generic function
  
# Compiler efficiency
  - Avoid creating unique typevars for multiple calls with the same types to a generic function?
  - Rewrite a bunch of functions in the compiler passes using pipe operator and update-in
  - Clean up the awful 'list-to-ast' function
  - Speed up some passes by mutating a single variable instead of copying immutable versions around



# Dynamic Runtime Big Features
  - Join-function blows the stack
  - Macro splicing
  - Use array for macro parameter list
  - Modules 
  - Get inferior lisp to work
  - Remove globals to enable several instances of the runner in parallel
  - Add more numeric types (double, unsigned long?)
  - A Set-type with reader syntax #{}

# Dynamic Runtime Small Features
  - concat doesn't check for arrays, crashes instead
  - Want to be able to send Obj-arrays to ffi functions
  - Allow map/filter/reduce to take arguments that are boxed void pointers to arrays
  - Be able to mark symbols/modules as "frozen" (with meta data) so that they can't be overriden by user
  - register/register-builtin should use the lisp name, not the C name 
  - Better error handling and input validation for primops, clean up the C error/assertion macros
  - ONLY allow [] in parameter list for function definitions
  - Use modules to solve problem of using same name for members in different structs
  - Use size_t where approperiate
  
# Dynamic Runtime Optimization

# Bugs
  - Can send compiled ffi-function to another ffi-function that expects a struct
  - Should be error when ptr of wrong type is sent to baked function
  - Don't allow sending compiled functions of wrong type to ffi functions (check their types with 'signature')
  - The paren_balance function in repl.c can be tricked by parens in strings and unmatched (), [], {}, etc.
  - Valgrind finds error with strdup in eval.c:312 ('apply' function)
  - Valgrind finds error with realloc in obj_string.c line 17
  - glfw-demo closes repl second time

# Sanity checks
  - Ensure correctness of GC (run at every step)
  - Don't leak values returned from calling ffi functions at the repl (but how..?)
  - Profile the evaluator
  
  
  
# Lisp Core Libs
  - 'import' function that searches paths for carp files
  - assert-eq shows wrong result when the assertion fails? (in ffi situations, the wrong type is produced and compared to something else)
  - -> and ->>
  - 'case'/'cond' macro
  - shuffle (for lists)
  - Conversions between a list of pairs and dictionaries
  - 'for' macro with multiple bindings (i, j, etc...)

# Maybes
  - Compile keywords?
  - Add void constraints for (do ...) statements ?
  - Add proper no-op :node for () ?
  - Polymorphic math operators?
  - Matching/destructuring in let statements and function arguments too?
  - Reading of dotted pairs?
  - Not possible to write an 'eat-void' function: (register-builtin "eat_void" '(:void) :void), need a proper unit type for that
  - :when clauses in match?
  - Use a more generalized method for generating 'str' function when inspecting ptr:s at the REPL (some kind of "hook" system)
  - Ownership tracking to enable returning refs from functions (it's forbidden at the moment)

# Niceties
  - Built in tutorial for the language
  - Built in manual

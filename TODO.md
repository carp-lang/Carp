# Compiler Big Features
  - Compile globals so that they can be mutated (and all references refer to the same storage)
  - Compile a whole file to a single dylib
  - Lambdas / lambda lifting
  - enum / union / deftype (tagged unions)
  - Generic structs
  - Allow recompiling changed defstructs
  - Structs refering to other structs
  - Special handling of POD structs (stack allocated, not sent by pointer)
  - Compile match statements
  - Equality
  - Option Type (Maybe in Haskell)
  - Err Type (Either in Haskell)
  - Compile global variables with correct initialization and any kind of type

# Compiler Small Features
  - Reorder arguments to "set"/"update"-lens to make them less problematic for borrow checking (the main structure is given away to the first argument)
  - Shorter names for concrete versions of generic functions
  - Be able to compare C-array in pointer to Obj array
  - A deref function that can remove the ref from primitive types?
  - Use the new key-is-true function instead of has-key?

# Compiler Correctness
  - Disallow "returning" of ref in let-form
  - Ownership in while loops
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
  - Allow varargs to macros
  - Macro splicing
  - Modules 
  - Remove globals to enable several instances of the runner in parallel
  - A Set-type with reader syntax #{}

# Dynamic Runtime Small Features
  - Want to be able to send Obj-arrays to ffi functions
  - Allow map/filter/reduce to take arguments that are boxed void pointers to arrays
  - Be able to mark symbols/modules as "frozen" (with meta data) so that they can't be overriden by user
  - Better error handling and input validation for primops, clean up the C error/assertion macros
  - ONLY allow [] in parameter list for function definitions
  - Use modules to solve problem of using same name for members in different structs
  - Use size_t where approperiate
  
# Dynamic Runtime Optimization

# Bugs
  - Don't allow sending compiled functions of wrong type to ffi functions (check their types with 'signature')
  - The paren_balance function in repl.c can be tricked by parens in strings and unmatched (), [], {}, etc.

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
  - if-let function

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

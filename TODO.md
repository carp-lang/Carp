# Compiler Big Features
  - Allow non-ref types for all generic primops IF the type is builtin and don't need managing.
  - Compile "or"
  - Special handling of POD structs (stack allocated, not sent by pointer)
  - Generic structs
  - Compile match statements
  - All types should have capital first letter
  - Lambdas / lambda lifting
  
# Compiler Small Features
  - a 'map-primitive' that can map a function that takes non-refs as argument since it's annying to not be able to use functions like 'itos' directly with 'map-copy' (it requires a fn of type &a -> b)
  - Reorder arguments to "set"/"update"-lens to make them less problematic for borrow checking (the main structure is given away to the first argument)
  - Shorter names for concrete versions of generic functions
  - A deref function that can remove the ref from primitive types?
  - Use the new key-is-true function instead of has-key? in lots of places

# Compiler Correctness
  - Variables named the same thing as a struct can override the dylib generated for the struct group.
  - Use 'generic-name' when concretesizing generic primops
  - Compiler doesn't catch when a let-binding refers to a variable that's defined later (in the same let binding)
  - Avoid problems with name shadowing when freeing a local variable (is this possible? disallow shadowing instead?)
  - Complete type constraints for binops, check for "numeric" types (use a union type of some sort?). Turn binops into normal funcs?
  - Automatic recompilation doesn't work when depending on concrete instantiation of generic function
  
# Compiler efficiency
  - Avoid creating unique typevars for multiple calls with the same types to a generic function?
  - Rewrite a bunch of functions in the compiler passes using pipe operator and update-in
  - Speed up some passes by mutating a single variable instead of copying immutable versions around



# Dynamic Runtime Big Features
  - Macro splicing
  - Modules 
  - A Set-type with reader syntax #{}
  - Instantiate generic functions like '=' for primitive types when calling them
  
## Modules
  - Name
  - List of imported modules (with the name used for importation)
  - List of opened modules
  - Environment (with all the bindings)

# Dynamic Runtime Small Features
  - Delete content of global var when resetting from repl
  - Be able to mark symbols/modules as "frozen" (with meta data) so that they can't be overriden by user
  - Better error handling and input validation for primops, clean up the C error/assertion macros
  - ONLY allow [] in parameter list for function definitions
  - Use modules to solve problem of using same name for members in different structs
  - Use size_t where approperiate
  - Make the reader warn when the text to read is too big (repl.c)
  - Resetting a global variable pointing to an array can be fooled by using wrong kind of array (anything goes at the moment)
  
# Dynamic Runtime Optimization

# Bugs
  - Don't allow sending compiled functions of wrong type to ffi functions (check their types with 'signature')
  - assert-eq shows wrong result when the assertion fails? (in ffi situations, the wrong type is produced and compared to something else)
  
# Sanity checks
  - Ensure correctness of GC (run at every step)
  - Don't leak values returned from calling ffi functions at the repl (but how..?)
  - Profile the evaluator
  
  
  
# Lisp Core Libs
  - 'import' function that searches paths for carp files
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

# Gotchas
  - Unloading of function/dylib doesn't work after another function has linked to it during its compilation.
  - Variable shadowing doesn't work properly when referencing itself

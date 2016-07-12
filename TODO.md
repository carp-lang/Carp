# The Big 'ref' debacle - alternatives:
  1. Allow ref:ed value types to be coerced into non-ref:ed types
  2. A deref function that can remove the ref from primitive types
  3. A 'map-primitive' that can map a function that takes non-refs as argument 
     since it's annying to not be able to use functions like 'itos' directly with 'map-copy' 
     (it requires a fn of type &a -> b)

# Compiler Big Features
  - Live Reloading (requires threads and bytecode interpreter)
  - Special handling of POD structs (stack allocated, referenced by pointer)
  - Compile match statements (should it be a macro?)
  - Compile modules (when they exist in the dynamic runtime...)
  - Compile dictionaries (requires hashing function)
  - Lambdas
  - get / set special forms?

# Bytecode
  - Print bytecode properly (the jump instructions destroy the "stringiness" of the byte array
  - Make bytecode 'match' use labels and gotos instead of recursive calls to eval

# Compiler Small Features
  - Shorter names for concrete versions of generic functions
  - All types should have capital first letter?
  - Be able to save concretized struct types for type checking etc

# Compiler Correctness
  - Variables/functions named the same thing as a struct can override the dylib generated for the struct group.
  - Must unload all concretized structs when the parent struct is redefined
  - Compiler doesn't catch when a let-binding refers to a variable that's defined later (in the same let binding)
  - Avoid problems with name shadowing when freeing a local variable (is this possible? disallow shadowing instead?)
  - Complete type constraints for binops, check for "numeric" types (use a union type of some sort?). Or turn binops into normal funcs?
  
# Compiler efficiency / beauty
  - Avoid creating unique typevars for multiple calls with the same types to a generic function?
  - Use 'sicp unifier' instead of the current mess
  - Use 'generic-name' when concretizing generic primops
  - Rewrite a bunch of functions in the compiler passes using pipe operator and update-in
  - Speed up some passes by mutating a single variable instead of copying immutable versions around
  - Use the new key-is-true function instead of has-key? in lots of places
  - Calls back to the compiler from runtime should be minimized and only require a single call, not two or three like it often is now

# Dynamic Runtime Big Features
  - Desugar [...] to (array ...) in reader
  - Macro splicing
  - Modules 
  - A Set-type with reader syntax #{}
  - Instantiate generic functions like '=' for primitive types when calling them
  - Line numbers for dictionary literals
  
# Modules
  - Name
  - List of imported modules (with the name used for importation)
  - List of opened modules
  - Environment (with all the bindings)

# Dynamic Runtime Small Features
  - Delete content of global var when resetting from repl
  - Be able to mark symbols/modules as "frozen" (with meta data) so that they can't be overriden by user
  - Better error handling and input validation for primops, clean up the C error/assertion macros
  - ONLY allow [] in parameter list for function definitions
  - Use size_t where approperiate
  - Make the reader warn when the text to read is too big (repl.c)
  - Resetting a global variable pointing to an array can be fooled by using wrong kind of array (anything goes at the moment)
  
# Dynamic Runtime Optimization

# Bugs
  - Don't allow sending compiled functions of wrong type to ffi functions (check their types with 'signature')
  - assert-eq shows wrong result when the assertion fails? (in ffi situations, the wrong type is produced and compared to something else)
  
# Sanity checks
  - Ensure correctness of GC (run at every step)
  - Don't leak values returned from calling ffi functions via non-compiled code
  
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
  - Reorder arguments to "set"/"update"-lens to make them less problematic for borrow checking (the main structure is given away to the first argument) ?
  - Use modules to solve problem of using same name for members in different structs?
  - Create a carp_bool type with defined size

# Niceties
  - Built in tutorial for the language
  - Built in manual

# Gotchas
  - Unloading of function/dylib doesn't work after another function has linked to it during its compilation.
  - Variable shadowing doesn't work properly when referencing itself
  - Size of bool is undefined
  - Must mark global variables on windows as __declspec(dllimport) when using them and __declspec(dllexport) when providing for others

# Dynamic Runtime
  - meta data on Objs: Line nr, line pos, source file,
  - warn if a let statement has more than one body form (implicit do)
  - add array as its own tag for Obj, [] syntax, etc
  - split c program into several files: Runtime, Reader, Printer, Evaluator, Primops, Obj, Gc
  - jump table in evaluator, use a 'dispatch' member with a label adress in Obj
  - primops should have signatures
  - remove globals to enable several instances of the runner in parallel
  - nicer pretty printing of lists of lists
  - better error handling and input validation for primops, clean up the C macros
  - lambdas should be able to have their signature set/get
  - profile the evaluator
  - namespaces
  - equality for dictionaries

# Maybes
  - polymorphic math operators?
  - matching/destructuring in let statements and function arguments too?
  - reading of dotted pairs

# Compiler
  - Track dependencies between functions
  - Change :a and :b in binop and if to :left and :right
  - Compilation of generic functions
  - lambdas / lambda lifting
  - compile a whole file to a single dylib
  - nicer names for compiler generated variables
  - speed up some passes by mutating a single variable instead of copying immutable versions around


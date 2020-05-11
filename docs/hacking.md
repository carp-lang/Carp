# Hacking the Carp Compiler

This doc contains various tips and tricks, notes, explanations and examples
that can help you make changes to the Carp compiler. Be forewarned that it's
not an exhuasitve guide book, and likely will remain a hodgepodge of
accumulated remarks, observations and hints contributed by people that have
modified the compiler in the past.

> Note: General familiarity with compilers and compilation terminology is
> assumed.

## Structure

The Carp compiler source lives in the `src/` directory. Carp is, roughly
speaking, oragnized into four primary passes or components:

![carp compiler phases](../compiler-passes.svg)

Each source file plays a part in one or more components/phases in the compiler.
The sections below breifly describe the purpose of each stage and list
important source files. You can use these sections to get a rough idea of what
files you might need to edit in order to alter the functionality of a
particular phase.
 
> Note: Some sources contain defintions that are important or used in pretty
> much every phase of the compiler, in result some files may appear more than
> once in the sections below.

### Parsing

The parsing phase translates `.carp` source files into abstract syntax trees
(AST). In carp, AST nodes are represented using an abstract data type called
`XObj`. `XObj`s are ubiquitous across the compiler and are used in serveral
different phases and contexts. Every `XObj` consists of:

- An `Obj` which is the representation of some carp source code as an abstract
  data type
- `Info`: which contains additional information about the source code that
  generated an `Obj` (e.g. its location in a source file)
- `Ty`: An valid carp Type for the `Obj`, as determined by the [type
  system](#type-system).

The following sources are important for parsing:

- `Parsing.hs` -- parsing logic that translates carp source code into abstract
  syntax.
- `XObj.hs` -- defines the valid Carp AST nodes.

### Dynamic Evaluator

As stated in the [Macro guide](Macros.md#inner-workings) the dynamic evaluator
is the central component in the compiler. As the name suggests, the evaluator
evaluates parsed carp code (`XObjs`) and prepares it for
[emission](#code-emission). Evaluation entails:

- Expanding macros and dynamic functions
- Resolving bindings to other forms
- Requesting type inference for forms
- Requesting borrow checking for forms

In addition to the `XObjs` corresponding to the source file being compiled, the
evauator relies on a `Context`--`Context` is a global object that contains
state for the compiler. The compiler's `Context` is comprised of several
environments, defined by the `Env` type--which hold references to known
bindings. Different environments are used by different pahses of the compiler
to evaluate forms, resolve types, and, generally speaking prepare code for
emission.

`Binders` are another important abstract data type used in evaluation. Any
value that's bound to a name in a source program is translated into a `binder`,
which is comprised of the `XObj` of the form bound to the name, as well as
additional metadata for the binding. `Binders` are added to the environments in
the `Context`.

The following sources are important for evaluation:

- `src/Eval.hs` -- this is the entry point for the evalautor.
- `src/Obj.hs` -- Defines `Context` which carries compiler state across
  evaluation passes in the form of `Env`s, defines `Env` which holds `Binders`
from names to `XObjs`.
- `src/Primitives.hs` -- builtin functions or "keywords" that **do not**
  evaluate their arguments
- `src/Commands.hs` -- builtin functions or "keywords" that evalaute their
  arguments
- `src/StartingEnv.hs` -- defines the starting environement for the compiler to
  work with. All commands and primitives are registered here, so that
evaluation passes can use them.
- `Lookup.hs` -- Functions for looking up `Binders` in a given environment
  (`Env`).
- `Expand.hs` -- Fucntions for expanding macro forms.
- `Infer.hs` -- Functions for performing type inference--entry point into the
  type system.
- `Qualify` -- Qualifies symbols with appropriate module names.

Some other peices of the type system and borrow checking mechanisms could be
included in this list as well, but this list captures the core functionality
related to evaluation. Generlaly speaking, the evaluation component is the
conductor of our compilation smphony and orchestrates all the other parts of
the compiler.

> Note: For a more in depth look at the dynamic evaluator, see [the section on
> inner workings in the Macro guide](Macros.md#inner-workings)

### Type System

The type system is responsible for checking the types of Carp forms and
ensuring programs are type safe. It also supports polymorphism and is
reponsible for replacing polymorphic types with concrete types.

Carp types are represented by the `Ty` data type.

The following sources are important for the type system:

- `Types.hs` -- defines the `Ty` data type, which represents valid carp types.
  Also contains unification checking code to determine whether or not two types
are compatible. Also contains mangling code, that translates carp type names
with valid C identifiers.
- `TypeError.hs` -- defines type checking errors.
- `AssignTypes.hs` -- Assigns concrete types to variables.
- `Polymorphism.hs` -- Given a concretized polymorphic function, determines the
  correct valid C identifier for the concrete function.
- `Validate.hs` -- Checks that user-defined types are valid.
- `Constraints.hs` --  Determines and solves constraints between types and type
  variables in an enviornment.
- `Concretize.hs` --  Transforms forms that involve polymorphic types into
  concrete types.
- `InitialTypes.hs` -- determines the initial type of a given `XObj` (AST
  node).
- `GenerateConstraints.hs` -- determines type constraints for a given form.

### Borrow Checking/Ownership System

Borrow checking an lifetime parameters are an extension of the [type
system](#type-system). All of the files that are important to the type system
are likewise important for the borrow checker.

### Code Emission

The compiler's final job is to emit C code corresponding to the source Carp
input. Emission relies heavily on the concept of `Templates` -- effectively a
structured way to generate C strings based on evaluated Carp AST nodes.

The following sources are important for the code emission system:

- `ArrayTemplates.hs` -- Templates for C code corresponding to Array use in
  Carp.
- `StaticArrayTemplates.hs` -- Templates for C code corresponding to
  StaticArray use in Carp.
- `Deftype.hs` -- Templates for C code correpsonding to user defined structs in
  Carp (aka product types) (also contains some other logic for registering
bindings for such types).
- `Sumtypes.hs` -- Templates for C code correpsonding to user defined sumtypes
  in Carp (also contains some other logic for registering bindings for such
types).
- `StructUtils.hs` -- Templates for C code corresponding to utility functions
  for Carp structs.
- `Template.hs` -- General compiler instructions for generating C code.
- `ToTemplate.hs` -- Helper for creating templates from strings of C code.
- `Scoring.hs` -- determines an appropriate sort order for emitted C bindings
  based on typing and `XObj` information.
- `Emit.hs` -- Emits generated C code based on evaluated, compiled Carp source
  code.

### Other sources

In addition to the sources listed above, there are other miscellaneous source
files that serve different purposes in the compiler:

- `Repl.hs` -- defines repl functionality, such as keyword completion, repl
  commands, etc.
- `Util.hs` -- various utility functions
- `ColorText.hs` -- supports colored output in the Repl/compiler output.
- `Path.hs` -- Filepath manipulation functions.
- `RenderDocs.hs` -- Functionality for generating documentation from annotated
  carp Code.


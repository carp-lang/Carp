# Hacking the Carp Compiler

This doc contains various tips and tricks, notes, explanations and examples
that can help you make changes to the Carp compiler. Be forewarned that it's
not an exhaustive guide book, and likely will remain a hodgepodge of
accumulated remarks, observations and hints contributed by people that have
modified the compiler in the past.

> Note: General familiarity with compilers and compilation terminology is
> assumed.

## Structure

The Carp compiler source lives in the `src/` directory. Carp is, roughly
speaking, organized into four primary passes or components:

![carp compiler phases](./compiler-passes.svg)

Each source file plays a part in one or more components/phases in the compiler.
The sections below briefly describe the purpose of each stage and list
important source files. You can use these sections to get a rough idea of what
files you might need to edit in order to alter the functionality of a
particular phase.

> Note: Some sources contain definitions that are important or used in pretty
> much every phase of the compiler, in result some files may appear more than
> once in the sections below.

### Parsing

The parsing phase translates `.carp` source files into abstract syntax trees
(AST). In carp, AST nodes are represented using an abstract data type called
`XObj`. `XObj`s are ubiquitous across the compiler and are used in several
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
evaluator relies on a `Context`--`Context` is a global object that contains
state for the compiler. The compiler's `Context` is comprised of several
environments, defined by the `Env` type--which hold references to known
bindings. Different environments are used by different phases of the compiler
to evaluate forms, resolve types, and, generally speaking prepare code for
emission.

`Binders` are another important abstract data type used in evaluation. Any
value that's bound to a name in a source program is translated into a `binder`,
which is comprised of the `XObj` of the form bound to the name, as well as
additional metadata for the binding. `Binders` are added to the environments in
the `Context`.

The following sources are important for evaluation:

- `Eval.hs` -- this is the entry point for the evaluator.
- `Obj.hs` -- Defines `Context` which carries compiler state across
  evaluation passes in the form of `Env`s, defines `Env` which holds `Binders`
from names to `XObjs`.
- `Primitives.hs` -- builtin functions or "keywords" that **do not**
  evaluate their arguments
- `Commands.hs` -- builtin functions or "keywords" that evaluate their
  arguments
- `StartingEnv.hs` -- defines the starting environment for the compiler to
  work with. All commands and primitives are registered here, so that
evaluation passes can use them.
- `Lookup.hs` -- Functions for looking up `Binders` in a given environment
  (`Env`).
- `Expand.hs` -- Functions for traversing forms and doing syntactic analysis.
   Historically also expanded macros (that functionality was moved into
   `Eval.macroExpand`).
- `Infer.hs` -- Functions for performing type inference -- entry point into the
  type system.
- `Qualify` -- Qualifies symbols with appropriate module names.

Some other pieces of the type system and borrow checking mechanisms could be
included in this list as well, but this list captures the core functionality
related to evaluation. Generally speaking, the evaluation component is the
conductor of our compilation symphony and orchestrates all the other parts of
the compiler.

> Note: For a more in depth look at the dynamic evaluator, see [the section on
> inner workings in the Macro guide](Macros.md#inner-workings)

### Type System

The type system is responsible for checking the types of Carp forms and
ensuring programs are type safe. It also supports polymorphism and is
responsible for replacing polymorphic types with concrete types.

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
  variables in an environment.
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
- `Deftype.hs` -- Templates for C code corresponding to user defined structs in
  Carp (aka product types) (also contains some other logic for registering
bindings for such types).
- `Sumtypes.hs` -- Templates for C code corresponding to user defined sumtypes
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

## Mini HowTos

Select compiler changes are more frequent than others and have common
high-level steps. The following sections provide some guidance on making such
changes.

### Adding a new Primitive

If it doesn't require anything fancy or out of the ordinary, adding a new
primitive to the compiler entails the following:

1. Define your new primitive in `Primitives.hs`
2. Add your primitive to the starting environment using `makePrim` in
  `StartingEnv.hs`

#### Define your Primitive

Primitives are functions of the `Primitive` type:

```
type Primitive = XObj -> Context -> [XObj] -> IO (Context, Either EvalError XObj)
```

Every primitive takes an xobj, the form that represents the primitive, a
compiler context, and a list of XObjs the primitive form's arguments.
Primitives return a new `Context`, updated based on the logic they performed,
and either an XObj or evaluation error that's reported to the user.

For example, here's how the `defmodule` primitive maps to the `Primitive` type:

```
(defmodule Foo (defn bar [] 1))
 |         |-----------------|
 XObj      [XObj] (arguments)
```

> The `Context` argument captures the state of the compiler and doesn't have a
> corresponding direct representation in Carp forms.

In `Primitives.hs`, you should name your primitive using the naming scheme
`primitive<name>`, where `<name>` is the name of the symbol that will call your
primitive in Carp code. For example, `defmodule` is given by the primitive
`primitiveDefmodule`.

Most of the time, primitives have three core steps:

- Pattern match on their argument XObjs
- Lookup existing binders in the current `Context`
- Perform some logic based on the type of argument XObjs, then update the
  `Context` as needed.

Let's step through each of these core steps by implementing a simple
`immutable` primitive. The `immutable` primitive will take a variable (the name
of a form passed to a `def`) and mark it as `immutable`, preventing users from
calling `set!` on it.

- Step 1. Pattern match on arguments.

  First thing's first, our primitive, in carp code, should look like this:

  ```
  (immutable my-var)
  ```

  This means that our primitive should only take a single argument XObj, and
  that argument should be a `Sym`.

  Let's match some patterns:

  ```
  primitiveImmutable :: Primitive -- our new primitive
  primitiveImmutable xobj ctx [XObj (Sym path@(SymPath) _)] =
    -- TODO: Implement me!
  primitiveImmutable _ _ xobjs = -- any other number or types of xobj arguments are incorrect! Let's error.
    return $ evalError ctx ("`immutable` expected a single symbol argument, but got" ++ show xobjs) (info xobj)
  ```

  And that's all we need to do to pattern match!

- Step 2. Lookup binders in the current context

  Assuming `immutable` gets a correct argument, our next step is to use the
  `Sym` XObj it received to find out if the symbol is bound to a variable or not.

  `Lookup.hs` defines functions for looking up bindings in the various
  environments contained in a context. We'll call lookup functions to check
  whether or not the symbol argument we get is bound to a `def` form (in which
  case it's a variable). If the symbol isn't bound to a `def` we'll error.


  So, we'll get the binding for our argument (a `Binder`), match against the
  binding's `XObj` and continue working only if it's a `def`.

  ```
  primitiveImmutable :: Primitive -- our new primitive
  primitiveImmutable xobj ctx [XObj (Sym path@(SymPath) _)] =
    let global = contextGlobalEnv ctx
        binding = lookupInEnv path global
    in  case binding of
          Just (_, Binder meta (XObj )) -> -- TODO: This is a def! Great. Do more work here.
          _ -> -- anything that isn't a def; error
            return $ evalError ctx ("`immutable` expects a variable as an argument") (info xobj)
  primitiveImmutable _ _ xobjs = -- any other number or types of xobj arguments are incorrect! Let's error.
    return $ evalError ctx ("`immutable` expected a single symbol argument, but got" ++ show xobjs) (info xobj)
  ```

- Step 3. Perform logic; update the `Context`

  Finally, now that we're certain we've got a def, we'll just perform our
  special logic then update the context with our modified binder.

  To keep things simple, all we'll do in this primitive is update the binder's
  `MetaData` with a new key called `immutable` set to `true`. We can later use
  the value of this meta field to prevent calls to `set!`.

  ```
  primitiveImmutable :: Primitive -- our new primitive
  primitiveImmutable xobj ctx [XObj (Sym path@(SymPath) _)] =
    let global = contextGlobalEnv ctx
        binding = lookupInEnv path global
    in  case binding of
          Just (_, Binder meta def@(XObj Def _ _)) ->
            let oldMeta = getMeta meta
                newMeta = meta {getMeta = Map.insert "immutable" trueXObj oldMeta}  -- update the binder metadata
            in  return $ ctx {contextGlobalEnv = Env (envInsertAt global path (Binder newMeta def))} -- update the context with the binder and it's new meta and return
          _ -> -- anything that isn't a def; error
            return $ evalError ctx ("`immutable` expects a variable as an argument") (info xobj)
  primitiveImmutable _ _ xobjs = -- any other number or types of xobj arguments are incorrect! Let's error.
    return $ evalError ctx ("`immutable` expected a single symbol argument, but got" ++ show xobjs) (info xobj)
  ```

And that wraps up the core logic of our primitive. To make it available, we
just need to register it in `StartingEnv.hs`.


#### Add your primitive to the starting environment

To add a primitive to the starting environment, call `makePrim`:

```
, makePrim "immutable" 1 "annotates a variable as immutable" "(immutable my-var)" primitiveImmutable
```

That's about it. Note that this implementation just adds special metadata to
bindings--to actually prevent users from calling `set!` on an immutable `def`
we'd need to update `set!`'s logic to check for the presence of the `immutable`
metadata.

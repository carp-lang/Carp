# Embedded

Programming for embedded devices is a bit like living in the desert. Everything
is scarce, you have to constantly ration, and you better stay out of the sun.

This document aims to be a guide for how to ration with Carp. To do that, we
first have to identify what resource it is that we’re most concerned about: is
it executable size? Do we have timing-critical code? Do we want to avoid
allocations? Once you have an answer to those questions, this guide can help
you get there.

Because Carp compiles to C, a lot of the same considerations, tricks, and
reasoning apply to Carp. If you already know what flags you need to get the job
done, great! Carp will probably work with them out of the box. Still, getting
acquainted with the tools it provides might help you have an easier time
getting productive.

## A picture is worth...

Just to give you an idea of what is possible, here's a picture to whet your appetite:
<img src="carp_on_arduboy.jpg">


## Fundamentals

In order to tame the compiler to do as you tell it to, a firm grasp on the
configuration options it gives you is in order. This section aims to help you
get an overview of what you can do to make your project compile.

### Compiler

There are a few dynamic functions for you to peruse to instruct the compiler.
Here is a list of them:

```clojure
; tells Carp what C compiler executable to use
(Project.config "compiler" "mycompiler")

; tells Carp to add this flag to the compiler invocation
(add-cflag "-myflag")

; tells Carp to add this library flag to the compiler invocation
(add-lib "-mylibflag")

; tells Carp to run pkg-config for the libs and cflags of a library
(add-pkg "mypkg")
```

### Cross-compiling

On embedded systems it's quite usual to use cross-compilers. See the
cross-compiling section of the Manual for details on how to use a
cross-compiler.

### Compile-time conditional code

There are some macros to help you find out stuff about the host system you are
compiling on.

Here are a few helpful functions to get you started:

```clojure
; will return the host OS
(host-os)

; will return the host architecture bit width (e.g. 32 or 64 bit)
(host-bit-width)
```

Most of the time you'll be interested in the target platform details
instead.

```clojure
; will return the target architecture
(target-arch)

; will return the target OS
(target-os)

; will return the target ABI
(target-abi)

```

There're some macros for conditional code in `Macros.carp`. If your
target doesn't have an underlying OS you'll probably want to roll your
own macros for a `freestanding` target.


### The Way Out

Sometimes you have to do funky stuff like using your own linker scripts and
other such tricks. If it comes to that, it’s often best to just instruct Carp
to generate the C only, and deal with it yourself from there. This can be
achieved by telling Carp to `--generate-only`.

Sometimes you will even have to exclude some files that are usually loaded by
the prelude from loading at all and instead generating your own core load file.
This can be done by using `--no-core`. You can then use [the default prelude as
a template for your own](https://github.com/carp-lang/Carp/blob/master/core/Core.carp).

## What to optimize for

### Binary size

Binary size is something that Carp does not optimizie for by default. You can
usually shave off a fairly large amount of memory by using [Link Time
Optimization (LTO)](https://wiki.debian.org/LTO) and telling your compiler to
optimize for size (if you are using GCC or Clang, `(add-cflag "-Os")` will do
the trick).

### Speed

Often, speed is not as important as other factors might be. Still, using the
highest optimization setting (often `-O3`, together with the Carp flag
`--optimize`) might be appropriate if you need to squeeze out those extra
milliseconds. This is of course not a catch-all: speed is usually more about
how your code is structured than what the compiler does. If you avoid
allocations, copies, and cache misses, that will probably do more for speed
than optimizers ever could.

### Allocations

There are a few tricks for avoiding allocations. Literal strings are not
allocated but embedded in the binary by default, and if you don’t have to touch
them for a copy, this can be golden. Likewise, there are static arrays (using
the literal `$[]`) which will avoid you having to allocate. Their size and
structure must be known at compile-time, however.

To log memory allocations during development and debugging, pass `--log-memory`
to the Carp compiler and put the form `(Debug.log-memory-balance! true)` at the
beginning of your program. This will log all allocations for you, helping you
track down any stray allocations that might happen without your knowledge.

# Installation

This document describes how to build and install the Carp compiler from the
compiler source repository.

## Repository Releases

We do not currently release precompiled binaries of the Carp compiler. You'll
need to build the compiler from source in order to install and use it on your
system. You can either clone the respository from the current head commit, or
see [the releases page](https://github.com/carp-lang/Carp/releases) to download
archives of the source pinned to tagged releases.

## Prerequisites

Building and using the Carp compiler requires that you install the following
prerequisites on your system:

- [Stack](https://docs.haskellstack.org/en/v1.0.0/index.html): the build system
used to build the Carp compiler from source.
- A C Compiler: the carp compiler calls external C compilers to build emitted C
code into an executable. You will need at least one C compiler on your system
for use with Carp emitted C code.

## Building the Carp Compiler from Source

The Carp compiler is written in Haskell and built using [Stack](https://docs.haskellstack.org/en/v1.0.0/index.html). To build the compiler, do the following:

1. Make sure you have a recent version of [Stack](https://docs.haskellstack.org/en/stable/README/) installed.
2. Clone the Carp repository to your machine, or download a release.
3. To build the Carp compiler, run `stack build` in the root of the project directory.

   For example, if you cloned the repository to `~/dev/Carp`, you'd run the
   following sequence of commands:

   ```sh
   cd ~/dev/Carp
   stack build
   ```

If the build succeeds, it will create a binary named `carp`. The `carp` binary
is a combined compiler and interactive read-eval-print-loop (REPL) for the Carp
language. By default, stack places the built compiler into a local
`.stack-work/` directory. To make the compiler available from anywhere on your
system, you can either copy the built artifact to a location in your `$PATH` or
run `stack install`. Stack will copy the compiler to the local binary directory
specified in your Stack settings (typically `~/.local/bin`). You'll need to
ensure this directory is included in your `$PATH` environment variable, for
example `export PATH=~/.local/bin:$PATH`

> [!IMPORTANT]
> Do not delete the Carp source repository after installing `carp`. The source
also contains the Carp core libraries, which you'll need if you want to use the
language-level core libraries, rather than provide your own. 

> [!NOTE]
> For more information on using Stack, consult the [official stack documentation](https://docs.haskellstack.org/en/v1.0.0/index.html). 

## Configuring the Compiler and REPL

You can specify a few configuration settings to customize the behavior of the
Carp compiler. Most users will, at minimum, want to set the location of Carp
core libraries, set up UTF-8 REPL compatibility, and (optionally) specify a C
compiler. These settings are described in the sections that follow.

## Setting the Core Library Location: CARP_DIR

To run the `carp` compiler from anywhere on your system, the compiler must know
where to find language core libraries and other compile-time files. The compiler
uses the `CARP_DIR` environment variable as the location in which to look for
core library sources.

The language core libraries are currently bundled with the compiler source in
the Carp source repository under the `core/` directory.

Set the environment variable `CARP_DIR` so that it points to the root of the
Carp repo.

For example, if you cloned the repository to `~/dev/Carp`, you'd add the
following snippet to your `.bashrc` or other shell run configuration:

```bash
export CARP_DIR=~/dev/Carp/
```

You should now be able to run the Carp compiler or REPL from any directory. The
following command will start the Carp REPL:

```bash
$ carp
```

> [!NOTE]
> You can also run the compiler and REPL without using the core libraries. To do
this, run `carp` with the `--no-core` flag: `carp --no-core`

### Setting up UTF-8 Character Support in the REPL in POSIX Environments

The Carp compiler and core libraries handle UTF-8 encoded text natively, the
REPL, however, does not process extended UTF-8 inputs (such as emoji) by
default. To enable support for inputing UTF-8 characters while using the REPL in
POSIX environments (such as Linux, macOS, and more), you must set the `LC_CTYPE`
environment variable.

For example, adding the following snippet to your `.bashrc` or other shell run
configuration will enable the REPL to handle UTF-8 input:

```bash
export LC_CTYPE=C.UTF-8
```

> [!NOTE]
> The environment variable `LC_ALL`, when set, overrides the value of
`LC_CTYPE`. So you may want to `unset` `LC_ALL` or to set and export it to an
UTF-8 aware value.  You can see the values of `LC_ALL` and `LC_CTYPE` with the
command `locale`.

### Specifying a C Compiler

The `carp` compiler compiles Carp code into a single C source file, `main.c` and
attempts to compile it using an external C compiler. The C compiler that `carp`
attempts to use is configurable. On macOS and Linux it defaults to the `clang`
compiler. 

> [!Note]
> If you have not installed `clang` on your system and do not specify an
alternative compiler, the C compilation step will fail. Ensure you have
installed clang before attempting to compile and run Carp code, or specify an
alternate compiler (see below).

On Windows the default C compiler used by Carp is `clang-cl.exe` which compiles
the code using Clang but links it with the Visual Studio linker[^1].

> [!Tip] 
> Use the package manager [Scoop](https://scoop.sh/) to install LLVM for an easy
way to set this up on Windows. Also make sure you have Visual Studio with the
C/C++ addon installed. Please note that you *don't* need WSL (Windows Subsystem
for Linux) to use Carp.

If you want to use another C compiler, you can configure the exact C compilation
command that the Carp compiler invokes in your project Carp source. For example,
the following code will instruct the Carp compiler to use the `gcc` compiler,
with flag `--important-flag` when compiling C output:

```clojure
(Project.config "compiler" "gcc --important-flag")
```

## Compilation on Windows

When compiling files with `carp` on Windows you must ensure that:

- the file is encoded either as ANSI or UTF-8. (Using another encoding like
UTF-8-BOM doesn't work.)
- using either Unix (LF) or Windows (CR LF) as linefeed/newline. (Using
Macintosh (CR) as newline doesn't work.)

## Installing Additional Libraries for Examples (SDL, GLFW, etc.)

The Carp respository comes with a number of examples illustrating usage of the
language in the `examples/` directory. The examples involving
graphics/sound/interaction require the following libraries to be installed on
your system:

* [SDL 2](https://www.libsdl.org/download-2.0.php) (cross platform game/interactivity library)
* [SDL_image 2](https://www.libsdl.org/projects/SDL_image/) (image helpers)
* [SDL_ttf 2](https://www.libsdl.org/projects/SDL_ttf/) (font rendering)
* [SDL_mixer 2](https://www.libsdl.org/projects/SDL_mixer/) (audio playback)
* [glfw](http://www.glfw.org) (Create a rendering context for OpenGL or Vulcan)

On macOS and Linux we use
[pkg-config](https://www.freedesktop.org/wiki/Software/pkg-config/) to handle
include paths and linking flags, so make sure you have that properly installed
and configured to find the external libraries.

Please let us know if you have trouble getting these bindings to work! We have
tried making everything as reliable as possible but there are often corner cases
when it comes to dependency management. And remember that you're always welcome
to start an issue or ask questions in [the gitter
channel](https://gitter.im/carp-lang/Carp).

[^1]: You can install clang with mingw64 but you'll also want to run
`vcvarsall.bat amd64` or `vcvarsall.bat x86` each time you start your shell to
help clang find the right headers.  See
https://github.com/carp-lang/Carp/issues/700 or
https://github.com/carp-lang/Carp/issues/1323 for more information.

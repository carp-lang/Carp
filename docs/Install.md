# Installation

## Latest release

See [https://github.com/carp-lang/Carp/releases](https://github.com/carp-lang/Carp/releases).

## Building the Carp executable from source

1. Make sure you have a recent version of [Stack](https://docs.haskellstack.org/en/stable/README/) installed.
2. Clone this repo to your machine.
3. Run ```stack build``` in the root of the project directory.
4. ```stack install``` will install the Carp command line tool for easy access on your system.
5. Make sure that the directory where stack installs executables is on your PATH, i.e: ```export PATH=~/.local/bin:$PATH```.

## Setting the CARP_DIR

To be able to run `carp` from anywhere on you system, the executable must know where to find its core libraries and other files.
Set the environment variable CARP_DIR so that it points to the root of the Carp repo.

For example, add this to your `.bashrc` or similar:

```bash
export CARP_DIR=~/Carp/
```

You should now be able to start Carp from anywhere:

```bash
$ carp
```

## Ensuring an UTF-8 aware LC_CTYPE locale in POSIX environments

To be able to handle UTF-8 correctly when using `carp`'s interactive repl (binaries from `carp` always handle UTF-8 correctly),
all POSIX aware environments (Linux, MacOs or even Emacs's eshell inside a Windows 10)
need to have an `LC_CTYPE` environment variable set and exported to an UTF-8 aware value.

For example, add this to your `.bashrc` or similar:
```bash
export LC_CTYPE=C.UTF-8
```
Take into account that the the environment variable `LC_ALL`, when set, overrides the value of `LC_CTYPE`.
So you may want to `unset` `LC_ALL` or to set and export it to an UTF-8 aware value.
You can see the values of `LC_ALL` and `LC_CTYPE` with the command `locale`.

## C compiler

The `carp` executable will emit a single file with C code, `main.c` and try to compile it using an external C compiler.
On macOS and Linux it defaults to `clang`, so make sure you have that installed (On macOS this is preferably done by installing XCode, including its developer tools).

On Windows the default C compiler used by Carp is `clang-cl.exe` which compiles the code using Clang but links it with the Visual Studio linker. Tip: use the package manager [Scoop](https://scoop.sh/) to install LLVM for an easy way to set this up on Windows. Also make sure you have Visual Studio with the C/C++ addon installed. Please note that you *don't* need WSL (Windows Subsystem for Linux) to use Carp.

If you want to use another compiler, you can configure the exact build command like so:

```clojure
(Project.config "compiler" "gcc --important-flag")
```

## SDL, GLFW, etc

The examples involving graphics/sound/interaction will require the following libraries installed on your system:

* [SDL 2](https://www.libsdl.org/download-2.0.php) (cross platform game/interactivity library)
* [SDL_image 2](https://www.libsdl.org/projects/SDL_image/) (image helpers)
* [SDL_ttf 2](https://www.libsdl.org/projects/SDL_ttf/) (font rendering)
* [SDL_mixer 2](https://www.libsdl.org/projects/SDL_mixer/) (audio playback)
* [glfw](http://www.glfw.org) (Create a rendering context for OpenGL or Vulcan)

On macOS and Linux we use [pkg-config](https://www.freedesktop.org/wiki/Software/pkg-config/) to handle include paths and linking flags, so make sure you have that properly installed and configured to find the external libraries.

Please let us know if you have trouble getting these bindings to work! We have tried making everything as reliable as possible but there are often corner cases when it comes to dependency management. And remember that you're always welcome to start an issue or ask questions in [the gitter channel](https://gitter.im/carp-lang/Carp).

## Footnote for Windows
You can install clang with mingw64 but you'll also want to run `vcvarsall.bat amd64` or `vcvarsall.bat x86` each time you start your shell to help clang find the right headers.
See https://github.com/carp-lang/Carp/issues/700 or https://github.com/carp-lang/Carp/issues/1323 for more information.

Also when compiling files with `carp` from Windows you must ensure that :
* the file is encoded either as ANSI or UTF-8. (Using another encoding like UTF-8-BOM doesn't work.)
* using either Unix (LF) or Windows (CR LF) as linefeed/newline. (Using Macintosh (CR) as newline doesn't work.)

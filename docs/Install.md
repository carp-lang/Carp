# Installation

Carp is mainly developed on macOS, but it also works fine on Linux. Windows is currently not supported - but please get in touch in case you want to help out with that!

## Building the Carp executable from source

1. Make sure you have [Stack](https://docs.haskellstack.org/en/stable/README/) installed.
2. Clone this repo to your machine.
3. Run ```stack build``` in the root of the project directory.
4. ```stack install``` will install the Carp command line tool for easy access on your system.
5. Make sure that the directory where stack installs executables is on your PATH, i.e: ```export PATH=~/.local/bin:$PATH```.

## Setting the CARP_DIR

The `carp` executable must know where to find its core libraries and other files.
Set the environment variable CARP_DIR so that it points to the root of the Carp repo.

For example, add this to your `.bashrc` or similar:

```bash
export CARP_DIR=~/Carp/
```

You should now be able to start Carp from anywhere:

```bash
$ carp
```

## C compiler

The `carp` executable will emit a single file with C code, `main.c` and try to compile it using an external C compiler.
On macOS and Linux it defaults to `clang`, on Windows it's `cl.exe`.

You can configure the exact compiler command like so:

```clojure
(Project.config "compiler" "gcc --important-flag")
```

## SDL, GLFW, etc

The examples involving graphics/sound/interaction will require the following libraries installed on your system:

* [SDL 2](https://www.libsdl.org/download-2.0.php) (cross platform game/interactivity library)
* [SDL_image 2](https://www.libsdl.org/projects/SDL_image/) (image helpers)
* [SDL_ttf 2](https://www.libsdl.org/projects/SDL_ttf/) (font rendering
* [SDL_mixer 2](https://www.libsdl.org/projects/SDL_mixer/) (audio playback)
* [glfw](http://www.glfw.org) (Create a rendering context for OpenGL or Vulcan)

Please let us know if you have trouble getting these examples to work.

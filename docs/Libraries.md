## Working with libraries and modules

Below is a list of 'core' modules that comes included with the Carp compiler.
Modules marked with the symbol '⦁' are imported by default, other modules must be imported by loading their file. For example, to get access to the Bench module, do the following:

```clojure
(load "Bench.carp")
```

Using the functions in the Bench module still requires pre-pending them with the module name, i.e. `(Bench.bench fib)`. To avoid that, also do `(use Bench)`.

To see what functions a certain module contains, use the `info` command:

```clojure
(info Bench)
```

External librares can be loaded by using their relative or absolute location in your file system as the path. To make a library publically available in a more general way you can add it to your Carp project's search path:s (found by running `:p` in the repl). For instance, here's how you would add the NCurses library so that it can be loaded with just `(load "NCurses.carp")`.

```clojure
(Project.config "search-path" "~/Projects/carp-ncurses")
```

This line of configuration can be put into a `~/.carp/profile.carp` file to make it apply in all your projects.

## Loading via git

You can also load libraries via Git like that:

```clojure
(load "git@github.com:hellerve/anima.carp@master")
```

This will download the [Anima](https://github.com/hellerve/anima) library to
`~/.carp/libs/<library>/<tag>` and load the file `anima.carp` in it. To get a
stable version of the library you should specify a git tag rather than
`@master`.

If you want to make a library ready for loading, either prepare a file that has the same name
as the library—in the case above, `anima.carp`—or a file called `main.carp` as
an entrypoint.

Please note that for private repos only loading through SSH is supported. For public repos you can use HTTPS:

```clojure
(load "https://github.com/hellerve/anima@master")
```

## Core Modules

### Basics
* [Macros ⦁](../core/Macros.carp)
* [Interfaces ⦁](../core/Interfaces.carp)
* [Dynamic ⦁](http://carp-lang.github.io/carp-docs/core/Dynamic.html) (only available in the repl and at compile time)

* [Maybe ⦁](http://carp-lang.github.io/carp-docs/core/Maybe.html)
* [Result ⦁](http://carp-lang.github.io/carp-docs/core/Result.html)

### Numerics
* [Int ⦁](http://carp-lang.github.io/carp-docs/core/Int.html)
<!-- * SafeInt -->
* [Long ⦁](http://carp-lang.github.io/carp-docs/core/Long.html)
* [Bool ⦁](http://carp-lang.github.io/carp-docs/core/Bool.html)
* [Float ⦁](http://carp-lang.github.io/carp-docs/core/Float.html)
* [Double ⦁](http://carp-lang.github.io/carp-docs/core/Double.html)
* [Vector2](http://carp-lang.github.io/carp-docs/core/Vector2.html)
* [Vector3](http://carp-lang.github.io/carp-docs/core/Vector3.html)
* [VectorN](http://carp-lang.github.io/carp-docs/core/VectorN.html)
* [Geometry](http://carp-lang.github.io/carp-docs/core/Geometry.html)
* [Statistics](http://carp-lang.github.io/carp-docs/core/Statistics.html)

### Text
* [String ⦁](http://carp-lang.github.io/carp-docs/core/String.html)
* [Char ⦁](http://carp-lang.github.io/carp-docs/core/Char.html)
<!-- * Format ⦁ -->
* [Pattern ⦁](http://carp-lang.github.io/carp-docs/core/Pattern.html)

### Collections
* [Array ⦁](http://carp-lang.github.io/carp-docs/core/Array.html)
* [Map ⦁](http://carp-lang.github.io/carp-docs/core/Map.html)

### System
* [IO ⦁](http://carp-lang.github.io/carp-docs/core/IO.html)
* [System ⦁](http://carp-lang.github.io/carp-docs/core/System.html)

### Development
* [Bench](http://carp-lang.github.io/carp-docs/core/Bench.html)
* [Debug ⦁](http://carp-lang.github.io/carp-docs/core/Debug.html)
* [Test ⦁](http://carp-lang.github.io/carp-docs/core/Test.html)

### Graphics, sound and interaction
* [SDL](http://carp-lang.github.io/Carp/sdl/SDL_index.html)
* [SDL Image](http://carp-lang.github.io/Carp/sdl/IMG.html)
* [SDL TTF](http://carp-lang.github.io/Carp/sdl/TTF.html)
* [SDL Mixer](http://carp-lang.github.io/Carp/sdl/Mixer.html)
* [GLFW](../core/GLFW.carp)
* [OpenGL](../core/OpenGL.carp)

## External Libraries
* [Stdint](https://github.com/hellerve/stdint) (A wrapper around the types defined in stdint.h)
* [Socket](https://github.com/hellerve/socket) (A wrapper around C sockets)
* [Physics](https://github.com/hellerve/physics) (A port of phys.js)
* [NCurses](https://github.com/eriksvedang/carp-ncurses) ([https://www.gnu.org/software/ncurses/](https://www.gnu.org/software/ncurses/))
* [Anima](https://github.com/hellerve/anima) (A simple drawing and animation framework)
* [Curl](https://github.com/eriksvedang/carp-curl) (Simple bindings to the Curl library)

For a growing list of Carp packages, see [Carpentry](https://github.com/carpentry-org).

## Working with libraries and modules

Below is a list of 'core' modules that comes included with the Carp compiler.
Modules marked with the symbol '⦁' are imported by default, other modules must be imported by loading their file. For example, to get access to the Bench module, do the following:

```clojure
(load "Bench.carp")
```

Using the functions in the Bench module still requires pre-pending them with the module name, i.e. `(Bench.bench fib)`. To avoid that, also do `(use Bench)`.

To see what functions a certain module contains, use the `info` command:

```clojure
(info "Bench")
```

External librares can be loaded by using their relative or absolute location in your file system as the path. To make a library publically available in a more general way you can add it to your Carp project's search path:s (found by running `:p` in the repl). For instance, here's how you would add the NCurses library so that it can be loaded with just `(load "NCurses.carp")`.

```clojure
(Project.config "search-path" "~/Projects/carp-ncurses")
```

This line of configuration can be put into a `~/.carp/profile.carp` file to make it apply in all your projects.

## Core Modules

### Basics
* [Macros ⦁](../core/Macros.carp)
* [Interfaces ⦁](../core/Interfaces.carp)
* [Dynamic ⦁](http://carp-lang.github.io/Carp/core/Dynamic.html) (only available in the repl and at compile time)

### Numerics
* [Int ⦁](http://carp-lang.github.io/Carp/core/Int.html)
* SafeInt
* [Long ⦁](http://carp-lang.github.io/Carp/core/Long.html)
* [Bool ⦁](http://carp-lang.github.io/Carp/core/Bool.html)
* [Float ⦁](http://carp-lang.github.io/Carp/core/Float.html)
* [Double ⦁](http://carp-lang.github.io/Carp/core/Double.html)
* [Vector](http://carp-lang.github.io/Carp/core/Vector.html)
* [Geometry](http://carp-lang.github.io/Carp/core/Geometry.html)
* [Statistics](http://carp-lang.github.io/Carp/core/Statistics.html)

### Text
* [String ⦁](http://carp-lang.github.io/Carp/core/String.html)
* [Char ⦁](http://carp-lang.github.io/Carp/core/Char.html)
* Format ⦁
* [Pattern ⦁](http://carp-lang.github.io/Carp/core/Pattern.html)

### Collections
* [Array ⦁](http://carp-lang.github.io/Carp/core/Array.html)
* [Map ⦁](http://carp-lang.github.io/Carp/core/Map.html)

### System
* [IO ⦁](http://carp-lang.github.io/Carp/core/IO.html)
* [System ⦁](http://carp-lang.github.io/Carp/core/System.html)

### Development
* [Debug ⦁](http://carp-lang.github.io/Carp/core/Debug.html)
* [Test](http://carp-lang.github.io/Carp/core/Test.html)
* [Bench](http://carp-lang.github.io/Carp/core/Bench.html)

### Graphics, sound and interaction
* [SDL](../core/SDL.carp)
* [SDL Image](../core/SDL_image.carp)
* [SDL TTF](../core/SDL_ttf.carp)
* [SDL Mixer](../core/SDL_mixer.carp)
* [GLFW](../core/GLFW.carp)
* [OpenGL](../core/OpenGL.carp)

## External Libraries
* [Stdint](https://github.com/hellerve/stdint) (A wrapper around the types defined in stdint.h)
* [Socket](https://github.com/hellerve/socket) (A wrapper around C sockets)
* [Physics](https://github.com/hellerve/physics) (A port of phys.js)
* [NCurses](https://github.com/eriksvedang/carp-ncurses) ([https://www.gnu.org/software/ncurses/](https://www.gnu.org/software/ncurses/))
* [Anima](https://github.com/hellerve/anima) (A simple drawing and animation framework)
* [Curl](https://github.com/eriksvedang/carp-curl) (Simple bindings to the Curl library)

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
* [Dynamic ⦁](../core/Dynamic.carp) (only available in the repl and at compile time)

### Numerics
* [Int ⦁](../core/Int.carp)
* [SafeInt](../core/SafeInt.carp)
* [Long ⦁](../core/Long.carp)
* [Bool ⦁](../core/Bool.carp)
* [Float ⦁](../core/Float.carp)
* [Double ⦁](../core/Double.carp)
* [Vector](../core/Vector.carp)
* [Geometry](../core/Geometry.carp)
* [Statistics](../core/Statistics.carp)

### Text
* [String ⦁](../core/String.carp)
* [Char ⦁](../core/Char.carp)
* [Format ⦁](../core/Format.carp)
* [Pattern ⦁](../core/Pattern.carp)

### Collections
* [Array ⦁](../core/Array.carp)

### System
* [IO ⦁](../core/IO.carp)
* [System ⦁](../core/System.carp)

### Development
* [Debug ⦁](../core/Debug.carp)
* [Test](../core/Test.carp)
* [Bench](../core/Bench.carp)

### Graphics
* [SDL](../core/SDL.carp)
* [GLFW](../core/GLFW.carp)
* [OpenGL](../core/OpenGL.carp)

## External Libraries
* [Stdint](https://github.com/hellerve/stdint) (A wrapper around the types defined in stdint.h)
* [Socket](https://github.com/hellerve/socket) (A wrapper around C sockets)
* [Physics](https://github.com/hellerve/physics) (A port of phys.js)
* [NCurses](https://github.com/eriksvedang/carp-ncurses) ([https://www.gnu.org/software/ncurses/](https://www.gnu.org/software/ncurses/))
* [Anima](https://github.com/hellerve/anima) (A simple drawing and animation framework)

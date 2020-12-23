## Working with libraries and modules

Carp comes with a standard library called [Core](../core/) where only certain files are loaded by default (see [Core.carp](../core/Core.carp)). If your `CARP_DIR` environment variable is [set properly](https://github.com/carp-lang/Carp/blob/master/docs/Install.md#setting-the-carp_dir), these libraries will be easily imported using the `load` command. For example, to get access to the Bench module, do the following:

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

This line of configuration can be put into [config.carp](Manual.md#Profile-settings) to make it apply in all your projects.

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

## Documentation

You can generate HTML documentation for a set of modules by running `save-docs` in the REPL:

```clojure
> (save-docs Int Float String)
```

See the file [generate_core_docs.carp](./core/generate_core_docs.carp) for an example of how to configure the result.


## Auto generated API documentation

* [Core](http://carp-lang.github.io/carp-docs/core/core_index.html)
* [SDL](http://carp-lang.github.io/carp-docs/sdl/SDL_index.html)

## Some External Libraries
* [Anima](https://github.com/hellerve/anima) (A simple drawing and animation framework)
* [Stdint](https://github.com/hellerve/stdint) (A wrapper around the types defined in stdint.h)
* [Socket](https://github.com/hellerve/socket) (A wrapper around C sockets)
* [Physics](https://github.com/hellerve/physics) (A port of phys.js)
* [NCurses](https://github.com/eriksvedang/carp-ncurses) ([https://www.gnu.org/software/ncurses/](https://www.gnu.org/software/ncurses/))
* [Curl](https://github.com/eriksvedang/carp-curl) (Simple bindings to the Curl library)

For a growing list of Carp packages, see [Carpentry](https://github.com/carpentry-org).

Do you have a library that you want to plug here? Pleas make a PR!

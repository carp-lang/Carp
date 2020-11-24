## The Compiler Manual

### Related pages

* [Installation](Install.md) - how to acquire and configure the Carp compiler
* [How To Run Code](HowToRunCode.md) - compile and execute .carp files
* [Tooling](Tooling.md) - supported editors
* [Libraries](Libraries.md) - how to work with libraries and modules
* [Multimedia](Multimedia.md) - graphics, sounds, etc
* [Macros](Macros.md) - a guide to the Carp macro system
* [Embedded](Embedded.md) - tips and tricks for working with Carp on embedded system
* [Terminology](Terminology.md) - commonly used terms and what we mean with them

To learn more about the Carp language and its syntax and semantics, check out the [Carp Language Guide](LanguageGuide.md).

### REPL Basics

The Carp language is very tightly integrated with the REPL, everything you want to do to your program can be controlled from here.

To load code from disk, use ```(load "filename.carp")```, this will add the source file `filename.carp` to the current 'project'. A project is a light weight concept in the repl that ties together source files and compiler settings much like in an IDE like Eclipse or Visual Studio.

To build your current project, call ```(build)```. This will emit an executable or dynamic library depending on if you have defined a main-function or not. Please note that a project emitting a library will not initialize global variables automatically, the user of the library needs to call the C function `carp_init_globals` or the Carp function `System.carp-init-globals` instead.

Everything emitted by the compiler will be saved in a directory named ```out``` by default. This, and other settings regarding the project can be changed by various commands. To see a list of available commands, call ```(help "project")```.

There are a bunch of handy shortcuts for doing common things at the REPL:

```
:r   Reload all the files of the project
:b   Build the project
:x   Run the executable (if it exists)
:c   Look at the emitted C code
:e   Display the environment with all the function bindings and their types
:p   Show project information and settings
:h   Show the help screen
:q   Quit the repl
```

### Differences compared to REPL:s in other Lisp:s

While powerful, the REPL in Carp currently has some big limitations compared to most other Lisp:s. If you type in an expression and press enter one of the following things will happen:

1. If you're calling a dynamic function (something defined with `defndynamic`, or a built in `command`) it will be executed right away. This works very much like a classic, dynamically typed Lisp interpreter. The dynamic functions are not available in compiled code! Their main usage is in macros and to programatically control your build settings.

2. If you're calling a function defined with `defn` it's a "normal" Carp function which will be compiled (via C) to an executable binary, which is then run in a child process to the REPL. This means that the function has no chance to modify anything else in your program, like global variables and the like.

3. If the top-level form isn't a function call, the REPL might get confused. For example, entering an array of calls to a Carp function will give unexpected results (the array will be dynamic but the function calls will not). The easiest way to work around that at the moment is to wrap the expression in a `defn` and call that one instead. This will be fixed in a future version of Carp.

### Adding annotations

Carp has a flexible metadata system (inspired by the one in Clojure) that lets anyone add and retrieve data on the bindings in the environment. The general way to do that is with `(meta-set! <path> <key> <value>)` and `(meta <path> <key>)`.

A couple of useful macros are implemented on top of this system:

```clojure
(doc <path> "This is a nice function.") ; Documentation
(sig <path> (Fn [Int] Bool))            ; Type annotation
(private <path>)                        ; Will make the function inaccesible to other modules
```

Note that `<path>` in all these cases are symbols, e.g. `foo` or `Game.play`.

To generate html docs from the doc strings, run:

```clojure
(save-docs <module 1> <module 2> <etc>)
```

### Getting types from bindings

```clojure
鲮 (type <binding>)
鲮 :t <binding>
```

### Listing bindings in a module

```clojure
鲮 (info <module name>)
鲮 :i <module name>
```

### Expanding a macro

```clojure
鲮 (expand 'yourcode)
鲮 :m yourcode
```

### Configuring a project

The current session in the repl is called a "project" and can be configured using the `(Project.config <setting> <value>)` command. The following settings can be configured with this command:

* ```"cflag"```              - Add a flag to the compiler.
* ```"libflag"```            - Add a library flag to the compiler.
* ```"pkgconfigflag"```      - Add a flag to pkg-config invocations.
* ```"compiler"```           - Set what compiler should be run with the 'build' command.
* ```"target"```             - Set the target triple (useful when cross-compiling).
* ```"title"```              - Set the title of the current project, will affect the name of the binary produced.
* ```"prompt"```             - Set the prompt in the repl.
* ```"search-path"```        - Add a path where the Carp compiler will look for '*.carp' files.
* ```"output-directory"```    - Where to put build artifacts.
* ```"docs-directory"```      - Where to put generated docs.
* ```"generate-only"```      - Set to true if you don't want to run the C compiler when building.

* ```"echo-c"```             - When a form is defined using 'def' or 'defn' its C code will be printed.
* ```"echo-compiler-cmd"```  - When building the project the command for running the C compiler will be printed.
* ```"print-ast"```          - The 'info' command will print the AST for a binding.

For example, to set the title of your project:

```clojure
鲮 (Project.config "title" "Fishy")
```

To use another compiler:
```clojure
鲮 (Project.config "compiler" "tcc")
```

### Profile settings

If a file called ```profile.carp``` is placed in the XDG config folder ```carp/```, that file will get loaded after the compiler has started (after loading the core libraries but before any other source files are loaded). This file is meant for user specific settings that you want in all your projects, like little helper functions and other customizations.

On Windows this file is located at ```C:/Users/USERNAME/AppData/Roaming/carp/profile.carp```.

### Compiler flags

When invoking the compiler from the command line you can supply the following flags to configure the behaviour:

* ```-b``` Build the code, then quit the compiler.
* ```-x``` Build and run the code (make sure it has a main function defined), then quit the compiler.
* ```--no-core``` Run the compiler without loading any of the core libraries.
* ```--log-memory``` The executable will log all calls to malloc and free.
* ```--optimize``` Removes safety checks (like array bounds access, etc.) and runs the C-compiler with the `-O3` flag.
* ```--check``` Run the compiler without emitting any binary, just report all errors found (in a machine readable way).
* ```--generate-only``` Don't compile the C source.
* ```--eval-preload``` Evaluate the provided string before loading any code (but after loading ```profile.carp```).

### Inspecting the C code generated by an expression

```clojure
鲮 (c '(+ 2 3))
```

### Cross-compiling

Cross-compilation needs to be enabled early. You can do so in ```profile.carp``` as:

```clojure
(Project.config "compiler" "zig cc --target=x86_64-windows-gnu")
(Project.config "target" "x86_64-windows-gnu")
```

Or using ```--eval-preload``` as:

```sh
carp --eval-preload '(Project.config "compiler" "zig cc --target=x86_64-windows-gnu") (Project.config "target" "x86_64-windows-gnu")' -b whatever.carp
```

## The Compiler
The Carp language is very tightly integrated with the REPL, everything you want to do to your program can be controlled from here.

To explore the commands available, enter ```(help)``` and press enter.

To load code from disk, use ```(load "filename.carp")```, this will add the source file `filename.carp` to the current 'project'. A project is a light weight concept in the repl that ties together source files and compiler settings much like in an IDE like Eclipse or Visual Studio.

To build your current project, call ```(build)```. This will emit an executable or dynamic library depending on if you have defined a main-function or not. Everything emitted by the compiler will be saved in a directory named ```out``` by default. This, and other settings regarding the project can be changed by various commands. To see a list of available commands, call ```(help "project")```.

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
While powerful, the REPL in Carp currently has some big limitations compared to most other Lisp:s. To execute the code in a compiled function (the ones defined with `defn`) you have to define a function called `main` and then build & run the project (with `:bx` or `(build) (run)`. To make this somewhat nicer there is a helper macro called `eval` that can be used like this:

```clojure
鲮 (eval (my-function 12345 "w00t"))
```

Dynamic functions (defined with `defndynamic`) do not share this limitation but they are not available in compiled code.

### Adding annotations
Carp has a flexible meta data system (inspired by the one in Clojure) that lets anyone add and retrieve data on the bindings in the environment. The general way to do that is with `(meta-set! <path> <key> <value>)` and `(meta <path> <key>)`.

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
```

### Listing bindings in a module
```clojure
鲮 (info <module name>)
```

### Configuring a project
The current session in the repl is called a "project" and can be configured using the `(Project.config <setting> <value>)` command. The following settings can be configured with this command:

* ```"cflag"```              - Add a flag to the compiler.
* ```"libflag"```            - Add a library flag to the compiler.
* ```"compiler"```           - Set what compiler should be run with the 'build' command.
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

### Profile settings
If a file called ```profile.carp``` is placed in the folder ```~/.carp/```, that file will get loaded after the compiler has started (after loading the core libraries but before any other source files are loaded). This file is meant for user specific settings that you want in all your projects, like little helper functions and other customizations.

<!-- If a file called ```project.carp``` is placed in the folder where you invoke the ```carp``` command this file will get loaded after the compiler has started (and after 'user.carp' has loaded). This files is intended for setting up the build process of this particular project, for example by loading the correct source files, configuring the compiler variables, etc. -->

### Compiler flags
When invoking the compiler from the command line you can supply the following flags to configure the behaviour:

* ```-b``` Build the code, then quit the compiler.
* ```-x``` Build and run the code (make sure it has a main function defined), then quit the compiler.
* ```--no-core``` Run the compiler without loading any of the core libraries.
* ```--log-memory``` The executable will log all calls to malloc and free.
* ```--optimize``` Removes safety checks (like array bounds access, etc.) and runs the C-compiler with the `-O3` flag.
* ```--check``` Run the compiler without emitting any binary, just report all errors found (in a machine readable way).
* ```--generate-only``` Don't compile the C source.

### Inspecting the C code generated by an expression
```clojure
鲮 (c '(+ 2 3))
```

## The Compiler
The Carp language is very tightly integrated with the REPL, everything you want to do to your program can be controlled from here.

To explore the commands available, enter ```(help)``` and press enter.

To load code, use ```(load <filename>)```, this will add the source file to the current 'project'. A project is a light weight concept in the repl that ties together source files and compiler settings much like in an IDE like Eclipse or Visual Studio.

To build your current project, call ```(build)```. This will emit an executable or dynamic library depending on if you have defined a main-function or not. Everything emitted by the compiler will be saved in a directory named ```out``` by default. This, and other settings regarding the project can be changed by various commands. To see a list of available commands, call ```(help project)```.

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

### Getting types from bindings
```
(type <binding>)
```

### Listing bindings in a module
```
(info <module name>)
```

<!-- ### Special Files -->
<!-- If a file called ```user.carp``` is placed in the folder ```~/.carp/```, that file will get loaded after the compiler has started. This file is meant for user specific settings that you want in all your projects, like little helper functions and other customizations. -->

<!-- If a file called ```project.carp``` is placed in the folder where you invoke the ```carp``` command this file will get loaded after the compiler has started (and after 'user.carp' has loaded). This files is intended for setting up the build process of this particular project, for example by loading the correct source files, configuring the compiler variables, etc. -->

### Compiler flags
When invoking the compiler from the command line you can supply the following flags to configure the behaviour:

```-b``` Build the code, then quit the compiler.
```-x``` Build and run the code (make sure it has a main function defined), then quit the compiler.
```--no-core``` Run the compiler without loading any of the core libraries.
```--log-memory``` The executable will log all calls to malloc and free.

## The Compiler
The Carp language is very tightly integrated with it's compiler which itself is written in a dynamic version of Carp (implemented in C). To work on a Carp program you run ```carp``` (first making sure it's in your $PATH, see installation instructions below) which starts the REPL. Everything you want to do to your program can be controlled from here.

For example, to compile a function named 'fib' you enter the following:
```clojure
λ> (bake fib)
```

This results in the compiler analyzing the code form for 'fib' and compiling it to (hopefully very fast) binary code, immediately loading this back into the REPL so that it can be called from there. The resulting C-code, AST and type signature are bound to the three variables 'c', 'ast' and 's', respectively. Inspecting their contents will teach you more about the innards of the Carp language, for sure!

From the REPL you can also inspect your the state of variables, extend the compiler, script the build process of your project, or statically analyze its code. All these operations should be really quick to execute and easy to remember so you can focus on developing your program.

To start the Carp compiler in development mode (which will run its test suite), invoke it like this instead:

```CARP_DEV=1 carp```

### Compiler Variables
* ```carp-dir``` The root folder of the Carp compiler, should be the same folder as the on where this README.md file resides.
* ```out-dir``` A string with the name of the folder where build artifacts should be put. Standard value is the 'out' folder in the carp directory.
* ```exe-out-dir``` Where the exe:s produced by (bake-exe ...) should be placed. Standard value is "./" (working directory)
* ```echo-signature-after-bake``` If this is true the type signature of freshly baked functions will be printed in the REPL.
* ```prompt``` The prompt displayed in the repl
* ```profile-infer-time``` Set to true if you want to know the time it takes to infer the types for each function
* ```profile-external-compiler-time``` Set to true if you want to know the time it takes to run the external C compiler
* ```log-unloading-of-dylibs``` Should the compiler log when it unloads dynamic libraries?
* ```log-deps-when-baking-ast``` Should the compiler log the libraries it links to?

### Special Files
If a file called ```user.carp``` is placed in the folder ```~/.carp/```, that file will get loaded after the compiler has started. This file is meant for user specific settings that you want in all your projects, like little helper functions and other customizations.

If a file called ```project.carp``` is placed in the folder where you invoke the ```carp``` command this file will get loaded after the compiler has started (and after 'user.carp' has loaded). This files is intended for setting up the build process of this particular project, for example by loading the correct source files, configuring the compiler variables, etc.

### Recovering from errors
If an error occurs at the REPL, Carp will intercept the error signal and try to recover. Sometimes this does not work (because of memory corruption or similar) and your only option is to restart the process. Quite often it works though, so make sure to try it before resorting to a hard reset.

When working with glfw windows a crash will not close the window, and creating a new one will not work either. To be able to continue, call ```(glfwTerminate)``` first. This will clear everything related to the window and allow you to start anew. A similar process is probably worth working out for other kind or resources that gets lost when a crash happens. Also make sure you fix the problematic code or state that caused the error or you will just crash again immediately.

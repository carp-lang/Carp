# Carp

[![Join the chat at https://gitter.im/eriksvedang/Carp](https://badges.gitter.im/eriksvedang/Carp.svg)](https://gitter.im/eriksvedang/Carp?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

<img src="https://github.com/eriksvedang/Carp/blob/master/img/temp_logo2.jpg" alt="Logo" align="right" />

<i>WARNING! This is a research project and a lot of information here might become outdated and misleading without any explanation. Don't try it out just yet!</i>

Carp is a small programming language designed to work well for interactive and performance sensitive use cases like games, sound synthesis and visualizations.

The key features of Carp are the following:
* Automatic and deterministic memory management (no garbage collector or VM)
* Inferred static types for great speed and reliability
* Live reloading of code, REPL-driven development, a fun and helpful workflow
* Ownership tracking enables a functional programming style while still using mutation of cache friendly data structures under the hood
* No hidden performance penalties – allocation and copying is explicit
* Very good integration with exisiting C code

## Quick OpenGL/GLFW Example
```clojure
(defn app ()
  (if (glfwInit)
    (let [window (glfwCreateWindow 640 480 "Yeah!" NULL NULL)]
      (if (null? window)
        (panic "No window.")
        (do (glfwMakeContextCurrent window)
            (while (not (glfwWindowShouldClose window))
              (do
                (glClearColor 0.6f 0.85f 0.85f 1.0f)
                (glClear gl-color-buffer-bit)
                (glColor3f 1.0f 0.9f 0.2f)
                (draw-rect -0.5f -0.5f 1.0f 1.0f)              
                (glfwSwapBuffers window)
                (glfwPollEvents)))
            (glfwTerminate))))
    (panic "Failed to initialize glfw.")))
```

To build this example, load the gl bindings with ```(load-gl)```, then execute ```(bake-gl-demo-exe)``` to build an executable, or just ```(gl-demo)``` to run the program directly from the REPL.

## The Compiler
The Carp language is very tightly integrated with it's compiler which itself is written in a dynamic version of Carp (implemented in C). To work on a Carp program you run ```carp``` (first making sure it's in your $PATH, see installation instructions below) which starts the REPL. Everything you want to do to your program can be controlled from here.

For example, to compile a function named 'fib' you enter the following:
```clojure
λ> (bake fib)
```

This results in the compiler analyzing the code form for 'fib' and compiling it to (hopefully very fast) binary code, immediately loading this back into the REPL so that it can be called from there. The resulting C-code, ast and type signature are bound to the three variables 'c', 'ast' and 's', respectively. Inspecting their contents will teach you more about the innards of the Carp language, for sure!

From the REPL you can also inspect your the state of variables, extend the compiler, script the build process of your project, or statically analyze its code. All these operations should be really quick to execute and easy to remember so you can focus on developing your program.

To start the Carp compiler in development mode (which will run its test suite), invoke it like this instead:

```CARP_DEV=1 carp```

### Installation

Clone this repo, then run ```make``` in its root. Add the 'bin' directory to your path to enable calling the ```carp``` command. To do this, add the following to your .bashrc / .zshrc / whatever:

```export PATH=$PATH:~/Carp/bin/```

Carp is currently only tested on OSX 10.10. More platforms are coming though, the project is written in C99 so I hope it's not TOO much work. There are a few dependencies that have to be installed:
 * libffi
 * glfw3
 * rlwrap
 
Note: 'rlwrap' is not strictly needed but makes the REPL experience much nicer, modify the '/bin/carp' script if you don't want to use it.

### Compiler Variables
* ```out-dir``` A string with the name of the folder where build artifacts should be put. Standard value is "".
* ```carp-dir``` The root folder of the Carp compiler, should be the same folder as this README.md file.

### Special Files
If a file called ```user.carp``` is placed in the folder ```~/.carp/```, that file will get loaded after the compiler has started. This file is meant for user specific settings that you want in all your projects, like little helper functions and other customizations.

If a file called ```project.carp``` is placed in the folder where you invoke the ```carp``` command this file will get loaded after the compiler has started (and after 'user.carp' has loaded). This files is intended for setting up the build process of this particular project, for example by loading the correct source files, configuring the compiler variables, etc.

## The Language
Carp borrows its looks from Clojure but the runtime semantics are much closer to those of ML or Rust. Here's a sample program:

```clojure
(defn fib (n)
  (if (< n 2)
    1
    (+ (fib (- n 2)) (fib (- n 1)))))
```

This compiles to the equivalent of the following C program:
```C
int fib(int n) {
  if(n < 2) {
    return 1;
  } else {
    return fib(n - 2) + fib(n - 1);
  }
}
```

The most important thing in Carp is to work with arrays of data. Here's an example of how that looks:

```clojure
(defn weird-sum (nums)
  (reduce + 0 (map inc (filter even? nums))))
```

All the array modification functions like 'map', 'filter', etc. use C-style mutation of the array and return the same data structure back afterwards, no allocation or deallocation needed. The lifetime analyzer ("borrow checker" in Rust parlance) makes sure that the same data structure isn't used in several places.

### Data Literals
```clojure
100 ; int
3.14 ; float
true ; bool
"hello" ; string
'e' ; char
[1 2 3] ; array
```

### Special Forms
```clojure
(def variable-name value)
(defn function-name (arg1 arg2 ...) (function-body ...))
(let [var1 expr1, var2 expr2, ...] body)
(do expr1 expr2 ...)
(if expression true-branch false-branch)
(while expression body)
(for (i 0 100, j 0 100) body)
(ref x) ;; Turns an owned value into an unowned one
(reset! variable value)
```

### Structs (not implemented)
```clojure
(defstruct Vector2 [x :float, y :float])

(def my-pos (Vector2 102.2f 210.3f))
```

### Algebraic Data Types (not implemented)
```clojure
(defdata Color 
  RGB [r :float, g :float, b :float]
  Grayscale [amount :float])

(def color (Grayscale 50.0f))
```

Omit the name tag to create a data constructor with the same name as the type:
```clojure
(defdata Vector3 [x :float, y :float, z :float])

(def position (Vector3 4.0 5.0 -2.0))
(def x-position (.x position)
```

### C interop
```clojure
(def blah (load-dylib "./libs/blah.so"))
(register blah "foo" (:int :int) :string) ;; will register the function 'foo' in the dynamic library 'blah' that takes two ints and returns a string
```

## License

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

© Erik Svedäng 2015 - 2016


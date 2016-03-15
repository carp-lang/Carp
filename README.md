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

## A Small OpenGL/GLFW Example
```clojure
(load gl)

^ann glfw-key-callback-type
(defn on-key [window key scancode action mods]
  (if (= key-esc key)
    (glfwSetWindowShouldClose window true)
    (println &(str key))))

(defn draw []
  (do
    (glClearColor 0.1 0.1 0.1 1.0)
    (glClear carp-gl-color-buffer-bit)
    (glColor3f 1.0 1.0 1.0)
    (let [r 0.9
          t (glfwGetTime)]
      (draw-line 0.0 0.0 (* r (cosf t)) (* r (sinf t))))
    ))

(defn game []
  (glfw-app "This is a demo" draw on-key))
```

To build this example, save it to a file called 'example.carp' and load it with ```(load-lisp "example.carp")```, then execute ```(bake-exe game)``` to build an executable, or just ```(bake game)``` followed by ```(game)``` to run the program directly from the REPL.

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

### Installation

Clone this repo, then use cmake to generate the project files that you desire. A tip is to put the cmake output into a separate directory called 'build' or similar. Then build the project and make sure the resulting executable is put into the 'bin' directory (cmake should arrange that automatically). An example of how to do all of this for Xcode is in the file 'xcode.sh' in the root of the project.

Add the 'bin' directory to your path to enable calling the ```carp``` command. To do this, add the following to your .bashrc / .zshrc / whatever:

```export PATH=$PATH:~/Carp/bin/```

Carp is currently only tested on OSX 10.10. More platforms are coming soon. There are a few dependencies that have to be installed:
 * libffi
 * glfw3
 * rlwrap
 
Tip: Build libffi with ```./configure --enable-static --disable-shared``` to avoid generating a dylib that might interfere with other installations of it (like one installed with brew).

You will have to tell cmake the location of 'libffi' before it can build correctly. Try using their GUI application if you have trouble, it's pretty self explanatory (first press 'Configure', then set up the paths to 'libffi', and then press 'Generate').

If 'libffi' is installed with Brew, you can find the include files at "/usr/local/opt/libffi/lib/libffi-3.0.13/include".

Note: 'rlwrap' is not strictly needed but makes the REPL experience much nicer, modify the '/bin/carp' script if you don't want to use it.

### Compiler Variables
* ```carp-dir``` The root folder of the Carp compiler, should be the same folder as the on where this README.md file resides.
* ```out-dir``` A string with the name of the folder where build artifacts should be put. Standard value is the 'out' folder in the carp directory.
* ```echo-signature-after-bake``` If this is true the type signature of freshly baked functions will be printed in the REPL.
* ```prompt``` The prompt displayed in the repl

### Special Files
If a file called ```user.carp``` is placed in the folder ```~/.carp/```, that file will get loaded after the compiler has started. This file is meant for user specific settings that you want in all your projects, like little helper functions and other customizations.

If a file called ```project.carp``` is placed in the folder where you invoke the ```carp``` command this file will get loaded after the compiler has started (and after 'user.carp' has loaded). This files is intended for setting up the build process of this particular project, for example by loading the correct source files, configuring the compiler variables, etc.

## The Language
Carp borrows its looks from Clojure but the runtime semantics are much closer to those of ML or Rust. Here's a sample program:

```clojure
(defn say-hi (text)
  (while true
    (if (< (strlen text) 10)
      (println "Too short!")
      (println text))))
```

This compiles to the following C program:
```C
void say_hi(string text) {
  bool while_expr_601 = 1;
  while(while_expr_601) {
    int strlen_result_604 = strlen(text);
    bool if_expr_603 = strlen_result_604 < 10;
    if(if_expr_603) {
      println("Too short!");
    } else {
      println(text);
    }
    while_expr_601 = 1;
  }
}
```

If-statements are kind of tricky in regards to memory management:
```clojure
(defn say-what (text)
  (let [manage-me (string-copy text)]
    (if (< (strlen text) 10)
      (string-copy "Too short")
      manage-me)))
```

The 'manage-me' variable is the return value in the second branch, but should get freed if "Too short" is returned.
The output is a somewhat noisy (working on it!) C program:
```C
string say_what(string text) {
  string _let_result_0;
  {
    string _result_0 = string_copy(text);
    string manage_me = _result_0;
    int _result_1 = strlen(text);
    string _if_result_0;
    if(_result_1 < 10) {
      string _result_2 = string_copy("Too short");
      free(manage_me);
      _if_result_0 = _result_2;
    } else {
      _if_result_0 = manage_me;
    }
    _let_result_0 = _if_result_0;
  }
  string _final_result_0 = _let_result_0;
  return _final_result_0;
}
```

The most important thing in Carp is to work with arrays of data. Here's an example of how that looks:

```clojure
(defn weird-sum (nums)
  (reduce + 0 (map inc (filter even? nums))))
```

All the array modification functions like 'map', 'filter', etc. use C-style mutation of the array and return the same data structure back afterwards, no allocation or deallocation needed. The lifetime analyzer ("borrow checker" in Rust parlance) makes sure that the same data structure isn't used in several places.

To know whether a function takes over the responsibility of freeing some memory (through its args) or generates some new memory that the caller has to handle (through the return value), just look at the type of the (compiled) function. The type signature can be found with ```(signature ...)```. If the value is a simple type like :string, :Vector3, or similar, it means that the memory ownership gets handed over. If it's a ref signature, meaning that it's a list starting with :ref (i.e. '(:ref :string)'), the memory is just temporarily lended out and someone else will make sure it gets deleted. When interoping with existing C code it's probably useful to send your data structures to C as refs, keeping the memory management inside the Carp section of the program.

### Data Literals
```clojure
100 ; int
3.14 ; float
true ; bool
"hello" ; string
'e' ; char
[1 2 3] ; array
```

### Dynamic-only Data Literals
```clojure
(1 2 3) ; list
foo ; symbol
:string ; keyword
{:a 10 :b 20} ; dictionary
```

### Special Forms
```clojure
(def variable-name value)
(defn function-name (arg1 arg2 ...) (function-body ...))
(let [var1 expr1, var2 expr2, ...] body)
(do expr1 expr2 ...)
(if expression true-branch false-branch)
(while expression body)
(ref x) ;; Turns an owned value into an unowned one
(reset! variable value)
```

### Structs
```clojure
(defstruct Vector2 [x :float, y :float])

(def my-pos (Vector2 102.2 210.3))

;; A 'lens' is automatically generated for each member:
(get-x my-pos) ;; => 102.2
(set-x my-pos 3.0) ;; => (Vector2 10.2 3.0)
(update-x my-pos inc) ;; => (Vector2 10.2 4.0)
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

### Contributors
 * Erik Svedäng
 * Markus Gustavsson

## License

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

© Erik Svedäng 2015 - 2016


# Carp

<img src="https://github.com/eriksvedang/Carp/blob/master/img/temp_logo2.jpg" alt="Logo" style="float=right;"/>

Carp is a programming language in the Lisp family. It's designed to work well for interactive and performance sensitive use cases like games, sound synthesis and visualizations.

The key features of Carp are the following:

* No garbage collection or runtime system, all memory management is deterministic and compiled into the final binary executable
* Static (100% inferred) type system that enables very fast code execution and fewer runtime errors
* Dynamic code loading and execution enables interactive (REPL-driven) development of your program
* No hidden performance penalties, everything that can slow down your program is explicit and visible in the code
* Ownership system that enables a functional programming style with the performance characteristics of C

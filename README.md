# Carp

<img src="https://github.com/eriksvedang/Carp/blob/master/img/temp_logo2.jpg" alt="Logo" align="right" />

<i>WARNING! This is a research project and a lot of information here might become outdated and misleading without any explanation. Don't try it out just yet!</i>

Carp is a small programming language designed to work well for interactive and performance sensitive use cases like games, sound synthesis and visualizations.

The key features of Carp are the following:
* Automatic and deterministic memory management (no garbage collector or VM)
* Inferred static types for great speed and reliability
* Live reloading of code, REPL-driven development, aka "fun"
* Ownership tracking enables a functional programming style while still using mutation for fast updating of data structures
* No hidden performance penalties – allocation and copying is explicit
* Very good integration with exisiting C code

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

The most important thing in Carp is to work with arrays of data. Here's how that looks:

```clojure
(defn weird-sum (nums)
  (reduce + 0 (map inc (filter even? nums))))
```

All the array modification functions like 'map', 'filter', etc. use C-style mutation of the array and return the same data structure back afterwards, no allocation or deallocation needed!

### Data Literals
```clojure
100 ; int
3.14 ; float
true ; bool
"hello" ; string
[1,2,3] ; array
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
(set! variable value)
```

### Algebraic Data Types
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

## The Compiler
Carp is very tightly integrated with it's compiler which itself is written in a dynamic version of Carp (which in turn is implemented in C). To work on a Carp program you run ```carp``` which starts the REPL. Everything you want to do to your program can be controlled from here.

For example, to compile the function defined above you would enter the following:
```clojure
λ> (bake fib)
```

This results in the compiler analyzing the code form for 'fib' and compiling it to some very fast binary code, immediately loading this back into the REPL so that it can be called from there.

(C) Erik Svedäng 2015 - 2016

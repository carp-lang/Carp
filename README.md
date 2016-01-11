# Carp

<img src="https://github.com/eriksvedang/Carp/blob/master/img/temp_logo2.jpg" alt="Logo" align="right" />

Carp is a small programming language designed to work well for interactive and performance sensitive use cases like games, sound synthesis and visualizations.

The key features of Carp are the following:
* Automatic and deterministic memory management (no garbage collector or VM)
* Inferred static types for great speed and reliability
* Live reloading of code, REPL-driven development
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

## The Compiler
Carp is very tightly integrated with it's compiler which itself is written in a dynamic version of Carp (the dynamic version is implemented in C). To work on a Carp program you run ```carp``` which puts you in the compiler REPL. Everything you want to do with your program can be controlled from here.

For example, to compile the function defined above you would enter the following:
```clojure
λ> (bake fib)
```

This results in the compiler analyzing the code form for 'fib' and compiling it to some very fast binary code, loading this back into the REPL so that it can be called conveniently from there.

(C) Erik Svedäng 2015 - 2016

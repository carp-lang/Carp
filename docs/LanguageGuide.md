## The Language

### Introduction
Carp borrows its looks from Clojure but the runtime semantics are much closer to those of ML or Rust.
Types are inferred but can be annoted for readability using the ```the``` keyword (see below).

Memory management is handled by static analysis, a value is owned by the function where it was created.
When a value is returned or passed to another function the initial function will give up ownership of it
and any subsequent use will lead to a compiler error. To temporarily lend a value to another function
(i.e. to print it) a reference must be created, using the ```ref``` special form (or the ```&``` reader macro).

To learn more about the details of memory management, check out [Memory.md](https://github.com/carp-lang/Carp/blob/master/docs/Memory.md)

### Comments
```
;; Comments begin with a semicolon and continue until the end of the line.
```

### Data Literals
```
100     ;; Int
1500l   ;; Long
3.14f   ;; Float
10.0    ;; Double
true    ;; Bool
"hello" ;; &String
\#"hello" ;; &Pattern
\e      ;; Char
[1 2 3] ;; (Array Int)
```

### Type Literals
```
t ;; Type variables begin with a lowercase letter
Int
Long
Float
Double
Bool
String
Pattern
Char
(Array t)
(Fn [<arg-type1> <arg-type2> ...] <return-type>) ;; Function type
```

### Dynamic-only Data Literals
Right now the following data types are only available for manipulation in non-compiled code.

```
(1 2 3) ; list
foo ; symbol
```

### Defining things
```
(defn function-name [<arg1> <arg2> ...] <body>) ;; Define a function (will be compiled, can't be called at the REPL)
(definterface interface-name (Fn [<t1> <t2>] <return>)) ;; Define a generic function that can have multiple implementations
(def variable-name value) ;; Define a global variable (only handles primitive constants for the moment)
(defmacro <name> [<arg1> <arg2> ...] <macro-body>) ;; Define a macro, its argument will not be evaluated when called
(defdynamic <name> [<arg1> <arg2> ...] <function-body>) ;; A function that can only be used at the REPL or during compilation
(defmodule <name> <definition1> <definition2> ...) ;; The main way to organize your program into smaller parts
```

### Special Forms
The following forms can be used in Carp source code and will be compiled to C after type checking
and other static analysis. The first three of them are also available in dynamic functions.

```
(let [<var1> <expr1> <var2> <expr2> ...] <body>) ;; Create local bindings
(do <expr1> <expr2> ... <return-expression>) ;; Perform side-effecting functions, then return a value
(if <expression> <true-branch> <false-branch>) ;; Branching
(while <expression> <body>) ;; Loop until expression is false
(ref <expression>) ;; Borrow an owned value
(address <expression>) ;; Takes the memory address of a value, returns a C-style pointer
(set! <variable> <expression>) ;; Mutate a variable
(the Int <expression>) ;; Explicitly declare the type of an expression
```

### Reader Macros
```
&x ;; same as (ref x)
@x ;; same as (copy x)
```

### Named Holes
When using a statically typed language like Carp it can sometimes be hard to know what value should
be used at a specific point in your program. In such cases the concept of 'holes' can be useful. Just
add a hole in your source code and reload (":r") to let the Carp compiler figure out what type goes there.

```
(StringCopy.append ?w00t @"!") ;; Will generate a type error telling you that the type of '?w00t' is String
```

### Special forms during evaluation of dynamic code
```
(quote <expression>) ;; Avoid further evaluation of the expression
(and) (or) (not) ;; Logical operators
```

### Dynamic functions
These can only be used at the REPL and during macro evaluation. Here's a subset with some of the most commonly used ones:

```
(car <collection>) ;; Return the first element of a list or array
(cdr <collection>) ;; Return all but the first element of a list or array
(cons <expr> <list>) ;; Add the value of <expr> as the first element the <list>
(cons-last <expr> <list>) ;; Add the value of <expr> as the last element the <list>
(list <expr1> <expr2> ...) ;; Create a list from a series of evaluated expressions
(array <expr1> <expr2> ...) ;; Create an array from a series of evaluated expressions
```

To see all functions available in the `Dynamic` module, enter `(info Dynamic)` at the REPL.

### Modules and Name Lookup
Functions and variables can be stored in modules which are named and can be nested. To use a symbol inside a module
you need to qualify it with the module name, like this: ```Float.cos```.

*Using* a module makes it possible to access its members without qualifying them:

```
(use Float)

(defn f []
  (cos 3.2f))
```

If there are several used modules that contain symbols with the same name, the type inferer will try to figure
out which one of the symbols you really mean (based on the types in your code). If it can't, it will display an error.
For example, both the module ```String``` and ```Array``` contain a function named 'length'. In the following code it's
possible to see that it's the array version that is needed, and that one will be called:

```
(use String)
(use Array)

(defn f []
  (length [1 2 3 4 5]))
```

In the following example it's not possible to figure out which type is intended:
```
(use String)
(use Array)

(defn f [x]
  (length x))
```

Specifying the type solves this error:
```
(use String)
(use Array)

(defn f [x]
  (String.length x))
```

### Structs
```
(deftype Vector2 [x Int, y Int])

(let [my-pos (Vector2.init 10 20)]
  ...)

;; A 'lens' is automatically generated for each member:
(Vector2.x my-pos) ;; => 10
(Vector2.set-x my-pos 30) ;; => (Vector2 30 20)
(Vector2.update-x my-pos inc) ;; => (Vector2 11 20)
```

### C Interop
```
(system-include "math.h") ;; compiles to #include <math.h>
(local-include "math.h") ;; compiles to #include "math.h"

(register blah (Fn [Int Int] String)) ;; Will register the function 'blah' that takes two Int:s and returns a String
(register pi Double) ;; Will register the global variable 'pi' of type Double

(register blah (Fn [Int Int] String) "exit") ;; Will register the function 'blah' but use the name 'exit' in the emitted C code.

(register-type Apple) ;; Register an opaque C type
(register-type Banana [price Double, size Int]) ;; Register an external C-structs, this will generate getters, setters and updaters.
```

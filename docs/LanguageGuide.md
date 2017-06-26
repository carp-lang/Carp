## The Language

Carp borrows its looks from Clojure but the runtime semantics are much closer to those of ML or Rust.
Types are inferred but can be annoted for readability using the ```the``` keyword (see below).

### Data Literals
```
100     ; Int
3.14f   ; Float
10.0    ; Double
true    ; Bool
"hello" ; String
\e      ; Char
[1 2 3] ; (Array Int)
```

### Dynamic-only Data Literals
Right now the following data types are only available for manipulation in non-compiled code.

```
(1 2 3) ; list
foo ; symbol
```

### Special Forms
```
(def variable-name value)
(defn function-name [<arg1> <arg2> ...] <body>)
(let [<var1> <expr1> <var2> <expr2> ...] <body>)
(do <expr1> <expr2> ... <return-expression>)
(if <expression> <true-branch> <false-branch>)
(while <expression> <body>)
(ref <expression>) ;; Turns an owned value into an unowned one
(address <expression>) ;; Takes the memory address of a value, returns a C-style pointer
(set! <variable> <expression>) ;; Mutate a variable
(the Int <expression>) ;; Explicitly declare the type of an expression
```

### Named holes
When using a statically typed language like Carp it can sometimes be hard to know what value should
be used at a specific point in your program. In such cases the concept of 'holes' can be useful. Just
add a hole in your source code and reload (":r") to let the Carp compiler figure out what type goes there.

```
(String.append ?w00t "!") ;; Will generate a type error telling you that the type of 'w00t' is String
```

### Reader macros
```
&x ; same as (ref x)
@x ; same as (copy x)
```

### Dynamic-only Special Forms
These can only be used in the REPL and during macro evaluation.

```
(defmacro <name> [<arg1> <arg2> ...] <macro-body>)
(dynamic <name> [<arg1> <arg2> ...] <function-body>)
(quote <expr>)
(car <collection>)
(cdr <collection>)
(list <expr1> <expr2> ...)
```

### Modules and name lookup
Functions and variables can be stored in modules which are named and can be nested. To use a symbol inside a module
you need to qualify it with the module name, like this: ```Float.cos```.

Importing a module makes it possible to access its members without qualifying them:

```
(import Float)

(defn f []
  (cos 3.2f))
```

If there are several imported modules that contain symbols with the same name, the type inferer will try to figure
out which one of the symbols you really mean (based on the types in your code). If it can't, it will display an error.
For example, both the module ```String``` and ```Array``` contain a function named 'count'. In the following code it's
possible to see that it's the array version that is needed, and that one will be called:

```
(import String)
(import Array)

(defn f []
  (count [1 2 3 4 5]))
```

In the following example it's not possible to figure out which type is intended:
```
(import String)
(import Array)

(defn f [x]
  (count x))
```

Specifying the type solves this error:
```
(import String)
(import Array)

(defn f [x]
  (String.count x))
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

### C interop
```
(register blah (Fn (Int Int) String)) ;; Will register the function 'blah' that takes two Int:s and returns a String
(register pi Double) ;; Will register the global variable 'pi' of type Double
```


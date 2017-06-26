## The Language

Carp borrows its looks from Clojure but the runtime semantics are much closer to those of ML or Rust. Here's a sample program:

```clojure
(import String)
(import IO)

(defn say-hi [text]
  (while true
    (if (< (count text) 10)
      (println &"Too short!")
      (println text))))
```

This compiles to the following C program:
```C
void say_MINUS_hi(string* text) {
  bool _55 = true;
  while (_55) {
    int _62 = String_count(text);
    bool _60 = Int__LT_(_62, 10);
    if (_60) {
      string _69 = strdup("Too short!");
      string* _68 = &_69; // ref
      IO_println(_68);
    } else {
      IO_println(text);
    }
    _55 = true;
  }
}
```

If-statements are kind of tricky in regards to memory management:
```clojure
(defn say-what [text]
  (let [manage-me (copy text)]
    (if (< (count text) 10)
      (copy "Too short")
      manage-me)))
```

The 'manage-me' variable is the return value in the second branch, but should get freed if "Too short" is returned.
The output is a somewhat noisy C program:
```C
string say_MINUS_what(string text) {
    string _78;
    string* _84 = &text; // ref
    int _82 = String_count(_84);
    bool _80 = Int__LT_(_82, 10);
    if (_80) {
        string _90 = strdup("Too short");
        string* _89 = &_90; // ref
        string _87 = String_copy(_89);
        String_delete(text);
        _78 = _87;
    } else {
        _78 = text;
    }
    return _78;
}
```

The most important thing in Carp is to work with arrays of data. Here's an example of how that is supposed to look: (NOT FULLY IMPLEMENTED YET)

```clojure
(defn weird-sum [nums]
  (reduce + 0 (map inc (filter even? nums))))
```

All the array modification functions like 'map', 'filter', etc. use C-style mutation of the array and return the same data structure back afterwards, no allocation or deallocation needed. The lifetime analyzer ("borrow checker" in [Rust](https://www.rust-lang.org) parlance) makes sure that the same data structure isn't used in several places.

To know whether a function takes over the responsibility of freeing some memory (through its args) or generates some new memory that the caller has to handle (through the return value), just look at the type of the function (right now the easiest way to do that is with the ```(env)``` command). If the value is a simple type like String, Vector3, or similar, it means that the memory ownership gets handed over. If it's a reference signature (i.e. ```(Ref String)```), the memory is just temporarily lended out and someone else will make sure it gets deleted. When interoping with existing C code it's often correct to send your data structures to C as refs or pointers (using ```(address <variable>)```), keeping the memory management inside the Carp section of the program.

### Data Literals
```clojure
100 ; Int
3.14f ; Float
10.0 ; Double
true ; Bool
"hello" ; String
\e ; Char
[1 2 3] ; (Array Int)
```

### Dynamic-only Data Literals
Right now the following data types are only available for manipulation in non-compiled code.

```clojure
(1 2 3) ; list
foo ; symbol
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
(address x) ;; Takes the memory address of a value, returns a C-style pointer
(set! variable value)
(the Int x) ;; explicitly tell the type of an expression
```

### Named holes
```clojure
(String.append ?w00t "!") ;; Will generate a type error telling you that the type of 'w00t' is String
```

### Reader macros
```clojure
&x ; same as (ref x)
@x ; same as (copy x)
```

### Dynamic-only Special Forms
```
(import <module>) ;; Make it possible to access members of a module without qualifying them with "ModuleName."
```

### Structs
```clojure
(deftype Vector2 [x Float, y Float])

(let [my-pos (Vector2.init 102.2f 210.3f)]
  ...)

;; A 'lens' is automatically generated for each member:
(Vector2.x my-pos) ;; => 102.2f
(Vector2.set-x my-pos 3.0f) ;; => (Vector2 10.2f 3.0f)
(Vector2.update-x my-pos inc) ;; => (Vector2 10.2f 4.0f)
```

### C interop
```clojure
(register blah "foo" (Fn (Int Int) String)) ;; will register the function 'foo' that takes two ints and returns a string
```


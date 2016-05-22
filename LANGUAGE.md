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
  bool while_expr_1 = 1;
  while(while_expr_1) {
    int strlen_result_2 = strlen(text);
    bool if_expr_3 = strlen_result_2 < 10;
    if(if_expr_3) {
      println("Too short!");
    } else {
      println(text);
    }
    while_expr_1 = 1;
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
3.14f ; float
10.0 ; double
true ; bool
"hello" ; string
\e ; char
[1 2 3] ; array
```

### Dynamic-only Data Literals
Right now the following data types are only available for manipulation in non-compiled code.

```clojure
(1 2 3) ; list
foo ; symbol
:blergh ; keyword
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

### Reader macros
```clojure
&x ; same as (ref x)
@x ; same as (copy x)
```

### Dynamic-only Special Forms
```
(quote x)
(match x pattern-1 expr-1, pattern-2 expr-2, ...)
```

### Structs
```clojure
(defstruct Vector2 [x :float, y :float])

(def my-pos (Vector2 102.2f 210.3f))

;; A 'lens' is automatically generated for each member:
(get-x my-pos) ;; => 102.2f
(set-x my-pos 3.0f) ;; => (Vector2 10.2f 3.0f)
(update-x my-pos inc) ;; => (Vector2 10.2f 4.0f)
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
(defdata Vector3 [x :double, y :double, z :double])

(def position (Vector3 4.0 5.0 -2.0))
(def x-position (.x position)
```

### C interop
```clojure
(def blah (load-dylib "./libs/blah.so"))
(register blah "foo" (:int :int) :string) ;; will register the function 'foo' in the dynamic library 'blah' that takes two ints and returns a string
```

### Type annotations
There should never be a need for explicit type annotations in Carp. Still, they can be useful to show intent and make sure that the compiler does thing you were planning for it to do. Type annotations are added using meta data ('ann' stands for 'annotation') on the function form, like this:

```clojure 
^ann '(:fn ((:ref :Ship)) :void)

(defn draw-ship [ship]
  (let [pos (get-shipPos ship)]
    (draw-rect (get-x pos) (get-y pos) 10f 10f)))
```

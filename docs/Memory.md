# Memory Management - a closer look

### Related pages

* [Drop](Drop.md) - a deeper look at the `drop` interface

The goals of the memory management system in Carp are the following:

* Predictable
* Efficient
* Safe

This is achieved through a linear type system where memory is owned by the function or let-scope that allocated it. When the scope is exited the memory is deleted, unless it was returned to its outer scope or handed off to another function (passed as an argument). The other thing that can be done is temporarily lending out some piece of memory to another function using a ref:

```
(let [s (make-string)]
  (println &s))
```

In the example above s is of type String and it's contents are temporarily borrowed by 'println'. When the let-scope ends Carp will make sure that a call to `(String.delete s)` is inserted at the correct position. To avoid 's' being deleted, the let-expression could return it:

```
(let [s (make-string)]
  (do (println &s)
      s))
```

## Rule of thumb

To know whether a function takes over the responsibility of freeing some memory (through its args) or generates some new memory that the caller has to handle (through the return value), just look at the type of the function (right now the easiest way to do that is with the ```(env)``` command). If the value is a non-referenced struct type like String, Vector3, or similar, it means that the memory ownership gets handed over. If it's a reference signature (i.e. ```(Ref String)```), the memory is just temporarily lended out and someone else will make sure it gets deleted. When interoping with existing C code it's often correct to send your data structures to C as refs or pointers (using ```(Pointer.address <variable>)```), keeping the memory management inside the Carp section of the program.


## Working with arrays

The most important thing in Carp is to process arrays of data. Here's an example of how that is supposed to look:

```clojure
(defn weird-sum []
  (let [stuff [3 5 8 9 10]]
    (reduce add 0 &(endo-map square (filter even? stuff)))))
```

All the array transforming functions 'endo-map' and 'filter' use C-style mutation of the array and return the same data structure back afterwards, no allocation or deallocation needed. The lifetime analyzer ("borrow checker" in [Rust](https://www.rust-lang.org) parlance) makes sure that the same data structure isn't used in several places.

The restriction of 'endo-map' is that it must return an array of the same type as the input. If that's not possible, use 'copy-map' instead. It works like the normal 'map' found in other functional languages. The 'copy-' prefix is there to remind you of the fact that the function is allocating memory.

To execute side-effects, use the `doall` macro:

```
(doall IO.println [@"Yo" @"Hola" @"Hej"])
```

Or `foreach` (works like a foreach loop construct in a lot of other programming languages):

```
(foreach [x &[1 2 3]]
  (println* "x: " x))
```


## Under the hood

A simple piece of code:

```clojure
(use Int)
(use String)
(use IO)

(defn say-hi [text]
  (while true
    (if (< (length &text) 10)
      (println "Too short!")
      (println &text))))
```

This compiles to the following C program:
```C
void say_MINUS_hi(string text) {
    bool _5 = true;
    while (_5) {
        string* _14 = &text; // ref
        int _12 = String_length(_14);
        bool _10 = Int__LT_(_12, 10);
        if (_10) {
            string _19 = "Too short!";
            string *_19_ref = &_19;
            IO_println(_19_ref);
        } else {
            string* _22 = &text; // ref
            IO_println(_22);
        }
        _5 = true;
    }
    String_delete(text);
}
```

If-statements are kind of tricky in regards to memory management:
```clojure
(defn say-what [text]
  (let [manage (copy &text)]
    (if (< (length &text) 10)
      (copy "Too short")
      manage)))
```

The 'manage' variable is the return value in the second branch, but should get freed if "Too short" is returned.
The output is a somewhat noisy C program:
```C
string say_MINUS_what(string text) {
    string _5;
    /* let */ {
        string* _11 = &text; // ref
        string _9 = String_copy(_11);
        string manage = _9;
        string _13;
        string* _19 = &text; // ref
        int _17 = String_length(_19);
        bool _15 = Int__LT_(_17, 10);
        if (_15) {
            string _24 = "Too short";
            string *_24_ref = &_24;
            string _22 = String_copy(_24_ref);
            String_delete(manage);
            _13 = _22;
        } else {
            _13 = manage;
        }
        _5 = _13;
    }
    String_delete(text);
    return _5;
}
```

## Custom deletion functions

The Carp compiler will autogenerate a deletion function for your types unless
you specify your own—`delete` is an interface like any other. Sometimes you
might want to do some work of your own when a value goes out of scope, be it
because it’s an value defined in C or because it references an OS resource that
needs a cleanup action, like a file or socket.

The `delete` function is responsible for cleaning up any memory associated with
the value—otherwise the program will leak memory. It can execute arbitrary
code, however, and can thus be used for other purposes as well.

Let’s look at an example program:

```clojure
(register-type Foo)
(register-type Bar)

(defmodule Foo
  (register init (Fn [] Foo))
  (register delete (Fn [Foo] ()))
  (implements delete Foo.delete))

(defmodule Bar
  (register init (Fn [] Bar)))

(defn f []
  (let [a (Foo.init)
        b (Bar.init)]
    ()))
```

The code for `f` will look like this:

```c
void f() {
    /* let */ {
        Foo _6 = Foo_init();
        Foo a = _6;
        Bar _9 = Bar_init();
        Bar b = _9;
        /* () */
        Foo_delete(a);
    }
}
```

Note that a deleter is emitted for the value of type `Foo` once the `let` block
ends and it goes out of scope, but not for the value of type `Bar`, which has
no deleter associated with it.

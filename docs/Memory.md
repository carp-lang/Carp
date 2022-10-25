# Memory Management - a closer look

Carp uses a *linear type system* to manage the memory associated with different
values throughout a program. The goals of the memory management system in Carp
are the following:

* Predictable: The memory management system's behavior should be easy to reason
  about.
* Efficient: The memory management system should not have significant impacts
  on performance.
* Safe: The memory management system should prevent errors related to memory
  management, such as "use after free" and "double free"

This document describes how the memory management system works.

## Linear Types and Scopes

Carp's linear type system tracks the *ownership* of the memory associated with
a given value as part of its type signature. A *linear type* is a traditional
type with additional information, called a *lifetime* that allows the type
system to track a value's association with a given memory location.

The memory management system *only* manages linear types; not all types are
linear. Some of Carp's builtin types are linear by default:

- The String type is linear and managed by the memory system.
- The Pattern type is linear and managed by the memory system.
- The Array type is linear and managed by the memory system.
- The Box type is linear and managed by the memory system.
- Function types are linear and managed by the memory system.

All other builtin types are not linear, and thus aren't managed by the memory
system.

A few conditions determine whether or not a user defined type is linear:

- **Implementation of the `blit` interface**: this interface explicitly marks a
  type as *non-linear*. Any type that implements it is ignored by the memory
  management system and is assumed to pose no risks in relation to memory
  allocation and deallocation. For example, any type that can be stack allocated
  in C and never needs to be passed by reference.
- **Implementation of the `delete` interface**: this interface explicitly marks
  a type as *linear*. Any type that implements it is managed by the memory
  management system. Carp will call the implementation of this interface whenever
  the memory management system decides it's safe to deallocate the memory
  associated with a value of the type that implements this interface. 
 
In the next section, we'll explore a few key memory system operations and
examples to illustrate how the system manages values of linear types.

## Ownership

By default, the memory associated with a value of a linear type is associated
with the *binding* of the value. The binding is always some name given to the
value. For example, in a let declaration, such as `[s (make-string)]`, `s` is
the binding for an associated linear String value. In a function call, such as
`(do-something (make-string))` the allocated string is instead associated with
the binding determined by the function declaration's parameters; e.g. if we
declared `do-something` as `(defun do-something [y])` the linear String value
would be associated with the binding `y`. 

When a binding is associated with a linear type value, the binding *owns* the
value. This association is called the value's *lifetime*. In the most basic
case, the value's lifetime is the same as the binding's *scope*: the binding is
no longer valid when its lexical scope ends (e.g. we cannot refer to a function
parameter outside the body of a function), when this happens, the memory system
ensures that the memory associated with the linear value tied to the binding is
deallocated. These concepts in concert are often referred to as *ownership*. A
binding is said to *own* a value when the invalidation of the binding
corresponds to the invalidation of its associated memory. We could also express
this by claiming the linear value has a *lexical lifetime*: the validity of its
memory is determined by lexical scope.

We'll consider a short example to illustrate this point, consider the following
let declaration:

```
(let [s (make-string)])
```

In this short let form, the binding `s` owns the memory associated with a
linear string value. We don't do anything with this value, and as soon as the
let scope ends (at the second parenthesis) the memory associated with `s` is
cleaned up and no longer usable. 

This is all well and good, but we cannot do much with strictly lexical memory.
We'll explore how we can actually use linear values next.

## Borrowing and Transferring  
 
Where memory is owned by the function or let-scope that allocated it. When the
scope is exited the memory is deleted, unless it was returned to its outer
scope or handed off to another function (passed as an argument). The other
thing that can be done is temporarily lending out some piece of memory to
another function using a ref:

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

To know whether a function takes over the responsibility of freeing some memory (through its args) or generates some new memory that the caller has to handle (through the return value), just look at the type of the function (right now the easiest way to do that is with the `(env)` command). If the value is a non-referenced struct type like String, Vector3, or similar, it means that the memory ownership gets handed over. If it's a reference signature (i.e. `(Ref String)`), the memory is just temporarily lended out and someone else will make sure it gets deleted. When interoping with existing C code it's often correct to send your data structures to C as refs or pointers (using `(Pointer.address <variable>)`), keeping the memory management inside the Carp section of the program.


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

The Carp compiler will auto-generate a deletion function for types created on
the Carp side. The `delete` function is responsible for cleaning up any memory
associated with its associated value when it goes out of scope. Type that are
defined in C do not have a `delete` function generated for them automatically,
you can write your own deletion function, declare it to be implementing `delete`
and the Carp compiler will call it automatically for you. You can check if a
type has `delete` implemented by using `Dynamic.managed?`.

As the `delete` interface is responsible for freeing memory, it is **unsafe**
to override it, if you are looking for how to release other type of resources
(sockets, file handle, etc...) when a value goes out of scope use the [`drop`
interface](./Drop.md) instead.

Let’s look at an example program of how to add a deletion function to a type
defined in C:

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

### Related pages

* [Drop](Drop.md) - a deeper look at the `drop` interface


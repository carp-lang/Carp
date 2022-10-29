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


## Under the Hood: The Implementation of Carp's Memory Management System

This section explores the implementation of Carp's memory management system in
greater technical detail. Most users won't need to read this, but if you'd like
to have a deeper understanding of how the system works, you'll find an
explanation in this section.

### AST Info, Identifiers, and Deleters

Like other portions the Carp compiler, the memory management system operates on
the abstract syntax tree (AST) representation of the forms in your program. When
the compiler compiles your code, it assigns addition *information objects*,
called `Info`, to each form in your program; these objects are particularly
important to the memory management system. Among other things, these `Info`
objects contain unique identifiers for each form in your program. The memory
management system uses these identifiers to keep track of memory as it moves
across different parts of your code.

In addition to identifiers, form information objects also contain `Deleters`.
These are a special data structure used to hold information about the `delete`
functions needed for each linear value in your program. One of the memory
management system's main responsibilities is to assign and keep track of these
deleters for each form in your program that makes use of a linear value.

Essentially, as the memory management system examines your code, if it finds a
form that uses a linear value that should be deleted at a certain point, it adds
an appropriate deleter to the info object for the form. If the linear value is
*moved* to some other part of your code, the memory management system will
remove the corresponding deleter, which will be added to the form it's moved
into later. 

The key point to understand is that the memory management system primarily
models the movements of linear values using the presence or absence of these
deleter objects. When the compiler's code emission component encounters a form,
if the form has an associated deleter, the emitter will produce a call to the
deletion routine in the corresponding output C code.

As we'll see in a moment, there are some further complications, but this is the
basic approach taken by the memory management system.

### Lifetimes

The basic operation of the memory management system entails moving deleters
across different Info objects for the forms in your program. As the system
performs this task, it also has to account for the way *references* are used
throughout your code, and how they relate to linear values. In order to track
this, the memory management system uses *lifetimes* which determine whether or
not a reference is valid in a given form.

The following function provides an example of this reference validity tracking
in action:

```clojure
(defn invalid-ref []
  &[1 2 3])
```

In the prior example, our `invalid-ref` function returns a reference to the
literal linear array value `[1 2 3]`. This code is problematic because the
linear array value will be deleted at the end of the function, so the returned
reference will point to nothing! The memory management system catches this for
us and let's us know about the problem.

Contrarily, the following code is perfectly fine:

```clojure
(def an-array [1 2 3])

(defn valid-ref []
  &an-array)
```

The `valid-ref` function also returns a reference, but this reference is valid
since it points to a linear array value (`an-array`) that won't be deleted (it
will still be "alive") by the time the function returns the reference.

The system will also catch cases when we attempt to reference a linear value
that's already been *moved* into a different location/binding:

```clojure
(defn unowned-ref []
  (let [a [1 2 3]
        b a 
        c &a]
  ()))
```

In this example, we move the linear array from `a` into `b`, but then try to set
`c` to a reference to `a`, which, after the move, no longer points to anything.

Internally, the memory management system uses *lifetimes* to model the
relationships between references and linear values and track the validity of
reference across your code.

#### Lifetimes in Detail

Carp's lifetimes are made up of two pieces of information. Only references have
lifetimes, and every reference has *exactly one* lifetime assigned to it:

- A unique type variable that identifies the lifetime.
- A lifetime mode, that indicates if the linear value tied to the reference has
  a lexical scope that extends beyond the reference's lexical scope or if it's
  limited to the reference's lexical scope.

In general, a reference is valid only when the value it points to has either an
equivalent or greater lexical scope. This property is encoded in its lifetime.

Let's look at some examples to help illustrate this:

```clojure
(def an-array [1 2 3])

(defn valid-ref [] 
  (let [array-ref &an-arry]) ())
```

In this example, the anonymous reference `&an-array` has a unique lifetime that
*extends beyond the lexical scope* of the reference itself. The lexical scope of
the reference value `[1 2 3]` is greater than or equal to the lexical scope of
the reference, which only extends across the let form, so, this reference is
valid.

Contrarily, the following reference is not valid:

```clojure
(defn invalid-ref []
  &[1 2 3])
```

Here, the reference has a greater lexical scope than the linear value it points
to. The anonymous linear value `[1 2 3]` will be deleted at the end of the
function scope, but the reference will be returned from the function, so its
lifetime is potentially greater than that of the value it points to. 

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

# Memory Management - a closer look

Carp uses a *linear type system* to manage the memory associated with different
values throughout a program. Carp's memory management system is designed and
implemented with the following goals in mind: 

* Predictable: The memory management system's behavior should be easy to reason
  about.
* Efficient: The memory management system should not have significant impacts
  on performance.
* Safe: The memory management system should prevent errors related to memory
  management, such as "use after free" and "double free"

This document introduces the basic concepts behind the kind of linear type
system Carp uses. In addition, it takes a deeper look at how the system is
currently implemented.

## Linear Types and Memory Management

Carp's linear type system tracks the *ownership* of the memory associated with a
given value as part of its type signature. A *linear type* is a traditional type
with additional information called a *lifetime* that allows the type system to
track a value's association with a given memory location.

The memory management system *only* manages linear types; not all types are
linear. Some of Carp's builtin types are linear by default:

- The String type is linear and managed by the memory system.
- The Pattern type is linear and managed by the memory system.
- The Array type is linear and managed by the memory system.
- The Box type is linear and managed by the memory system.
- Function types are linear and managed by the memory system.

All other builtin types are *not* linear, and thus aren't managed by the memory
system.

A few conditions determine whether or not a user defined type is linear:

- **Implementation of the `blit` interface**: this interface explicitly marks a
  type as *non-linear*. Any type that implements it is ignored by the memory
  management system and is assumed to pose no risks in relation to memory
  allocation and deallocation.
- **Implementation of the `delete` interface**: this interface explicitly marks
  a type as *linear*. Any type that implements it is managed by the memory
  management system. Carp will call the implementation of this interface whenever
  the memory management system decides it's safe to deallocate the memory
  associated with a value of the type that implements this interface. 

When you define a type directly in Carp code, using `deftype` Carp will
*automatically implement the delete interface for you*. As a consequence, any
type that you declare using `deftype` will be managed by the memory management
system. In the most cases, this automatic management of user defined types is
beneficial. You can always redefine `delete` for your type if you need to write
a custom memory deallocation routine. However, if you need to define a type
that requires fine-grained control over its memory deallocation, it might be
better to define both the type and its deallocation routines in C, and register
them in Carp.

The same conditions hold for [registered types][3] as well. If you register an
external type defined in C, Carp won't manage it unless you provide an
implementation of `delete` for the corresponding Carp type. See the [C interop
documentation][3] for more information.

In the following sections, we'll explore a few key memory system operations.
Along the way, we'll present examples using Carp's builtin linear String type
to illustrate how the system manages values of linear types.

## Bindings, Ownership, and Lexical Scopes

Unless your program is incredibly short, you'll likely have one or more
*bindings* that associate names with values in your program. Typically, we can
assign the value of one binding to another. Consider the following local
variables in a let form: 

```clojure
(let [x 1
      y x
      z x]
  x)
```

In the example above, we assign the non-linear value *1* to `x`, then assign the
value of `x` to `y`, then assign the value of `x` to `z`.

In Carp, linear values are treated differently. When we assign a linear value to
a binding, such as a local variable name, the *memory location* associated with
the value is also bound to the name. This changes the rules about how we can
assign values and pass them around a program. If we try to write the same
program as we did above, using a value of the linear type, `String`, we get
quite a different result:

```clojure
;; Don't worry about the @ before the string literal. We'll explain it soon.
(let [string @"linear types!"
      other-string string ;; used here!
      yet-another-string string] ;; and here!
  string) ;; and again here!
```

If you try to pass this program to the Carp compiler, you'll get an error in
return: `You’re using a given-away value string`.

This illustrates the 'golden rule' that the memory management system enforces:
**every linear value can only be used once**. When we first assign `@"linear
types!"` to the variable `string`, we've already used it once. When we attempt
to assign `string` to `other-string` *and* `yet-another-string`, the memory
management system will detect that we're attempting to use the single value
`@"linear types!"` multiple times, which it won't allow. Note that *only*
assigning the value to string, then `string` to `other-string` is OK, as long as
we return `other-string`—we'll explain why in a later section.

In casual terminology, this concept is called *ownership*. The binding to which
the linear value is assigned *owns* its associated memory. We can call such a
binding the value's *owner*. In this example, `string` is the initial owner of
the memory allocated for the linear value `@"linear types!"`.

Every binding in Carp has a *lexical scope* that determines where in the program
the binding name is defined and can be validly referenced. The lexical scope of
`string` in our example happens to be our let form. The lexical scope of a
function parameter is only the body of the function.

A linear value can only be used *once* in a single *lexical scope*. We "use" a
linear value whenever we *pass it to a different lexical scope*. For example, we
"use" `string`, if we return it:

```clojure
(let [string @"linear types!"]
  string) ;; used here!
```

We also use it when we pass it to another function:

```clojure
(let [string @"linear types!"]
  (do (reverse string) ;; used here!
      ())) 
```

What do both of these cases have in common? They raise the possibility that
`string`'s value (and it's assocaited memory) is passed to *another binding*
(when we pass it to a function, it's rebound to the function parameter; when we
return it, the caller might bind it to a new name in the lexical scope that
contains our let). As we'll see later, these are two particular examples of a
specific form of an operation we'll call *moving*. Binding the value of an
existing linear binding, as in `(let [string @"linear types!" other-string
string] ()))` is also a case of moving.

### Safe Deallocations

The "use once" restriction is the mechanism that allows the memory management
system to prevent classical memory errors such as "use after free" and "double
free". Enforcing that a linear value is only used *once* in any lexical scope,
allows the management system to determine precisely when a binding's associated
memory can be freed.

When the memory management system determines some linear value will no longer be
used in the lexical scope of it's *owner*, it automatically calls the
corresponding linear type's `delete` implementation to free the associated
memory.

Now that we have an initial sense about the restrictions the memory management
system enforces around our use of linear values, we'll explore a few operations
the system performs that allow us to have greater flexibility.

## Moving, Borrowing, and Copying

At a high level, the functionality of the linear type system can be organized
into three primary operations: *moving*, *borrowing*, and *copying*. These are
casual, intuitive terms for what the system does with linear values as it
manages them across your program. We'll explore precise technical terminology
for each of these operations later on.

### Moving: Transferring Ownership

The memory management system ensures that only one binding ever owns the memory
associated with a given linear value. As a result, unlike non-linear values,
the compiler won't let you bind the same linear value to more than one
variable. Instead, when you reassign a linear value to another variable, The
old binding is invalidated, as we saw earlier:

```clojure
(let [string @"linear types!"
      other-string string
      yet-another-string string] ;; error here!
  ()) 
```

In the prior example, the binding `string` is invalidated as soon as we assign
it to the binding `other-string`. The memory associated with `string` is now
associated with the binding `other-string`, and `other-string` is the linear
string's new owner. This process is called a *transfer of ownership*, or
*moving*.

If we were to move our value across a number of bindings in sequence, we'd fix
our problem! There's no issue with moving some linear value across different
bindings in a lexical scope, there's only an issue if we attempt to move a value
out of *the same binding* more than once:

```clojure
(let [string @"linear types!"          ;; linear string value here.
      other-string string              ;; moved over to this binding
      yet-another-string other-string] ;; still ok, moved to this binding
  ()) 
```

The important, and only rule about moving linear values is: *you can only move a
linear value from an individual binding __once__* in any given lexical scope. If
your code attempts to move a linear value from a binding more than once, the
memory management system will chastise you!

#### Moving to a New Scope

Just as we transferred ownership of a linear value to another binding in the
same lexical scope, we can use ownership transfers to move a linear value into
a binding *beyond* its lexical scope. Consider this next example:

```clojure
(let [string @"linear moves!"]
  string)
```
 
Though it's not as obvious, a move is happening here! In this case, a transfer
of ownership occurs across lexical scope boundaries. The linear string
associated with `string` and its corresponding memory are returned from the let
form. If the result of this let form is bound to some other variable, that new
binding will receive ownership of the linear string. Thus, the lifetimes of
linear values are not only limited to their lexical scopes. We can move linear
values in and out of other scopes, and the memory management system will
determine in which part of our code the value can be safely deallocated.

Again, since returning the value is a *move*, or ownership transfer, the same
rules around moves apply: we can only make *one* move out of a given binding in
a single lexical scope:

```clojure
(let [string @"linear moves!"
      other-string string] ;; ok; first move out of string
  string) ;; error! Second move out of string
```

Passing a linear value as an argument to a function is another example of a move
across lexical scopes. For instance, consider the following example:

```clojure
(let [string @"linear moves!"
      reversed (reverse string)] ;; moved here!
  reversed)
```

In this example, we *move* the linear value associated with `string` into the
function's lexical scope, binding it to whatever parameter name the function
declaration used for its first argument. Since we only moved the value out of
`string` once, the memory management system happily accepts this program.

#### Beyond Moves

In some cases, transferring ownership might be too limiting. Let's reconsider
the earlier example, in which we tried to transfer ownership from `string` more
than once:

```clojure
(let [string @"linear moves!"
      other-string string
      yet-another-string string] ;; error!
  ())
```

We might want to write "multi-move" code like this, but under the current rules
of the linear type system, we can't. Luckily, there's a way out: references. 

### Borrowing: Lending Ownership

As we explored in the previous section, we can’t assign a linear value to
multiple bindings without transferring ownership. If we move a value from one
binding to another, we can only do so once, even if one of those moves transfers
ownership beyond the current scope. At any given point in a lexical scope, only
one binding can ever own the linear value.

This restriction ensures the type system knows exactly when to deallocate the
memory associated with a linear value, but it can be a bit limiting. For
example, what if we wanted to process the value using a function, then use our
original value afterwards?

```clojure
(let [string @"linear borrow!"
      reversed (reverse string)]
  (concatenate string reversed)) ;; error!
```

This short let block calls some imaginary functions to first reverse our linear
string, then join it with itself, returning the result. However there’s a big
problem here, once we move our string to `reverse`'s parameter the memory
management system won’t let us use it in `concatenate` since it violates the
"one move" rule.

This time, the rule has put us in quite a difficult situation. We want to write
a program that uses `string` twice, but there's no way for us to use it twice
directly, thanks to the linear type system rules. Just passing `string` along to
other bindings won't help us here either.

Luckily, there’s a mechanism that allows us to reuse `string` more than once in
our let block: *references*.

References are another special type that the memory management system
understands how to work with. References are *not* linear types, but they give
us another way of working with linear types that allows us to get around the
type system’s “one owner” and "one move" restriction safely.

A reference value points to some linear value, but because the reference is not
linear value itself, but rather a new, non-linear value, we’re allowed to pass
them around freely, just like we can with other non-linear values. Assigning a
reference to a linear value to some binding is called *borrowing*. Instead of
transferring ownership of a linear value to a new binding, we’re giving it a
temporary way to access the value, without taking it over and moving it.

Use the `&` operator, or `ref` special form, to create a reference:

```clojure
(let [string @"hello, linear world!"
      reversed (reverse &string)] ;; reference to string
  (concatenate string reversed) ;; ok; first move of string
```

Using references, we can get our initial string reversal and concatenation
program to work, the memory manager won’t complain. Since `string` is *borrowed*
by `reverse`, using a reference, there’s no longer an issue using it directly in
`concatenate` since this is now the one and only time it transfers ownership
(moves).

In this case, we have no idea how `reverse` actually uses the reference to
produce a reversed string, but we’ve followed the memory management system’s
rules correctly. In the next section, we'll explore how we can actually make use
of the reference in an implementation of a function like reverse.

### Copying: Increasing Supply

Now that we’ve explored references and borrowing, you might wonder what we can
do with references. Again, references are not linear values themselves, but
they “point” to linear values. Their behaviors and relation to the type system
differ. So, what can we accomplish with references?

In Carp, references, in general, (but we'll see that there are some special
cases) support only a single operation, called *copying*. Copying a reference
creates a new linear value that *duplicates* the linear value the reference is
pointing to. This new linear value is completely distinct from the original
linear value the reference points to. It has its own owner, and, just like other
linear values, the memory management system will determine when to remove it. 

Copying allows us to work with some linear value in multiple places in a safe
way. To copy the value pointed to by a reference, use the `@` operator. The
following example shows how the `reverse` function might be implemented:

```clojure
(defn reverse [string-ref]
  (reverse-internal @string-ref)) ;; reference copied here!
```

The function takes a reference to a linear string value, makes a copy of it,
reverses the copy, then returns the resulting linear value to the caller. Note
that we haven’t touched the original linear string pointed to by the
`string-ref` reference, we only work with a copy!

We can also now understand the general string literal syntax we've used
throughout this text: 

```clojure
string @"hello, linear world!"
```

This binds a *copy* of the string literal to the variable `string`. This
reveals an important aspect of Carp’s builtin string literals: they are
references! We’ll explore why this makes sense a bit later.

## Rule of thumb

To know whether a function takes over the responsibility of freeing some memory
(through its args) or generates some new memory that the caller has to handle
(through the return value), just look at the type of the function (right now
the easiest way to do that is with the `(env)` command). If the value is a
non-referenced struct type like String, Vector3, or similar, it means that the
memory ownership gets handed over. If it's a reference signature (i.e. `(Ref
String)`), the memory is just temporarily lended out and someone else will make
sure it gets deleted. When interoping with existing C code it's often correct
to send your data structures to C as refs or pointers (using `(Pointer.address
<variable>)`), keeping the memory management inside the Carp section of the
program.

## Working with arrays

The most important thing in Carp is to process arrays of data. Here's an example of how that is supposed to look:

```clojure
(defn weird-sum []
  (let [stuff [3 5 8 9 10]]
    (reduce add 0 &(endo-map square (filter even? stuff)))))
```

All the array transforming functions 'endo-map' and 'filter' use C-style mutation of the array and return the same data structure back afterwards, no allocation or deallocation needed. The lifetime analyzer ("borrow checker" in [Rust][1] parlance) makes sure that the same data structure isn't used in several places.

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

* [Drop][2] - a deeper look at the `drop` interface


[1]:	https://www.rust-lang.org
[2]:	Drop.md
[3]:    CInterop.md#register-types

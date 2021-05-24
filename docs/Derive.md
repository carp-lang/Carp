# Derive

`derive` is a mechanism that automatically determines how to implement
interfaces for datatypes based on their members. It also allows you to write
your own rules for `derive`, called a `deriver`.

If you’d like to learn how to `derive` interfaces for your types, read the
[first section](#i-using-derive) of this document. If you’d like to provide a
deriver for an interface, read [the second section](#ii-writing-derivers) of
this document.

## I: Using `derive`

In most cases, using `derive` should be as simple as calling it with the type
name and interface to implement:

```clojure
(deftype Point [
  x Int
  y Int
])
(derive Point zero)
(derive Point =)

; if you’d like to generate a different function name
; pass it as a third argument. This is useful to avoid
; name collisions
(derive Point str my-str)
```

The code above will provide implementations of `zero` and `=` for the type
`Point` based on its members. The prerequisites for this to work
are that types are *concrete*—there are no type variables present—and *its
members implement the interface*. This is because the definition of both
functions hinges on the definition of its members: `zero` on a type is just
`zero` of all its members, equality of a type just equality of all of its
members.

Carp only provides automatic derivation of `=`, `zero`, and `str`. Since the
code you depend on might provide other derivers, you can inspect them by
calling `(derivables)`. If you want to find out if a certain interface is
derivable, you can call `(derivable? <interface>)`. Please note that the
interface name needs to be quoted.

If either of the preconditions above is not met, you will have to write your
own version of these functions, and may not use `derive`.

Some users might want to be able to derive update interfaces that take a type,
do the same thing to all its members, and return it. A good example for this
in the context of `Point` is `inc`.

While generally this might require you to write your own deriver—see [section
II](#ii-writing-derivers) of this document to learn how to do that—, Carp
provides a special dynamic function called `make-update-deriver`. It takes a
unary interface that updates a value and returns it, and extrapolates a
definition for the encompassing type. This is what this would look like for
`Point`:

```clojure
(make-update-deriver 'inc) ; notice the quote
(derive Point inc)

(inc (Point.zero)) ; => (Point 1 1)
```

While this can be useful at times, it is limited to the special case of
functions outlined above: it can only used on functions you would also be able
to pass into `update-<member>` style functions.

## II: Writing derivers

Sometimes you might want to provide your own derivation strategy for other
interfaces than the ones provide out of the box. In these cases you can provide
your own deriver using `make-deriver`.

The dynamic function `make-deriver` takes three arguments: the quoted name of
the interface, the names of the arguments it will be passed, and a function
that, given a type, knows how to generate an implementation for that type.

This might sound a little strange, so let’s consider the deriver for `zero` as
an example:

```clojure
(make-deriver 'zero []
  (fn [t]
    (cons 'init
      (map (fn [_] '(zero)) (members t)))))
```

It usually makes sense to read `make-deriver` similar to a function definition:
its interface name is `zero`, which takes no argument, and we know that if
we’re given a type we can create a definition for `zero` if we just emit a
call to `zero` for every member, wrapped in an `init`. Thus the definition for
`zero` for the type `Point` from above will end up looking like this:

```clojure
(init (zero) (zero))
```

`derive` itself will emit all the surrounding boilerplate, such that the entire
call to `(derive Point zero)` will be rewritten to:

```clojure
(defmodule Point
  (defn zero []
    (init (zero) (zero)))
  (implements Point.zero zero)
)
```

This means that all a deriver has to know is how to generate a function body
when it’s given a type. Since it also has control over the argument names, it
can use the arguments in its definition as well.

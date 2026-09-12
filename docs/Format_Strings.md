# Format Strings

{% raw %}

Carp as to ways to format strings, `fmt` and `fstr`. In this document, we
explore both functions in detail.

## `fmt`

`fmt` allows for more control than `fstr`, but also requires more knowledge
about the data.

```clojure
(fmt "this is an integer %d and this is a string %s." 1 "hi")
```

It’s works similarly to [`printf`](https://en.wikipedia.org/wiki/Printf_format_string)
in C. `fmt` will check that the amount of arguments and format specifiers in
the format string match.

All arguments to `fmt` must implement the `unsafe-format` interface, which is
defined as:

```clojure
(definterface unsafe-format (Fn [String a] String))
```

The types are expected to take a format specifier and format according to it.
As such, which format specifiers are supported is dependent on the
implementation of `unsafe-format` on that type. Standard library types expose
regular format specifiers as in C.

Also, all `fmt` format strings must be literals.

## `unsafe-format`

`unsafe-format` formats a single value. It is a thin wrapper over `snprintf`
and similar C functions, and it is unsafe in the way its name suggests: it does
no compile-time checking, so a format string that does not carry exactly one
directive reads arguments that were never passed.

```clojure
(Int.unsafe-format "%d%d" 1) ; one argument, two directives
```

Unless the program is built with `NDEBUG`, every `unsafe-format` implementation
checks its format string at runtime and aborts on a mismatch, in the same way
array indexing is bounds-checked. This is a backstop, not a substitute for
checking: prefer `fmt`, which verifies the format string at compile time and
works for any number of arguments.

## `fstr`

Similarly to `fmt`, `fstr` takes a literal string. It uses a simpler interface
than `fmt`, however, in which the expressions are embedded directly into the
string and formatted using `str`. As such, the return types of all expressions
in a `fstr` must implement the `str` interface.

```clojure
(def x 1)
(def y "hi")

(fstr "this is an integer {x} and this is the first character of a string {(head x)}")
```

Any parseable expression may be  embedded in a `fstr`. Expressions are
delimited using `{}`. Any lone `}` will be  interpreted as a literal, whereas
literal `{` need to be escaped as `{{`.

```clojure
(fstr "{{}") ; => {}
```

While possible, it is discouraged to use complicated or even multiline
expressions inside `fstr`.

{% endraw %}

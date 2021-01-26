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

All arguments to `fmt` must implement the `format` interface, which is defined
as:

```clojure
(definterface format (Fn [String a] String)
```

The types are expected to take a format specifier and format according to it.
As such, which format specifiers are supported is dependent on the
implementation of `format` on that type. Standard library types expose regular
format specifiers as in C.

Please note that, because `format` is often implemented on top of `snprintf`
and similar functions, using faulty format specifiers might lead to problems.

Also, all `fmt` format strings must be literals.

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

# Macros

Macros are among the most divisive features about any Lisp. There are many
different design decisions to be made, and all of them have proponents and
detractors.

This document aims to give a comprehensive overview of the macro system and
how to use it. If you’re in a hurry or want to see whether Carp implements your
favorite macro feature, you probably want to read the section [“In a
Nutshell”](#in-a-nutshell). If you want to spend some quality understanding
how to work on or with the macro systems, the sections [“Working with
Macros”](#working-with-macros) and [“Inner Workings”](#inner-workings) will
probably be more useful to you.

## In a Nutshell

The macro system we’ve settled on for Carp is fairly simple. It is:

- not hygienic, but provides `gensym` capabilities,
- does not currently provide quasiquoting (this is not a requirement, it is
  currently just not implemented); thus the bread and butter in your macro
  toolbox will be `car`, `cdr`, `cons`, and `list`,
- defines macros with a fairly simple `defmacro`-based syntax, and has support
  for compile-time or dynamic functions (for more information on this aspect,
  please read [“Working with Macros”](#working-with-macros) below), and
- it sees the dynamic environment not just as an environment in which to
  generate code through expanding macros, but also as a place for telling the
  compiler more about the source. As an example, consider the dynamic function
  `Project.config`, which allows you to set such things as the C compiler to
  use, the name of the compiled project, or the output directory. To see this
  in action, consider [this Carp snippet](https://github.com/carpentry-org/snippets/blob/master/build_system.carp)
  which implements a simple multi-compiler build system for Carp in the dynamic
  environment.

## Working with Macros

Macros are defined using the `defmacro` primitive form, like this:

```clojure
(defmacro apply [f args] (cons f args))

(apply + (1 2)) ; => (+ 1 2)
(apply Array.replicate (5 "hello")) ; => (Array.replicate 5 "hello")
```

The example above defines `apply`, a macro that takes a function and a set of
arguments defined as a list and rewrites it so that the function gets applied
to these arguments by `cons`tructing a list with `f` as a head and `args` as
tail.

Because `apply` is a macro you will not need to quote the list passed to it. If
that looks strange, you might want to define `apply` as a dynamic function
instead. The main difference between macros and dynamic functions is that
dynamic functions evaluate their arguments and macros are expanded inside their
definitions. You may define a dynamic function like this:

```clojure
(defndynamic apply [f args] (cons f args))

(apply '+ '(1 2)) ; => (+ 1 2)
(apply 'Array.replicate '(5 "hello")) ; => (Array.replicate 5 "hello")
```

If you compare this code example to the macro example above, you’ll see that
they are extremely similar, except for the invocation `defndynamic` and the
quotes in their invocation.

Macros also provide rest arguments; this basically means that you may define
variadic macros by providing a “catch-all” argument as the last argument.

```clojure
(defmacro apply-or-sym [head :rest tail]
  (if (= (length tail) 0)
    head
    (cons head tail)))

(apply-or-sym *global*) ; => *global*
(apply-or-sym + 1 2) ; => (+ 1 2)
```

The macro `apply-or-sym` is slightly ridiculous, but it should drive the point
home. It takes one formal argument, `head`. You may provide any number of
arguments after that—they will be bound to `tail`. Thus, tail will be a list of
zero or more arguments. If we do not provide any, `apply-or-sym` will just
return `head`. If we do, we treat it as a regular invocation. This kind of
macro might look slightly silly, but rest assured that using rest arguments has
many legitimate use cases.

If you’d like to see more examples of macros big and small, you should now be
equipped to understand a lot of the macros in [the standard
library](/core/Macros.carp) and even [`fmt`](/core/Format.carp), a fairly
complex piece of macro machinery.

Some helpful functions for exploring macros in the REPL are `expand`, `eval`,
and `macro-log`. `expand` will expand macros for you, while `eval` evaluates
the resulting code. `macro-log` is useful for tracing your macro, a form of
“printf debugging”.

## Inner Workings

TODO

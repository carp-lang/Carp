# Core Libraries
See [lisp/core.carp](lisp/core.carp), proper docs are coming soon!

# The C standard library (wrapped)
See [lisp/core.carp](lisp/builtins.carp)

# OpenGL
See [lisp/gl.carp](lisp/gl.carp)

# The '*' macros
Since the functions in Carp can't accept a variable number of args there are a bunch of helper macros that allows you to circumvent this limitation. Here are some examples:

```clojure
(str* "This string " "and this string, here's a number " 123 ", etc...")
(println* "X = " x ", Y = " y)
(and* true false false true false)
```

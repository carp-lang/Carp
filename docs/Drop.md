# Drop

One of the “special” interfaces implementable by any type in Carp is `drop`,
the signature of which is `(Fn [&a] ())`. It takes a reference to a type and
is run before that type is deleted. This is meant for types that need special
treatment before being deallocated, such as files that need to be closed.

```clojure
(deftype A [])

(defmodule A
  (sig drop (Fn [(Ref A)] ()))
  (defn drop [a]
    (IO.println "Hi from drop!"))
)

(defn main []
  (let [a (A)]
    ()))
```

In the case above, `A.drop` will be  run and `Hi from drop` will be printed
when the `let` scope ends.

# Dynamic Types

While the primitive types that dynamic Carp has to offer—lists, arrays,
numbers, symbols, and strings—are often enough for writing simple macros,
sometimes more abstract types are needed.

Maps in dynamic Carp are one such example, but you can define your own type
just as easily. Let’s assume that you need a type `Pair` with a `fst` and
`snd` property:

```clojure
; this will define the type `Pair`
(deftype-dynamic Pair [fst snd])

; we can now construct that type
(defdynamic p (Pair.init 1 2)) ; => (Pair 1 2)

; we can get and set properties
(Pair.x p) ; => 1
(Pair.set-x p 4) ; => (Pair 4 2)

; we can check whether something is a `Pair`
(Pair.Pair? p) ; => true
(Pair.Pair? "not a pair") ; => false

; we can stringify it, compare it, and check the type
(str p) ; => "(Pair 1 2)"
(= p (Pair.init 2 3)) ; => false
(dynamic-type p) ; => Pair
```

As you might see from the example, many of the features of dynamic types
emulate those of types in static Carp.

Something to look out for is that while if you choose to overwrite the `str`
function on your generated type this will work as expected, `=` does not work
like this due to performance constraints (the cost of looking up the right
function proved to be too slow for the time being). Patches for this are very
welcome!

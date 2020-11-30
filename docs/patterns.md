# Carp Patterns

This doc captures some common programming patterns in Carp.

## Ref-let

You may occasionally want to compute the result of running two different
ownership-taking functions on a non-ref argument from an enclosing scope.
Consider the following function:

```clojure
(defn needs-ref-let [x]
  (Pair.init (Maybe.or-zero x) (Maybe.or-zero x)))
```

Carp will complain about this function since `Maybe.or-zero` *takes ownership*
of the non-ref, managed argument `x`<sup>1</sup> -- this is nice since it ensures we
don't somehow mutate the memory associated with `x` -- if we did, we would get
unpredictable results, especially in multi-threaded contexts in which
`Maybe.or-zero` may try to access `x` simultaneously.

But you may still want to compute both `Maybe.or-zero` calls on the single `x`
value!  For these cases, it's useful to introduce a variable bound to a `Ref`
to `x`:

```clojure
(defn needs-ref-let [x]
  (let [x* &x]
    (Pair.init (Maybe.or-zero @x*) (Maybe.or-zero @x*))))
```

`x*`, which is just a reference to the `x` argument, allows you to flexibly
copy `x` at will throughout the function body. Of course, the second copy here
is actually unnecessary, since once we've copied `x` once, we're free to use `x`
itself again:

```clojure
(defn needs-ref-let [x]
  (let [x* &x]
    (Pair.init (Maybe.or-zero @x*) (Maybe.or-zero x))))
```

---
1: In Carp, sumtypes like `Maybe` are *managed* by the borrow checker. This
means that Carp will ensure any memory associated with them has proper
ownership and is deleted when they are no longer needed. Not all types are
managed. `Int`, for example, is not a managed type, and so the issue described
above won't be relevant for `Int` arguments. For more information see
[docs/memory.md](docs/memory.md)

## Update a Sumtype Reference in-place

Product types automatically generate *mutating setters*, which provide a nice
means of updating their fields in-place, given you have a reference to an
instance you want to update:

```clojure
;; An example of a mutating setter, `Pair.set-a!`
Pair.set-a!: (Fn [(Ref (Pair a b) q), a] ())
```

Contrarily, sumtypes don't generate such mutating setters, and naturally don't
lend themselves to such functions; instead, they are primarily copied around and
passed as values.

However, if you find yourself in the rare position in which you'd like to alter
the value of a Sumtype reference in-place, you can use an *unsafe coercion to a
pointer* to do so. The steps are as follows:

- Get the reference to the sumtype value you'd like to mutate
- Use `Unsafe.coerce` to coerce the reference to a `Ptr` to the value.
- Use `Pointer.set` to set the value of the pointer to a new value of the same
  type.

Here's the pattern in action:

```clojure
(defn up-to-eleven [val]
  (let [val* (the (Ptr (Maybe Int)) (Unsafe.coerce &val))]
    (Pointer.set m* (Maybe.Just 11))))

(let-do [volume (Maybe.Just 0)]
  (up-to-eleven &volume)
  volume)
;; => (Just 11)
```

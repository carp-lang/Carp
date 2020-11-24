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

# Carp Patterns

This doc captures some common programming patterns in Carp.

## Ref-let

You may occasionally want to compute the result of running two different
ownership-taking functions on a non-ref argument from an enclosing scope.
Consider the following function:

```
(defn needs-ref-let [x]
  (Pair.init (inc x) (dec x)))
```

Carp will complain about this function since `inc` and `dec` both
*take-ownership* of the non-ref argument, `x` -- this is nice since it ensures
we don't somehow mutate the memory associated with `x` -- if we did, we would
get unpredictable results, especially in multi-threaded contexts in which `inc`
and `dec` may try to access `x` simultaneously.

But you may still want to compute both `inc` and `dec` on the single `x` value!
For these cases, it's useful to introduce a variable bound to a `Ref` to `x`:

```
(defn needs-ref-let [x]
  (let [x* &x]
    (Pair.init (inc @x*) (dec @x*))))
```

`x*`, which is just a reference to the `x` argument, allows you to flexibly
copy `x` at will throughout the function body. Of course, the second copy here
is actually unnecessary, since once we've copied `x` once, we're free to use `x`
itself again, since there's no longer threat of bad mutations or races between
`inc` and `dec`:

```
(defn needs-ref-let [x]
  (let [x* &x]
    (Pair.init (inc @x*) (dec x))))
```

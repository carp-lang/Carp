# C Interop

Carp supports interoperability with C programs. 

## Tutorial

### Getting Our Feet Wet

The `register` special form lets you call C routines from a Carp program. For
example, suppose we have the following C code:

```c
int value = 2;

int somefunction() {
  return value;
}
```

We can `register` Carp bindings for `somefunction()` to call it directly from
within Carp:

```clojure
(register somefunction (Fn [] Int))
```

In this case, the type of the Carp binding matches the corresponding C function
directly. This is the most basic form of registration, but there

## Null Pointers



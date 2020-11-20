# C Interop

This is an extension of what is covered in the [Language Guide](./LanguageGuide.md#c-interop).

## Content

- [Managed types](#managed-types)
  - [String](#string)
  - [Array](#array)
- [Embedding C code in Carp](#embedding-c-code-in-carp)
  - [`deftemplate`](#deftemplate)
    - [`Basic example`](#basic-example)
    - [`Generics`](#generics)

## Managed types

In Carp types like `String` and `Array` are _managed_ types in that they are
allocated on the Heap and the compiler will automatically free the allocated
memory when they go out of scope. We'll see how we can go from these managed
type to C and back.

### String

To use a managed `String` with a C function requiring a `char*` you can use the
`String.cstr` function that will turn your `(Ref String)` into `(Ptr CChar)`:


```clojure
(register puts (Fn [(Ptr CChar)] ()))

(let [a-str @"A string."]
  (puts (String.cstr &a-str)))
(puts (String.cstr "Hello"))
```

It might be a good idea not to expose the C type to the end user directly:

```clojure
(defmodule MyMod
  (hidden puts-c)
  (private puts-c)
  (register puts-c (Fn [(Ptr CChar)] ()) "puts")
  (defn puts [str-ref] (puts-c (String.cstr str-ref))))

(let [a-str @"A string."]
  (MyMod.puts &a-str))
(MyMod.puts "Hello")
```

---

If you are given a `char*` and want to turn it into a managed `String` you can
use `String.from-cstr`. It will allocate and copy the content of the C string.

```c
// static-str.h
char* returns_a_static_str() {
  return "Hello";
}
```

```clojure
(relative-include "static-str.h")

(register returns-a-static-str (Fn [] (Ptr CChar)) "returns_a_static_str")

(let [a-str (String.from-cstr (returns-a-static-str))]
  (println* (String.concat &[a-str @" " @"Carp"])))
```

---

The function you're consuming might be allocating the string on the Heap for
you. In that case you can declare the function as returning a managed `String`.
However this might be unsafe, you need to ensure that the string is actually
Heap-allocated and that the allocator is the same as the one that Carp is
using.

```c
char* returns_a_heap_string() {
  char *hello = "Hello from the heap";
  char *str = malloc((strlen(hello)+1));
  strcpy(str, hello);
  return str;
}
```

```clojure
(relative-include "heap-string.h")

(register returns-a-heap-str (Fn [] String) "returns_a_heap_string")

(let [a-str (returns-a-heap-str)]
  (println* a-str))
```

If you are the one writing the C code, you can use the `CARP_MALLOC` macro to
ensure you are using the same allocator as the Carp compiler:

```c
char* returns_a_heap_string() {
  char *hello = "Hello from the heap";
  char *str = CARP_MALLOC((strlen(hello)+1));
  strcpy(str, hello);
  return str;
}
```

### Array

`Array.unsafe-raw` can be used in case you have a function taking an C array as
a parameter.

```c
int sum(int *arr, int len) {
  int acc = 0;
  for (int i = 0; i < len; i++) {
    acc += arr[i];
  }
  return acc;
}
```

```clojure
(relative-include "sum.h")

(register sum-c (Fn [(Ptr Int) Int] Int) "sum")

(let [ints [1 2 3]]
  (println* (sum-c (Array.unsafe-raw &ints) (Array.length &ints))))
```

Again, you might want to wrap the bare C function in more Carp-esque interface.

```clojure
(relative-include "sum.h")

(defmodule MyMod
  (hidden sum-c)
  (private sum-c)
  (register sum-c (Fn [(Ptr Int) Int] Int) "sum")

  (sig sum (Fn [(Ref (Array Int))] Int))
  (defn sum [ints] (sum-c (Array.unsafe-raw ints) (Array.length ints))))

(MyMod.sum &[1 2 3])
```

---

In cases where the consuming function takes ownership over the data,
`Array.raw` can be used. It becomes the responsibility of the consuming
function to call `free` on the pointer and any managed types it contains.

```c
// printall.h
void println_all(char **arr, int len) {
  for (int i = 0; i < len; i++) {
    printf("%s\n", arr[i]);
    CARP_FREE(arr[i]);
  }
  CARP_FREE(arr);
}
```

```clojure
(relative-include "printall.h")

(register println-all (Fn [(Ptr String) Int] ()) "println_all")

(let [lines [@"One" @"Two" @"Three"]
      len (Array.length &lines)]
  (println-all (Array.raw lines) len))
```

## Embedding C code in Carp

When interfacing C libraries it is sometimes beneficial to wrap the libraries
function with some custom C code. An entirely valid method is the write your
code in a header file, include it from the Carp side and `register` it:

```c
// print.h
// String is a carp core alias for char*
void print_that_takes_ownership(String str) {
  printf("%s", str);
  CARP_FREE(str);
}
```

```clojure
(relative-include "print.h")

(register print (Fn [String] ()) "print_that_takes_ownership")

(print @"Print this!")
```

However you might prefer to keep your C code close to your Carp code, enter `deftemplate`...

### `deftemplate`

#### Basic example
We can instead define the previous example like so:

```clojure
(deftemplate print (Fn [String] ())
                   "void $NAME(String str)"
                   "$DECL {
                     printf(\"%s\", str);
                     CARP_FREE(str);
                   }")

(print @"Print this!")
```

Let's break down what's going on here:  
The **first** argument to `deftemplate` is the name we'll use to refer to the
function.  
The **second** is a type signature and is identical to the one found
in our previous `register` call.  
The **third** is our function declaration, it'll be injected at the top of the
generated C file.  
The **last** argument represent the function definition.

Two more thing to look at:  
`$NAME` is a variable that will be derived from the name you've given the
function plus any module it's defined in, so no need to worry about name
clashes with other `print` functions in other modules.  
`$DECL` will be replaced with the declaration passed as a third argument when
the function is defined

So we've seen how `deftemplate` can be used to keep Carp and C code close to
each other and help you write less code in general but it's real power lies
somewhere else...

#### Generics

Let's say one would like to write a function that adds two numbers, it would be
tedious to write a version for every type of number, let's see how
`deftemplate` can help us with that.

```clojure
(deftemplate add (Fn [a a] a)
                 "$a $NAME($a x, $a y)"
                 "$DECL {
                   return x + y;
                 }")

(add 1 2)
(add 20l 22l)
(add 2.0f 5.0f)

; Can't do that as they're different types
; (add 2.0f 22l)
```

Carp allows us to use generic type in type signatures, `a` in that example. You
can use `$` plus the generic name you used in your signature to refer to that
type in your C code. Carp will then generate a separate function everytime the
template is used with a different type.

Warning! You'll need to be careful when calling that function as you've lost
all type safety the Carp compiler guarantees. You will have to hope the C
compiler will catch it.

``` clojure
(deftemplate add (Fn [a a] a)
                 "$a $NAME($a x, $a y)"
                 "$DECL {
                   return x + y;
                 }")

(add @"A string" @" another string")
```

This thankfully result in this Clang error, but it's probably good not to rely on it.

```
out/main.c:9153:29: error: invalid operands to binary expression ('String' (aka 'char *') and 'String')
                   return x + y;
                          ~ ^ ~
1 error generated.
```


# C Interop

This is an extension of what is was covered in the [Language Guide](./LanguageGuide.md#c-interop).

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


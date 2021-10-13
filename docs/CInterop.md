# C Interop

This is an extension of what is covered in the [Language Guide](./LanguageGuide.md#c-interop).

## Content

- [How Carp generates identifiers]("#how-carp-generates-identifiers")
- [Managed types](#managed-types)
  - [String](#string)
  - [Array](#array)
- [Embedding C code in Carp](#embedding-c-code-in-carp)
  - [`deftemplate`](#deftemplate)
    - [`Basic example`](#basic-example)
    - [`Generics`](#generics)
  - [`emit-c`](#unsafe-emit-c)
  - [`preproc`](#unsafe-preproc)
  - [Registering Types](#register-types)
- [Callbacks](#callbacks)


## How Carp generates identifiers

When creating a function or def it might be useful to know what identifier gets
generated on the C side. Here are some examples:

```clojure
(def a-def 100)
; => a_MINUS_def

(defn hello [] (println* "Hello"))
; => hello

(sig true? (Fn [Bool] Bool))
(defn true? [b] b)
; true_QMARK_

(defmodule Reverse
  (defn hello [] (println* "Goodbye"))
  ; => Reverse_hello
  (defmodule ReReverse
    (defn hello [] (println* "Hello"))))
    ; => Reverse_ReReverse_hello

; Generic signature
(sig print-first-and-add (Fn [(Ref (Array a)) b b] b))
(defn print-first-and-add [arr x y]
 (do
   (println* (Array.unsafe-first arr))
   (+ x y)))
; Generates no code until it is called

(print-first-and-add &[1] 1 1)
; => print_MINUS_first_MINUS_and_MINUS_add__int_int
(print-first-and-add &[@"hello"] 2l 40l)
; => print_MINUS_first_MINUS_and_MINUS_add__String_Long
```

Looking at the examples should be clear enough but let's break it down:
Carp will replace illegal characters in C with a string representation of them
(`- => _MINUS_`, `? => _QMARK_`, etc...)
If in modules it will prefix the identifier with the modules name.
When the arguments to a function are generic it will suffix the types to the
identifiers, the identifiers are not able to be generated until it is used. If
a function is potentially generic but you don't want it to be you can add a
non-generic signature to it to make Carp generate your function like in our
`true?` example.

When creating bindings to a library it would be hard to coerce this logic into
creating the exact identifiers the library uses, this is why `register` and
`register-type` accepts an optional argument to specify what identifiers to use:

```clojure
(defmodule CURL
  (register-type HttpPost "curl_httppost")
  (register form-free (Fn [(Ref HttpPost)] ()) "curl_formfree"))
```

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

You may want to hide the C type from the end-user:

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

Two more things to look at:
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

### `Unsafe.emit-c`

While `deftemplate` is flexible and sufficient for most use cases, there are
certain scenarios in which it won't accomplish what you need. For example, some
C macros, such as c11's `static_assert` require a string literal argument.
`deftemplate` can't accomplish this. In such cases, you can use `Unsafe.emit-c`
to emit a literal string in the Carp compiler's C output. `emit-c` is perfect
for scenarios like `static_assert` calls. Assuming `static_assert` is
`register`ed as `static-assert`, we can use `emit-c` in the following way to
ensure it is passed a string literal in the compiler's emitted C code:

```
(register static-assert (Fn [a C] ()))

(static-assert 0 (Unsafe.emit-c "\"foo\""))
```

which will emit the corresponding C:

```
static_assert(0, "foo")
```

`emit-C` returns values of the `C` type, a special type that represents literal
C code in Carp.

### `Unsafe.preproc`

The Carp compiler emits C code in an order that ensures the dependencies of
functions are available before functions are called. Sometimes, you may want to
include C code before the Carp compiler's output. For instance, you might want
to provide some preprocessor directives to a C compiler. The `Unsafe.preproc`
function was designed with this use case in mind. You can use `preproc` to
inject arbitrary C code prior to the Carp compiler's normal C output. Any code
passed to `preproc` will be emitted after file `includes` but before any other
emitted C code.

`preproc` takes a value of type `C` as an argument, so it must be used in
combination with `Unsafe.emit-c`. The C code you pass to `preproc` isn't
checked at all, so be careful!

If you do define C symbols using `preproc`, you'll still need to call
`register` to reference them in Carp code. For example, the following snippet
uses `preproc` to make a C macro and function available in the Carp compiler's
output and then calls `register` to reference these symbols in the `main`
function in the Carp source:

```
(Unsafe.preproc (Unsafe.emit-c "#define FOO 0"))
(Unsafe.preproc (Unsafe.emit-c "void foo() { printf(\"%d\\n\", 1); }"))

(register FOO Int)
(register foo (Fn [] ()))

(defn main []
  (do (foo)
      (IO.println &(fmt "%d" FOO))))
```

You can use this technique to add provisional definitions you need to reference
in compiler output. If your helper functions, macros, or preprocessor
directives are lengthy or complex, you may want to define them in a separate
`h` file and `relative-include` it in your Carp source instead.

### Registering Types

Carp supports a few different ways of registering types defined in C. You can
register types using the `register-type` function. Calling `register-type` with
only a symbol argument registers the C type with a name corresponding to the
symbol.  For example, the following code registers the C type `A` as the type
`A` in Carp.

```c
typedef int A;
```

```clojure
(register-type A)
```

After this call to `register-type`, you can use the type `A` anywhere type
names are valid in Carp code. For example, you can use it in function
signatures:

```clojure
(sig a-prn (Fn [A] String))
```

The prior type registration *only* registers the type name in Carp. In other
words, the type is entirely "opaque" from the perspective of your Carp program.
Carp knows the type exists, but it knows nothing about its implementation or
how to construct values of the type--all of that is left up to your C code.

If you want to construct values of this type from Carp code, you have two
options:

1. You can define your own initializers for the type in C and register them in Carp.
2. You can use `register-type` to generate initializers for the type in Carp.

If you define an initializer for the type in C, you can access it from Carp by
using `register`:

```c
typedef int A;

A initializer() {
  return 0;
}
```

```clojure
(register-type A)
(register initializer (Fn [] A))
;; returns a value of type A
(initializer)
```

Alternatively, you can add a non-empty array of type members in your
`register-type` call to have Carp generate initializers, getters and setters,
and printing functions for the external type. The initializer Carp generates
will only initialize the fields you specify. If you omit or misname a field,
the generated initializer might cause errors.

```clojure
(register-type B [])
:i B
=> B : Type
     init : (Fn [] B)
     prn  : (Fn [(Ref B q)] String)
     str  : (Fn [(Ref B q)] String)
}
(register-type C [x Int])
:i C
=> C : Type
   C : Module {
     init     : (Fn [Int] C)
     prn      : (Fn [(Ref C q) String])
     str      : (Fn [(Ref C q) String])
     set-x    : (Fn [C, Int] C)
     set-x!   : (Fn [(Ref C q), Int] ())
     update-x : (Fn [C, (Ref (Fn [Int] Int) q)] C)
     x        : (Fn [(Ref C q)] (Ref Int q))
}
```

The `prn` and `str` functions for the type will also automatically implement
their corresponding interfaces.

Be mindful that Carp *does not manage the memory associated with external types
by default!* Unlike types defined in Carp, Carp will not generate `copy` and
`delete` functions for registered types. If you use generated initializers for
a registered type for convenience, remember that you still need to manage the
memory associated with values of the type manually. If you want Carp to manage
the memory for a registered type, you can provide implementations of the `copy`
and `delete` interfaces.

If needed, you can override the name Carp emits for a registered type by
providing an additional string argument. This comes in handy when the type's
name in C does not follow lisp or Carp naming conventions. For example, the
type in C might begin with a lowercase letter, while Carp requires all types to
begin with uppercase letters:

```clojure
;; Emitted in C code as "A"
(register-type A)
;; Emitted in C code a "a_type"
(register-type A "a_type")
;; Emitted in C code as "b_type"
(register-type B "b_type" [x Int])
```

## Callbacks

Some C APIs rely on callbacks, let's define a C function that accepts a
callback and an argument and returns the result of calling that function as an
example:

```clojure
(deftemplate runner (Fn [(Ptr ()) (Ptr ())] a)
                    "$a $NAME(void* fnptr, void* args)"
                    "$DECL {
                       return (($a(*)(void*))fnptr)(args);
                    }")

; Using a lambda capturing variables from its environment
(let [x 20 y 22 fnfn (fn [] (+ @&x @&y))]
  (= (runner (Function.unsafe-ptr &fnfn) (Function.unsafe-env-ptr &fnfn))
     42))

; Using a static function
(defn double [x] (Int.* @x 2))

(let [x 42]
  (= (runner (Function.unsafe-ptr &double) (Unsafe.coerce &x))
     84))
```

In the first example we want to use a lambda capturing some variable, we can
use `Function.unsafe-ptr` to get a `void*` to the function and in the case of
lambdas capturing environment the first argument to that function is the
environment so we have to use `Function.unsafe-env-ptr` to pass in that
environment.

In the second example we want to use a static function so we can use
`Function.unsafe-ptr` again, and the argument we pass in needs to be coerced
from a `Ref` into a `(Ptr ())`.

Because everything gets turned into a void pointer all type safety is lost so
it is the responsibility of the caller to ensure the operation is safe. It is
also important to ensure the lifetime of the `Ptr` doesn't not exceed the
lifetime of the function/env it represents.


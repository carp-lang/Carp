# Memory Management - a closer look

A simple piece of code:

```clojure
(use String)
(use IO)

(defn say-hi [text]
  (while true
    (if (< (count text) 10)
      (println &"Too short!")
      (println text))))
```

This compiles to the following C program:
```C
void say_MINUS_hi(string* text) {
  bool _55 = true;
  while (_55) {
    int _62 = String_count(text);
    bool _60 = Int__LT_(_62, 10);
    if (_60) {
      string _69 = strdup("Too short!");
      string* _68 = &_69; // ref
      IO_println(_68);
    } else {
      IO_println(text);
    }
    _55 = true;
  }
}
```

If-statements are kind of tricky in regards to memory management:
```clojure
(defn say-what [text]
  (let [manage-me (copy text)]
    (if (< (count text) 10)
      (copy "Too short")
      manage-me)))
```

The 'manage-me' variable is the return value in the second branch, but should get freed if "Too short" is returned.
The output is a somewhat noisy C program:
```C
string say_MINUS_what(string text) {
    string _78;
    string* _84 = &text; // ref
    int _82 = String_count(_84);
    bool _80 = Int__LT_(_82, 10);
    if (_80) {
        string _90 = strdup("Too short");
        string* _89 = &_90; // ref
        string _87 = String_copy(_89);
        String_delete(text);
        _78 = _87;
    } else {
        _78 = text;
    }
    return _78;
}
```

The most important thing in Carp is to work with arrays of data. Here's an example of how that is supposed to look:

```clojure
(defn weird-sum []
  (let [stuff [3 5 8 9 10]]
    (reduce + 0 (emap square (filter even? stuff)))))
```

All the array transforming functions 'endo-map' and 'filter' use C-style mutation of the array and return the same data structure back afterwards, no allocation or deallocation needed. The lifetime analyzer ("borrow checker" in [Rust](https://www.rust-lang.org) parlance) makes sure that the same data structure isn't used in several places.

The restriction of 'endo-map' is that it must return an array of the same type as the input. If that's not possible, use 'copy-map' instead. It works like the normal 'map' found in other functional languages. The 'copy-' prefix is there to remind you of the fact that the function is allocating memory.

To know whether a function takes over the responsibility of freeing some memory (through its args) or generates some new memory that the caller has to handle (through the return value), just look at the type of the function (right now the easiest way to do that is with the ```(env)``` command). If the value is a simple type like String, Vector3, or similar, it means that the memory ownership gets handed over. If it's a reference signature (i.e. ```(Ref String)```), the memory is just temporarily lended out and someone else will make sure it gets deleted. When interoping with existing C code it's often correct to send your data structures to C as refs or pointers (using ```(address <variable>)```), keeping the memory management inside the Carp section of the program.

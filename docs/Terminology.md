# Terminology (WORK IN PROGRESS)

## Introduction
This document contains commonly used words, concepts and jargon for the Carp programming language. It should be particularly helpful when naming things and writing documentation. Ideally it will remove ambiguity, make us spell things the same way, and generally make the Carp ecosystem feel more unified.

Pull requests for this document are very welcome, and please tell us in the [Gitter Channel](https://gitter.im/eriksvedang/Carp?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge) if something is missing!

### Mutating function
What should this mean exactly? Ideally it should only be used for functions ending in a `!` that return `()`. Perhaps it should be called "externally mutating function"..? Or maybe "exo-functions" :)

### Owning function
An *owning* function is a function that takes ownership over the value it is passed. This means that is has responsibility of freeing that memory (or pass it to another owning function, or return it).

### Borrowing function
A *borrowing* function is a function that does not take ownership over the value it is passed. This means that is is not allowed to free the memory of that value but must leave it intact.

### Transforming function / endo-functions
What is a good name for a function that is both owning and internally mutating, like `endo-map`? These functions are common in Carp and allow for a functional programming style without the need to copy or allocate memory.

### Unsafe function
Unsafe functions (most often) start with the prefix `unsafe-` and *can* crash the program if certain preconditions are not met. A good example is `Array.unsafe-first` which gets the first value in an array. Calling this if the array is empty this will crash the program.

The `unsafe-` prefix is an optional naming convention; there *are* a few functions that are not safe which does not use this naming scheme. A notable example is `Array.nth` which crashes if given an invalid index. Any other unsafe functions should be thoroughly documented as such, and using the `unsafe-` prefix is very much encouraged in library code!

### REPL
*REPL* stands for "Read Eval Print Loop" and is a common feature of Lisp system. It allows expressions to be entered and executed at a command prompt. In Carp it is spelled with uppercase letters.

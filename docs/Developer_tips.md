Developer Tips
==============

Code structure
--------------

At the top-level Carp has two obvious folders for C code:
`src/` and `shared/`.

`src/` is used for the implementation of the compiler and interpreter,
whereas `shared/` is used for implementation of the runtime
(which is exposed to both interpreted and compiled programs).

The third code directory is `lisp/` which contains all the Carp code
written in carp itself - being a lisp a lot of Carp's actual implementation
happens within this directory.

C functions defined within `shared/` must also be bound by a file within `lisp/`,
more information on this will be provided in this document later.



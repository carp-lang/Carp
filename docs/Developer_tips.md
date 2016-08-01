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


Code formatting
---------------

C code is automatically formatted using `clang-format`,
if you have `clang-format` installed you can invoke the formatter via running:

    # from the top level directory
    $ cmake .
    $ make format

It is recommend that you format your code before making a pull request,
although semi-regular formats are run to catch any code that wasn't
pre-formatted.


A quick outline of the C style:

 - 2 space indent
 - Never use tabs
 - No column width limit


A very short example showing some of the features of this style

    int main(int argc, char **argv) {
      int a = 14;
      int *b = &a;

      switch(a) {
      case 14:
        puts("a was 14")
        break;

      default:
        puts("I have no idea what happened")
        break;
      }

      if(*b > 5) {
        puts("*b is bigger than 5")
      }
      else {
        puts("*b is not bigger than 5")
      }
    }


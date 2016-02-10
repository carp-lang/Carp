#pragma once

#include <ffi.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <assert.h>

typedef void (*VoidFn)(void);

/* Type tags
   C = Cons cell
   I = Integer
   S = String
   K = Keyword (:keyword)
   Y = Symbol
   L = Lambda
   E = Environment
   P = Primop / raw C function pointer
   M = Macro
   F = libffi function
   D = Dylib
   V = Float
   W = Double (not implemented yet)
   A = Array (not implemented yet)
   Q = Void pointer
*/

typedef struct Obj {
  union {
    // Cons cells
    struct {
      struct Obj *car;
      struct Obj *cdr;      
    };
    // Integers
    int i;
    // Strings, symbols and keywords
    char *s;
    // Lambdas / Macros
    struct {
      struct Obj *params;
      struct Obj *body;
      struct Obj *env;
      struct Obj *code;
    };
    // Environment
    struct {
      struct Obj *parent;
      struct Obj *bindings;
    };
    // Primitive C function pointer f(arglist, argcount)
    struct Obj* (*primop)(struct Obj**, int);
    // Libffi function
    struct {
      ffi_cif *cif;
      VoidFn funptr;
      struct Obj *arg_types;
      struct Obj *return_type;
      bool builtin; // TODO: make this meta data instead?
    };
    // Dylib
    void *dylib;
    // Void pointer
    void *void_ptr;
    // Float
    float f32;
  };
  struct Obj *meta;
  // GC
  struct Obj *prev;
  char alive;
  char given_to_ffi;
  // Type tag (see table above)
  char tag;
} Obj;

typedef Obj* (*Primop)(Obj**, int);

Obj *obj_new_cons(Obj *car, Obj *cdr);
Obj *obj_new_int(int i);
Obj *obj_new_float(float x);
Obj *obj_new_string(char *s);
Obj *obj_new_symbol(char *s);
Obj *obj_new_keyword(char *s);
Obj *obj_new_primop(Primop p);
Obj *obj_new_dylib(void *dylib);
Obj *obj_new_ptr(void *ptr);
Obj *obj_new_ffi(ffi_cif* cif, VoidFn funptr, Obj *arg_types, Obj *return_type_obj, bool builtin);
Obj *obj_new_lambda(Obj *params, Obj *body, Obj *env, Obj *code);
Obj *obj_new_macro(Obj *params, Obj *body, Obj *env, Obj *code);
Obj *obj_new_environment(Obj *parent);

Obj *obj_copy(Obj *o);

Obj *obj_list_internal(Obj *objs[]);
#define obj_list(...) obj_list_internal((Obj*[]){__VA_ARGS__, NULL});

bool obj_eq(Obj *a, Obj *b);
bool is_true(Obj *o);

void obj_print_cout(Obj *o);

Obj *obj_latest;
int obj_total;
int obj_total_max;

Obj *global_env;

Obj *nil;
Obj *lisp_false;
Obj *lisp_true;
Obj *lisp_quote;
Obj *error;
Obj *ampersand;
Obj *lisp_NULL;

Obj *type_int;
Obj *type_bool;
Obj *type_string;
Obj *type_list;
Obj *type_lambda;
Obj *type_primop;
Obj *type_foreign;
Obj *type_env;
Obj *type_keyword;
Obj *type_symbol;
Obj *type_macro;
Obj *type_void;
Obj *type_float;
Obj *type_ptr;
Obj *type_ref;


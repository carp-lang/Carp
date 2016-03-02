#pragma once

#ifdef WIN32
/* For correct linking against static libffi */
#define FFI_BUILDING
#endif
#include <ffi.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <assert.h>

typedef void (*VoidFn)(void);

/* Type tags
   A = Array
   B = Char
   C = Cons cell
   D = Dylib
   E = Environment
   F = libffi function
   G
   H
   I = Integer
   J
   K = Keyword (:keyword)
   L = Lambda
   M = Macro
   N
   O
   P = Primop / raw C function pointer
   Q = Void pointer
   R 
   S = String
   T
   U
   V = Float
   W = Double (not implemented yet)
   X
   Y = Symbol
   Z
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
	  char* name;
      struct Obj *arg_types;
      struct Obj *return_type;
    };
    struct {
      struct Obj **array;
      int count;
    };
    // Dylib
    void *dylib;
    // Void pointer
    void *void_ptr;
    // Float
    float f32;
    // Char
    char b;
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
Obj *obj_new_ffi(const char* name, ffi_cif* cif, VoidFn funptr, Obj *arg_types, Obj *return_type_obj);
Obj *obj_new_lambda(Obj *params, Obj *body, Obj *env, Obj *code);
Obj *obj_new_macro(Obj *params, Obj *body, Obj *env, Obj *code);
Obj *obj_new_environment(Obj *parent);
Obj *obj_new_char(char b);
Obj *obj_new_array(int count);

Obj *obj_copy(Obj *o);

Obj *obj_list_internal(Obj *objs[]);
#define obj_list(...) obj_list_internal((Obj*[]){__VA_ARGS__, NULL});

void obj_set_line_info(Obj *o, int line, int pos, Obj *filename);

bool obj_eq(Obj *a, Obj *b);
bool is_true(Obj *o);

void obj_print_cout(Obj *o);
void obj_copy_meta(Obj *to, Obj *from);

Obj *obj_latest;
int obj_total;
int obj_total_max;

Obj *global_env;

Obj *nil;
Obj *lisp_false;
Obj *lisp_true;
Obj *lisp_quote;
Obj *eval_error;
Obj *ampersand;
Obj *hash;
Obj *lisp_NULL;

Obj *prompt;
Obj *prompt_unfinished_form;

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
Obj *type_char;
Obj *type_array;

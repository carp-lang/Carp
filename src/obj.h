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
#include "constants.h"

typedef void (*VoidFn)(void);

/* Type tags
   A = Array
   B = Bool
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
   R = Global variable
   S = String
   T = Char
   U
   V = Float
   W = Double (not implemented yet)
   X = Bytecode
   Y = Symbol
   Z
*/

struct Process;

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
    struct {
      char *s;
      int dispatch_index; // used for quick dispatch of special forms in eval
    };
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
    // Primitive C function pointer f(process, arglist, argcount)
    struct Obj* (*primop)(struct Process*, struct Obj**, int);
    // Libffi function
    struct {
      ffi_cif *cif;
      VoidFn funptr;
      char* name;
      struct Obj *arg_types;
      struct Obj *return_type;
    };
    // Array
    struct {
      struct Obj **array;
      int count;
    };
    // Bytecode
    struct {
      char *bytecode;
      struct Obj *bytecode_literals;
    };
    // Dylib
    void *dylib;
    // Void pointer / global variable
    void *void_ptr;
    // Float
    float f32;
    // Double
    double f64;
    // Char
    char character;
    // Bool
    bool boolean;
  };
  struct Obj *meta;
  // GC
  struct Obj *prev;
  char alive;
  char given_to_ffi;
  // Type tag (see table above)
  char tag;
} Obj;

typedef struct {
  Obj *caller;
  Obj *callee;
} StackTraceCallSite;

typedef struct {
  int p;
  Obj *bytecodeObj;
  Obj *env;
} BytecodeFrame;

typedef struct {
  struct Obj *stack[STACK_SIZE];
  int stack_pos;

  Obj *shadow_stack[SHADOW_STACK_SIZE];
  int shadow_stack_pos;

  StackTraceCallSite function_trace[STACK_SIZE];
  int function_trace_pos;

  Obj *final_result;
  
  bool dead;
  struct Obj *global_env;

  Obj *bytecodeObj;
  BytecodeFrame frames[256];
  int frame;
} Process;

typedef Obj* (*Primop)(Process*, Obj**, int);

Obj *obj_new_cons(Obj *car, Obj *cdr);
Obj *obj_new_int(int i);
Obj *obj_new_float(float x);
Obj *obj_new_double(double x);
Obj *obj_new_string(char *s);
Obj *obj_new_symbol(char *s);
Obj *obj_new_keyword(char *s);
Obj *obj_new_primop(Primop p);
Obj *obj_new_dylib(void *dylib);
Obj *obj_new_ptr(void *ptr);
Obj *obj_new_ptr_to_global(void *ptr);
Obj *obj_new_ffi(const char* name, ffi_cif* cif, VoidFn funptr, Obj *arg_types, Obj *return_type_obj);
Obj *obj_new_lambda(Obj *params, Obj *body, Obj *env, Obj *code);
Obj *obj_new_macro(Obj *params, Obj *body, Obj *env, Obj *code);
Obj *obj_new_environment(Obj *parent);
Obj *obj_new_char(char character);
Obj *obj_new_array(int count);
Obj *obj_new_bool(bool b);
Obj *obj_new_bytecode(char *bytecode);

Obj *obj_copy(Obj *o);
bool obj_eq(Process *process, Obj *a, Obj *b);

Obj *obj_list_internal(Obj *objs[]);
#define obj_list(...) obj_list_internal((Obj*[]){__VA_ARGS__, NULL});

void obj_set_line_info(Process *process, Obj *o, int line, int pos, Obj *filename);

bool is_true(Obj *o);

void obj_print_cout(Obj *o);
void obj_copy_meta(Obj *to, Obj *from);

Obj *obj_latest;
int obj_total;
int obj_total_max;

Obj *eval_error;

Obj *nil;
Obj *lisp_false;
Obj *lisp_true;
Obj *lisp_quote;
Obj *lisp_NULL;

Obj *ampersand; // "&"
Obj *hash; // "#"
Obj *dotdotdot; // "..."

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
Obj *type_double;
Obj *type_ptr;
Obj *type_ref;
Obj *type_char;
Obj *type_array;
Obj *type_ptr_to_global;

Obj *prompt;
Obj *prompt_unfinished_form;


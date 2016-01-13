#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <assert.h>
#include <ctype.h>
#include <dlfcn.h>
#include <unistd.h>
#include <ffi.h>
#include <math.h>
#include <sys/time.h>

#include "../out/shared.h"

#define LOG_STACK 0
#define SHOW_MACRO_EXPANSION 0
#define LOG_GC_KILLS 0
#define LOG_FUNC_APPLICATION 0

bool setting_print_lambda_body = true;

typedef void (*VoidFn)(void);

#define assert_or_return(assertion, ...) if(!(assertion)) { printf(_VA_ARGS_); printf("\n"); return; }

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
    };
    // Dylib
    void *dylib;
    // Void pointer
    void *void_ptr;
    // Float
    float f32;
  };
  // GC
  struct Obj *prev;
  char alive;
  // Type tag (see table above)
  char tag;
} Obj;

typedef Obj* (*Primop)(Obj**, int);

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

Obj *obj_latest = NULL;
int obj_total = 0;

Obj *obj_new() {
  Obj *o = malloc(sizeof(Obj));
  o->prev = obj_latest;
  o->alive = false;
  obj_latest = o;
  obj_total++;
  return o;
}

Obj *obj_new_cons(Obj *car, Obj *cdr) {
  Obj *o = obj_new();
  o->tag = 'C';
  o->car = car;
  o->cdr = cdr;
  return o;
}

Obj *obj_new_int(int i) {
  Obj *o = obj_new();
  o->tag = 'I';
  o->i = i;
  return o;
}

Obj *obj_new_float(float x) {
  Obj *o = obj_new();
  o->tag = 'V';
  o->f32 = x;
  return o;
}

Obj *obj_new_string(char *s) {
  Obj *o = obj_new();
  o->tag = 'S';
  o->s = strdup(s);
  return o;
}

Obj *obj_new_symbol(char *s) {
  Obj *o = obj_new();
  o->tag = 'Y';
  o->s = strdup(s);
  return o;
}

Obj *obj_new_keyword(char *s) {
  Obj *o = obj_new();
  o->tag = 'K';
  o->s = strdup(s);
  return o;
}

Obj *obj_new_primop(Primop p) {
  Obj *o = obj_new();
  o->tag = 'P';
  o->primop = p;
  return o;
}

Obj *obj_new_dylib(void *dylib) {
  Obj *o = obj_new();
  o->tag = 'D';
  o->primop = dylib;
  return o;
}

Obj *obj_new_ptr(void *ptr) {
  Obj *o = obj_new();
  o->tag = 'Q';
  o->void_ptr = ptr;
  return o;
}

Obj *obj_new_ffi(ffi_cif* cif, VoidFn funptr, Obj *arg_types, Obj *return_type_obj) {
  assert(cif);
  assert(arg_types);
  assert(arg_types->tag == 'C');
  assert(return_type_obj);
  Obj *o = obj_new();
  o->tag = 'F';
  o->cif = cif;
  o->funptr = funptr;
  o->arg_types = arg_types;
  o->return_type = return_type_obj;
  return o;
}

Obj *obj_new_lambda(Obj *params, Obj *body, Obj *env, Obj *code) {
  assert(params);
  assert(params->tag == 'C');
  assert(body);
  assert(env);
  assert(env->tag == 'E');
  assert(code);
  Obj *o = obj_new();
  o->params = params;
  o->body = body;
  o->env = env;
  o->tag = 'L';
  o->code = code;
  return o;
}

Obj *obj_new_macro(Obj *params, Obj *body, Obj *env, Obj *code) {
  assert(params);
  assert(params->tag == 'C');
  assert(body);
  assert(env);
  assert(env->tag == 'E');
  Obj *o = obj_new();
  o->params = params;
  o->body = body;
  o->env = env;
  o->tag = 'M';
  o->code = code;
  return o;
}

Obj *obj_new_environment(Obj *parent) {
  Obj *o = obj_new();
  o->parent = parent;
  o->bindings = NULL;
  o->tag = 'E';
  return o;
}

void obj_mark_alive(Obj *o) {
  if(!o || o->alive) {
    return;
  }
  
  o->alive = true;
  
  if(o->tag == 'C') {
    obj_mark_alive(o->car);
    obj_mark_alive(o->cdr);
  }
  else if(o->tag == 'L' || o->tag == 'M') {
    obj_mark_alive(o->params);
    obj_mark_alive(o->body);
    obj_mark_alive(o->env);
    obj_mark_alive(o->code);
  }
  else if(o->tag == 'E') {
    obj_mark_alive(o->parent);
    obj_mark_alive(o->bindings);
  }
  else if(o->tag == 'F') {
    obj_mark_alive(o->arg_types);
    obj_mark_alive(o->return_type);
  }
}

void obj_string_mut_append(Obj *string_obj, const char *s2) {
  assert(string_obj);
  assert(string_obj->tag == 'S');
  int string_obj_len = strlen(string_obj->s);
  int s2_len = strlen(s2);
  int total_length = (string_obj_len + s2_len);
  char *s3 = realloc(string_obj->s, sizeof(char) * (total_length + 1));
  s3[total_length] = '\0';
  strncpy(s3 + string_obj_len, s2, s2_len);
  string_obj->s = s3;
}

Obj *concat_c_strings(char *a, const char *b) {
  Obj *s = obj_new_string(a);
  obj_string_mut_append(s, b);
  return s;
}

#define set_error(message, obj) \
  error = concat_c_strings((message), obj_to_string((obj) ? (obj) : nil)->s); \
  stack_push(nil); \
  return;

#define set_error_and_return(message, obj) \
  error = concat_c_strings((message), obj_to_string((obj) ? (obj) : nil)->s); \
  return nil;

#define assert_or_set_error(assertion, message, obj)	\
  if(!(assertion)) {					\
    set_error(message, obj);				\
  }

#define assert_or_return_nil(assertion, ...) if(!(assertion)) { printf(_VA_ARGS_); printf("\n"); return; }

void add_indentation(Obj *total, int indent) {
  for(int i = 0; i < indent; i++) {
    obj_string_mut_append(total, " ");
  }
}

Obj *obj_to_string(const Obj *o);

void obj_to_string_internal(Obj *total, const Obj *o, bool prn, int indent) {
  assert(o);
  int x = indent;
  if(o->tag == 'C') {
    obj_string_mut_append(total, "(");
    x++;
    int save_x = x;
    const Obj *p = o;
    while(p && p->car) {
      obj_to_string_internal(total, p->car, true, x);
      if(p->cdr && p->cdr->tag != 'C') {
      	obj_string_mut_append(total, " . ");
      	obj_to_string_internal(total, o->cdr, true, x);
      	break;
      }
      else if(p->cdr && p->cdr->car) {
	if(/* p->car->tag == 'C' ||  */p->car->tag == 'E') {
	  obj_string_mut_append(total, "\n");
	  x = save_x;
	  add_indentation(total, x);
	}
	else {
	  obj_string_mut_append(total, " ");
	  x++;
	}
      }
      p = p->cdr;
    }
    obj_string_mut_append(total, ")");
    x++;
  }
  else if(o->tag == 'E') {
    obj_string_mut_append(total, "{");
    x++;
    Obj *p = o->bindings;
    while(p && p->car) {
      char *key_s = obj_to_string(p->car->car)->s;
      obj_string_mut_append(total, key_s);
      obj_string_mut_append(total, " ");
      obj_to_string_internal(total, p->car->cdr, true, x + strlen(key_s) + 1);
      p = p->cdr;
      if(p && p->car && p->car->car) {
	obj_string_mut_append(total, ", \n");
	add_indentation(total, x);
      }
    }
    obj_string_mut_append(total, "}");
    if(o->parent) {
      obj_string_mut_append(total, " -> \n");
      Obj *parent_printout = obj_to_string(o->parent);
      obj_string_mut_append(total, parent_printout->s);
    }
  }
  else if(o->tag == 'I') {
    static char temp[64];
    snprintf(temp, 64, "%d", o->i);
    obj_string_mut_append(total, temp);
  }
  else if(o->tag == 'V') {
    static char temp[64];
    snprintf(temp, 64, "%f", o->f32);
    obj_string_mut_append(total, temp);
  }
  else if(o->tag == 'S') {
    if(prn) {
      obj_string_mut_append(total, "\"");
    }
    obj_string_mut_append(total, o->s);
    if(prn) {
      obj_string_mut_append(total, "\"");
    }
  }
  else if(o->tag == 'Y') {
    obj_string_mut_append(total, o->s);
  }
  else if(o->tag == 'K') {
    obj_string_mut_append(total, ":");
    obj_string_mut_append(total, o->s);
  }
  else if(o->tag == 'P') {
    obj_string_mut_append(total, "<primop:");
    static char temp[256];
    snprintf(temp, 256, "%p", o->primop);
    obj_string_mut_append(total, temp);
    obj_string_mut_append(total, ">");
  }
  else if(o->tag == 'D') {
    obj_string_mut_append(total, "<dylib:");
    static char temp[256];
    snprintf(temp, 256, "%p", o->primop);
    obj_string_mut_append(total, temp);
    obj_string_mut_append(total, ">");
  }
  else if(o->tag == 'Q') {
    obj_string_mut_append(total, "<ptr:");
    static char temp[256];
    snprintf(temp, 256, "%p", o->primop);
    obj_string_mut_append(total, temp);
    obj_string_mut_append(total, ">");
  }
  else if(o->tag == 'F') {
    obj_string_mut_append(total, "<ffi:");
    static char temp[256];
    snprintf(temp, 256, "%p", o->funptr);
    obj_string_mut_append(total, temp);
    obj_string_mut_append(total, ">");
  }
  else if(o->tag == 'L') {
    if(setting_print_lambda_body) {
      obj_string_mut_append(total, "(fn");
      obj_string_mut_append(total, " ");
      obj_string_mut_append(total, obj_to_string(o->params)->s);
      obj_string_mut_append(total, " ");
      obj_string_mut_append(total, obj_to_string(o->body)->s);
      obj_string_mut_append(total, ")");
    }
    else {
      obj_string_mut_append(total, "<lambda>");
    }
  }
  else if(o->tag == 'M') {
    if(setting_print_lambda_body) {
      obj_string_mut_append(total, "(macro");
      obj_string_mut_append(total, " ");
      obj_string_mut_append(total, obj_to_string(o->params)->s);
      obj_string_mut_append(total, " ");
      obj_string_mut_append(total, obj_to_string(o->body)->s);
      obj_string_mut_append(total, ")");
    }
    else {
      obj_string_mut_append(total, "<macro>");
    }
  }
  else {
    printf("obj_to_string() can't handle type tag %c (%d).\n", o->tag, o->tag);
    assert(false);
  }
}

Obj *obj_to_string(const Obj *o) {
  Obj *s = obj_new_string("");
  obj_to_string_internal(s, o, true, 0);
  return s;
}

Obj *obj_to_string_not_prn(const Obj *o) {
  Obj *s = obj_new_string("");
  obj_to_string_internal(s, o, false, 0);
  return s;
}

void obj_print(Obj *o) {
  assert(o);
  Obj *s = obj_to_string(o);
  printf("%s", s->s);
}

void obj_print_not_prn(Obj *o) {
  Obj *s = obj_to_string_not_prn(o);
  printf("%s", s->s);
}

Obj *obj_copy(Obj *o) {
  assert(o);
  if(o->tag == 'C') {
    //printf("Making a copy of the list: %s\n", obj_to_string(o)->s);
    Obj *list = obj_new_cons(NULL, NULL);
    Obj *prev = list;
    Obj *p = o;
    while(p && p->car) {
      Obj *new = obj_new_cons(NULL, NULL);
      prev->car = obj_copy(p->car);
      if(p->cdr) {
	prev->cdr = obj_copy(p->cdr);
	return list; // early break when copying dotted pairs!
      } else {
	prev->cdr = obj_new_cons(NULL, NULL);
	prev = new;
	p = p->cdr;
      }
    }
    return list;
  }
  else if(o->tag == 'E') {
    //printf("Making a copy of the env: %s\n", obj_to_string(o)->s);
    Obj *new_env = obj_new_environment(o->parent);
    new_env->bindings = obj_copy(o->bindings);
    return new_env;
  }
  else if(o->tag == 'Q') {
    return obj_new_ptr(o->void_ptr);
  }
  else if(o->tag == 'I') {
    return obj_new_int(o->i);
  }
  else if(o->tag == 'V') {
    return obj_new_float(o->f32);
  }
  else if(o->tag == 'S') {
    return obj_new_string(strdup(o->s));
  }
  else if(o->tag == 'Y') {
    return obj_new_symbol(strdup(o->s));
  }
  else if(o->tag == 'K') {
    return obj_new_keyword(strdup(o->s));
  }
  else if(o->tag == 'P') {
    return obj_new_primop(o->primop);
  }
  else if(o->tag == 'D') {
    return obj_new_dylib(o->dylib);
  }
  else if(o->tag == 'F') {
    return obj_new_ffi(o->cif, o->funptr, obj_copy(o->arg_types), obj_copy(o->return_type));
  }
  else if(o->tag == 'L') {
    return o;
  }
  else if(o->tag == 'M') {
    return o;
  }
  else {
    printf("obj_copy() can't handle type tag %c (%d).\n", o->tag, o->tag);
    assert(false);
  }
}

void free_internal_data(Obj *dead) {
  if(dead->tag == 'F') {
    free(dead->cif);
  }
  else if(dead->tag == 'S' || dead->tag == 'Y' || dead->tag == 'K') {
    free(dead->s);
  }
}

void gc_sweep() {
  int kill_count = 0;
  Obj **p = &obj_latest;
  while(*p) {
    if(!(*p)->alive) {
      Obj *dead = *p;
      *p = dead->prev;
      free_internal_data(dead);
      //printf("free %p %c\n", dead, dead->tag);
      free(dead);
      obj_total--;
      kill_count++;
    }
    else {
      (*p)->alive = false; // for next gc collect
      p = &(*p)->prev;
    }
  }
  if(LOG_GC_KILLS) {
    printf("\e[33mDeleted %d Obj:s.\e[0m\n", kill_count);
  }
}

Obj *obj_list_internal(Obj *objs[]) {
  Obj *list = obj_new_cons(NULL, NULL);
  Obj **o = objs;
  Obj *prev = list;
  while(*o) {
    prev->car = *o;
    Obj *new = obj_new_cons(NULL, NULL);
    prev->cdr = new;
    prev = new;
    o++;
  }
  return list;
}

#define obj_list(...) obj_list_internal((Obj*[]){__VA_ARGS__, NULL});

bool obj_eq(Obj *a, Obj *b) {
  //printf("Comparing %s with %s.\n", obj_to_string(a)->s, obj_to_string(b)->s);

  if(a == b) {
    return true;
  }
  else if(a == NULL || b == NULL) {
    return false;
  }
  else if(a->tag != b->tag) {
    return false;
  }
  else if(a->tag == 'S' || a->tag == 'Y' || a->tag == 'K') {
    return (strcmp(a->s, b->s) == 0);
  }
  else if(a->tag == 'Q') {
    return a->void_ptr == b->void_ptr;
  }
  else if(a->tag == 'I') {
    return a->i == b->i;
  }
  else if(a->tag == 'V') {
    return a->f32 == b->f32;
  }
  else if(a->tag == 'C') {
    Obj *pa = a;
    Obj *pb = b;
    while(1) {
      if(obj_eq(pa->car, pb->car)) {
	if(!pa->cdr && !pb->cdr) {
	  return true;
	}
	else if(pa->cdr && !pb->cdr) {
	  return false;
	}
	else if(!pa->cdr && pb->cdr) {
	  return false;
	}
	else {
	  pa = pa->cdr;
	  pb = pb->cdr;
	}
      }
      else {
	return false;
      }
    }
  }
  else if(a->tag == 'E') {
    if(!obj_eq(a->parent, b->parent)) { return false; }
    printf("Can't compare dicts just yet...\n");
    return false;
  }
  else {
    char buffer[512];
    snprintf(buffer, 512, "Can't compare %s with %s.\n", obj_to_string(a)->s, obj_to_string(b)->s);
    error = obj_new_string(strdup(buffer));
    return false;
  }
}

int read_line_nr;
int read_line_pos;
int read_pos = 0;
#define CURRENT s[read_pos]

bool is_ok_in_symbol(char c, bool initial) {
  if(isdigit(c) && initial) {
    return false;
  }
  else if(c == '\'' && initial) {
    return true;
  }
  else if(c == '!' || c == '?' || c == '<' || c == '>' || c == '=' || c == '%' || 
	  c == '+' || c == '*' || c == '/' || c == '-' || c == '_') {
    return true;
  }
  else if(isalpha(c) || isdigit(c)) {
    return true;
  }
  else {
    return false;
  }
}

bool is_whitespace(char c) {
  return c == ' ' || c == '\t' || c == '\n' || c == ',';
}

void hit_new_line() {
  read_line_nr++;
  read_line_pos = 0;
}

void skip_whitespace(char *s) {
  while(is_whitespace(CURRENT)) {
    if(CURRENT == '\n') {
      hit_new_line();
    }
    read_pos++;
  }
  if(CURRENT == ';') {
    while(CURRENT != '\n' && CURRENT != '\0') {
      read_pos++;
    }
    read_pos++;
    skip_whitespace(s);
  }
}

void print_read_pos() {
  printf("Line: %d, pos: %d.\n", read_line_nr, read_line_pos);
}

#define STACK_SIZE 512
Obj *stack[STACK_SIZE];
int stack_pos;

#define STACK_TRACE_LEN 256
char function_trace[STACK_SIZE][STACK_TRACE_LEN];
int function_trace_pos;

void stack_print() {
  printf("----- STACK -----\n");
  for(int i = 0; i < stack_pos; i++) {
    printf("%d\t%s\n", i, obj_to_string(stack[i])->s);
  }
  printf("-----  END  -----\n\n");
}

void stack_push(Obj *o) {
  if(LOG_STACK) {
    printf("Pushing %s\n", obj_to_string(o)->s);
  }
  if(stack_pos >= STACK_SIZE) {
    printf("Stack overflow.");
    exit(1);
  }
  stack[stack_pos++] = o;
  if(LOG_STACK) {
    stack_print();
  }
}

Obj *stack_pop() {
  if(error) {
    return nil;
  }
  if(stack_pos <= 0) {
    printf("Stack underflow.");
    assert(false);
  }
  if(LOG_STACK) {
    printf("Popping %s\n", obj_to_string(stack[stack_pos - 1])->s);
  }
  Obj *o = stack[--stack_pos];
  if(LOG_STACK) {
    stack_print();
  }
  return o;
}

void function_trace_print() {
  printf("     -----------------\n");
  for(int i = function_trace_pos - 1; i >= 0; i--) {
    printf("%3d  %s\n", i, function_trace[i]);
  }
  printf("     -----------------\n");
}

void eval_internal(Obj *env, Obj *o);

Obj *read_internal(Obj *env, char *s) {
  skip_whitespace(s);

  if(CURRENT == ')' || CURRENT == ']') {
    read_pos++;
    printf("Too many parenthesis at the end.\n");
    print_read_pos();
    return nil;
  }
  else if(CURRENT == '(' || CURRENT == '[') {
    Obj *list = obj_new_cons(NULL, NULL);
    Obj *prev = list;
    read_pos++;
    while(1) {
      skip_whitespace(s);
      if(CURRENT == '\0') {
	printf("Missing parenthesis at the end.\n");
	print_read_pos();
	return nil;
      }
      if(CURRENT == ')' || CURRENT == ']') {
	read_pos++;
	break;
      }
      Obj *o = read_internal(env, s);
      Obj *new = obj_new_cons(NULL, NULL);
      prev->car = o;
      prev->cdr = new;
      prev = new;
    }
    return list;
  }
  else if(CURRENT == '{') {
    Obj *list = obj_new_cons(NULL, NULL);
    Obj *prev = list;
    read_pos++;
    while(1) {
      skip_whitespace(s);
      if(CURRENT == '\0') {
	printf("Missing } at the end.\n");
	print_read_pos();
	return nil;
      }
      if(CURRENT == '}') {
	read_pos++;
	break;
      }
      Obj *key = read_internal(env, s);

      if(CURRENT == '}') {
	printf("Uneven number of forms in dictionary.\n");
	print_read_pos();
	return nil;
      }
      
      Obj *value = read_internal(env, s);
      
      Obj *new = obj_new_cons(NULL, NULL);
      Obj *pair = obj_new_cons(key, value);
      prev->car = pair;
      prev->cdr = new;
      prev = new;
    }
    Obj *dict = obj_new_environment(NULL);
    dict->bindings = list;
    return dict;
  }
  else if(CURRENT == '&') {
    read_pos++;
    return ampersand;
  }
  else if(isdigit(CURRENT) || (CURRENT == '-' && isdigit(s[read_pos + 1]))) {
    int negator = 1;
    if(CURRENT == '-') {
      negator = -1;
      read_pos++;
    }
    bool is_floating = false;
    char scratch[32];
    int i = 0;
    while(isdigit(CURRENT)) {
      scratch[i++] = CURRENT;
      read_pos++;
      if(CURRENT == '.' && !is_floating) {
	scratch[i++] = CURRENT;
	is_floating = true;
	read_pos++;
      }
      if(CURRENT == 'f') {
	is_floating = true;
	read_pos++;
	break;
      }
    }
    scratch[i] = '\0';
    if(is_floating) {
      float x = atof(scratch) * negator;
      return obj_new_float(x);
    } else {
      int num = atoi(scratch) * negator;
      return obj_new_int(num);
    }
  }
  else if(CURRENT == '\'') {
    read_pos++;
    Obj *sym = read_internal(env, s);
    Obj *cons2 = obj_new_cons(sym, nil);
    Obj *cons1 = obj_new_cons(lisp_quote, cons2);
    return cons1;
  }
  else if(is_ok_in_symbol(CURRENT, true)) {
    char name[512];
    int i = 0;
    while(is_ok_in_symbol(CURRENT, false)) {
      name[i++] = CURRENT;
      read_pos++;
    }
    name[i] = '\0';
    return obj_new_symbol(name);
  }
  else if(CURRENT == ':') {
    read_pos++;
    char name[512];
    int i = 0;
    while(is_ok_in_symbol(CURRENT, true)) {
      name[i++] = CURRENT;
      read_pos++;
    }
    name[i] = '\0';
    return obj_new_keyword(name);
  }
  else if(CURRENT == '"') {
    read_pos++;
    char str[512];
    int i = 0;
    while(CURRENT != '"') {
      if(CURRENT == '\0') {
	printf("Missing quote in string\n");
	break;
      }
      else if(CURRENT == '\\') {
	read_pos++;
	if(CURRENT == 'n') {
	  str[i++] = '\n';
	}
	else if(CURRENT == '"') {
	  str[i++] = '"';
	}
	else if(CURRENT == '\\') {
	  str[i++] = '\\';
	}
	else {
	  printf("Can't read '%c' after backslash (%d)\n", CURRENT, CURRENT);
	  read_pos++;
	  return nil;
	}
	read_pos++;
      }
      else {
	str[i++] = CURRENT;
	read_pos++;
      }
    }
    str[i] = '\0';
    read_pos++;
    return obj_new_string(str);
  }
  else if(CURRENT == 0) {
    return nil;
  }
  else {
    printf("Can't read '%c' (%d)\n", CURRENT, CURRENT);
    read_pos++;
    return nil;
  }
}

Obj *read_string(Obj *env, char *s) {
  read_line_nr = 1;
  read_line_pos = 0;
  read_pos = 0;
  Obj *top_forms = NULL;
  Obj *prev = NULL;
  while(s[read_pos] != '\0') {
    Obj *o = read_internal(env, s);
    Obj *cons = obj_new_cons(NULL, NULL);
    cons->car = o;
    if(!top_forms) {
      top_forms = cons;
    }
    if(prev) {
      prev->cdr = cons;
    }
    prev = cons;
    skip_whitespace(s);
  }
  return top_forms;
}

Obj *open_file(const char *filename) {
  assert(filename);
  
  char * buffer = 0;
  long length;
  FILE * f = fopen (filename, "rb");

  if(f) {
    fseek (f, 0, SEEK_END);
    length = ftell (f);
    fseek (f, 0, SEEK_SET);
    buffer = malloc (length + 1);
    if (buffer)	{
      fread (buffer, 1, length, f);
      buffer[length] = '\0';
    }
    fclose (f);
  } else {
    set_error_and_return("Failed to open file: ", obj_new_string((char*)filename));
  }

  if (buffer) {
    return obj_new_string(buffer);
  } else {
    set_error_and_return("Failed to open buffer from file: ", obj_new_string((char*)filename));
  }
}

Obj *save_file(const char *filename, const char *contents) {
  FILE * f = fopen (filename, "w");
  if(f) {
    fprintf(f, "%s", contents);
    fclose(f);
    return obj_new_symbol("success");
  } else {
    set_error_and_return("Failed to save file: ", obj_new_string((char*)filename));
  }
}

Obj *env_lookup(Obj *env, Obj *symbol) {
  Obj *p = env->bindings;
  while(p && p->car) {
    Obj *pair = p->car;
    if(obj_eq(pair->car, symbol)) {
      return pair->cdr;
    }
    else {
      p = p->cdr;
    }
  }
  if(env->parent) {
    return env_lookup(env->parent, symbol);
  }
  else {
    return NULL;
  }
}

Obj *env_lookup_binding(Obj *env, Obj *symbol) {
  Obj *p = env->bindings;
  while(p && p->car) {
    Obj *pair = p->car;
    if(obj_eq(pair->car, symbol)) {
      return pair;
    }
    else {
      p = p->cdr;
    }
  }
  if(env->parent) {
    return env_lookup_binding(env->parent, symbol);
  }
  else {
    return nil;
  }
}

void env_extend(Obj *env, Obj *key, Obj *value) {
  assert(env->tag == 'E');
  
  Obj *pair = obj_new_cons(key, value);
  Obj *cons = obj_new_cons(pair, env->bindings);

  env->bindings = cons;
}

void env_extend_with_args(Obj *calling_env, Obj *function, int arg_count, Obj **args) {
  Obj *paramp = function->params;
  for(int i = 0; i < arg_count; i++) {
    if(paramp && !paramp->car) {
      set_error("Too many arguments to function: ", function);
    }
    env_extend(calling_env, paramp->car, args[i]);
    paramp = paramp->cdr;
  }
  if(paramp && paramp->cdr) {
    set_error("Too few arguments to function: ", function);
  }
}

bool is_true(Obj *o) {
  //printf("is_true? %s\n", obj_to_string(o)->s);
  if(o == lisp_false || (o->tag == 'Y' && strcmp(o->s, "false") == 0)) {
    return false;
  }
  else {
    return true;
  }
}

bool obj_match(Obj *env, Obj *attempt, Obj *value);

bool obj_match_lists(Obj *env, Obj *attempt, Obj *value) {
  //printf("Matching list %s with %s\n", obj_to_string(attempt)->s, obj_to_string(value)->s);
  Obj *p1 = attempt;
  Obj *p2 = value;
  while(p1 && p1->car) {
    if(obj_eq(p1->car, ampersand) && p1->cdr && p1->cdr->car) {
      //printf("Matching & %s against %s\n", obj_to_string(p1->cdr->car)->s, obj_to_string(p2)->s);
      bool matched_rest = obj_match(env, p1->cdr->car, p2);
      return matched_rest;
    }
    else if(!p2 || !p2->car) {
      return false;
    }
    bool result = obj_match(env, p1->car, p2->car);
    if(!result) {
      return false;
    }
    p1 = p1->cdr;
    p2 = p2->cdr;
  }
  if(p2 && p2->car) {
    return false;
  }
  else {
    return true;
  }
}

bool obj_match(Obj *env, Obj *attempt, Obj *value) {

  if(attempt->tag == 'C' && obj_eq(attempt->car, lisp_quote) && attempt->cdr && attempt->cdr->car) {
    // Dubious HACK to enable matching on quoted things...
    // Don't want to extend environment in this case!
    Obj *quoted_attempt = attempt->cdr->car;
    return obj_eq(quoted_attempt, value);
  }
  else if(attempt->tag == 'Y') {
    //printf("Binding %s to value %s in match.\n", obj_to_string(attempt)->s, obj_to_string(value)->s);
    env_extend(env, attempt, value);
    return true;
  }
  else if(attempt->tag == 'C' && value->tag == 'C') {
    return obj_match_lists(env, attempt, value);
  }
  else if(obj_eq(attempt, value)) {
    return true;
  }
  else {
    /* printf("attempt %s (%c) is NOT equal to value %s (%c)\n", */
    /* 	   obj_to_string(attempt)->s, */
    /* 	   attempt->tag, */
    /* 	   obj_to_string(value)->s, */
    /* 	   value->tag); */
    return false;
  }
}

void match(Obj *env, Obj *value, Obj *attempts) {
  Obj *p = attempts;
  while(p && p->car) {
    //printf("\nWill match %s with value %s\n", obj_to_string(p->car)->s, obj_to_string(value)->s);
    Obj *new_env = obj_new_environment(env);
    bool result = obj_match(new_env, p->car, value);

    if(result) {
      //printf("Match found, evaling %s in env\n", obj_to_string(p->cdr->car)->s); //, obj_to_string(new_env)->s);
      eval_internal(new_env, p->cdr->car); // eval the following form using the new environment
      return;
    }
    
    if(!p->cdr) {
      set_error("Uneven nr of forms in match.", attempts);
    }
      
    p = p->cdr->cdr;
  }

  set_error("Failed to find a suitable match for: ", value);
}

void apply(Obj *function, Obj **args, int arg_count) {
  if(function->tag == 'L') {
    Obj *calling_env = obj_new_environment(function->env);
    //printf("Calling function that has parameters: %s\n", obj_to_string(function->params)->s);
    env_extend_with_args(calling_env, function, arg_count, args);
    //printf("Lambda env: %s\n", obj_to_string(calling_env)->s);
    eval_internal(calling_env, function->body);
  }
  else if(function->tag == 'P') {
    Obj *result = function->primop(args, arg_count);
    stack_push(result);
  }
  else if(function->tag == 'F') {
    assert(function);

    if(!function->funptr) {
      error = obj_new_string("Can't call foregin function, it's funptr is NULL. May be a stub function with just a signature?");
      return;
    }
    
    assert(function->cif);
    assert(function->arg_types);
    assert(function->return_type);
     
    void *values[arg_count];

    Obj *p = function->arg_types;
    for(int i = 0; i < arg_count; i++) {
      if(p && p->cdr) {
	assert(p->car);
	if(obj_eq(p->car, type_int)) {
	  assert_or_set_error(args[i]->tag == 'I', "Invalid type of arg: ", args[i]);
	  values[i] = &args[i]->i;
	}
	else if(obj_eq(p->car, type_float)) {
	  assert_or_set_error(args[i]->tag == 'V', "Invalid type of arg: ", args[i]);
	  values[i] = &args[i]->f32;
	}
	else if(obj_eq(p->car, type_string)) {
	  assert_or_set_error(args[i]->tag == 'S', "Invalid type of arg: ", args[i]);
	  values[i] = &args[i]->s;
	}
	else if(p->car->tag == 'C' && obj_eq(p->car->car, obj_new_keyword("ptr"))) { // TODO: replace with a shared keyword to avoid allocs
	  assert_or_set_error(args[i]->tag == 'Q', "Invalid type of arg: ", args[i]);
	  values[i] = &args[i]->void_ptr;
	}
	else {
	  set_error("Can't call foreign function with argument of type ", p->car);
	}
	p = p->cdr;
      }
      else {
	set_error("Too many arguments to ", function);
      }	
    }

    if(p && p->car) {
      set_error("Too few arguments to ", function);
    }

    Obj *obj_result = NULL;
    
    if(obj_eq(function->return_type, type_string)) {
      //printf("Returning string.\n");
      char *c = NULL;
      ffi_call(function->cif, function->funptr, &c, values);

      if(c == NULL) {
	//printf("c is null");
	obj_result = obj_new_string("");
      }
      else {      
	obj_result = obj_new_string(c);
      }
    }
    else if(obj_eq(function->return_type, type_int)) { 
      //printf("Returning int.\n");
      int result;
      ffi_call(function->cif, function->funptr, &result, values);
      obj_result = obj_new_int(result);
    }
    else if(obj_eq(function->return_type, type_bool)) { 
      //printf("Returning bool.\n");
      int result;
      ffi_call(function->cif, function->funptr, &result, values);
      obj_result = result ? lisp_true : lisp_false;
    }
    else if(obj_eq(function->return_type, type_float)) { 
      //printf("Returning float.\n");
      float result;
      ffi_call(function->cif, function->funptr, &result, values);
      obj_result = obj_new_float(result);
    }
    else if(obj_eq(function->return_type, type_void)) { 
      //printf("Returning void.\n");
      int result;
      ffi_call(function->cif, function->funptr, &result, values);
      obj_result = nil;
    }
    else if(function->return_type->tag == 'C' && obj_eq(function->return_type->car, type_ptr)) {
      void *result;
      ffi_call(function->cif, function->funptr, &result, values);
      //printf("Creating new void* with value: %p\n", result);
      obj_result = obj_new_ptr(result);
    }
    else {
      set_error("Returning what? ", function->return_type);
    }

    assert(obj_result);
    stack_push(obj_result);
  }
  else if(function->tag == 'K') {
    if(arg_count != 1) {
      error = obj_new_string("Args to keyword lookup must be a single arg.");
    }
    else if(args[0]->tag != 'E') {
      error = obj_new_string("Arg 0 to keyword lookup must be a dictionary: ");
      obj_string_mut_append(error, obj_to_string(args[0])->s);
    }
    else {
      Obj *value = env_lookup(args[0], function);
      if(value) {
	stack_push(value);
      } else {
	error = obj_new_string("Failed to lookup keyword '");
	obj_string_mut_append(error, obj_to_string(function)->s);
	obj_string_mut_append(error, "'");
      }
    }
  }
  else {
    set_error("Can't call non-function: ", function);
  }
}

void global_env_extend(Obj *key, Obj *val) {
  assert(global_env);
  Obj *existing_binding = env_lookup_binding(global_env, key);
  if(existing_binding->car) {
    existing_binding->cdr = val;
  } else {
    env_extend(global_env, key, val);
  }
}

#define HEAD_EQ(str) (o->car->tag == 'Y' && strcmp(o->car->s, (str)) == 0)

void eval_list(Obj *env, Obj *o) {
  assert(o);
  //printf("Evaling list %s\n", obj_to_string(o)->s);
  if(!o->car) {
    stack_push(o); // nil, empty list
  }
  else if(HEAD_EQ("do")) {
    Obj *p = o->cdr;
    while(p && p->car) {
      eval_internal(env, p->car);
      if(error) { return; }
      p = p->cdr;
      if(p && p->car) {
	stack_pop(); // remove result from form that is not last
      }
    }
  }
  else if(HEAD_EQ("let")) {
    Obj *let_env = obj_new_environment(env);
    Obj *p = o->cdr->car;
    assert_or_set_error(o->cdr->car, "No bindings in 'let' form.", o);
    while(p && p->car) {
      if(!p->cdr) {
	set_error("Uneven nr of forms in let: ", o);
      }
      assert_or_set_error(p->car->tag == 'Y', "Must bind to symbol in let form: ", p->car);
      eval_internal(let_env, p->cdr->car);
      if(error) { return; }
      env_extend(let_env, p->car, stack_pop());
      p = p->cdr->cdr;
    }
    assert_or_set_error(o->cdr->cdr->car, "No body in 'let' form.", o);
    eval_internal(let_env, o->cdr->cdr->car);
  }
  else if(HEAD_EQ("not")) {
    Obj *p = o->cdr;
    while(p) {
      if(p->car) {
	eval_internal(env, p->car);
	if(error) { return; }
	if(is_true(stack_pop())) {
	  stack_push(lisp_false);
	  return;
	}
      }
      p = p->cdr;
    }
    stack_push(lisp_true);
  }
  else if(HEAD_EQ("quote")) {
    if(o->cdr == nil) {
      stack_push(nil);
    } else {
      stack_push(o->cdr->car);
    }
  }
  else if(HEAD_EQ("while")) {
    eval_internal(env, o->cdr->car);
    if(error) {
      return;
    }
    while(is_true(stack_pop())) {
      eval_internal(env, o->cdr->cdr->car);
      stack_pop();
      eval_internal(env, o->cdr->car);
      if(error) {
	return;
      }
    }
    stack_push(nil);
  }
  else if(HEAD_EQ("if")) {
    eval_internal(env, o->cdr->car);
    if(error) {
      return;
    }
    else if(is_true(stack_pop())) {
      eval_internal(env, o->cdr->cdr->car);
    }
    else {
      eval_internal(env, o->cdr->cdr->cdr->car);
    }
  }
  else if(HEAD_EQ("match")) {
    eval_internal(env, o->cdr->car);
    if(error) { return; }
    Obj *value = stack_pop();
    Obj *p = o->cdr->cdr;   
    match(env, value, p);
  }
  else if(HEAD_EQ("reset!")) {
    assert_or_set_error(o->cdr->car->tag == 'Y', "Must use 'reset!' on a symbol.", o->cdr->car);
    Obj *pair = env_lookup_binding(env, o->cdr->car);
    if(!pair->car || pair->car->tag != 'Y') {
      printf("Can't reset! binding '%s', it's '%s'\n", o->cdr->car->s, obj_to_string(pair)->s);
      stack_push(nil);
      return;
    }
    eval_internal(env, o->cdr->cdr->car);
    if(error) { return; }
    pair->cdr = stack_pop();
    stack_push(pair->cdr);
  }
  else if(HEAD_EQ("fn")) {
    assert_or_set_error(o->cdr, "Lambda form too short (no parameter list or body).", o);
    assert_or_set_error(o->cdr->car, "No parameter list in lambda.", o);
    Obj *params = o->cdr->car;
    assert_or_set_error(o->cdr->cdr, "Lambda form too short (no body).", o);
    assert_or_set_error(o->cdr->cdr->car, "No body in lambda: ", o);
    Obj *body = o->cdr->cdr->car;
    //printf("Creating lambda with env: %s\n", obj_to_string(env)->s);
    Obj *lambda = obj_new_lambda(params, body, env, o);
    stack_push(lambda);
  }
  else if(HEAD_EQ("macro")) {
    assert_or_set_error(o->cdr, "Macro form too short (no parameter list or body): ", o);
    assert_or_set_error(o->cdr->car, "No parameter list in macro: ", o);
    Obj *params = o->cdr->car;
    assert_or_set_error(o->cdr->cdr, "Macro form too short (no body): ", o);
    assert_or_set_error(o->cdr->cdr->car, "No body in macro: ", o);
    Obj *body = o->cdr->cdr->car;
    Obj *macro = obj_new_macro(params, body, env, o);
    stack_push(macro);
  }
  else if(HEAD_EQ("def")) {
    assert_or_set_error(o->cdr, "Too few args to 'def': ", o);
    assert_or_set_error(o->cdr->car, "Can't assign to nil: ", o);
    assert_or_set_error(o->cdr->car->tag == 'Y', "Can't assign to non-symbol: ", o);
    Obj *key = o->cdr->car;
    eval_internal(env, o->cdr->cdr->car); // eval the second arg to 'def', the value to assign
    if(error) { return; } // don't define it if there was an error
    Obj *val = stack_pop();
    global_env_extend(key, val);
    //printf("def %s to %s\n", obj_to_string(key)->s, obj_to_string(val)->s);
    stack_push(val);
  }
  else if(HEAD_EQ("def?")) {
    Obj *key = o->cdr->car;
    if(obj_eq(nil, env_lookup_binding(env, key))) {
      stack_push(lisp_false);
    } else {
      stack_push(lisp_true);
    }
  }
  else {
    // Lambda, primop or macro
    eval_internal(env, o->car);
    if(error) { return; }
    
    Obj *function = stack_pop();
    assert_or_set_error(function, "Can't call NULL.", o);
    
    bool eval_args = function->tag != 'M'; // macros don't eval their args
    Obj *p = o->cdr;
    int count = 0;
    
    while(p && p->car) {
      if(error) {
	return;
      }
      
      if(eval_args) {
	eval_internal(env, p->car);
      }
      else {
	stack_push(p->car); // push non-evaled
      }
      count++;
      p = p->cdr;
    }

    if(error) {
      return;
    }
    
    Obj *args[count];
    for(int i = 0; i < count; i++) {
      Obj *arg = stack_pop();
      args[count - i - 1] = arg;
    }

    if(function->tag == 'M') {
      Obj *calling_env = obj_new_environment(function->env);
      env_extend_with_args(calling_env, function, count, args);
      eval_internal(calling_env, function->body);
      if(error) { return; }
      Obj *expanded = stack_pop();
      if(SHOW_MACRO_EXPANSION) {
	printf("Expanded macro: %s\n", obj_to_string(expanded)->s);
      }
      eval_internal(env, expanded);
    }
    else {
      if(function_trace_pos > STACK_SIZE - 1) {
	printf("Out of function trace stack.\n");
	stack_print();
	function_trace_print();
	exit(1);
      }

      if(LOG_FUNC_APPLICATION) {
	printf("evaluating form %s\n", obj_to_string(o)->s);
      }
      
      snprintf(function_trace[function_trace_pos], STACK_TRACE_LEN, "%s", obj_to_string(o)->s);
      function_trace_pos++;
      
      apply(function, args, count);
      if(!error) {
	function_trace_pos--;
      }
    }
  }
}

void eval_internal(Obj *env, Obj *o) {
  if(error) { return; }

  //printf("%s\n", obj_to_string(o)->s);
  
  if(!o) {
    stack_push(nil);
  }
  else if(o->tag == 'C') {
    eval_list(env, o);
  }
  else if(o->tag == 'E') {
    Obj *new_env = obj_copy(o);
    Obj *p = new_env->bindings;
    while(p && p->car) {
      Obj *pair = p->car;
      eval_internal(env, pair->cdr);
      //printf("Evaling env-binding %s, setting cdr to %s.\n", obj_to_string(pair)->s, obj_to_string(stack[stack_pos - 1])->s);
      pair->cdr = stack_pop();
      p = p->cdr;
    }
    stack_push(new_env);
  }
  else if(o->tag == 'Y') {
    Obj *result = env_lookup(env, o);
    if(!result) {
      char buffer[256];
      snprintf(buffer, 256, "Can't find '%s' in environment.", obj_to_string(o)->s);
      error = obj_new_string(buffer);
      stack_push(nil);
    } else {
      stack_push(result);
    }
  }
  else {
    stack_push(o);
  }
}

Obj *eval(Obj *env, Obj *form) {
  error = NULL;
  stack_pos = 0;
  function_trace_pos = 0;
  eval_internal(env, form);
  Obj *result = stack_pop();
  return result;
}

void gc(Obj *env, Obj *forms) {
  if(forms) {
    obj_mark_alive(forms);
  }
  obj_mark_alive(env);
  for(int i = 0; i < stack_pos - 1; i++) {
    obj_mark_alive(stack[i]);
  }
  gc_sweep();
}

void eval_text(Obj *env, char *text, bool print) {
  Obj *forms = read_string(env, text);
  Obj *form = forms;
  while(form && form->car) {
    Obj *result = eval(env, form->car);
    if(error) {
      printf("\e[31mERROR: %s\e[0m\n", obj_to_string_not_prn(error)->s);
      function_trace_print();
      error = NULL;
      gc(env, NULL);
      return;
    }
    if(print) {
      if(result) {
	obj_print(result);
      }
      else {
	printf("Result was NULL when evaling %s\n", obj_to_string(form->car)->s);
      }
      printf("\n");
    }
    form = form->cdr;
    gc(env, forms);
  }
}

#define MAX_INPUT_BUFFER_SIZE 2048
char input[MAX_INPUT_BUFFER_SIZE];

void repl(Obj *env) {
  while(1) {
    printf("\e[36mλ>\e[0m ");
    fgets(input, MAX_INPUT_BUFFER_SIZE, stdin);
    if(strcmp(input, "q\n") == 0) {
      break;
    }
    eval_text(env, input, true);
    while(stack_pos > 0) {
      //printf("°"); // Popping extra stack value
      stack_pop();
    }
    printf("\n");
    //assert(stack_pos == 0);
    //stack_print();
  }
  gc_sweep();
}

Obj *p_open_file(Obj** args, int arg_count) {
  if(arg_count != 1) { return nil; }
  if(args[0]->tag != 'S') { return nil; }
  return open_file(args[0]->s);
}

Obj *p_save_file(Obj** args, int arg_count) {
  if(arg_count != 2) { return nil; }
  if(args[0]->tag != 'S') { return nil; }
  if(args[1]->tag != 'S') { return nil; }
  return save_file(args[0]->s, args[1]->s);
}

Obj *p_add(Obj** args, int arg_count) {
  if(arg_count == 0 || args[0]->tag == 'I') {
    int sum = 0;
    for(int i = 0; i < arg_count; i++) {
      if(args[i]->tag != 'I') {
	printf("Args to add must be integers.\n");
	return nil;
      }
      sum += args[i]->i;
    }
    return obj_new_int(sum);
  }
  else if(args[0]->tag == 'V') {
    float sum = 0;
    for(int i = 0; i < arg_count; i++) {
      if(args[i]->tag != 'V') {
	printf("Args to add must be floats.\n");
	return nil;
      }
      sum += args[i]->f32;
    }
    return obj_new_float(sum);
  }
  else {
    error = obj_new_string("Can't add non-numbers together.");
    return nil;
  }
}

Obj *p_sub(Obj** args, int arg_count) {
  if(arg_count == 0 || args[0]->tag == 'I') {
    if(arg_count == 1) { return obj_new_int(-args[0]->i); }
    int sum = args[0]->i;
    for(int i = 1; i < arg_count; i++) {
      sum -= args[i]->i;
    }
    return obj_new_int(sum);
  }
  else if(args[0]->tag == 'V') {
    if(arg_count == 1) {
      return obj_new_int(-args[0]->f32);
    }
    float sum = args[0]->f32;
    for(int i = 1; i < arg_count; i++) {
      sum -= args[i]->f32;
    }
    return obj_new_float(sum);
  }
  else {
    error = obj_new_string("Can't subtract non-numbers.");
    return nil;
  }
}

Obj *p_mul(Obj** args, int arg_count) {
  if(arg_count == 0) {
    return obj_new_int(1);
  }
  
  if(args[0]->tag == 'I') {
    int prod = args[0]->i;
    for(int i = 1; i < arg_count; i++) {
      prod *= args[i]->i;
    }
    return obj_new_int(prod);
  }
  else if(args[0]->tag == 'V') {
    float prod = args[0]->f32;
    for(int i = 1; i < arg_count; i++) {
      prod *= args[i]->f32;
    }
    return obj_new_float(prod);
  }
  else {
    error = obj_new_string("Can't multiply non-numbers.");
    return nil;
  }
}

Obj *p_div(Obj** args, int arg_count) {
  if(arg_count == 0) {
    return obj_new_int(1);
  }
  
  if(args[0]->tag == 'I') {
    int prod = args[0]->i;
    for(int i = 1; i < arg_count; i++) {
      prod /= args[i]->i;
    }
    return obj_new_int(prod);
  }
  else if(args[0]->tag == 'V') {
    float prod = args[0]->f32;
    for(int i = 1; i < arg_count; i++) {
      prod /= args[i]->f32;
    }
    return obj_new_float(prod);
  }
  else {
    error = obj_new_string("Can't divide non-numbers.");
    return nil;
  }
}

Obj *p_mod(Obj** args, int arg_count) {
  if(arg_count == 0) {
    return obj_new_int(1);
  }
  int prod = args[0]->i;
  for(int i = 1; i < arg_count; i++) {
    prod %= args[i]->i;
  }
  return obj_new_int(prod);
}

Obj *p_eq(Obj** args, int arg_count) {
  if(arg_count < 2) {
    printf("The function '=' requires at least 2 arguments.\n");
    return nil;
  }
  for(int i = 0; i < arg_count - 1; i++) {
    if(!obj_eq(args[i], args[i + 1])) {
      return lisp_false;
    }
  }
  return lisp_true;
}

Obj *p_list(Obj** args, int arg_count) {
  Obj *first = NULL;
  Obj *prev = NULL;
  for(int i = 0; i < arg_count; i++) {
    Obj *new = obj_new_cons(args[i], nil);
    if(!first) {
      first = new;
    }
    if(prev) {
      prev->cdr = new;
    }
    prev = new;
  }
  return first;
}

Obj *p_str(Obj** args, int arg_count) {
  Obj *s = obj_new_string("");
  for(int i = 0; i < arg_count; i++) {
    obj_string_mut_append(s, obj_to_string_not_prn(args[i])->s);
  }
  return s;
}

Obj *p_str_append_bang(Obj** args, int arg_count) {
  if(arg_count != 2) {
    error = obj_new_string("'str-append!' takes exactly two arguments");
    return nil;
  }
  if(args[0]->tag != 'S') {
    error = obj_new_string("'str-append!' arg0 invalid");
    return nil;
  }
  if(args[1]->tag != 'S') {
    error = obj_new_string("'str-append!' arg1 invalid");
    return nil;
  }
  Obj *s = args[0];
  obj_string_mut_append(s, args[1]->s);
  return s;
}

char *str_replace(const char *str, const char *old, const char *new) {

	/* Adjust each of the below values to suit your needs. */

	/* Increment positions cache size initially by this number. */
	size_t cache_sz_inc = 16;
	/* Thereafter, each time capacity needs to be increased,
	 * multiply the increment by this factor. */
	const size_t cache_sz_inc_factor = 3;
	/* But never increment capacity by more than this number. */
	const size_t cache_sz_inc_max = 1048576;

	char *pret, *ret = NULL;
	const char *pstr2, *pstr = str;
	size_t i, count = 0;
	ptrdiff_t *pos_cache = NULL;
	size_t cache_sz = 0;
	size_t cpylen, orglen, retlen, newlen, oldlen = strlen(old);

	/* Find all matches and cache their positions. */
	while ((pstr2 = strstr(pstr, old)) != NULL) {
		count++;

		/* Increase the cache size when necessary. */
		if (cache_sz < count) {
			cache_sz += cache_sz_inc;
			pos_cache = realloc(pos_cache, sizeof(*pos_cache) * cache_sz);
			if (pos_cache == NULL) {
				goto end_repl_str;
			}
			cache_sz_inc *= cache_sz_inc_factor;
			if (cache_sz_inc > cache_sz_inc_max) {
				cache_sz_inc = cache_sz_inc_max;
			}
		}

		pos_cache[count-1] = pstr2 - str;
		pstr = pstr2 + oldlen;
	}

	orglen = pstr - str + strlen(pstr);

	/* Allocate memory for the post-replacement string. */
	if (count > 0) {
		newlen = strlen(new);
		retlen = orglen + (newlen - oldlen) * count;
	} else	retlen = orglen;
	ret = malloc(retlen + 1);
	if (ret == NULL) {
		goto end_repl_str;
	}

	if (count == 0) {
		/* If no matches, then just duplicate the string. */
		strcpy(ret, str);
	} else {
		/* Otherwise, duplicate the string whilst performing
		 * the replacements using the position cache. */
		pret = ret;
		memcpy(pret, str, pos_cache[0]);
		pret += pos_cache[0];
		for (i = 0; i < count; i++) {
			memcpy(pret, new, newlen);
			pret += newlen;
			pstr = str + pos_cache[i] + oldlen;
			cpylen = (i == count-1 ? orglen : pos_cache[i+1]) - pos_cache[i] - oldlen;
			memcpy(pret, pstr, cpylen);
			pret += cpylen;
		}
		ret[retlen] = '\0';
	}

end_repl_str:
	/* Free the cache and return the post-replacement string,
	 * which will be NULL in the event of an error. */
	free(pos_cache);
	return ret;
}

Obj *p_str_replace(Obj** args, int arg_count) {
  if(arg_count != 3) {
    error = obj_new_string("'str-replace' takes exactly three arguments");
    return nil;
  }
  if(args[0]->tag != 'S') {
    error = obj_new_string("'str-replace' arg0 invalid: ");
    obj_string_mut_append(error, obj_to_string(args[0])->s);
    return nil;
  }
  if(args[1]->tag != 'S') {
    error = obj_new_string("'str-replace' arg1 invalid");
    return nil;
  }
  if(args[2]->tag != 'S') {
    error = obj_new_string("'str-replace' arg2 invalid");
    return nil;
  }

  char *s = args[0]->s;
  char *lookup = args[1]->s;
  char *replacement = args[2]->s;
  char *replaced = str_replace(s, lookup, replacement);
  return obj_new_string(replaced);
}

Obj *p_copy(Obj** args, int arg_count) {
  if(arg_count != 1) {
    error = obj_new_string("'copy' takes exactly one argument");
    return nil;
  }
  Obj *a = args[0];
  //printf("Will make a copy of: %s\n", obj_to_string(a)->s);
  Obj *b = obj_copy(a);
  return b;
}

Obj *p_print(Obj** args, int arg_count) {
  for(int i = 0; i < arg_count; i++) {
    obj_print_not_prn(args[i]);
  }
  return nil;
}

Obj *p_prn(Obj** args, int arg_count) {
  Obj *s = obj_new_string("");
  for(int i = 0; i < arg_count; i++) {
    Obj *s2 = obj_to_string(args[i]);
    obj_string_mut_append(s, s2->s);
  }
  return s;
}

Obj *p_println(Obj** args, int arg_count) {
  for(int i = 0; i < arg_count; i++) {
    obj_print_not_prn(args[i]);
    /* if(i < arg_count - 1) { */
    /*   printf(" "); */
    /* } */
  }
  printf("\n");
  return nil;
}

Obj *p_system(Obj** args, int arg_count) {
  if(arg_count != 1) { printf("Wrong argument count to 'system'\n"); return nil; }
  if(args[0]->tag != 'S') { printf("'system' takes a string as its argument\n"); return nil; }
  system(args[0]->s);
  return obj_new_symbol("success");
}

Obj *p_get(Obj** args, int arg_count) {
  if(arg_count != 2) { printf("Wrong argument count to 'get'\n"); return nil; }
  if(args[0]->tag == 'E') {
    Obj *o = env_lookup(args[0], args[1]);
    if(o) {
      return o;
    } else {
      Obj *s = obj_new_string("Can't get key '");
      obj_string_mut_append(s, obj_to_string(args[1])->s);
      obj_string_mut_append(s, "' in dict '");
      obj_string_mut_append(s, obj_to_string(args[0])->s);
      obj_string_mut_append(s, "'");
      error = s;
      return nil;
    }
  }
  else if(args[0]->tag == 'C') {
    if(args[1]->tag != 'I') {
      error = obj_new_string("get requires arg 1 to be an integer\n");
      return nil;
    }
    int i = 0;
    int n = args[1]->i;
    Obj *p = args[0];
    while(p && p->car) {
      if(i == n) {
	return p->car;
      }
      p = p->cdr;
      i++;
    }
    error = obj_new_string("Index ");
    obj_string_mut_append(error, obj_to_string(obj_new_int(i))->s);
    obj_string_mut_append(error, " out of bounds in");
    obj_string_mut_append(error, obj_to_string(args[0])->s);
    return nil;
  }
  else {
    error = obj_new_string("'get' requires arg 0 to be a dictionary or list: ");
    obj_string_mut_append(error, obj_to_string(args[0])->s);
    return nil;
  }
}

Obj *p_get_maybe(Obj** args, int arg_count) {
  if(arg_count != 2) { printf("Wrong argument count to 'get-maybe'\n"); return nil; }
  if(args[0]->tag == 'E') {
    Obj *o = env_lookup(args[0], args[1]);
    if(o) {
      return o;
    } else {
      return nil;
    }
  }
  else if(args[0]->tag == 'C') {
    if(args[1]->tag != 'I') { printf("get-maybe requires arg 1 to be an integer\n"); return nil; }
    int i = 0;
    int n = args[1]->i;
    Obj *p = args[0];
    while(p && p->car) {
      if(i == n) {
	return p->car;
      }
      p = p->cdr;
      i++;
    }
    return nil;
  }
  else {
    printf("'get-maybe' requires arg 0 to be a dictionary or list: %s\n", obj_to_string(args[0])->s);
    return nil;
  }
}

Obj *p_dict_set_bang(Obj** args, int arg_count) {
  if(arg_count != 3) { printf("Wrong argument count to 'dict-set!'\n"); return nil; }
  if(args[0]->tag != 'E') {
    printf("'dict-set!' requires arg 0 to be a dictionary: %s\n", obj_to_string(args[0])->s);
    return nil;
  }
  Obj *pair = env_lookup_binding(args[0], args[1]);
  if(pair && pair->car && pair->cdr) {
    pair->cdr = args[2];
  }
  else {
    //printf("Pair not found, will add new key.\n");
    Obj *new_pair = obj_new_cons(args[1], args[2]);
    Obj *new_cons = obj_new_cons(new_pair, args[0]->bindings);
    args[0]->bindings = new_cons;
  }
  return args[0];
}

Obj *p_dict_remove_bang(Obj** args, int arg_count) {
  if(arg_count != 2) { printf("Wrong argument count to 'dict-remove!'\n"); return nil; }
  if(args[0]->tag != 'E') {
    printf("'dict-remove!' requires arg 0 to be a dictionary: %s\n", obj_to_string(args[0])->s);
    return nil;
  }

  Obj *prev = NULL;
  Obj *p = args[0]->bindings;
  while(p && p->car) {
    Obj *pair = p->car;
    if(obj_eq(pair->car, args[1])) {
      if(prev) {
	prev->cdr = p->cdr;
      }
      else {
	args[0]->bindings = p->cdr;
      }
      break;
    }
    else {
      prev = p;
      p = p->cdr;
    }
  }
  
  return args[0];
}

Obj *p_first(Obj** args, int arg_count) {
  if(arg_count != 1) { printf("Wrong argument count to 'first'\n"); return nil; }
  if(args[0]->tag != 'C') { printf("'first' requires arg 0 to be a list: %s\n", obj_to_string(args[0])->s); return nil; }
  if(args[0]->car == NULL) {
    printf("Can't take first element of empty list.\n");
    return nil;
  }
  return args[0]->car;
}

Obj *p_rest(Obj** args, int arg_count) {
  if(arg_count != 1) { printf("Wrong argument count to 'rest'\n"); return nil; }
  if(args[0]->tag != 'C') {
    char buffer[512];
    snprintf(buffer, 512, "'rest' requires arg 0 to be a list: %s\n", obj_to_string(args[0])->s);
    error = obj_new_string(strdup(buffer));
    return nil;
  }
  if(args[0]->cdr == NULL) {
    printf("Can't take rest of empty list.\n");
    return nil;
  }
  return args[0]->cdr;
}

Obj *p_cons(Obj** args, int arg_count) {
  if(arg_count != 2) { printf("Wrong argument count to 'cons'\n"); return nil; }
  if(args[1]->tag != 'C') {
    char buffer[512];
    snprintf(buffer, 512, "'cons' requires arg 1 to be a list: %s\n", obj_to_string(args[0])->s);
    error = obj_new_string(strdup(buffer));
    return nil;
  }
  Obj *new_cons = obj_new_cons(args[0], args[1]);
  return new_cons;
}

Obj *p_cons_last(Obj** args, int arg_count) {
  if(arg_count != 2) { printf("Wrong argument count to 'cons'\n"); return nil; }
  if(args[0]->tag != 'C') { printf("'rest' requires arg 0 to be a list: %s\n", obj_to_string(args[1])->s); return nil; }
  Obj *new_list = obj_copy(args[0]);
  Obj *p = new_list;
  while(p && p->cdr) { p = p->cdr; }
  Obj *last = p;
  Obj *new_nil = obj_new_cons(NULL, NULL);
  last->car = args[1];
  last->cdr = new_nil;
  return new_list;
}

Obj *p_concat(Obj** args, int arg_count) {
  if(arg_count == 0) {
    return nil;
  }
  
  for(int i = 0; i < arg_count; i++) {
    if(args[0]->tag != 'C') { error = obj_new_string("'concat' requires all args to be lists\n"); return nil; }
  }

  int i = 0;
  Obj *new = obj_copy(args[i]);

  while(!new->car) {
    new = args[++i];
    if(i >= arg_count) {
      return nil;
    }
  }
  
  Obj *last = new;

  for(i++; i < arg_count; i++) {
    //printf("Will concat %s\n", obj_to_string(args[i])->s);
    if(!last->cdr) {
      // continue
    } else {
      while(last->cdr->cdr) {
	last = last->cdr;
      }
      Obj *o = args[i];
      if(o->car) {
	last->cdr = obj_copy(o);
      }
    }
  }
  return new;
}

Obj *p_nth(Obj** args, int arg_count) {
  if(arg_count != 2) { printf("Wrong argument count to 'nth'\n"); return nil; }
  if(args[0]->tag != 'C') { printf("'nth' requires arg 0 to be a list\n"); return nil; }
  if(args[1]->tag != 'I') { printf("'nth' requires arg 1 to be an integer\n"); return nil; }
  int i = 0;
  int n = args[1]->i;
  Obj *p = args[0];
  while(p && p->car) {
    if(i == n) {
      return p->car;
    }
    p = p->cdr;
    i++;
  }
  printf("Index %d out of bounds in %s\n", n, obj_to_string(args[0])->s);
  return nil;
}

Obj *p_count(Obj** args, int arg_count) {
  if(arg_count != 1) { printf("Wrong argument count to 'count'\n"); return nil; }
  if(args[0]->tag != 'C') { printf("'count' requires arg 0 to be a list: %s\n", obj_to_string(args[0])->s); return nil; }
  int i = 0;
  Obj *p = args[0];
  while(p && p->car) {
    p = p->cdr;
    i++;
  }
  return obj_new_int(i);
}

bool is_callable(Obj *obj) {
  return obj->tag == 'P' || obj->tag != 'L' || obj->tag != 'F';
}

Obj *p_map(Obj** args, int arg_count) {
  if(arg_count != 2) { printf("Wrong argument count to 'map'\n"); return nil; }
  if(!is_callable(args[0])) { printf("'map' requires arg 0 to be a function or lambda: %s\n", obj_to_string(args[0])->s); return nil; }
  if(args[1]->tag != 'C') { printf("'map' requires arg 1 to be a list\n"); return nil; }
  Obj *f = args[0];
  Obj *p = args[1];
  Obj *list = obj_new_cons(NULL, NULL);
  Obj *prev = list; 
  while(p && p->car) {
    Obj *arg[1] = { p->car };
    apply(f, arg, 1);
    prev->car = stack_pop();
    Obj *new = obj_new_cons(NULL, NULL);
    prev->cdr = new;
    prev = new;
    p = p->cdr;
  }
  return list;
}

Obj *p_map2(Obj** args, int arg_count) {
  if(arg_count != 3) { printf("Wrong argument count to 'map2'\n"); return nil; }
  if(!is_callable(args[0])) { printf("'map2' requires arg 0 to be a function or lambda: %s\n", obj_to_string(args[0])->s); return nil; }
  if(args[1]->tag != 'C') { printf("'map2' requires arg 1 to be a list\n"); return nil; }
  if(args[2]->tag != 'C') { printf("'map2' requires arg 2 to be a list\n"); return nil; }
  Obj *f = args[0];
  Obj *p = args[1];
  Obj *p2 = args[2];
  Obj *list = obj_new_cons(NULL, NULL);
  Obj *prev = list; 
  while(p && p->car && p2 && p2->car) {
    Obj *argz[2] = { p->car, p2->car };
    apply(f, argz, 2);
    prev->car = stack_pop();
    Obj *new = obj_new_cons(NULL, NULL);
    prev->cdr = new;
    prev = new;
    p = p->cdr;
    p2 = p2->cdr;
  }
  return list;
}

Obj *p_keys(Obj** args, int arg_count) {
  if(arg_count != 1) { printf("Wrong argument count to 'keys'\n"); return nil; }
  if(args[0]->tag != 'E') { printf("'keys' requires arg 0 to be a dictionary.\n"); return nil; }
  Obj *p = args[0]->bindings;
  Obj *list = obj_new_cons(NULL, NULL);
  Obj *prev = list; 
  while(p && p->car) {
    Obj *new = obj_new_cons(NULL, NULL);
    prev->car = p->car->car;
    prev->cdr = new;
    prev = new;
    p = p->cdr;
  }
  return list;
}

Obj *p_values(Obj** args, int arg_count) {
  if(arg_count != 1) { printf("Wrong argument count to 'values'\n"); return nil; }
  if(args[0]->tag != 'E') { printf("'values' requires arg 0 to be a dictionary.\n"); return nil; }
  Obj *p = args[0]->bindings;
  Obj *list = obj_new_cons(NULL, NULL);
  Obj *prev = list; 
  while(p && p->car) {
    Obj *new = obj_new_cons(NULL, NULL);
    prev->car = p->car->cdr;
    prev->cdr = new;
    prev = new;
    p = p->cdr;
  }
  return list;
}

Obj *p_signature(Obj** args, int arg_count) {
  if(arg_count != 1) { error = obj_new_string("Wrong argument count to 'signature'"); return nil; }
  if(args[0]->tag != 'F') { error = obj_new_string("'signature' requires arg 0 to be a foreign function."); return nil; }
  Obj *a = obj_copy(args[0]->arg_types);
  Obj *b = args[0]->return_type;
  Obj *sig = obj_list(obj_new_keyword("arrow"), a, b);
  return sig;
}

Obj *p_eval(Obj** args, int arg_count) {
  if(arg_count != 1) { error = obj_new_string("Wrong argument count to 'eval'"); return nil; }
  eval_internal(global_env, args[0]);
  Obj *result = stack_pop();
  return result;
}

Obj *p_null_predicate(Obj** args, int arg_count) {
  if(arg_count != 1) { error = obj_new_string("Wrong argument count to 'null?'"); return nil; }
  if(args[0]->tag != 'Q') { error = obj_new_string("Argument to 'null?' must be void pointer."); return nil; }
  if(args[0]->void_ptr == NULL) {
    return lisp_true;
  } else {
    return lisp_false;
  }
}

Obj *p_and(Obj** args, int arg_count) {
  for(int i = 0; i < arg_count; i++) {
    if(!is_true(args[i])) {
      return lisp_false;
    }
  }
  return lisp_true;
}

Obj *p_filter(Obj** args, int arg_count) {
  if(arg_count != 2) { printf("Wrong argument count to 'filter'\n"); return nil; }
  if(!is_callable(args[0])) { printf("'filter' requires arg 0 to be a function or lambda: %s\n", obj_to_string(args[0])->s); return nil; }
  if(args[1]->tag != 'C') { printf("'filter' requires arg 1 to be a list\n"); return nil; }
  Obj *f = args[0];
  Obj *p = args[1];
  Obj *list = obj_new_cons(NULL, NULL);
  Obj *prev = list; 
  while(p && p->car) {
    Obj *arg[1] = { p->car };
    apply(f, arg, 1);
    Obj *result = stack_pop();
    if(is_true(result)) {
      Obj *new = obj_new_cons(NULL, NULL);
      prev->car = p->car;
      prev->cdr = new;
      prev = new;
    }
    p = p->cdr;
  }
  return list;
}

Obj *p_reduce(Obj** args, int arg_count) {
  if(arg_count != 3) { printf("Wrong argument count to 'reduce'\n"); return nil; }
  if(!is_callable(args[0])) { printf("'reduce' requires arg 0 to be a function or lambda: %s (%c)\n", obj_to_string(args[0])->s, args[0]->tag); return nil; }
  if(args[2]->tag != 'C') { printf("'reduce' requires arg 2 to be a list\n"); return nil; }
  Obj *f = args[0];
  Obj *total = args[1];
  Obj *p = args[2]; 
  while(p && p->car) {
    Obj *args[2] = { total, p->car };
    apply(f, args, 2);
    total = stack_pop();
    p = p->cdr;
  }
  return total;
}

Obj *p_apply(Obj** args, int arg_count) {
  if(arg_count != 2) { printf("'apply' takes two arguments.\n"); return nil; }
  if(args[0]->tag != 'P' && args[0]->tag != 'L') {
    printf("'apply' requires arg 0 to be a function or lambda: %s (%c)\n", obj_to_string(args[0])->s, args[0]->tag);
    return nil;
  }
  if(args[1]->tag != 'C') {
    printf("'apply' requires arg 1 to be a list: %s (%c)\n", obj_to_string(args[0])->s, args[0]->tag);
    return nil;
  }
  Obj *p = args[1];
  int apply_arg_count = 0;
  while(p && p->car) {
    apply_arg_count++;
    p = p->cdr;
  }
  Obj *apply_args[apply_arg_count];
  Obj *q = args[1];
  for(int i = 0; i < apply_arg_count; i++) {
    apply_args[i] = q->car;
    q = q->cdr;
  }
  apply(args[0], apply_args, apply_arg_count);
  return stack_pop();
}

Obj *p_type(Obj** args, int arg_count) {
  if(arg_count != 1) { printf("'type' takes one argument.\n"); return nil; }
  if(args[0]->tag == 'S') {
    return type_string;
  }
  else if(args[0]->tag == 'I') {
    return type_int;
  }
  else if(args[0]->tag == 'V') {
    return type_float;
  }
  else if(args[0]->tag == 'C') {
    return type_list;
  }
  else if(args[0]->tag == 'L') {
    return type_lambda;
  }
  else if(args[0]->tag == 'P') {
    return type_primop;
  }
  else if(args[0]->tag == 'F') {
    return type_foreign;
  }
  else if(args[0]->tag == 'E') {
    return type_env;
  }
  else if(args[0]->tag == 'Y') {
    return type_symbol;
  }
  else if(args[0]->tag == 'K') {
    return type_keyword;
  }
  else if(args[0]->tag == 'Q') {
    return type_ptr;
  }
  else {
    printf("Unknown type tag: %c\n", args[0]->tag);
    //error = obj_new_string("Unknown type.");
    return nil;
  }
}

Obj *p_lt(Obj** args, int arg_count) {
  if(arg_count == 0) { return lisp_true; }
  if(args[0]->tag == 'I') {
    int smallest = args[0]->i;
    for(int i = 1; i < arg_count; i++) {
      if(smallest >= args[i]->i) { return lisp_false; }
      smallest = args[i]->i;
    }
    return lisp_true;
  }
  else if(args[0]->tag == 'V') {
    float smallest = args[0]->f32;
    for(int i = 1; i < arg_count; i++) {
      if(smallest >= args[i]->f32) { return lisp_false; }
      smallest = args[i]->f32;
    }
    return lisp_true;
  }
  else {
    error = obj_new_string("Can't call < on non-numbers.");
    return lisp_false;
  }
}

int current_timestamp() {
    struct timeval te;
    gettimeofday(&te, NULL); // get current time
    long long milliseconds = te.tv_sec * 1000LL + te.tv_usec / 1000; // calculate milliseconds
    return milliseconds;
}

Obj *p_now(Obj** args, int arg_count) {
  if(arg_count != 0) { printf("Wrong argument count to 'now'\n"); return nil; }
  return obj_new_int(current_timestamp());
}

Obj *p_name(Obj** args, int arg_count) {
  if(arg_count != 1) {
    error = obj_new_string("Wrong arg count to 'name'.");
    return nil;
  }
  if(args[0]->tag != 'S' && args[0]->tag != 'Y' && args[0]->tag != 'K') {
    Obj *s = obj_new_string("Argument to 'name' must be string, keyword or symbol: ");
    obj_string_mut_append(s, obj_to_string(args[0])->s);
    error = s;
    return nil;
  }
  return obj_new_string(args[0]->s);
}

Obj *p_symbol(Obj** args, int arg_count) {
  if(arg_count != 1) {
    error = obj_new_string("Wrong arg count to 'symbol'.");
    return nil;
  }
  if(args[0]->tag != 'S') {
    Obj *s = obj_new_string("Argument to 'symbol' must be string: ");
    obj_string_mut_append(s, obj_to_string(args[0])->s);
    error = s;
    return nil;
  }
  return obj_new_symbol(args[0]->s);
}

Obj *p_error(Obj** args, int arg_count) {
  if(arg_count != 1) { error = obj_new_string("Wrong argument count to 'error'\n"); return nil; }
  error = args[0];
  return nil;
}

Obj *p_env(Obj** args, int arg_count) {
  return global_env;
}

Obj *p_load_lisp(Obj** args, int arg_count) {
  Obj *file_string = open_file(args[0]->s);
  if(file_string->tag == 'S') {
    Obj *forms = read_string(global_env, file_string->s);
    Obj *form = forms;
    while(form && form->car) {
      eval_internal(global_env, form->car);
      if(error) { return nil; }
      Obj *result = stack_pop();
      form = form->cdr;
    }
  }
  return nil;
}

Obj *p_load_dylib(Obj** args, int arg_count) {
  char *filename = args[0]->s;
  void *handle = dlopen (filename, RTLD_LAZY);
  if (!handle) {
    set_error_and_return("Failed to open dylib: ", args[0]);
    return nil;
  }
  char *load_error;
  if ((load_error = dlerror()) != NULL)  {
    set_error_and_return("Failed to load dylib: ", args[0]);
  }
  //printf("dlopen %p\n", handle);
  return obj_new_dylib(handle);
}

Obj *p_unload_dylib(Obj** args, int arg_count) {
  //assert_or_return_nil(arg_count == 1, "'unload-dylib' must take one argument.");
  //assert_or_return_nil(args[0]->tag, "'unload-dylib' must take dylib as argument.", args[0]);
  if (!(args[0]->tag == 'D')) {
    set_error_and_return("unload-dylib takes a dylib as argument: ", args[0]);
    return nil;
  }
  void *handle = args[0]->dylib;
  if(!handle) {
    return obj_new_symbol("no handle to unload");
  }
  //printf("dlclose %p\n", handle);
  int result = dlclose(handle);
  if(result) {
    error = obj_new_string(dlerror());
    return nil;
  }
  else {
    return obj_new_symbol("dylib successfully unloaded");
  }
}

Obj *p_read(Obj** args, int arg_count) {
  //assert_or_return_nil(args[0], "No argument to 'read'.", args[0]);
  //assert_or_return_nil(args[0]->tag == 'S', "'read' must take a string as an argument.", args[0]);
  Obj *forms = read_string(global_env, args[0]->s);
  return forms->car;
}

Obj *p_read_many(Obj** args, int arg_count) {
  Obj *forms = read_string(global_env, args[0]->s);
  return forms;
}

Obj *p_code(Obj** args, int arg_count) {  
  if(args[0]->tag != 'L' && args[0]->tag != 'M') {
    set_error_and_return("'code' must take lambda/macro as argument: ", args[0]);
  }
  if(!args[0]->code) {
    set_error_and_return("code of lambda/macro is NULL: ", args[0]);
  }
  Obj *code = args[0]->code;
  return code;
}

ffi_type *lisp_type_to_ffi_type(Obj *type_obj) {
  if(obj_eq(type_obj, type_string)) {
    return &ffi_type_pointer;
  }
  else if(obj_eq(type_obj, type_int)) {
    return &ffi_type_uint;
  }
  else if(obj_eq(type_obj, type_float)) {
    return &ffi_type_float;
  }
  else if(obj_eq(type_obj, type_void)) {
    return &ffi_type_uint;
  }
  else if(obj_eq(type_obj, type_bool)) {
    return &ffi_type_uint;
  }
  else if(type_obj->tag == 'C' && obj_eq(type_obj->car, type_ptr)) {
    return &ffi_type_pointer;
  }
  else {
    error = obj_new_string("Unhandled return type for foreign function.");
    return NULL;
  }
}

char *lispify(char *name) {
  char *s0 = str_replace(name, "_", "-");
  char *s1 = str_replace(s0, "BANG", "!");
  char *s2 = str_replace(s1, "QMARK", "?");
  free(s0);
  free(s1);
  return s2;
}

Obj *register_ffi_internal(char *name, VoidFn funptr, Obj *args, Obj *return_type_obj) {

  if(!funptr) {
    printf("funptr for %s is NULL\n", name);
    return nil;
  }
  
  int arg_count = 0;
  Obj *p = args;
  while(p && p->car) {
    p = p->cdr;
    arg_count++;
  }
  //printf("Arg count for %s: %d\n", name, arg_count);
  
  ffi_type **arg_types_c_array = malloc(sizeof(ffi_type) * (arg_count + 1));

  p = args;
  for(int i = 0; i < arg_count; i++) {
    ffi_type *arg_type = lisp_type_to_ffi_type(p->car);
    if(!arg_type) {
      char buffer[512];
      snprintf(buffer, 512, "Arg %d for function %s has invalid type: %s\n", i, name, obj_to_string(p->car)->s);
      error = obj_new_string(strdup(buffer));
      return nil;
    }
    arg_types_c_array[i] = arg_type;
    p = p->cdr;
  }
  arg_types_c_array[arg_count] = NULL; // ends with a NULL so we don't need to store arg_count

  ffi_type *return_type = lisp_type_to_ffi_type(return_type_obj);

  if(!return_type) {
    return nil;
  }

  ffi_cif *cif = malloc(sizeof(ffi_cif));
  int init_result = ffi_prep_cif(cif,
				 FFI_DEFAULT_ABI,
				 arg_count,
				 return_type,
				 arg_types_c_array);
  
  if (init_result != FFI_OK) {
    printf("Registration of foreign function %s failed.\n", name);
    return nil;
  }

  //printf("Registration of '%s' OK.\n", name);
  
  Obj *ffi = obj_new_ffi(cif, funptr, args, return_type_obj);

  char *lispified_name = lispify(name);
  //printf("Registering %s\n", lispified_name);
  
  global_env_extend(obj_new_symbol(lispified_name), ffi);

  return ffi;
}

Obj *register_ffi_variable_internal(char *name, void *varptr, Obj *var_type_obj) {

  if(!varptr) {
    printf("varptr for %s is NULL\n", name);
    return nil;
  }
  
  Obj *new_variable_value;

  if(obj_eq(var_type_obj, type_int)) {
    int *i = varptr;
    new_variable_value = obj_new_int(*i);
  }
  else {
    error = obj_new_string("Invalid variable type.");
    return nil;
  }

  char *lispified_name = lispify(name);
  //printf("Registering variable %s\n", lispified_name);  
  global_env_extend(obj_new_symbol(lispified_name), new_variable_value);

  return new_variable_value;
}


#define define(name, value) env_extend(global_env, obj_new_symbol(name), value);
#define register_primop(name, primop) env_extend(global_env, obj_new_symbol(name), obj_new_primop(primop));

// (register <dylib> <function-name> <arg-types> <return-type>)
Obj *p_register(Obj** args, int arg_count) {
  if(arg_count != 4 || args[0]->tag != 'D' || args[1]->tag != 'S' || args[2]->tag != 'C') {
    printf("Args to register must be: (handle, function-name, argument-types, return-type)");
    printf("Arg count: %d\n", arg_count);
    printf("Args %c %c %c %c\n", args[0]->tag, args[1]->tag, args[2]->tag, args[3]->tag);
    return nil;
  }
  void *handle = args[0]->dylib;
  char *name = args[1]->s;
  
  VoidFn f = dlsym(handle, name);

  if(!f) {
    printf("Failed to load dynamic C function with name '%s' from %s\n", name, obj_to_string(args[0])->s);
    return nil;
  }
  
  return register_ffi_internal(name, f, args[2], args[3]);
}

Obj *p_register_variable(Obj** args, int arg_count) {
  if(arg_count != 3 || args[0]->tag != 'D' || args[1]->tag != 'S') {
    printf("Args to register-variable must be: (handle, variable-name, type)");
    printf("Arg count: %d\n", arg_count);
    printf("Args %c %c %c\n", args[0]->tag, args[1]->tag, args[2]->tag);
    return nil;
  }
  
  void *handle = args[0]->dylib;
  char *name = args[1]->s;
  
  void *variable = dlsym(handle, name);

  if(!variable) {
    printf("Failed to load dynamic C variable with name '%s' from %s\n", name, obj_to_string(args[0])->s);
    return nil;
  }
  
  return register_ffi_variable_internal(name, variable, args[2]);
}

Obj *p_register_builtin(Obj** args, int arg_count) {
  if(arg_count != 3 || args[0]->tag != 'S' || args[1]->tag != 'C') {
    printf("Args to register-builtin must be: (function-name, argument-types, return-type)\n");
    printf("Arg count: %d\n", arg_count);
    printf("Args %c %c %c\n", args[0]->tag, args[1]->tag, args[2]->tag);
    return nil;
  }
  char *name = args[0]->s;
  VoidFn f = dlsym(RTLD_DEFAULT, name);

  if(!f) {
    printf("Failed to load dynamic C function with name '%s' from executable.\n", name);
    return nil;
  }
  
  return register_ffi_internal(name, f, args[1], args[2]);
}

void env_new_global() {
  global_env = obj_new_environment(NULL);

  nil = obj_new_cons(NULL, NULL);
  define("nil", nil);

  lisp_false = obj_new_symbol("false");
  define("false", lisp_false);
  
  lisp_true = obj_new_symbol("true");
  define("true", lisp_true);

  lisp_quote = obj_new_symbol("quote");
  define("quote", lisp_quote);

  ampersand = obj_new_symbol("&");
  define("&", ampersand);

  lisp_NULL = obj_new_ptr(NULL);
  define("NULL", lisp_NULL);

  type_int = obj_new_keyword("int");
  define("type-int", type_int); // without this it will get GC'd!

  type_bool = obj_new_keyword("bool");
  define("type-bool", type_bool);

  type_float = obj_new_keyword("float");
  define("type-float", type_float);
  
  type_string = obj_new_keyword("string");
  define("type-string", type_string);

  type_symbol = obj_new_keyword("symbol");
  define("type-symbol", type_symbol);
  
  type_keyword = obj_new_keyword("keyword");
  define("type-keyword", type_keyword);
  
  type_foreign = obj_new_keyword("foreign");
  define("type-foreign", type_foreign);
  
  type_primop = obj_new_keyword("primop");
  define("type-primop", type_primop);
  
  type_env = obj_new_keyword("env");
  define("type-env", type_env);
  
  type_macro = obj_new_keyword("macro");
  define("type-macro", type_macro);

  type_lambda = obj_new_keyword("lambda");
  define("type-lambda", type_lambda);
  
  type_list = obj_new_keyword("list");
  define("type-list", type_list);

  type_void = obj_new_keyword("void");
  define("type-void", type_void);

  type_ptr = obj_new_keyword("ptr");
  define("type-ptr", type_ptr);

  register_primop("open", p_open_file);
  register_primop("save", p_save_file);
  register_primop("+", p_add);
  register_primop("-", p_sub);
  register_primop("*", p_mul);
  register_primop("/", p_div);
  register_primop("mod", p_mod);
  register_primop("=", p_eq);
  register_primop("list", p_list);
  register_primop("str", p_str);
  register_primop("str-append!", p_str_append_bang);
  register_primop("str-replace", p_str_replace);
  register_primop("register", p_register);
  register_primop("register-variable", p_register_variable);
  register_primop("register-builtin", p_register_builtin);
  register_primop("print", p_print);
  register_primop("println", p_println);
  register_primop("prn", p_prn);
  register_primop("system", p_system);
  register_primop("get", p_get);
  register_primop("get-maybe", p_get_maybe);
  register_primop("dict-set!", p_dict_set_bang);
  register_primop("dict-remove!", p_dict_remove_bang);
  register_primop("first", p_first);
  register_primop("rest", p_rest);
  register_primop("cons", p_cons);
  register_primop("cons-last", p_cons_last);
  register_primop("concat", p_concat);
  register_primop("nth", p_nth);
  register_primop("count", p_count);
  register_primop("map", p_map);
  register_primop("map2", p_map2);
  register_primop("filter", p_filter);
  register_primop("reduce", p_reduce);
  register_primop("apply", p_apply);
  register_primop("type", p_type);
  register_primop("<", p_lt);
  register_primop("env", p_env);
  register_primop("load-lisp", p_load_lisp);
  register_primop("load-dylib", p_load_dylib);
  register_primop("unload-dylib", p_unload_dylib);
  register_primop("read", p_read);
  register_primop("read-many", p_read_many);
  register_primop("code", p_code);
  register_primop("copy", p_copy);
  register_primop("now", p_now);
  register_primop("name", p_name);
  register_primop("symbol", p_symbol);
  register_primop("error", p_error);
  register_primop("keys", p_keys);
  register_primop("values", p_values);
  register_primop("signature", p_signature);
  register_primop("eval", p_eval);
  register_primop("and", p_and);
  //register_primop("nullp", p_null_predicate);
  
  Obj *abs_args = obj_list(type_int);
  register_ffi_internal("abs", (VoidFn)abs, abs_args, type_int);

  Obj *exit_args = obj_list(type_int);
  register_ffi_internal("exit", (VoidFn)exit, exit_args, type_void);

  Obj *getenv_args = obj_list(type_string);
  register_ffi_internal("getenv", (VoidFn)getenv, getenv_args, type_string);
  
  //printf("Global env: %s\n", obj_to_string(env)->s);
}


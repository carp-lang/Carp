#include "obj.h"
#include "obj_string.h"
#include "env.h"
#include "bytecode.h"
#include "eval.h"

#define LOG_ALLOCS 0

Obj *obj_latest = NULL;
int obj_total = 0;

Obj *obj_new(char tag) {
  Obj *o = malloc(sizeof(Obj));
  o->prev = obj_latest;
  o->alive = false;
  o->given_to_ffi = false;
  o->tag = tag;
  o->meta = NULL;
  o->hash = 0;
  obj_latest = o;
  obj_total++;
  if(LOG_ALLOCS) {
    printf("alloc %p %c\n", o, o->tag);
  }
  return o;
}

Obj *obj_new_cons(Obj *car, Obj *cdr) {
  Obj *o = obj_new('C');
  o->car = car;
  o->cdr = cdr;
  return o;
}

Obj *obj_new_int(int i) {
  Obj *o = obj_new('I');
  o->i = i;
  return o;
}

Obj *obj_new_float(float x) {
  Obj *o = obj_new('V');
  o->f32 = x;
  return o;
}

Obj *obj_new_double(double x) {
  Obj *o = obj_new('W');
  o->f64 = x;
  return o;
}

Obj *obj_new_string(char *s) {
  assert(s);
  Obj *o = obj_new('S');
  o->s = strdup(s);
  return o;
}

Obj *obj_new_symbol(char *s) {
  Obj *o = obj_new('Y');
  o->s = strdup(s);

  if(strcmp(s, "do") == 0) {
    o->dispatch_index = 1;
  }
  else if(strcmp(s, "let") == 0) {
    o->dispatch_index = 2;
  }
  else if(strcmp(s, "not") == 0) {
    o->dispatch_index = 3;
  }
  else if(strcmp(s, "or") == 0) {
    o->dispatch_index = 4;
  }
  else if(strcmp(s, "and") == 0) {
    o->dispatch_index = 5;
  }
  else if(strcmp(s, "quote") == 0) {
    o->dispatch_index = 6;
  }
  else if(strcmp(s, "while") == 0) {
    o->dispatch_index = 7;
  }
  else if(strcmp(s, "if") == 0) {
    o->dispatch_index = 8;
  }
  else if(strcmp(s, "match") == 0) {
    o->dispatch_index = 9;
  }
  else if(strcmp(s, "reset!") == 0) {
    o->dispatch_index = 10;
  }
  else if(strcmp(s, "fn") == 0) {
    o->dispatch_index = 11;
  }
  else if(strcmp(s, "macro") == 0) {
    o->dispatch_index = 12;
  }
  else if(strcmp(s, "def") == 0) {
    o->dispatch_index = 13;
  }
  else if(strcmp(s, "def?") == 0) {
    o->dispatch_index = 14;
  }
  else if(strcmp(s, "ref") == 0) {
    o->dispatch_index = 15;
  }
  else if(strcmp(s, "catch-error") == 0) {
    o->dispatch_index = 16;
  }
  else {
    o->dispatch_index = 0;
  }

  return o;
}

Obj *obj_new_keyword(char *s) {
  Obj *o = obj_new('K');
  o->s = strdup(s);
  return o;
}

Obj *obj_new_primop(Primop p) {
  Obj *o = obj_new('P');
  o->primop = (struct Obj * (*)(struct Process *, struct Obj **, int))p;
  return o;
}

Obj *obj_new_dylib(void *dylib) {
  Obj *o = obj_new('D');
  o->primop = dylib;
  return o;
}

Obj *obj_new_ptr(void *ptr) {
  Obj *o = obj_new('Q');
  o->void_ptr = ptr;
  return o;
}

Obj *obj_new_ptr_to_global(void *ptr) {
  Obj *o = obj_new('R');
  o->void_ptr = ptr;
  return o;
}

Obj *obj_new_ffi(const char *name, ffi_cif *cif, VoidFn funptr, Obj *arg_types, Obj *return_type_obj) {
  assert(cif);
  assert(name);
  assert(arg_types);
  assert(arg_types->tag == 'C');
  assert(return_type_obj);
  Obj *o = obj_new('F');
  o->cif = cif;
  o->name = strdup(name);
  o->funptr = funptr;
  o->arg_types = arg_types;
  o->return_type = return_type_obj;
  return o;
}

Obj *obj_new_lambda(Obj *params, Obj *body, Obj *env, Obj *code) {
  assert(params);
  assert(params->tag == 'C' || params->tag == 'A');
  assert(body);
  assert(env);
  assert(env->tag == 'E');
  assert(code);
  Obj *o = obj_new('L');
  o->params = params;
  o->body = body;
  o->env = env;
  o->code = code;
  return o;
}

Obj *obj_new_macro(Obj *params, Obj *body, Obj *env, Obj *code) {
  assert(params);
  assert(params->tag == 'C' || params->tag == 'A');
  assert(body);
  assert(env);
  assert(env->tag == 'E');
  Obj *o = obj_new('M');
  o->params = params;
  o->body = body;
  o->env = env;
  o->code = code;
  return o;
}

Obj *obj_new_environment(Obj *parent) {
  //obj_print_cout(parent);
  Obj *o = obj_new('E');
  o->parent = parent;
  o->bindings = NULL;
  return o;
}

Obj *obj_new_char(char character) {
  Obj *o = obj_new('T');
  o->character = character;
  return o;
}

Obj *obj_new_array(int count) {
  Obj *o = obj_new('A');
  o->array = calloc(sizeof(Obj *), count);
  o->count = count;
  return o;
}

Obj *obj_new_bool(bool b) {
  Obj *o = obj_new('B');
  o->boolean = b;
  return o;
}

Obj *obj_new_bytecode(char *bytecode) {
  Obj *o = obj_new('X');
  o->bytecode = bytecode;
  o->bytecode_literals = obj_new_array(0);
  return o;
}

Obj *obj_copy(Process *process, Obj *o) {
  assert(o);
  if(o->tag == 'C') {
    //printf("Making a copy of the list: %s\n", obj_to_string(o)->s);
    Obj *list = obj_new_cons(NULL, NULL);
    Obj *prev = list;
    Obj *p = o;
    while(p && p->car) {
      Obj *new = obj_new_cons(NULL, NULL);
      shadow_stack_push(process, new);
      prev->car = obj_copy(process, p->car);
      shadow_stack_pop(process);
      if(p->cdr) {
        prev->cdr = obj_copy(process, p->cdr);
        return list; // early break when copying dotted pairs! TODO: is this case always selected?!
      }
      else {
        prev->cdr = obj_new_cons(NULL, NULL);
        prev = new;
        p = p->cdr;
      }
    }
    return list;
  }
  else if(o->tag == 'A') {
    Obj *copy = obj_new_array(o->count);
    shadow_stack_push(process, copy);
    for(int i = 0; i < o->count; i++) {
      copy->array[i] = obj_copy(process, o->array[i]);
    }
    shadow_stack_pop(process); // copy
    return copy;
  }
  else if(o->tag == 'E') {
    //printf("Making a copy of the env: %s\n", obj_to_string(o)->s);
    Obj *new_env = obj_new_environment(NULL);
    shadow_stack_push(process, new_env);
    new_env->bindings = obj_copy(process, o->bindings);
    shadow_stack_pop(process);
    return new_env;
  }
  else if(o->tag == 'Q' || o->tag == 'R') {
    Obj *type_meta = env_lookup(process, o->meta, obj_new_keyword("type"));
    if(type_meta) {
      //printf("COPY type_meta: %s\n", STR(type_meta));

      shadow_stack_push(process, o);

      Obj *reffed_arg_type = obj_list(obj_new_keyword("ref"), type_meta);
      Obj *args_type = obj_list(reffed_arg_type);
      Obj *signature = obj_list(obj_new_keyword("fn"), args_type, type_meta);
      Obj *quoted_sig = obj_list(lisp_quote, signature);

      shadow_stack_push(process, quoted_sig);

      // Figure out the name
      Obj *generic_name_result = generic_name(process, "copy", quoted_sig);
      if(eval_error) {
        return NULL;
      }
      shadow_stack_push(process, generic_name_result);
      //printf("generic_name_result: %s\n", STR(generic_name_result));

      //printf("Will bake 'copy' with quoted signature: %s\n", STR(quoted_sig));

      // Bake
      bake_generic_primop_auto(process, "copy", quoted_sig);
      if(eval_error) {
        return NULL;
      }
      else {
        //printf("Baked copying function: %s\n", generic_name_result->s);
      }

      // Call
      char *s = obj_to_string_not_prn(process, generic_name_result)->s;
      Obj *call_to_copy = obj_list(obj_new_symbol(s), o);
      shadow_stack_push(process, call_to_copy);

      //printf("call_to_copy: %s\n", STR(call_to_copy));

      Obj *copy_result = NULL;
      if(BYTECODE_EVAL) {
        copy_result = bytecode_sub_eval_form(process, process->global_env, call_to_copy);
      }
      else {
        copy_result = eval(process, process->global_env, call_to_copy);
        //printf("copy_result: %s with tag %c\n", STR(copy_result), copy_result->tag);
      }

      shadow_stack_push(process, copy_result);

      if(eval_error) {
        printf("Error when calling 'copy' function for void ptr of type '%s':\n", STR(type_meta));
        printf("%s\n", obj_to_string(process, eval_error)->s);
        return NULL;
      }

      Obj *pop1 = shadow_stack_pop(process);
      assert(pop1 == copy_result);

      Obj *pop2 = shadow_stack_pop(process);
      assert(pop2 == call_to_copy);

      Obj *pop3 = shadow_stack_pop(process); // generic_name_result
      assert(pop3 == generic_name_result);

      Obj *pop4 = shadow_stack_pop(process); // quoted_sig
      assert(pop4 == quoted_sig);

      Obj *pop5 = shadow_stack_pop(process); // o
      assert(pop5 == o);

      return copy_result;
    }
    else {
      // shallow copy
      printf("COPY no type_meta\n");
      return obj_new_ptr(o->void_ptr);
    }
  }
  else if(o->tag == 'I') {
    return obj_new_int(o->i);
  }
  else if(o->tag == 'V') {
    return obj_new_float(o->f32);
  }
  else if(o->tag == 'W') {
    return obj_new_float(o->f64);
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
    return obj_new_primop((Primop)o->primop);
  }
  else if(o->tag == 'D') {
    return obj_new_dylib(o->dylib);
  }
  else if(o->tag == 'F') {
    Obj *arg_types_copy = obj_copy(process, o->arg_types);
    return obj_new_ffi(o->name, o->cif, o->funptr, arg_types_copy, obj_copy(process, o->return_type));
  }
  else if(o->tag == 'L') {
    return o;
  }
  else if(o->tag == 'M') {
    return o;
  }
  else if(o->tag == 'T') {
    return obj_new_char(o->character);
  }
  else if(o->tag == 'B') {
    return obj_new_bool(o->boolean);
  }
  else if(o->tag == 'X') {
    Obj *copy = obj_new_bytecode(strdup(o->bytecode));
    shadow_stack_push(process, copy);
    copy->bytecode_literals = obj_copy(process, o->bytecode_literals);
    shadow_stack_pop(process);
    return copy;
  }
  else {
    printf("obj_copy() can't handle type tag %c (%d).\n", o->tag, o->tag);
    return NULL;
    assert(false);
  }
}

int string_to_hash(char *str) {
  unsigned long hash = 5381;
  int c;
  while((c = *str++)) {
    hash = ((hash << 5) + hash) + c; /* hash * 33 + c */
  }
  if(hash == 0) {
    hash = 1; // hash 0 means no hash
  }
  return hash;
}

int obj_hash(Process *process, Obj *o) {
  assert(o);

  shadow_stack_push(process, o);
  int hash = 123456789;

  if(o->tag == 'C') {
    Obj *p = o;
    int h = 1234;
    while(p && p->car) {
      h += obj_hash(process, p->car);
      if(p->cdr && p->cdr->tag != 'C') {
        // dotted pair
        h += obj_hash(process, p->cdr);
        break;
      }
      else {
        // normal list
        p = p->cdr;
      }
    }
    hash = h;
  }
  else if(o->tag == 'A') {
    int h = 5381;
    for(int i = 0; i < o->count; i++) {
      h = ((h << 5) + h) + obj_hash(process, o->array[i]);
    }
    hash = h;
  }
  else if(o->tag == 'E') {
    int h = o->bindings ? obj_hash(process, o->bindings) : 0;
    hash = h + 666;
  }
  else if(o->tag == 'Q') {
    hash = (int)o->void_ptr;
  }
  else if(o->tag == 'I') {
    hash = o->i;
  }
  else if(o->tag == 'V') {
    hash = (int)o->f32;
  }
  else if(o->tag == 'W') {
    hash = (int)o->f64;
  }
  else if(o->tag == 'S') {
    hash = string_to_hash(o->s);
  }
  else if(o->tag == 'Y') {
    hash = string_to_hash(o->s);
  }
  else if(o->tag == 'K') {
    hash = string_to_hash(o->s);
  }
  else if(o->tag == 'P') {
    hash = (int)o->primop;
  }
  else if(o->tag == 'D') {
    hash = (int)o->dylib;
  }
  else if(o->tag == 'F') {
    hash = (int)o->funptr;
  }
  else if(o->tag == 'L') {
    // ???
  }
  else if(o->tag == 'M') {
    // ???
  }
  else if(o->tag == 'T') {
    hash = (int)o->character;
  }
  else if(o->tag == 'B') {
    hash = o->boolean ? 29843 : 42391;
  }
  else if(o->tag == 'X') {
    // ???
  }
  else {
    printf("obj_hash() can't handle type tag %c (%d).\n", o->tag, o->tag);
    return 0;
    assert(false);
  }

  shadow_stack_pop(process); // o

  //printf("hash for %s is %d\n", obj_to_string(process, o)->s, hash->i);

  return hash;
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

bool is_true(Obj *o) {
  //printf("is_true? %s\n", obj_to_string(o)->s);
  if(o->tag == 'B' && !o->boolean) {
    return false;
  }
  else {
    return true;
  }
}

void obj_print_cout(Obj *o) {
  if(!o) {
    printf("NULL");
  }
  else if(o->tag == 'C') {
    printf("(");
    Obj *p = o;
    while(p && p->car && p->tag == 'C') {
      obj_print_cout(p->car);
      if(p->cdr && p->cdr->tag == 'C' && p->cdr->cdr) {
        printf(" ");
      }
      p = p->cdr;
    }
    printf(")");
  }
  else if(o->tag == 'A') {
    printf("[");
    for(int i = 0; i < o->count; i++) {
      obj_print_cout(o->array[i]);
      if(i < o->count - 1) {
        printf(" ");
      }
    }
    printf("]");
  }
  else if(o->tag == 'B') {
    printf("%s", o->boolean ? "true" : "false");
  }
  else if(o->tag == 'X') {
    printf("(Bytecode %s)", o->bytecode);
  }
  else if(o->tag == 'E') {
    printf("{ ... }");
  }
  else if(o->tag == 'Q') {
    printf("%p", o->void_ptr);
  }
  else if(o->tag == 'I') {
    printf("%d", o->i);
  }
  else if(o->tag == 'V') {
    printf("%f", o->f32);
  }
  else if(o->tag == 'W') {
    printf("%f", o->f64);
  }
  else if(o->tag == 'S') {
    printf("\"%s\"", o->s);
  }
  else if(o->tag == 'Y') {
    printf("%s", o->s);
  }
  else if(o->tag == 'K') {
    printf(":%s", o->s);
  }
  else if(o->tag == 'P') {
    printf("<primop:%p>", o->primop);
  }
  else if(o->tag == 'D') {
    printf("<dylib:%p>", o->dylib);
  }
  else if(o->tag == 'F') {
    printf("<foreign>");
  }
  else if(o->tag == 'L') {
    printf("(fn ");
    obj_print_cout(o->params);
    printf(" ");
    obj_print_cout(o->body);
    printf(")");
  }
  else if(o->tag == 'M') {
    printf("%p", o);
  }
  else {
    printf("obj_print_cout() can't handle type tag %c (%d).\n", o->tag, o->tag);
    assert(false);
  }
}

void obj_set_line_info(Process *process, Obj *o, int line, int pos, Obj *filename) {
  if(!o->meta) {
    o->meta = obj_new_environment(NULL);
  }
  env_assoc(process, o->meta, obj_new_keyword("line"), obj_new_int(line));
  env_assoc(process, o->meta, obj_new_keyword("pos"), obj_new_int(pos));
  env_assoc(process, o->meta, obj_new_keyword("file"), filename);
}

void obj_copy_meta(Process *process, Obj *to, Obj *from) {
  if(from->meta) {
    to->meta = obj_copy(process, from->meta);
  }
}

bool obj_eq(Process *process, Obj *a, Obj *b) {
  //printf("Comparing %s with %s.\n", obj_to_string(process, a)->s, obj_to_string(process, b)->s);

  if(a == b) {
    return true;
  }
  else if(a == NULL || b == NULL) {
    return false;
  }
  else if(a->tag != b->tag) {
    return false;
  }

  if(a->hash != 0 && b->hash != 0) {
    if(a->hash != b->hash) {
      /* Obj *a_str = obj_to_string(process, a); */
      /* shadow_stack_push(process, a_str); */
      /* Obj *b_str = obj_to_string(process, b); */
      /* shadow_stack_push(process, b_str); */
      /* printf("Hash of %s and %s are not equal: %d vs %d\n", a_str->s, b_str->s, a->hash, b->hash); */
      /* shadow_stack_pop(process); */
      /* shadow_stack_pop(process); */
      return false;
    }
  }

  if(a->tag == 'B') {
    return a->boolean == b->boolean;
  }
  else if(a->tag == 'S' || a->tag == 'Y' || a->tag == 'K') {
    return (strcmp(a->s, b->s) == 0);
  }
  else if(a->tag == 'T') {
    return a->character == b->character;
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
  else if(a->tag == 'X') {
    return a == b;
  }
  else if(a->tag == 'D') {
    return a->dylib == b->dylib;
  }

  if(a->tag == 'C') {
    Obj *pa = a;
    Obj *pb = b;
    while(1) {
      if(obj_eq(process, pa->car, pb->car)) {
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
  else if(a->tag == 'A') {
    if(a->count != b->count) {
      return false;
    }
    else {
      for(int i = 0; i < a->count; i++) {
        if(!obj_eq(process, a->array[i], b->array[i])) {
          return false;
        }
      }
      return true;
    }
  }
  else if(a->tag == 'E') {

    /* if(!a->meta) { */
    /*   printf("dict is missing meta: %s\n", obj_to_string(process, a)->s); */
    /* } */
    /* if(!b->meta) { */
    /*   printf("dict is missing meta: %s\n", obj_to_string(process, b)->s); */
    /* } */

    if(!obj_eq(process, a->parent, b->parent)) {
      return false;
    }

    {
      Obj *pa = a->bindings;
      while(pa && pa->cdr) {
        Obj *pair = pa->car;
        //printf("Will lookup %s\n", obj_to_string(process, pair->car)->s);
        Obj *binding = env_lookup_binding(process, b, pair->car);
        if(binding) {
          //printf("Found binding: %s\n", obj_to_string(process, binding)->s);
          bool eq = obj_eq(process, pair->cdr, binding->cdr);
          if(!binding->car) {
            //printf("binding->car was NULL\n");
            return false;
          }
          else if(!eq) {
            //printf("%s != %s\n", obj_to_string(process, pair->cdr)->s, obj_to_string(process, binding->cdr)->s);
            return false;
          }
        }
        else {
          return false;
        }
        pa = pa->cdr;
      }
    }

    {
      Obj *pb = b->bindings;
      while(pb && pb->cdr) {
        Obj *pair = pb->car;
        //printf("Will lookup %s\n", obj_to_string(process, pair->car)->s);
        Obj *binding = env_lookup_binding(process, a, pair->car);
        if(binding) {
          //printf("Found binding: %s\n", obj_to_string(process, binding)->s);
          bool eq = obj_eq(process, pair->cdr, binding->cdr);
          if(!binding->car) {
            //printf("binding->car was NULL\n");
            return false;
          }
          else if(!eq) {
            //printf("%s != %s\n", obj_to_string(process, pair->cdr)->s, obj_to_string(process, binding->cdr)->s);
            return false;
          }
        }
        else {
          return false;
        }
        pb = pb->cdr;
      }
    }

    return true;
  }
  else {
    char buffer[512];
    snprintf(buffer, 512, "Can't compare %s with %s.\n", obj_to_string(process, a)->s, obj_to_string(process, b)->s);
    eval_error = obj_new_string(strdup(buffer));
    return false;
  }
}

Obj *generic_name(Process *process, char *function_name, Obj *quoted_sig) {
  Obj *call_to_generic_name = obj_list(obj_new_symbol("generic-name"), obj_new_string(function_name), quoted_sig);
  shadow_stack_push(process, call_to_generic_name);
  Obj *generic_name_result = NULL;

  if(BYTECODE_EVAL) {
    generic_name_result = bytecode_sub_eval_form(process, process->global_env, call_to_generic_name);
  }
  else {
    generic_name_result = eval(process, process->global_env, call_to_generic_name);
  }

  shadow_stack_push(process, generic_name_result);

  if(eval_error) {
    printf("Error when calling 'generic-name':\n");
    printf("%s\n", obj_to_string(process, eval_error)->s);
    return NULL;
  }
  else {
    //printf("Generic name: %s\n", obj_to_string_not_prn(process, generic_name_result)->s);
  }

  Obj *pop1 = shadow_stack_pop(process);
  assert(pop1 == generic_name_result);

  Obj *pop2 = shadow_stack_pop(process);
  assert(pop2 == call_to_generic_name);

  return generic_name_result;
}

void bake_generic_primop_auto(Process *process, char *function_name, Obj *quoted_sig) {
  Obj *call_to_bake_generic_primop_auto = obj_list(obj_new_symbol("bake-generic-primop-auto"), obj_new_string(function_name), quoted_sig);
  shadow_stack_push(process, call_to_bake_generic_primop_auto);

  if(BYTECODE_EVAL) {
    bytecode_sub_eval_form(process, process->global_env, call_to_bake_generic_primop_auto);
  }
  else {
    //printf("CALL: %s\n", STR(call_to_bake_generic_primop_auto));
    eval(process, process->global_env, call_to_bake_generic_primop_auto);
  }

  if(eval_error) {
    printf("Error when calling bake-generic-primop-auto '%s' from C code: ", function_name);
    printf("%s\n", obj_to_string(process, eval_error)->s);
    function_trace_print(process);
    return;
  }
  else {
    //printf("%s should now exists\n", obj_to_string_not_prn(process, generic_name_result)->s);
  }

  Obj *pop1 = shadow_stack_pop(process);
  assert(pop1 == call_to_bake_generic_primop_auto);
}

#include "obj.h"
#include "obj_string.h"

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

bool is_true(Obj *o) {
  //printf("is_true? %s\n", obj_to_string(o)->s);
  if(o == lisp_false || (o->tag == 'Y' && strcmp(o->s, "false") == 0)) {
    return false;
  }
  else {
    return true;
  }
}


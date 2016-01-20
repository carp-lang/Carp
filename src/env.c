#include "env.h"
#include "eval.h"
#include "obj_string.h"
#include "assertions.h"

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
      //obj_print_cout(paramp);
      set_error("Too many arguments to function: ", function);
    }
    env_extend(calling_env, paramp->car, args[i]);
    paramp = paramp->cdr;
  }
  if(paramp && paramp->cdr) {
    set_error("Too few arguments to function: ", function);
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

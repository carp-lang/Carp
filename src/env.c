#include "env.h"
#include "eval.h"
#include "obj_string.h"
#include "assertions.h"

Obj *env_lookup(Process *process, Obj *env, Obj *symbol) {
  Obj *p = env->bindings;
  while(p && p->car) {
    Obj *pair = p->car;
    if(obj_eq(process, pair->car, symbol)) {
      return pair->cdr;
    }
    else {
      p = p->cdr;
    }
  }
  if(env->parent) {
    return env_lookup(process, env->parent, symbol);
  }
  else {
    return NULL;
  }
}

Obj *env_lookup_binding(Process *process, Obj *env, Obj *symbol) {
  Obj *p = env->bindings;
  while(p && p->car) {
    Obj *pair = p->car;
    if(obj_eq(process, pair->car, symbol)) {
      return pair;
    }
    else {
      p = p->cdr;
    }
  }
  if(env->parent) {
    return env_lookup_binding(process, env->parent, symbol);
  }
  else {
    return nil;
  }
}

Obj *env_extend(Obj *env, Obj *key, Obj *value) {
  assert(env->tag == 'E');
  
  Obj *pair = obj_new_cons(key, value);
  Obj *cons = obj_new_cons(pair, env->bindings);

  env->bindings = cons;

  return pair;
}

void env_extend_with_args(Process *process, Obj *calling_env, Obj *function, int arg_count, Obj **args, bool allow_restargs) {

  // TODO: remove the whole 'C' branch and only allow arrays for parameters
  
  Obj *paramp = function->params;
  if(paramp->tag == 'C') {
    for(int i = 0; i < arg_count; i++) {
      if(allow_restargs && obj_eq(process, paramp->car, dotdotdot)) {
        printf("Found dotdotdot\n");
        if(paramp->cdr->car) {
          int rest_count = arg_count - i;
          printf("Rest count: %d\n", rest_count);
          Obj *rest_array = obj_new_array(rest_count);
          for(int j = 0; j < rest_count; j++) {
            rest_array->array[j] = args[i + j];
          }
          env_extend(calling_env, paramp->cdr->car, rest_array);
          return;
        }
        else {
          printf("No arguments after dotdotdot\n");
          return;
        }
      }
      if(!paramp || !paramp->car) {
          set_error("Too many arguments (C) to function: ", function);
      }
      env_extend(calling_env, paramp->car, args[i]);
      paramp = paramp->cdr;
    }
    if(paramp && paramp->cdr) {
      set_error("Too few arguments to function: ", function);
    }
  }
  else if(paramp->tag == 'A') {

    int i = 0;
    for(; i < arg_count; i++) {
      if(allow_restargs && obj_eq(process, paramp->array[i], dotdotdot)) {
        int rest_count = arg_count - i;
        Obj *rest_list = obj_new_cons(NULL, NULL);
        Obj *last = rest_list;
        for(int j = 0; j < rest_count; j++) {
          Obj *new_element = args[i + j];
          last->car = new_element;
          Obj *new_last = obj_new_cons(NULL, NULL);
          last->cdr = new_last;
          last = new_last;
        }
        env_extend(calling_env, paramp->array[i + 1], rest_list);
        return;
      }
 
      env_extend(calling_env, paramp->array[i], args[i]);
    }

    if(i < paramp->count) {
      if(allow_restargs && obj_eq(process, paramp->array[i], dotdotdot)) {
        env_extend(calling_env, paramp->array[i + 1], obj_new_array(0));
      } else {
        set_error("Too few arguments to function/macro: ", function);
      }
    }

    if(arg_count > paramp->count) {
      set_error("Too many arguments (A) to function/macro: ", function);
    }
    
  }
}


    

void global_env_extend(Process *process, Obj *key, Obj *val) {
  assert(process->global_env);
  Obj *existing_binding = env_lookup_binding(process, process->global_env, key);
  if(existing_binding->car) {
    existing_binding->cdr = val;
  } else {
    env_extend(process->global_env, key, val);
  }
}

Obj *env_assoc(Process *process, Obj *env, Obj *key, Obj *value) {
  Obj *pair = env_lookup_binding(process, env, key);
  if(pair && pair->car && pair->cdr) {
    pair->cdr = value;
  }
  else {
    //printf("Pair not found, will add new key.\n");
    Obj *new_pair = obj_new_cons(key, value);
    Obj *new_cons = obj_new_cons(new_pair, env->bindings);
    env->bindings = new_cons;
  }
  return env;
}

void obj_set_meta(Obj *o, Obj *key, Obj *value) {
  if(!o->meta) {
    o->meta = obj_new_environment(NULL);
  }
  env_extend(o->meta, key, value);
}

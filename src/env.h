#pragma once

#include "obj.h"
#include "process.h"

Obj *env_lookup(Process *process, Obj *env, Obj *symbol);
Obj *env_lookup_binding(Process *process, Obj *env, Obj *symbol);

Obj *env_assoc(Process *process, Obj *env, Obj *key, Obj *value);

Obj *env_extend(Obj *env, Obj *key, Obj *value);
void env_extend_with_args(Process *process, Obj *calling_env, Obj *function, int arg_count, Obj **args, bool allow_restargs);

void global_env_extend(Process *process, Obj *key, Obj *val);

void obj_set_meta(Obj *o, Obj *key, Obj *value);

#pragma once

#include "obj.h"

Obj *env_lookup(Obj *env, Obj *symbol);
Obj *env_lookup_binding(Obj *env, Obj *symbol);

Obj *env_assoc(Obj *env, Obj *key, Obj *value);

Obj *env_extend(Obj *env, Obj *key, Obj *value);
void env_extend_with_args(Obj *calling_env, Obj *function, int arg_count, Obj **args);

void global_env_extend(Obj *key, Obj *val);

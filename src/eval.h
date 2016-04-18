#pragma once

#include "obj.h"
#include "obj_string.h"
#include "constants.h"

#define LOG_GC_POINTS 0

void apply(Process *process, Obj *function, Obj **args, int arg_count);

Obj *eval(Process *process, Obj *env, Obj *form);
void eval_internal(Process *process, Obj *env, Obj *o);
void eval_text(Process *process, Obj *env, char *text, bool print, Obj *filename);


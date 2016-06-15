#pragma once

#include "obj.h"

#define BYTECODE_EVAL 0

Obj *form_to_bytecode(Process *process, Obj *env, Obj *form);
Obj *bytecode_eval_bytecode(Process *process, Obj *bytecodeObj);
Obj *bytecode_eval_bytecode_in_env(Process *process, Obj *bytecodeObj, Obj *env);
Obj *bytecode_eval_form(Process *process, Obj *env, Obj *form);
Obj *bytecode_sub_eval_form(Process *process, Obj *env, Obj *form);
Obj *bytecode_eval_internal(Process *process, Obj *bytecodeObj, int steps, int top_frame);
Obj *bytecode_sub_eval_internal(Process *process, Obj *env, Obj *bytecode_obj);

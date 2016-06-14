#pragma once

#include "obj.h"

#define BYTECODE_EVAL 0

Obj *form_to_bytecode(Process *process, Obj *env, Obj *form);
Obj *bytecode_eval(Process *process, Obj *bytecode, bool restart);
Obj *bytecode_eval_internal(Process *process, Obj *bytecodeObj, int steps, int top_frame);

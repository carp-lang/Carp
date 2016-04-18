#pragma once

#include "obj.h"

Obj *form_to_bytecode(Obj *env, Obj *form);
Obj *bytecode_eval(Process *process, Obj *bytecode);

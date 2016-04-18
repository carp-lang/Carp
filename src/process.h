#pragma once

#include "obj.h"

Process *process_new();

void stack_print(Process *process);
void stack_push(Process *process, Obj *o);
Obj *stack_pop(Process *process);

bool obj_eq(Process *process, Obj *a, Obj *b);

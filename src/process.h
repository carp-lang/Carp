#pragma once

#include "obj.h"

Process *process_new();
Process *process_clone(Process *parent);

void stack_print(Process *process);
void stack_push(Process *process, Obj *o);
Obj *stack_pop(Process *process);

void shadow_stack_push(Process *process, Obj *o);
Obj *shadow_stack_pop(Process *process);

void function_trace_print(Process *process);

bool obj_eq(Process *process, Obj *a, Obj *b);

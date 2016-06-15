#pragma once

#include "obj.h"

Process *process_new();
Process *process_clone(Process *parent);

Process *parallell;

void process_reset(Process *process);
void process_eval(Process *process, Obj *form);
Obj *process_tick(Process *process);

void stack_print(Process *process);
void stack_push(Process *process, Obj *o);
Obj *stack_pop(Process *process);

void shadow_stack_push(Process *process, Obj *o);
Obj *shadow_stack_pop(Process *process);

void function_trace_print(Process *process);

void pop_stacks_to_zero(Process *process);

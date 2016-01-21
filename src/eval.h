#pragma once

#include "obj.h"
#include "obj_string.h"

#define LOG_GC_POINTS 0

#define STACK_SIZE 1024
Obj *stack[STACK_SIZE];
int stack_pos;

Obj *shadow_stack[STACK_SIZE];
int shadow_stack_pos;

void shadow_stack_push(Obj *o);
Obj *shadow_stack_pop();

void function_trace_print();

void stack_push(Obj *o);
Obj *stack_pop();

void apply(Obj *function, Obj **args, int arg_count);

Obj *eval(Obj *env, Obj *form);
void eval_internal(Obj *env, Obj *o);
void eval_text(Obj *env, char *text, bool print);



#pragma once

#include "obj.h"
#include "obj_string.h"

int stack_pos;
#define STACK_SIZE 512
Obj *stack[STACK_SIZE];
int stack_pos;

void function_trace_print();

void stack_push(Obj *o);
Obj *stack_pop();

void apply(Obj *function, Obj **args, int arg_count);
Obj *eval(Obj *env, Obj *form);
void eval_internal(Obj *env, Obj *o);
void eval_text(Obj *env, char *text, bool print);



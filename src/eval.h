#pragma once

#include "obj.h"
#include "obj_string.h"

#define LOG_GC_POINTS 0

#define STACK_SIZE 1024
Obj *stack[STACK_SIZE];
int stack_pos;

#define SHADOW_STACK_SIZE 2048
Obj *shadow_stack[SHADOW_STACK_SIZE];
int shadow_stack_pos;

void shadow_stack_push(Obj *o);
Obj *shadow_stack_pop();

typedef struct {
  Obj *caller;
  Obj *callee;
} StackTraceCallSite;

StackTraceCallSite function_trace[STACK_SIZE];
int function_trace_pos;
void function_trace_print();

void stack_push(Obj *o);
Obj *stack_pop();
void stack_print();

void apply(Obj *function, Obj **args, int arg_count);

Obj *eval(Obj *env, Obj *form);
void eval_internal(Obj *env, Obj *o);
void eval_text(Obj *env, char *text, bool print, Obj *filename);



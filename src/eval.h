#pragma once

#include "obj.h"
#include "obj_string.h"
#include "constants.h"

#define LOG_GC_POINTS 0

#define SHADOW_STACK_SIZE 5000
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

void apply(Process *process, Obj *function, Obj **args, int arg_count);

Obj *eval(Process *process, Obj *env, Obj *form);
void eval_internal(Process *process, Obj *env, Obj *o);
void eval_text(Process *process, Obj *env, char *text, bool print, Obj *filename);


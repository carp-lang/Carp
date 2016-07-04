#pragma once

#include "process.h"

typedef struct {
  Obj *lambda;
  Obj *signature;
  Process *process;
} LambdaAndItsType;

void call_lambda_from_ffi(ffi_cif *cif, void *ret, void *args[], LambdaAndItsType *lambda_and_its_type);
void call_foreign_function(Process *process, Obj *function, Obj **args, int arg_count);

#pragma once

#include "obj.h"

void repl(Obj *env);

void pop_stacks_to_zero();

void env_new_global();
void env_new_global_mini();


#pragma once

#include "obj.h"
#include "eval.h"

void gc(Obj *env, Obj *forms);
void gc_all();

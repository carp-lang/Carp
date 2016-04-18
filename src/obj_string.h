#pragma once

#include "obj.h"
#include "process.h"

void obj_string_mut_append(Obj *string_obj, const char *s2);
Obj *concat_c_strings(char *a, const char *b);

Obj *obj_to_string(Process *process, const Obj *o);
Obj *obj_to_string_not_prn(Process *process, const Obj *o);

void obj_print(Process *process, Obj *o);
void obj_print_not_prn(Process *process, Obj *o);


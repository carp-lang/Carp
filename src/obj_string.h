#pragma once

#include "obj.h"

void obj_string_mut_append(Obj *string_obj, const char *s2);
Obj *concat_c_strings(char *a, const char *b);
Obj *obj_to_string(const Obj *o);
void obj_to_string_internal(Obj *total, const Obj *o, bool prn, int indent);
Obj *obj_to_string(const Obj *o);
Obj *obj_to_string_not_prn(const Obj *o);

void obj_print(Obj *o);
void obj_print_not_prn(Obj *o);


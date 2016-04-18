#pragma once

#include "obj.h"
#include "../shared/types.h"

Obj *primitive_to_obj(Process *process, void *primitive, Obj *return_type);
Obj *primitive_array_to_obj_array(Process *process, Array *carp_array, Obj *inner_type);

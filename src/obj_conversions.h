#pragma once

#include "obj.h"
#include "../shared/types.h"

Obj *primitive_to_obj(void *primitive, Obj *return_type);
Obj *primitive_array_to_obj_array(Array *carp_array, Obj *inner_type);

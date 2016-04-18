#include "obj_array.h"

void obj_array_mut_append(Obj *a, Obj *o) {
  int count = a->count;
  a->array = realloc(a->array, sizeof(Obj*) * (count + 1));
  a->array[count] = o;
  a->count = count + 1;
}

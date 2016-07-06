#include "obj_conversions.h"
#include "assertions.h"
#include "env.h"
#include "obj_string.h"

Obj *primitive_array_to_obj_array(Process *process, Array *carp_array, Obj *inner_type) {

  Obj *new_array = obj_new_array(carp_array->count);

  //printf("Converting primitive array to Obj-array, inner type: %s\n", obj_to_string(inner_type)->s);

  if(obj_eq(process, inner_type, type_int)) {
    int *int_array = carp_array->data;
    for(int i = 0; i < carp_array->count; i++) {
      new_array->array[i] = obj_new_int(int_array[i]);
    }
  }
  else if(obj_eq(process, inner_type, type_float)) {
    float *int_array = carp_array->data;
    for(int i = 0; i < carp_array->count; i++) {
      new_array->array[i] = obj_new_float(int_array[i]);
    }
  }
  else if(obj_eq(process, inner_type, type_double)) {
    double *int_array = carp_array->data;
    for(int i = 0; i < carp_array->count; i++) {
      new_array->array[i] = obj_new_double(int_array[i]);
    }
  }
  else if(obj_eq(process, inner_type, type_bool)) {
    bool *int_array = carp_array->data;
    for(int i = 0; i < carp_array->count; i++) {
      new_array->array[i] = obj_new_bool(int_array[i]);
    }
  }
  else if(obj_eq(process, inner_type, type_char)) {
    char *char_array = carp_array->data;
    for(int i = 0; i < carp_array->count; i++) {
      new_array->array[i] = obj_new_char(char_array[i]);
    }
  }
  else if(obj_eq(process, inner_type, type_string)) {
    char **int_array = carp_array->data;
    for(int i = 0; i < carp_array->count; i++) {
      new_array->array[i] = obj_new_string(int_array[i]);
    }
  }
  else {
    /* eval_error = obj_new_string("Can't convert primitive Array to Obj-array, inner type: "); */
    /* obj_string_mut_append(eval_error, obj_to_string(inner_type)->s); */
    /* return NULL; */
    void **ptr_array = carp_array->data;
    for(int i = 0; i < carp_array->count; i++) {
      new_array->array[i] = primitive_to_obj(process, ptr_array[i], inner_type);
    }
  }

  return new_array;
}

Obj *primitive_to_obj(Process *process, void *primitive, Obj *return_type) {

  //printf("Will turn %s to primitive type.\n", obj_to_string(return_type)->s);

  Obj *obj_result = NULL;
  if(obj_eq(process, return_type, type_string)) {
    //printf("Returning string.\n");
    char *c = primitive;
    if(c == NULL) {
      // TODO: have an error here instead?
      //printf("Return value of type string from ffi function is null.\n");
      obj_result = obj_new_string("");
    }
    else {
      obj_result = obj_new_string(c);
    }
  }
  else if(obj_eq(process, return_type, type_int)) {
    //printf("Returning int.\n");
    ffi_sarg result = (ffi_sarg)primitive;
    obj_result = obj_new_int(result);
  }
  else if(obj_eq(process, return_type, type_bool)) {
    //printf("Returning bool.\n");
    ffi_arg result = (ffi_arg)primitive;
    obj_result = result ? lisp_true : lisp_false;
  }
  else if(obj_eq(process, return_type, type_char)) {
    ffi_sarg result = (ffi_sarg)primitive;
    obj_result = obj_new_char(result);
  }
  else if(obj_eq(process, return_type, type_float)) {
    //printf("Returning float.\n");
    float result = *(float *)&primitive;
    obj_result = obj_new_float(result);
  }
  else if(obj_eq(process, return_type, type_double)) {
    double result = *(double *)&primitive;
    obj_result = obj_new_double(result);
  }
  else if(obj_eq(process, return_type, type_void)) {
    //printf("Returning void.\n");
    //ffi_sarg result = (ffi_sarg)primitive;
    obj_result = nil;
  }
  else if(return_type->tag == 'C' && return_type->car && obj_eq(process, return_type->car, obj_new_keyword("Array")) && return_type->cdr && return_type->cdr->car) {
    //printf("Returning an Array.\n");
    void *result = primitive;
    Obj *inner_type = return_type->cdr->car;
    obj_result = primitive_array_to_obj_array(process, result, inner_type);
    if(!obj_result) {
      return NULL;
    }
    //printf("obj_result = %s\n", obj_to_string(obj_result)->s);
  }
  else {
    //set_error("Returning what? ", function->return_type);
    // Assume it's a user defined type:
    void *result = primitive;
    obj_result = obj_new_ptr(result);
    obj_set_meta(obj_result, obj_new_keyword("type"), return_type);
  }

  assert(obj_result);
  return obj_result;
}

Array *obj_array_to_carp_array(Process *process, Obj *obj_array) {
  Array *carp_array = malloc(sizeof(Array));
  carp_array->count = obj_array->count;

  Obj **oa = obj_array->array;

  if(obj_array->count == 0) {
  }
  else if(oa[0]->tag == 'I') {
    carp_array->data = malloc(sizeof(int) * carp_array->count);
    int *data = carp_array->data;
    for(int i = 0; i < carp_array->count; i++) {
      assert_or_set_error_return_null(oa[i]->tag == 'I', "All elements in array must be integers: ", oa[i]);
      data[i] = oa[i]->i;
    }
  }
  else if(oa[0]->tag == 'V') {
    carp_array->data = malloc(sizeof(int) * carp_array->count);
    int *data = carp_array->data;
    for(int i = 0; i < carp_array->count; i++) {
      assert_or_set_error_return_null(oa[i]->tag == 'V', "All elements in array must be floats: ", oa[i]);
      data[i] = oa[i]->f32;
    }
  }
  else if(oa[0]->tag == 'W') {
    carp_array->data = malloc(sizeof(float) * carp_array->count);
    int *data = carp_array->data;
    for(int i = 0; i < carp_array->count; i++) {
      assert_or_set_error_return_null(oa[i]->tag == 'W', "All elements in array must be doubles: ", oa[i]);
      data[i] = oa[i]->f64;
    }
  }
  else if(oa[0]->tag == 'B') {
    carp_array->data = malloc(sizeof(double) * carp_array->count);
    bool *data = carp_array->data;
    for(int i = 0; i < carp_array->count; i++) {
      assert_or_set_error_return_null(oa[i]->tag == 'B', "All elements in array must be booleans: ", oa[i]);
      data[i] = oa[i]->boolean;
    }
  }
  else if(oa[0]->tag == 'T') {
    carp_array->data = malloc(sizeof(char) * carp_array->count);
    char *data = carp_array->data;
    for(int i = 0; i < carp_array->count; i++) {
      assert_or_set_error_return_null(oa[i]->tag == 'T', "All elements in array must be chars: ", oa[i]);
      data[i] = oa[i]->character;
    }
  }
  else if(oa[0]->tag == 'S') {
    carp_array->data = malloc(sizeof(char *) * carp_array->count);
    char **data = carp_array->data;
    for(int i = 0; i < carp_array->count; i++) {
      assert_or_set_error_return_null(oa[i]->tag == 'S', "All elements in array must be strings: ", oa[i]);
      data[i] = strdup(oa[i]->s); // strdup!
    }
  }
  else if(oa[0]->tag == 'Q') {
    carp_array->data = malloc(sizeof(void *) * carp_array->count);
    void **data = carp_array->data;
    for(int i = 0; i < carp_array->count; i++) {
      assert_or_set_error_return_null(oa[i]->tag == 'Q', "All elements in array must be ptr:s ", oa[i]);
      data[i] = oa[i]->void_ptr;
    }
  }
  else if(oa[0]->tag == 'A') {
    carp_array->data = malloc(sizeof(void *) * carp_array->count);
    Array **data = carp_array->data;
    for(int i = 0; i < carp_array->count; i++) {
      Array *inner_array = obj_array_to_carp_array(process, oa[i]);
      if(eval_error) {
        return NULL;
      }
      data[i] = inner_array;
    }
  }
  else {
    eval_error = obj_new_string("Can't handle this kind of array element as argument: ");
    obj_string_mut_append(eval_error, obj_to_string(process, oa[0])->s);
    //printf("FAIL %s\n", obj_to_string(eval_error)->s);
    return NULL;
  }

  return carp_array;
}

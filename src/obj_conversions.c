#include "obj_conversions.h"
#include "env.h"
#include "obj_string.h"

Obj *primitive_array_to_obj_array(Array *carp_array, Obj *inner_type) {

  Obj *new_array = obj_new_array(carp_array->count);

  //printf("Converting primitive array to Obj-array, inner type: %s\n", obj_to_string(inner_type)->s);
  
  if(obj_eq(inner_type, type_int)) {
    int *int_array = carp_array->data;
    for(int i = 0; i < carp_array->count; i++) {
      new_array->array[i] = obj_new_int(int_array[i]);
    }
  }
  else if(obj_eq(inner_type, type_float)) {
    float *int_array = carp_array->data;
    for(int i = 0; i < carp_array->count; i++) {
      new_array->array[i] = obj_new_float(int_array[i]);
    }
  }
  else if(obj_eq(inner_type, type_double)) {
    double *int_array = carp_array->data;
    for(int i = 0; i < carp_array->count; i++) {
      new_array->array[i] = obj_new_double(int_array[i]);
    }
  }
  else if(obj_eq(inner_type, type_bool)) {
    bool *int_array = carp_array->data;
    for(int i = 0; i < carp_array->count; i++) {
      new_array->array[i] = obj_new_bool(int_array[i]);
    }
  }
  else if(obj_eq(inner_type, type_string)) {
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
      new_array->array[i] = primitive_to_obj(ptr_array[i], inner_type);
    }
  }

  return new_array;
}

Obj *primitive_to_obj(void *primitive, Obj *return_type) {

  //printf("Will turn %s to primitive type.\n", obj_to_string(return_type)->s);
    
  Obj *obj_result = NULL;
  if(obj_eq(return_type, type_string)) {
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
  else if(obj_eq(return_type, type_int)) { 
    //printf("Returning int.\n");
    ffi_sarg result = (ffi_sarg)primitive;
    obj_result = obj_new_int(result);
  }
  else if(obj_eq(return_type, type_bool)) { 
    //printf("Returning bool.\n");
    ffi_arg result = (ffi_arg)primitive;
    obj_result = result ? lisp_true : lisp_false;
  }
  else if(obj_eq(return_type, type_char)) { 
    ffi_sarg result = (ffi_sarg)primitive;
    obj_result = obj_new_char(result);
  }
  else if(obj_eq(return_type, type_float)) { 
    //printf("Returning float.\n");
    float result = *(float*)&primitive;
    obj_result = obj_new_float(result);
  }
  else if(obj_eq(return_type, type_double)) { 
    double result = *(double*)&primitive;
    obj_result = obj_new_double(result);
  }
  else if(obj_eq(return_type, type_void)) { 
    //printf("Returning void.\n");
    //ffi_sarg result = (ffi_sarg)primitive;
    obj_result = nil;
  }
  else if(return_type->tag == 'C'
          && return_type->car
          && obj_eq(return_type->car, obj_new_keyword("Array"))
          && return_type->cdr && return_type->cdr->car
          ) {
    //printf("Returning an Array.\n");
    void *result = primitive;
    Obj *inner_type = return_type->cdr->car;
    obj_result = primitive_array_to_obj_array(result, inner_type);
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

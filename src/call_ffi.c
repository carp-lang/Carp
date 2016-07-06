#include "call_ffi.h"
#include "obj_string.h"
#include "eval.h"
#include "assertions.h"
#include "obj_conversions.h"
#include "primops.h"
#include "../shared/types.h"
#include "env.h"

#define ALLOW_SENDING_LAMBDA_TO_FFI 1
#define LABELED_DISPATCH 0

void call_lambda_from_ffi(ffi_cif *cif, void *ret, void *args[], LambdaAndItsType *lambda_and_its_type) {
  //printf("Calling lambda %s from ffi function!\n", obj_to_string(lambda_and_its_type->lambda)->s);

  int arg_count = cif->nargs;
  //printf("arg count: %d\n", arg_count);

  Obj **obj_args = malloc(sizeof(Obj *) * arg_count);
  //Obj *obj_args[arg_count];
  Obj *lambda_type_signature = lambda_and_its_type->signature; // TODO: shadow stack?!
  Obj *lambda_return_type = lambda_type_signature->cdr->cdr->car;
  Obj *lambda_arg_type_list_p = lambda_type_signature->cdr->car;

  //printf("Lambda signature: %s\n", obj_to_string(lambda_type_signature)->s);

  Process *process = lambda_and_its_type->process;

  for(int i = 0; i < arg_count; i++) {

    Obj *lambda_arg_type_p = lambda_arg_type_list_p->car;

    if(!lambda_arg_type_p) {
      printf("Too many arguments (%d) sent to lambda with signature: %s\n", arg_count, obj_to_string(process, lambda_type_signature)->s);
      eval_error = obj_new_string("Too many args.");
      return;
    }

    // Unwrap ref args
    if(lambda_arg_type_p->tag == 'C' && lambda_arg_type_p->car && lambda_arg_type_p->cdr && lambda_arg_type_p->cdr->car && obj_eq(process, lambda_arg_type_p->car, obj_new_keyword("ref"))) {
      lambda_arg_type_p = lambda_arg_type_p->cdr->car; // the second element of the list
    }
    //printf("Lambda arg p: %s\n", obj_to_string(lambda_arg_type_p)->s);

    if(cif->arg_types[i] == &ffi_type_sint) {
      int *x = args[i];
      obj_args[i] = obj_new_int(*x);
    }
    else if(cif->arg_types[i] == &ffi_type_float) {
      float *x = args[i];
      obj_args[i] = obj_new_float(*x);
    }
    else if(cif->arg_types[i] == &ffi_type_double) {
      double *x = args[i];
      obj_args[i] = obj_new_double(*x);
    }
    else if(cif->arg_types[i] == &ffi_type_schar) {
      char *x = args[i];
      obj_args[i] = obj_new_char(*x);
    }
    else {
      if(obj_eq(process, lambda_arg_type_p, type_string)) {
        char **x = args[i];
        assert(*x);
        char *new_s = strdup(*x);
        //printf("new_s: %s\n", new_s);
        obj_args[i] = obj_new_string(new_s);
      }
      else {
        //printf("Lambda called from ffi with arg %d of type %s\n", i, obj_to_string(lambda_arg_type_p)->s);
        /* printf("Can't handle arg type %p when calling ffi function.\n", cif->arg_types[i]); */
        /* set_error("FFI function failed to call lambda: ", lambda_and_its_type->lambda); */
        /* return; */
        void **ptr = args[i];
        obj_args[i] = obj_new_ptr(*ptr);
      }
    }
    //printf("arg %d: %s\n", i, obj_to_string(obj_args[i])->s);
    lambda_arg_type_list_p = lambda_arg_type_list_p->cdr;

    //shadow_stack_push(obj_args[i]);
  }

  apply(process, lambda_and_its_type->lambda, obj_args, cif->nargs);
  Obj *result = stack_pop(process);
  free(obj_args);

  // unwrap ref
  if(lambda_return_type->tag == 'C' && lambda_return_type->car && lambda_return_type->cdr && lambda_return_type->cdr->car && obj_eq(process, lambda_return_type->car, obj_new_keyword("ref"))) {
    lambda_return_type = lambda_return_type->cdr->car; // the second element of the list
  }

  // TODO: extract this and refactor to common helper function
  if(obj_eq(process, lambda_return_type, type_int)) {
    assert_or_set_error(result->tag == 'I', "Invalid type of return value: ", result);
    int *integer = ret;
    *integer = result->i;
  }
  else if(obj_eq(process, lambda_return_type, type_bool)) {
    assert_or_set_error(result->tag == 'Y', "Invalid type of return value ", result);
    bool b = is_true(result);
    bool *boolean = ret;
    *boolean = b;
  }
  else if(obj_eq(process, lambda_return_type, type_char)) {
    assert_or_set_error(result->tag == 'T', "Invalid type of return value ", result);
    char c = result->character;
    char *character = ret;
    *character = c;
  }
  else if(obj_eq(process, lambda_return_type, type_float)) {
    assert_or_set_error(result->tag == 'V', "Invalid type of return value ", result);
    float *x = ret;
    *x = result->f32;
  }
  else if(obj_eq(process, lambda_return_type, type_double)) {
    assert_or_set_error(result->tag == 'W', "Invalid type of return value ", result);
    double *x = ret;
    *x = result->f64;
  }
  else if(obj_eq(process, lambda_return_type, type_string)) {
    assert_or_set_error(result->tag == 'S', "Invalid type of return value ", result);
    char **s = ret;
    *s = result->s;
  }
  else if(obj_eq(process, lambda_return_type, type_void)) {
  }
  else {
    //set_error("Calling lambda from FFI can't handle return type ", lambda_return_type);
    assert_or_set_error(result->tag == 'Q', "Invalid type of return value ", result);
    void **p = ret;
    *p = result->void_ptr;
  }

  /* for(int i = 0; i < arg_count; i++) { */
  /*   shadow_stack_pop(process); */
  /* } */
}

void call_foreign_function(Process *process, Obj *function, Obj **args, int arg_count) {
  assert(function);

  if(!function->funptr) {
    eval_error = obj_new_string("Can't call foregin function, it's funptr is NULL. May be a stub function with just a signature?");
    return;
  }

  assert(function->cif);
  assert(function->arg_types);
  assert(function->return_type);

  // TODO: change name to 'arg_values' or something like that
  void **values = calloc(sizeof(void *), arg_count);
  assert(values);

#define assert_or_free_values_and_set_error(assertion, message, object) \
  if(!(assertion)) {                                                    \
    free(values);                                                       \
  }                                                                     \
  assert_or_set_error((assertion), (message), (object));

  Obj *p = function->arg_types;
  for(int i = 0; i < arg_count; i++) {
    if(p && p->cdr) {
      assert(p->car);
      Obj *type_obj = p->car;

      // Handle ref types by unwrapping them: (:ref x) -> x
      if(type_obj->tag == 'C' && type_obj->car && type_obj->cdr && type_obj->cdr->car && obj_eq(process, type_obj->car, type_ref)) {
        type_obj = type_obj->cdr->car; // the second element of the list
      }

      args[i]->given_to_ffi = true; // This makes the GC ignore this value when deleting internal C-data, like inside a string

      if(obj_eq(process, type_obj, type_int)) {
        assert_or_free_values_and_set_error(args[i]->tag == 'I', "Invalid (expected int) type of arg: ", args[i]);
        values[i] = &args[i]->i;
      }
      else if(obj_eq(process, type_obj, type_bool)) {
        assert_or_free_values_and_set_error(args[i]->tag == 'B', "Invalid (expected bool) type of arg: ", args[i]);
        bool b = args[i]->boolean;
        values[i] = &b;
      }
      else if(obj_eq(process, type_obj, type_char)) {
        assert_or_free_values_and_set_error(args[i]->tag == 'T', "Invalid (expected char) type of arg: ", args[i]);
        char c = args[i]->character;
        values[i] = &c;
      }
      else if(obj_eq(process, type_obj, type_float)) {
        assert_or_free_values_and_set_error(args[i]->tag == 'V', "Invalid (expected float) type of arg: ", args[i]);
        values[i] = &args[i]->f32;
      }
      else if(obj_eq(process, type_obj, type_double)) {
        assert_or_free_values_and_set_error(args[i]->tag == 'W', "Invalid (expected double) type of arg: ", args[i]);
        values[i] = &args[i]->f64;
      }
      else if(obj_eq(process, type_obj, type_string)) {
        assert_or_free_values_and_set_error(args[i]->tag == 'S', "Invalid (expected string) type of arg: ", args[i]);
        //args[i]->s = strdup(args[i]->s); // OBS! Duplicating string here. TODO: Think about if this is the correct thing to do!
        values[i] = &args[i]->s;
      }
      else {
        //printf("Calling function with expected parameter of type %s. Argument is of type %c.\n", obj_to_string(process, p->car)->s, args[i]->tag);
        //printf("%s\n", STR(args[i])); // <- WARNING! This is an infinite loop!

        if(args[i]->tag == 'Q') {

#ifdef CHECKING
          if(args[i]->void_ptr == NULL || obj_eq(type_obj, obj_new_keyword("any"))) {
            goto hack;
          }

          assert_or_free_values_and_set_error(args[i]->meta, "Argument is missing meta data: ", args[i]);
          Obj *meta_type_tag = env_lookup(args[i]->meta, obj_new_keyword("type")); // TODO: make this keyword to a "singleton"
          assert_or_free_values_and_set_error(meta_type_tag, "Argument is missing meta 'type' tag: ", args[i]);

          bool eq = obj_eq(meta_type_tag, type_obj);
          if(!eq) {
            eval_error = obj_new_string("Invalid type of argument sent to function expecting '");
            obj_string_mut_append(eval_error, obj_to_string(type_obj)->s);
            obj_string_mut_append(eval_error, "' type: ");
            obj_string_mut_append(eval_error, obj_to_string(meta_type_tag)->s);
            return;
          }

        hack:;
#endif

          bool argExpectsRef = p->car->tag == 'C' && obj_eq(process, p->car->car, type_ref);
          //printf("argExpectsRef: %d\n", argExpectsRef);

          if(args[i] == NULL || args[i]->meta == NULL) {
            goto noCopyOfArg;
          }
          if(!argExpectsRef) {
            Obj *type = env_lookup(process, args[i]->meta, obj_new_keyword("type"));
            if(type) {

              /* printf("Sending void_ptr as argument to ffi function %s, ", STR(function)); */
              /* //printf(" it's value is '%s' ", STR(args[i])); */
              /* printf("type %s and tag %c, this should be copied! (for correctness)\n", STR(type), args[i]->tag); */
              
              Obj *copy = obj_copy(process, args[i]);

              if(eval_error) {
                return;
              }
              
              copy->meta = args[i]->meta;
              shadow_stack_push(process, copy);
              //printf("Copy with tag '%c': %s\n", copy->tag, STR(copy));
              shadow_stack_pop(process);
              values[i] = &copy->void_ptr;
            }
            else {
              printf("No type meta on %s, won't copy it.\n", STR(args[i]));
              goto noCopyOfArg;
            }
          }
          else {
          noCopyOfArg:;
            values[i] = &args[i]->void_ptr;
          }
        }
        else if(args[i]->tag == 'A') {
          // TODO: Do some type checking here!!!
          Array *a = obj_array_to_carp_array(process, args[i]);
          if(eval_error) {
            return;
          }
          assert(a);
          values[i] = &a;
        }
        else if(args[i]->tag == 'F') {
          values[i] = &args[i]->funptr;
        }
        else if(args[i]->tag == 'L') {
          if(ALLOW_SENDING_LAMBDA_TO_FFI) {
            //printf("Will call unbaked lambda from ffi function. Lambda should have types: %s\n", obj_to_string(type_obj)->s);

            ffi_type *closure_args[1];
            ffi_closure *closure;
            void (*closure_fun_ptr)();
            closure = ffi_closure_alloc(sizeof(ffi_closure), (void **)&closure_fun_ptr);

            if(closure) {
              /* Initialize the argument info vectors */
              closure_args[0] = &ffi_type_pointer;

              /* ffi_cif cif_static; */
              /* ffi_cif *cif = &cif_static; */
              /* ffi_prep_cif(cif, FFI_DEFAULT_ABI, 0, &ffi_type_void, closure_args); */

              //printf("Type obj: %s\n", obj_to_string(type_obj)->s);

              Obj *lambda_arg_types = type_obj->cdr->car;
              Obj *lambda_return_type = type_obj->cdr->cdr->car;
              int lambda_arg_count = 0;
              Obj *p = lambda_arg_types;
              while(p && p->car) {
                p = p->cdr;
                lambda_arg_count++;
              }

              ffi_cif *cif = create_cif(process, lambda_arg_types, lambda_arg_count, lambda_return_type, "TODO:proper-name");

              Obj *lambda_arg = args[i];
              LambdaAndItsType *lambda_and_its_type = malloc(sizeof(LambdaAndItsType)); // TODO: free!
              lambda_and_its_type->lambda = lambda_arg; // the uncompiled lambda that was passed to the ffi function
              lambda_and_its_type->signature = type_obj;
              lambda_and_its_type->process = process;

              typedef void (*LambdaCallback)(ffi_cif *, void *, void **, void *);

              if(ffi_prep_closure_loc(closure, cif, (LambdaCallback)call_lambda_from_ffi, lambda_and_its_type, closure_fun_ptr) == FFI_OK) {
                //printf("Closure preparation done.\n");
                values[i] = &closure_fun_ptr;
              }
              else {
                set_error("Closure prep failed. ", nil);
              }
            }
            else {
              set_error("Failed to allocate closure. ", nil);
            }
          }
          else {
            free(values);
            set_error("Can't send argument of lambda type (tag 'L') to ffi function, you need to compile it to a C function using (bake ...) first:\n", args[i]);
          }
        }
        else {
          free(values);
          printf("INVALID ARG TYPE: %c\n", args[i]->tag);
          printf("ARG: %s\n", obj_to_string(process, args[i])->s);
          set_error("Can't send argument of invalid type to foreign function taking parameter of type ", p->car);
        }
      }
      p = p->cdr;
    }
    else {
      free(values);
      set_error("Too many arguments to ", function);
    }
  }

  if(p && p->car) {
    free(values);
    set_error("Too few arguments to ", function);
  }

  // Handle refs:
  Obj *return_type = function->return_type;
  if(return_type->tag == 'C' && return_type->car && return_type->cdr &&
     return_type->cdr->car && obj_eq(process, return_type->car, type_ref)) {
    return_type = return_type->cdr->car; // the second element of the list
  }

  void *result;
  ffi_call(function->cif, function->funptr, &result, values);

  Obj *obj_result = primitive_to_obj(process, result, return_type);

  free(values);

  if(!obj_result) {
    printf("obj_result == NULL, return_type = %s\n", obj_to_string(process, return_type)->s);
    return; // something went wrong
  }

  stack_push(process, obj_result);
}

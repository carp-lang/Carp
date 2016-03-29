#include "eval.h"
#include "env.h"
#include "assertions.h"
#include "reader.h"
#include "gc.h"
#include "primops.h"
#include "obj.h"
#include "../shared/types.h"

#define LOG_EVAL 0
#define LOG_STACK 0
#define LOG_SHADOW_STACK 0
#define SHOW_MACRO_EXPANSION 0
#define LOG_FUNC_APPLICATION 0
#define GC_COLLECT_AFTER_EACH_FORM 0
#define ALLOW_SENDING_LAMBDA_TO_FFI 1

#define LABELED_DISPATCH 0

void stack_print() {
  printf("----- STACK -----\n");
  for(int i = 0; i < stack_pos; i++) {
    printf("%d\t%s\n", i, obj_to_string(stack[i])->s);
  }
  printf("-----  END  -----\n\n");
}

void stack_push(Obj *o) {
  assert(o);
  if(LOG_STACK) {
    printf("Pushing %s\n", obj_to_string(o)->s);
  }
  if(stack_pos >= STACK_SIZE) {
    printf("Stack overflow:\n");
    stack_print();
    exit(1);
  }
  stack[stack_pos++] = o;
  if(LOG_STACK) {
    stack_print();
  }
}

Obj *stack_pop() {
  if(eval_error) {
    return nil;
  }
  if(stack_pos <= 0) {
    printf("Stack underflow.\n");
    assert(false);
  }
  if(LOG_STACK) {
    printf("Popping %s\n", obj_to_string(stack[stack_pos - 1])->s);
  }
  Obj *o = stack[--stack_pos];
  if(LOG_STACK) {
    stack_print();
  }
  return o;
}

void shadow_stack_print() {
  printf("----- SHADOW STACK -----\n");
  for(int i = 0; i < shadow_stack_pos - 1; i++) {
    printf("%d\t", i);
    obj_print_cout(shadow_stack[i]);
    printf("\n");
  }
  printf("-----  END  -----\n\n");
}

void shadow_stack_push(Obj *o) {
  if(LOG_SHADOW_STACK) {
    printf("Pushing to shadow stack: %p ", o);
    obj_print_cout(o);
    printf("\n");
  }
  if(shadow_stack_pos >= SHADOW_STACK_SIZE) {
    printf("Shadow stack overflow.\n");
    shadow_stack_print();
    exit(1);
  }
  shadow_stack[shadow_stack_pos++] = o;
}

Obj *shadow_stack_pop() {
  if(shadow_stack_pos <= 0) {
    printf("Shadow stack underflow.\n");
    assert(false);
  }
  Obj *o = shadow_stack[--shadow_stack_pos];
  if(LOG_SHADOW_STACK) {
    printf("Popping from shadow stack: %p ", o);
    obj_print_cout(o);
    printf("\n");
  }
  return o;
}

void function_trace_print() {
  printf(" ----------------------------------------------------------------\n");
  
  for(int i = function_trace_pos - 1; i >= 0; i--) {
    printf("%3d ", i);

    StackTraceCallSite call_site = function_trace[i];
    Obj *o = call_site.caller;
    Obj *function = call_site.callee;
    
    if(o->meta) {
      //printf("%s\n", obj_to_string(o->meta)->s);
      char *func_name = "";
      Obj *func_name_data = NULL;
      if(function && function->meta) {
        func_name_data = env_lookup(function->meta, obj_new_keyword("name"));
      }
      if(func_name_data) {
        func_name = obj_to_string_not_prn(func_name_data)->s;
      } else {
        func_name = "???"; // obj_to_string(function)->s;
      }
      int line = env_lookup(o->meta, obj_new_keyword("line"))->i;
      int pos = env_lookup(o->meta, obj_new_keyword("pos"))->i;
      char *file_path = env_lookup(o->meta, obj_new_keyword("file"))->s;
      char *file = file_path;

      int len = (int)strlen(file_path);
      for(int i = len - 1; i >= 0; i--) {
        if(file_path[i] == '/') {
          file = strdup(file_path + i + 1);
          break;
        }
      }
      printf("%-30s %s %d:%d", func_name, file, line, pos);
    }
    else {
      printf("No meta data."); //"%s", obj_to_string(function)->s);
    }
    printf("\n");
  }
  
  printf(" ----------------------------------------------------------------\n");
}

bool obj_match(Obj *env, Obj *attempt, Obj *value);

bool obj_match_lists(Obj *env, Obj *attempt, Obj *value) {
  //printf("Matching list %s with %s\n", obj_to_string(attempt)->s, obj_to_string(value)->s);
  Obj *p1 = attempt;
  Obj *p2 = value;
  while(p1 && p1->car) {
    if(obj_eq(p1->car, dotdotdot) && p1->cdr && p1->cdr->car) {
      //printf("Matching & %s against %s\n", obj_to_string(p1->cdr->car)->s, obj_to_string(p2)->s);
      bool matched_rest = obj_match(env, p1->cdr->car, p2);
      return matched_rest;
    }
    else if(!p2 || !p2->car) {
      return false;
    }
    bool result = obj_match(env, p1->car, p2->car);
    if(!result) {
      return false;
    }
    p1 = p1->cdr;
    p2 = p2->cdr;
  }
  if(p2 && p2->car) {
    return false;
  }
  else {
    //printf("Found end of list, it's a match.\n");
    return true;
  }
}

bool obj_match_arrays(Obj *env, Obj *attempt, Obj *value) {
  //printf("Matching arrays %s with %s\n", obj_to_string(attempt)->s, obj_to_string(value)->s);
  int i;
  for(i = 0; i < attempt->count; i++) {
    Obj *o = attempt->array[i];
    if(obj_eq(o, dotdotdot) && ((i + 1) < attempt->count)) {
      int rest_count = value->count - i;
      //printf("rest_count: %d\n", rest_count);
      Obj *rest = obj_new_array(rest_count);
      for(int j = 0; j < rest_count; j++) {
        rest->array[j] = value->array[i + j]; // copy the rest of the objects to a smaller array
      }
      //printf("rest: %s\n", obj_to_string(rest)->s);
      Obj *symbol_after_dotdotdot = attempt->array[i + 1];
      //printf("symbol_after_dotdotdot: %s\n", obj_to_string(symbol_after_dotdotdot)->s);
      bool matched_rest = obj_match(env, symbol_after_dotdotdot, rest);
      //printf("%s\n", matched_rest ? "match" : "no match");
      return matched_rest;
    }
    else if(i >= value->count) {
      return false;
    }
    bool result = obj_match(env, o, value->array[i]);
    if(!result) {
      return false;
    }
  }
  if(i < value->count) {
    //printf("The value list is too long.\n");
    return false;
  }
  else {
    //printf("Found end of list, it's a match.\n");
    return true;
  }
}

bool obj_match(Obj *env, Obj *attempt, Obj *value) {
  //printf("Matching %s with %s\n", obj_to_string(attempt)->s, obj_to_string(value)->s);
  
  if(attempt->tag == 'C' && obj_eq(attempt->car, lisp_quote) && attempt->cdr && attempt->cdr->car) {
    // Dubious HACK to enable matching on quoted things...
    // Don't want to extend environment in this case!
    Obj *quoted_attempt = attempt->cdr->car;
    return obj_eq(quoted_attempt, value);
  }
  else if(attempt->tag == 'Y' && strcmp(attempt->s, "nil") == 0) {
    // Using 'nil' on the left side of a match will bind the right side to that symbol, which is NOT what you want!
    return obj_eq(value, nil);
  }
  else if(attempt->tag == 'Y') {
    //printf("Binding %s to value %s in match.\n", obj_to_string(attempt)->s, obj_to_string(value)->s);
    env_extend(env, attempt, value);
    return true;
  }
  else if(attempt->tag == 'C' && value->tag == 'C') {
    return obj_match_lists(env, attempt, value);
  }
  else if(attempt->tag == 'A' && value->tag == 'A') {
    return obj_match_arrays(env, attempt, value);
  }
  else if(obj_eq(attempt, value)) {
    return true;
  }
  else {
    /* printf("attempt %s (%c) is NOT equal to value %s (%c)\n", */
    /*     obj_to_string(attempt)->s, */
    /*     attempt->tag, */
    /*     obj_to_string(value)->s, */
    /*     value->tag); */
    return false;
  }
}

void match(Obj *env, Obj *value, Obj *attempts) {
  Obj *p = attempts;
  while(p && p->car) {
    //printf("\nWill match %s with value %s\n", obj_to_string(p->car)->s, obj_to_string(value)->s);
    Obj *new_env = obj_new_environment(env);
    shadow_stack_push(new_env);
    bool result = obj_match(new_env, p->car, value);

    if(result) {
      //printf("Match found, evaling %s in env\n", obj_to_string(p->cdr->car)->s); //, obj_to_string(new_env)->s);
      eval_internal(new_env, p->cdr->car); // eval the following form using the new environment
      Obj *pop = shadow_stack_pop(); // new_env
      if(eval_error) {
        return;
      }
      assert(pop == new_env);
      return;
    }
    
    if(!p->cdr) {
      set_error("Uneven nr of forms in match.", attempts);
    }
      
    p = p->cdr->cdr;

    Obj *e = shadow_stack_pop(); // new_env
    assert(e == new_env);
  }

  set_error("Failed to find a suitable match for: ", value);
}

typedef struct {
  Obj *lambda;
  Obj *signature;
} LambdaAndItsType;

void call_lambda_from_ffi(ffi_cif *cif, void *ret, void* args[], LambdaAndItsType *lambda_and_its_type) {
  //printf("Calling lambda %s from ffi function!\n", obj_to_string(lambda_and_its_type->lambda)->s);

  int arg_count = cif->nargs;
  //printf("arg count: %d\n", arg_count);

  Obj *obj_args[arg_count];
  Obj *lambda_type_signature = lambda_and_its_type->signature; // TODO: shadow stack?!
  Obj *lambda_return_type = lambda_type_signature->cdr->cdr->car;
  Obj *lambda_arg_type_list_p = lambda_type_signature->cdr->car;

  //printf("Lambda signature: %s\n", obj_to_string(lambda_type_signature)->s);
  
  for(int i = 0; i < arg_count; i++) {

    Obj *lambda_arg_type_p = lambda_arg_type_list_p->car;

    if(!lambda_arg_type_p) {
      printf("Too many arguments (%d) sent to lambda with signature: %s\n", arg_count, obj_to_string(lambda_type_signature)->s);
      set_error("Too many args. ", nil);
      return;
    }
    
    // Unwrap ref args
    if(lambda_arg_type_p->tag == 'C' && lambda_arg_type_p->car && lambda_arg_type_p->cdr && lambda_arg_type_p->cdr->car && obj_eq(lambda_arg_type_p->car, obj_new_keyword("ref"))) {
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
      if(obj_eq(lambda_arg_type_p, type_string)) {
        char **x = args[i];
        assert(*x);
        char *new_s = strdup(*x);
        printf("new_s: %s\n", new_s);
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

  apply(lambda_and_its_type->lambda, obj_args, cif->nargs);
  Obj *result = stack_pop();

  // unwrap ref
  if(lambda_return_type->tag == 'C' && lambda_return_type->car && lambda_return_type->cdr && lambda_return_type->cdr->car && obj_eq(lambda_return_type->car, obj_new_keyword("ref"))) {
      lambda_return_type = lambda_return_type->cdr->car; // the second element of the list
    }  
  
  // TODO: extract this and refactor to common helper function
  if(obj_eq(lambda_return_type, type_int)) {
    assert_or_set_error(result->tag == 'I', "Invalid type of return value: ", result);
    int *integer = ret;
    *integer = result->i;
  }
  else if(obj_eq(lambda_return_type, type_bool)) {
    assert_or_set_error(result->tag == 'Y', "Invalid type of return value ", result);
    bool b = is_true(result);
    bool *boolean = ret;
    *boolean = b;
  }
  else if(obj_eq(lambda_return_type, type_char)) {
    assert_or_set_error(result->tag == 'T', "Invalid type of return value ", result);
    char c = result->character;
    char *character = ret;
    *character = c;
  }
  else if(obj_eq(lambda_return_type, type_float)) {
    assert_or_set_error(result->tag == 'V', "Invalid type of return value ", result);
    float *x = ret;
    *x = result->f32;
  }
  else if(obj_eq(lambda_return_type, type_double)) {
    assert_or_set_error(result->tag == 'W', "Invalid type of return value ", result);
    double *x = ret;
    *x = result->f64;
  }
  else if(obj_eq(lambda_return_type, type_string)) {
    assert_or_set_error(result->tag == 'S', "Invalid type of return value ", result);
    char **s = ret;
    *s = result->s;
  }
  else if(obj_eq(lambda_return_type, type_void)) {
    
  }
  else {
    //set_error("Calling lambda from FFI can't handle return type ", lambda_return_type);
    assert_or_set_error(result->tag == 'Q', "Invalid type of return value ", result);
    void **p = ret;
    *p = result->void_ptr;
  }

  /* for(int i = 0; i < arg_count; i++) { */
  /*   shadow_stack_pop(); */
  /* } */
}

Array *obj_array_to_carp_array(Obj *obj_array) {
  Array *carp_array = malloc(sizeof(Array));
  carp_array->count = obj_array->count;
  
  Obj **oa = obj_array->array;

  if(obj_array->count == 0) {
          
  }
  else if(oa[0]->tag == 'I') {
    carp_array->data = malloc(sizeof(int) * carp_array->count);
    int *data = carp_array->data;
    for(int i = 0; i < carp_array->count; i++) {
      //assert_or_set_error(oa[i]->tag == 'I', "All elements in array must be integers.", nil);
      data[i] = oa[i]->i;
    }
  }
  else if(oa[0]->tag == 'Q') {
    carp_array->data = malloc(sizeof(void*) * carp_array->count);
    void **data = carp_array->data;
    for(int i = 0; i < carp_array->count; i++) {
      //assert_or_set_error(oa[i]->tag == 'I', "All elements in array must be ptr:s.", nil);
      data[i] = oa[i]->void_ptr;
    }
  }
  else if(oa[0]->tag == 'A') {
    printf("Nested arrays!\n");
    carp_array->data = malloc(sizeof(void*) * carp_array->count);
    Array **data = carp_array->data;
    for(int i = 0; i < carp_array->count; i++) {
      Array *inner_array = obj_array_to_carp_array(oa[i]);
      data[i] = inner_array;
    }
  }
  else {
    eval_error = obj_new_string("Can't handle this kind of array element as argument: ");
    obj_string_mut_append(eval_error, obj_to_string(oa[0])->s);
    //printf("FAIL %s\n", obj_to_string(eval_error)->s);
    return NULL;
  }

  return carp_array;
}

bool in_macro_expansion = false;

void apply(Obj *function, Obj **args, int arg_count) {
  if(function->tag == 'L') {

    //printf("Calling function "); obj_print_cout(function); printf(" with params: "); obj_print_cout(function->params); printf("\n");
    
    Obj *calling_env = obj_new_environment(function->env);
    bool allow_rest_args = true;
    env_extend_with_args(calling_env, function, arg_count, args, allow_rest_args);
    //printf("Lambda env: %s\n", obj_to_string(calling_env)->s);

    shadow_stack_push(function);
    shadow_stack_push(calling_env);
    
    eval_internal(calling_env, function->body);
    if(eval_error) {
      return;
    }
    
    Obj *pop1 = shadow_stack_pop();
    Obj *pop2 = shadow_stack_pop();
    assert(pop1 == calling_env);
    assert(pop2 == function);
  }
  else if(function->tag == 'P') {   
    Obj *result = function->primop(args, arg_count);
    stack_push(result);
  }
  else if(function->tag == 'F') {
    assert(function);

    if(!function->funptr) {
      eval_error = obj_new_string("Can't call foregin function, it's funptr is NULL. May be a stub function with just a signature?");
      return;
    }
    
    assert(function->cif);
    assert(function->arg_types);
    assert(function->return_type);
     
    void **values = calloc(sizeof(void*), arg_count);
    assert(values);

#define assert_or_free_values_and_set_error(assertion, message, object) \
    if(!(assertion)) {                                                  \
      free(values);                                                     \
    }                                                                   \
    assert_or_set_error((assertion), (message), (object));

    Obj *p = function->arg_types;
    for(int i = 0; i < arg_count; i++) {      
      if(p && p->cdr) {
        assert(p->car);
        Obj *type_obj = p->car;

        // Handle ref types by unwrapping them: (:ref x) -> x
        if(type_obj->tag == 'C' && type_obj->car && type_obj->cdr && type_obj->cdr->car && obj_eq(type_obj->car, type_ref)) {
          type_obj = type_obj->cdr->car; // the second element of the list
        }
        
        args[i]->given_to_ffi = true; // This makes the GC ignore this value when deleting internal C-data, like inside a string

        if(obj_eq(type_obj, type_int)) {
          assert_or_free_values_and_set_error(args[i]->tag == 'I', "Invalid (expected int) type of arg: ", args[i]);
          values[i] = &args[i]->i;
        }
        else if(obj_eq(type_obj, type_bool)) {
          assert_or_free_values_and_set_error(args[i]->tag == 'B', "Invalid (expected bool) type of arg: ", args[i]);
          bool b = args[i]->boolean;
          values[i] = &b;
        }
        else if(obj_eq(type_obj, type_char)) {
          assert_or_free_values_and_set_error(args[i]->tag == 'T', "Invalid (expected char) type of arg: ", args[i]);
          char c = args[i]->character;
          values[i] = &c;
        }
        else if(obj_eq(type_obj, type_float)) {
          assert_or_free_values_and_set_error(args[i]->tag == 'V', "Invalid (expected float) type of arg: ", args[i]);
          values[i] = &args[i]->f32;
        }
        else if(obj_eq(type_obj, type_double)) {
          assert_or_free_values_and_set_error(args[i]->tag == 'W', "Invalid (expected double) type of arg: ", args[i]);
          values[i] = &args[i]->f64;
        }
        else if(obj_eq(type_obj, type_string)) {
          assert_or_free_values_and_set_error(args[i]->tag == 'S', "Invalid (expected string) type of arg: ", args[i]);
          //args[i]->s = strdup(args[i]->s); // OBS! Duplicating string here. TODO: Think about if this is the correct thing to do!
          values[i] = &args[i]->s;
        }
        else {
          /* printf("Calling function with expected parameter of type %s. Argument is of type %c.\n", */
          /*        obj_to_string(p->car)->s, */
          /*        args[i]->tag); */         
          
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
            
            values[i] = &args[i]->void_ptr;
          }
          else if(args[i]->tag == 'F') {
            values[i] = &args[i]->funptr;
          }
          else if(args[i]->tag == 'L') {
            if(ALLOW_SENDING_LAMBDA_TO_FFI) {
              //printf("Will call unbaked lambda from ffi function. Lambda should have types: %s\n", obj_to_string(type_obj)->s);
	    
              ffi_type *closure_args[0];
              ffi_closure *closure;  
              void (*closure_fun_ptr)();
              closure = ffi_closure_alloc(sizeof(ffi_closure), (void**)&closure_fun_ptr);
     
              if (closure) {
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
		
                ffi_cif *cif = create_cif(lambda_arg_types, lambda_arg_count, lambda_return_type, "TODO:proper-name");

                Obj *lambda_arg = args[i];
                LambdaAndItsType *lambda_and_its_type = malloc(sizeof(LambdaAndItsType)); // TODO: free!
                lambda_and_its_type->lambda = lambda_arg; // the uncompiled lambda that was passed to the ffi function
                lambda_and_its_type->signature = type_obj;
                
                typedef void (*LambdaCallback)(ffi_cif *, void *, void **, void *);
	      
                if (ffi_prep_closure_loc(closure, cif, (LambdaCallback)call_lambda_from_ffi, lambda_and_its_type, closure_fun_ptr) == FFI_OK) {
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
            } else {
              free(values);
              set_error("Can't send argument of lambda type (tag 'L') to ffi function, you need to compile it to a C function using (bake ...) first:\n", args[i]);
            }
          }
          else {
            free(values);
            printf("INVALID ARG TYPE: %c\n", args[i]->tag);
            printf("ARG: %s\n", obj_to_string(args[i])->s);
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

    Obj *obj_result = NULL;

    // Handle refs:
    Obj *return_type = function->return_type;
    if(return_type->tag == 'C' && return_type->car && return_type->cdr && return_type->cdr->car && obj_eq(return_type->car, type_ref)) {
      return_type = return_type->cdr->car; // the second element of the list
    }
    
    if(obj_eq(return_type, type_string)) {
      //printf("Returning string.\n");
      char *c = NULL;
      ffi_call(function->cif, function->funptr, &c, values);

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
      ffi_sarg result;
      ffi_call(function->cif, function->funptr, &result, values);
      obj_result = obj_new_int(result);
    }
    else if(obj_eq(return_type, type_bool)) { 
      //printf("Returning bool.\n");
      ffi_arg result;
      ffi_call(function->cif, function->funptr, &result, values);
      obj_result = result ? lisp_true : lisp_false;
    }
    else if(obj_eq(return_type, type_char)) { 
      ffi_sarg result;
      ffi_call(function->cif, function->funptr, &result, values);
      obj_result = obj_new_char(result);
    }
    else if(obj_eq(return_type, type_float)) { 
      //printf("Returning float.\n");
      float result;
      ffi_call(function->cif, function->funptr, &result, values);
      obj_result = obj_new_float(result);
    }
    else if(obj_eq(return_type, type_double)) { 
      double result;
      ffi_call(function->cif, function->funptr, &result, values);
      obj_result = obj_new_double(result);
    }
    else if(obj_eq(return_type, type_void)) { 
      //printf("Returning void.\n");
      ffi_sarg result;
      ffi_call(function->cif, function->funptr, &result, values);
      obj_result = nil;
    }
    else {
      //set_error("Returning what? ", function->return_type);
      // Assume it's a user defined type:
      void *result;
      ffi_call(function->cif, function->funptr, &result, values);
      obj_result = obj_new_ptr(result);

      obj_set_meta(obj_result, obj_new_keyword("type"), return_type);
    }

    free(values);

    assert(obj_result);
    stack_push(obj_result);
  }
  else if(function->tag == 'K') {
    if(arg_count != 1) {
      eval_error = obj_new_string("Args to keyword lookup must be a single arg.");
    }
    else if(args[0]->tag != 'E') {
      eval_error = obj_new_string("Arg 0 to keyword lookup must be a dictionary: ");
      obj_string_mut_append(eval_error, obj_to_string(args[0])->s);
    }
    else {
      Obj *value = env_lookup(args[0], function);
      if(value) {
        stack_push(value);
      } else {
        eval_error = obj_new_string("Failed to lookup keyword '");
        obj_string_mut_append(eval_error, obj_to_string(function)->s);
        obj_string_mut_append(eval_error, "'");
        obj_string_mut_append(eval_error, " in \n");
        obj_string_mut_append(eval_error, obj_to_string(args[0])->s);
        obj_string_mut_append(eval_error, "\n");
      }
    }
  }
  else if(function->tag == 'E' && obj_eq(env_lookup(function, obj_new_keyword("struct")), lisp_true)) {
    // Evaluation of a struct-definition (a dictionary) in function position (which means that it is used as a constructor)
    Obj *name_obj = env_lookup(function, obj_new_keyword("name"));
    assert_or_set_error(name_obj, "no key 'name' on struct definition: ", function);
    char *name = name_obj->s;

    Obj *struct_size_obj = env_lookup(function, obj_new_keyword("size"));
    assert_or_set_error(struct_size_obj, "no key 'size' on struct definition: ", function);
    int struct_size = struct_size_obj->i;

    Obj *struct_member_count_obj = env_lookup(function, obj_new_keyword("member-count"));
    assert_or_set_error(struct_member_count_obj, "no key 'member-count' on struct definition: ", function);
    int member_count = struct_member_count_obj->i;
    
    Obj *offsets_obj = env_lookup(function, obj_new_keyword("member-offsets"));
    assert_or_set_error(offsets_obj, "no key 'member-offsets' on struct definition: ", function);
    assert_or_set_error(offsets_obj->tag == 'A', "offsets must be an array: ", function);
    Obj **offsets = offsets_obj->array;
    
    Obj *member_types_obj = env_lookup(function, obj_new_keyword("member-types"));
    assert_or_set_error(member_types_obj, "no key 'member-types' on struct definition: ", function);
    assert_or_set_error(member_types_obj->tag == 'A', "member-types must be an array: ", function);
    Obj **member_types = member_types_obj->array;
   
    //printf("Will create a %s of size %d and member count %d.\n", name, size, member_count);
    void *p = malloc(struct_size);
    Obj *new_struct = obj_new_ptr(p);

    shadow_stack_push(new_struct);
    
    if(!new_struct->meta) {
      new_struct->meta = obj_new_environment(NULL);
    }
    env_assoc(new_struct->meta, obj_new_keyword("type"), obj_new_keyword(name));

    assert_or_set_error(!(arg_count < member_count), "Too few args to struct constructor: ", obj_new_string(name));
    assert_or_set_error(!(arg_count > member_count), "Too many args to struct constructor: ", obj_new_string(name));
    
    for(int i = 0; i < arg_count; i++) {
      Obj *member_type = member_types[i];
      int offset = offsets[i]->i;
      if(args[i]->tag == 'V') {
        assert_or_set_error(obj_eq(member_type, type_float), "Can't assign float to a member of type ", obj_to_string(member_type));
        float *fp = (float*)(((char*)new_struct->void_ptr) + offset);
        float f = args[i]->f32;
        //printf("Setting member %d at offset %d to %f.\n", i, offset, f);
        *fp = f;
      }
      else if(args[i]->tag == 'I') {
        assert_or_set_error(obj_eq(member_type, type_int), "Can't assign int to a member of type ", obj_to_string(member_type));
        int *xp = (int*)(((char*)new_struct->void_ptr) + offset);
        int x = args[i]->i;
        *xp = x;
      }
      else if(args[i]->tag == 'B') {
        assert_or_set_error(obj_eq(member_type, type_bool), "Can't assign bool to a member of type ", obj_to_string(member_type));
        bool *xp = (bool*)(((char*)new_struct->void_ptr) + offset);
        bool x = args[i]->boolean;
        *xp = x;
      }
      else if(args[i]->tag == 'Q') {
	assert_or_set_error(!obj_eq(member_type, type_char), "Can't assign char to a member of type ", obj_to_string(member_type));
	assert_or_set_error(!obj_eq(member_type, type_int), "Can't assign int to a member of type ", obj_to_string(member_type));
	assert_or_set_error(!obj_eq(member_type, type_float), "Can't assign float to a member of type ", obj_to_string(member_type));
	assert_or_set_error(!obj_eq(member_type, type_string), "Can't assign string to a member of type ", obj_to_string(member_type));
        void **vp = (void**)(((char*)new_struct->void_ptr) + offset);
        *vp = args[i]->void_ptr;
      }
      else if(args[i]->tag == 'S') {
	assert_or_set_error(obj_eq(member_type, type_string), "Can't assign int to a member of type ", obj_to_string(member_type));
        char **sp = (char**)(((char*)new_struct->void_ptr) + offset);
        *sp = strdup(args[i]->s); // must strdup or the struct will ref Obj's on the stack that will get gc:ed
      }
      else if(args[i]->tag == 'T') {
        assert_or_set_error(obj_eq(member_type, type_char), "Can't assign char to a member of type ", obj_to_string(member_type));
        char *cp = (char*)(((char*)new_struct->void_ptr) + offset);
        *cp = args[i]->character;
      }
      else if(args[i]->tag == 'A') {
        //assert_or_set_error(obj_eq(member_type, type_array), "Can't assign array to a member of type ", obj_to_string(member_type));
        printf("Array as arg: %s\n", obj_to_string(args[i])->s);

        // TODO: use this code for sending arrays to normal FFI functions too!!!
        // TODO: use the SAME code for sending data to FFI and struct constructors.
        // TODO: check that we send the expected type to the constructor
        
        Array *a = obj_array_to_carp_array(args[i]);
        if(!a) {
          return;
        }
        
        void **ap = (void**)(((char*)new_struct->void_ptr) + offset);
        *ap = a;
      }
      else {
        eval_error = obj_new_string("Can't set member ");
        char buffer[32];
        sprintf(buffer, "%d", i);
        obj_string_mut_append(eval_error, buffer);
        obj_string_mut_append(eval_error, " of struct ");
        obj_string_mut_append(eval_error, name);
        obj_string_mut_append(eval_error, " to ");
        obj_string_mut_append(eval_error, obj_to_string(args[i])->s);
        obj_string_mut_append(eval_error, " (unhandled type).");
        return;
      }
    }
    shadow_stack_pop(); // pop new_struct
    stack_push(new_struct);
  } else {
    set_error("Can't call non-function: ", function);
  }
}

#define HEAD_EQ(str) (o->car->tag == 'Y' && strcmp(o->car->s, (str)) == 0)

void eval_list(Obj *env, Obj *o) {
  assert(o);
  
  //printf("Evaling list %s\n", obj_to_string(o)->s);
  if(!o->car) {
    stack_push(o); // nil, empty list
    return;
  }

  #if LABELED_DISPATCH
  static void* dispatch_table[] = {
    NULL, // index 0 means no dispatch
    &&dispatch_do, // 1
    &&dispatch_let, // 2
    &&dispatch_not, // 3
    &&dispatch_or, // 4
    &&dispatch_and, // 5
    &&dispatch_quote, // 6
    &&dispatch_while, // 7
    &&dispatch_if, // 8
    &&dispatch_match, // 9
    &&dispatch_reset, // 10
    &&dispatch_fn, // 11
    &&dispatch_macro, // 12
    &&dispatch_def, // 13
    &&dispatch_defp, // 14
    &&dispatch_ref, // 15
    &&dispatch_catch, // 16
  };
   
  Obj *head = o->car;
  if(head->tag == 'Y') {
    if(head->dispatch_index) {
      //printf("Will dispatch instruction %d\n", head->dispatch_index);
      goto *dispatch_table[head->dispatch_index];
    }
    else {
      //printf("Will dispatch non-special form symbol: %s\n", obj_to_string(head)->s);
      goto dispatch_function_evaluation;
    }
  }
  else {
    //printf("Will dispatch non-symbol: %s\n", obj_to_string(head)->s);
    goto dispatch_function_evaluation;
  }
  assert(false); // Don't go past here, it's SLOW.
  #endif

  if(HEAD_EQ("do")) {
    #if LABELED_DISPATCH
  dispatch_do:;
    #endif
    Obj *p = o->cdr;
    while(p && p->car) {
      eval_internal(env, p->car);
      if(eval_error) { return; }
      p = p->cdr;
      if(p && p->car) {
        stack_pop(); // remove result from form that is not last
      }
    }
  }
  else if(HEAD_EQ("let")) {
    #if LABELED_DISPATCH
  dispatch_let:;
    #endif
    Obj *let_env = obj_new_environment(env);
    shadow_stack_push(let_env);
    //Obj *p = o->cdr->car;
    assert_or_set_error(o->cdr->car, "No bindings in 'let' form: ", o);
    assert_or_set_error(o->cdr->car->tag == 'A', "Bindings in 'let' form must be an array: ", o);
    Obj *a = o->cdr->car;
    for(int i = 0; i < a->count; i += 2) {
      if(i + 1 == a->count) {
        set_error("Uneven nr of forms in let: ", o); // TODO: add error code for this kind of error, return error map instead
      }
      assert_or_set_error(a->array[i]->tag == 'Y', "Trying to bind to non-symbol in let form: ", a->array[i]);
      eval_internal(let_env, a->array[i + 1]);
      if(eval_error) { return; }
      Obj *value = stack_pop();
      env_extend(let_env, a->array[i], value);
      //printf("let %s to %s\n", obj_to_string(a->array[i])->s, obj_to_string(a->array[i + 1])->s);
      //obj_set_meta(value, obj_new_keyword("name"), a->array[i]); // TODO: only do this in certain situations
    }
    assert_or_set_error(o->cdr->cdr->car, "No body in 'let' form.", o);
    assert_or_set_error(o->cdr->cdr->cdr->car == NULL, "Too many body forms in 'let' form (use explicit 'do').", o);
    eval_internal(let_env, o->cdr->cdr->car);
    shadow_stack_pop(); // let_env
  }
  else if(HEAD_EQ("not")) {
    #if LABELED_DISPATCH
  dispatch_not:;
    #endif
    Obj *p = o->cdr;
    while(p) {
      if(p->car) {
        eval_internal(env, p->car);
        if(eval_error) { return; }
        if(is_true(stack_pop())) {
          stack_push(lisp_false);
          return;
        }
      }
      p = p->cdr;
    }
    stack_push(lisp_true);
  }
  else if(HEAD_EQ("or")) {
    #if LABELED_DISPATCH
  dispatch_or:;
    #endif
    Obj *p = o->cdr;
    while(p) {
      if(p->car) {
        eval_internal(env, p->car);
        if(eval_error) { return; }
        if(is_true(stack_pop())) {
          stack_push(lisp_true);
          return;
        }
      }
      p = p->cdr;
    }
    stack_push(lisp_false);
  }
  else if(HEAD_EQ("and")) {
    #if LABELED_DISPATCH
  dispatch_and:;
    #endif
    Obj *p = o->cdr;
    while(p) {
      if(p->car) {
        eval_internal(env, p->car);
        if(eval_error) { return; }
        if(!is_true(stack_pop())) {
          stack_push(lisp_false);
          return;
        }
      }
      p = p->cdr;
    }
    stack_push(lisp_true);
  }
  else if(HEAD_EQ("quote")) {
    #if LABELED_DISPATCH
  dispatch_quote:;
    #endif
    if(o->cdr == nil) {
      stack_push(nil);
    } else {
      //assert_or_set_error(o->cdr->cdr->car, "Too many forms in 'quote' form: ", o);
      if(o->cdr->cdr->car) {
        printf("Too many forms in 'quote' form: %s\n", obj_to_string(o)->s);
      }
      stack_push(o->cdr->car);
    }
  }
  else if(HEAD_EQ("while")) {
    #if LABELED_DISPATCH
  dispatch_while:;
    #endif
    assert_or_set_error(o->cdr->car, "Too few body forms in 'while' form: ", o);
    assert_or_set_error(o->cdr->cdr->cdr->car == NULL, "Too many body forms in 'while' form (use explicit 'do').", o);
    eval_internal(env, o->cdr->car);
    if(eval_error) {
      return;
    }
    while(is_true(stack_pop())) {
      eval_internal(env, o->cdr->cdr->car);
      stack_pop();
      eval_internal(env, o->cdr->car);
      if(eval_error) {
        return;
      }
    }
    stack_push(nil);
  }
  else if(HEAD_EQ("if")) {
    #if LABELED_DISPATCH
  dispatch_if:;
    #endif
    assert_or_set_error(o->cdr->car, "Too few body forms in 'if' form: ", o);
    assert_or_set_error(o->cdr->cdr->car, "Too few body forms in 'if' form: ", o);
    assert_or_set_error(o->cdr->cdr->cdr->car, "Too few body forms in 'if' form: ", o);
    assert_or_set_error(o->cdr->cdr->cdr->cdr->car == NULL, "Too many body forms in 'if' form (use explicit 'do').", o);
    eval_internal(env, o->cdr->car);
    if(eval_error) {
      return;
    }
    else if(is_true(stack_pop())) {
      eval_internal(env, o->cdr->cdr->car);
    }
    else {
      eval_internal(env, o->cdr->cdr->cdr->car);
    }
  }
  else if(HEAD_EQ("match")) {
    #if LABELED_DISPATCH
  dispatch_match:;
    #endif
    eval_internal(env, o->cdr->car);
    if(eval_error) { return; }
    Obj *value = stack_pop();
    Obj *p = o->cdr->cdr;   
    match(env, value, p);
  }
  else if(HEAD_EQ("reset!")) {
    #if LABELED_DISPATCH
  dispatch_reset:;
    #endif
    assert_or_set_error(o->cdr->car->tag == 'Y', "Must use 'reset!' on a symbol.", o->cdr->car);
    Obj *pair = env_lookup_binding(env, o->cdr->car);
    if(!pair->car || pair->car->tag != 'Y') {
      printf("Can't reset! binding '%s', it's '%s'\n", o->cdr->car->s, obj_to_string(pair)->s);
      stack_push(nil);
      return;
    }
    eval_internal(env, o->cdr->cdr->car);
    if(eval_error) { return; }
    pair->cdr = stack_pop();
    stack_push(pair->cdr);
  }
  else if(HEAD_EQ("fn")) {
    #if LABELED_DISPATCH
  dispatch_fn:;
    #endif
    assert_or_set_error(o->cdr, "Lambda form too short (no parameter list or body).", o);
    assert_or_set_error(o->cdr->car, "No parameter list in lambda.", o);
    Obj *params = o->cdr->car;
    if(params->tag == 'C') {
      static int depcount = 0;
      depcount++;
      //printf("NOTE: Please use [] in lambda parameter list now, () is deprecated. %d\n", depcount); // %s, %d:%d\n", file_path, line, pos);
    }
    assert_or_set_error(o->cdr->cdr, "Lambda form too short (no body).", o);
    assert_or_set_error(o->cdr->cdr->car, "No body in lambda: ", o);
    Obj *body = o->cdr->cdr->car;
    //printf("Creating lambda with env: %s\n", obj_to_string(env)->s);
    Obj *lambda = obj_new_lambda(params, body, env, o);
    obj_copy_meta(lambda, o);
    stack_push(lambda);
  }
  else if(HEAD_EQ("macro")) {
    #if LABELED_DISPATCH
  dispatch_macro:;
    #endif
    assert_or_set_error(o->cdr, "Macro form too short (no parameter list or body): ", o);
    assert_or_set_error(o->cdr->car, "No parameter list in macro: ", o);
    Obj *params = o->cdr->car;
    assert_or_set_error(o->cdr->cdr, "Macro form too short (no body): ", o);
    assert_or_set_error(o->cdr->cdr->car, "No body in macro: ", o);
    Obj *body = o->cdr->cdr->car;
    Obj *macro = obj_new_macro(params, body, env, o);
    obj_copy_meta(macro, o);
    stack_push(macro);
  }
  else if(HEAD_EQ("def")) {
    #if LABELED_DISPATCH
  dispatch_def:;
    #endif
    assert_or_set_error(o->cdr, "Too few args to 'def': ", o);
    assert_or_set_error(o->cdr->car, "Can't assign to nil: ", o);
    assert_or_set_error(o->cdr->car->tag == 'Y', "Can't assign to non-symbol: ", o);
    Obj *key = o->cdr->car;
    eval_internal(env, o->cdr->cdr->car); // eval the second arg to 'def', the value to assign
    if(eval_error) { return; } // don't define it if there was an error
    Obj *val = stack_pop();
    global_env_extend(key, val);
    //printf("def %s to %s\n", obj_to_string(key)->s, obj_to_string(val)->s);
    //obj_set_meta(val, obj_new_keyword("name"), obj_to_string(key));
    stack_push(val);
  }
  else if(HEAD_EQ("def?")) {
    #if LABELED_DISPATCH
  dispatch_defp:;
    #endif
    //assert_or_set_error(o->cdr, "Too few args to 'def?': ", o);
    //assert_or_set_error(o->cdr->cdr, "Too few args to 'def?': ", o);
    eval_internal(env, o->cdr->car);
    if(eval_error) { return; }
    Obj *key = stack_pop();
    assert_or_set_error(key->tag == 'Y', "Can't call 'def?' on non-symbol: ", key);
    if(obj_eq(nil, env_lookup_binding(global_env, key))) {
      stack_push(lisp_false);
    } else {
      stack_push(lisp_true);
    }
  }
  else if(HEAD_EQ("ref")) {
    #if LABELED_DISPATCH
  dispatch_ref:;
    #endif
    assert_or_set_error(o->cdr, "Too few args to 'ref': ", o);
    eval_internal(env, o->cdr->car);
  }
  else if(HEAD_EQ("catch-error")) {
    #if LABELED_DISPATCH
  dispatch_catch:;
    #endif
    assert_or_set_error(o->cdr, "Too few args to 'catch-error': ", o);
    int shadow_stack_size_save = shadow_stack_pos;
    int stack_size_save = stack_pos;
    int function_trace_save = function_trace_pos;
    eval_internal(env, o->cdr->car);

    shadow_stack_pos = shadow_stack_size_save;
    stack_pos = stack_size_save + 1;
    function_trace_pos = function_trace_save;

    if(eval_error) {      
      stack_push(eval_error);
      eval_error = NULL;
      return;
    }
    else {
      stack_pop();
      stack_push(nil);
      return;
    }
  }
  else if(HEAD_EQ("macroexpand")) {
    assert_or_set_error(o->cdr, "Wrong argument count to 'macroexpand'.", nil);
    in_macro_expansion = true; // TODO: this is an ugly global variable to avoid threading of state 
    eval_internal(env, o->cdr->car);
    in_macro_expansion = false;
  }
  else {
    #if LABELED_DISPATCH
  dispatch_function_evaluation:;
    #endif
    
    shadow_stack_push(o);
    
    // Lambda, primop or macro   
    eval_internal(env, o->car);
    if(eval_error) { return; }
    
    Obj *function = stack_pop();
    assert_or_set_error(function, "Can't call NULL.", o);
    shadow_stack_push(function);
    
    bool eval_args = function->tag != 'M'; // macros don't eval their args
    Obj *p = o->cdr;
    int count = 0;
    
    while(p && p->car) {
      if(eval_error) {
        shadow_stack_pop();
        return;
      }
      
      if(eval_args) {
        eval_internal(env, p->car);
      }
      else {
        stack_push(p->car); // push non-evaled
      }
      count++;
      p = p->cdr;
    }

    if(eval_error) {
      shadow_stack_pop();
      return;
    }

    //printf("Popping args!\n");
    Obj **args = NULL;
    if(count > 0) {
      args = malloc(sizeof(Obj*) * count);
    }
    for(int i = 0; i < count; i++) {
      Obj *arg = stack_pop();
      args[count - i - 1] = arg;
      shadow_stack_push(arg);
    }

    if(function->tag == 'M') {
      Obj *calling_env = obj_new_environment(function->env);
      env_extend_with_args(calling_env, function, count, args, true);
      shadow_stack_push(calling_env);
      eval_internal(calling_env, function->body);
      if (eval_error) { free(args); return; }
      Obj *expanded = stack_pop();
      if(SHOW_MACRO_EXPANSION) {
        //printf("Meta of macro: %s\n", obj_to_string(function->meta)->s);
        printf("Expanded macro: %s\n", obj_to_string(expanded)->s);
      }
      shadow_stack_push(expanded);
      if(in_macro_expansion) {
        stack_push(expanded);
      } else {
        eval_internal(env, expanded);
      }
      if(eval_error) {
        return;
      }
      Obj *pop1 = shadow_stack_pop(); // expanded
      Obj *pop2 = shadow_stack_pop(); // calling_env
      assert(pop1 == expanded);
      assert(pop2 == calling_env);
    }
    else {
      if(function_trace_pos > STACK_SIZE - 1) {
        printf("Out of function trace stack.\n");
        stack_print();
        function_trace_print();
        exit(1);
      }

      if(LOG_FUNC_APPLICATION) {
        printf("evaluating form %s\n", obj_to_string(o)->s);
      }

      StackTraceCallSite call_site = { .caller = o, .callee = function };
      function_trace[function_trace_pos] = call_site;
      function_trace_pos++;

      //printf("apply start: "); obj_print_cout(function); printf("\n");
      apply(function, args, count);
      //printf("apply end\n");
      
      if(!eval_error) {
        function_trace_pos--;
      }
    }

    if(!eval_error) {
      //printf("time to pop!\n");
      for(int i = 0; i < count; i++) {
        shadow_stack_pop();
      }
      Obj *pop = shadow_stack_pop();
      assert(pop == function);
      
      Obj *oo = shadow_stack_pop(); // o
      if(o != oo) {
        printf("o != oo\n");
        printf("o: %p ", o); obj_print_cout(o); printf("\n");
        printf("oo: %p ", oo); obj_print_cout(oo); printf("\n");
        assert(false);
      }
    }

    free(args);
  }
}

void eval_internal(Obj *env, Obj *o) {
  if(eval_error) { return; }

  //shadow_stack_print();
  
  if(LOG_EVAL) {
    printf("> "); obj_print_cout(o); printf("\n");
  }
  if(obj_total > obj_total_max) {
    //printf("obj_total = %d\n", obj_total);
    if(LOG_GC_POINTS) {
      printf("Running GC in eval:\n");
    }
    gc(global_env);
    obj_total_max += 1000;
    //printf("new obj_total_max = %d\n", obj_total_max);
  }
  else {
    //printf("%d/%d\n", obj_total, obj_total_max);
  }

  if(!o) {
    stack_push(nil);
  }
  else if(o->tag == 'C') {
    eval_list(env, o);
  }
  else if(o->tag == 'E') {
    Obj *new_env = obj_copy(o);
    obj_copy_meta(new_env, o);
    shadow_stack_push(new_env);
    Obj *p = new_env->bindings;
    while(p && p->car) {
      Obj *pair = p->car;
      eval_internal(env, pair->cdr);
      if(eval_error) {
        return;
      }
      //printf("Evaling env-binding %s, setting cdr to %s.\n", obj_to_string(pair)->s, obj_to_string(stack[stack_pos - 1])->s);
      pair->cdr = stack_pop();
      p = p->cdr;
    }
    stack_push(new_env);
    Obj *pop = shadow_stack_pop(); // new_env
    assert(pop == new_env);
  }
  else if(o->tag == 'A') {
    Obj *new_array = obj_new_array(o->count);
    obj_copy_meta(new_array, o);
    shadow_stack_push(new_array);
    for(int i = 0; i < o->count; i++) {
      eval_internal(env, o->array[i]);
      if(eval_error) {
        return;
      }
      new_array->array[i] = stack_pop();
    }
    stack_push(new_array);
    Obj *pop = shadow_stack_pop(); // new_array
    assert(pop == new_array);
  }
  else if(o->tag == 'Y') {
    Obj *result = env_lookup(env, o);
    if(!result) {
      char buffer[256];
      snprintf(buffer, 256, "Can't find '%s' in environment.", obj_to_string(o)->s);
      eval_error = obj_new_string(buffer);
      stack_push(nil);
    } else {
      stack_push(result);
    }
  }
  else {
    stack_push(o);
  }
}

Obj *eval(Obj *env, Obj *form) {
  eval_error = NULL;
  //function_trace_pos = 0;
  eval_internal(env, form);
  Obj *result = stack_pop();
  return result;
}

void eval_text(Obj *env, char *text, bool print, Obj *filename) {
  Obj *forms = read_string(env, text, filename);
  Obj *form = forms;
  stack_push(forms);
  while(form && form->car) {
    Obj *result = eval(env, form->car);
    if(eval_error) {
      Obj *lookup_message = NULL;
      if(eval_error->tag == 'E') {
        lookup_message = env_lookup(eval_error, obj_new_keyword("message"));
      }
      if(lookup_message) {
        printf("\e[31m%s\e[0m\n", obj_to_string_not_prn(lookup_message)->s);
      } else {
        printf("\e[31mERROR: %s\e[0m\n", obj_to_string_not_prn(eval_error)->s);
      }
      bool show_stacktrace = true;
      if(eval_error->tag == 'E') {
        Obj *lookup_show_stacktrace = env_lookup(eval_error, obj_new_keyword("show-stacktrace"));
        if(lookup_show_stacktrace && !is_true(lookup_show_stacktrace)) {
          show_stacktrace = false;
        }
      }
      if(show_stacktrace) {
        function_trace_print();
      }
      /* printf("\n"); */
      /* stack_print(); */
      eval_error = NULL;
      if(LOG_GC_POINTS) {
        printf("Running GC after error occured:\n");
      }
      gc(env);
      return;
    }
    if(print) {
      if(result) {
        obj_print(result);
      }
      else {
        printf("Result was NULL when evaling %s\n", obj_to_string(form->car)->s);
      }
      printf("\n");
    }
    form = form->cdr;
    if(GC_COLLECT_AFTER_EACH_FORM) {
      if(LOG_GC_POINTS) {
        printf("Running GC after evaluation of single form in eval_text:\n");
      }
      gc(env);
    }
  }
  stack_pop(); // pop the 'forms' that was pushed above
}


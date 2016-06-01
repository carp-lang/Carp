#include "eval.h"
#include "env.h"
#include "assertions.h"
#include "reader.h"
#include "gc.h"
#include "primops.h"
#include "obj.h"
#include "obj_conversions.h"
#include "constants.h"
#include "../shared/types.h"
#include "match.h"

#define LOG_EVAL 0
#define SHOW_MACRO_EXPANSION 0
#define LOG_FUNC_APPLICATION 0
#define GC_COLLECT_AFTER_EACH_FORM 0

bool in_macro_expansion = false;

void call_struct_constructor(Process *process, Obj *function, Obj **args, int arg_count) {
  // Evaluation of a struct-definition (a dictionary) in function position (which means that it is used as a constructor)
  Obj *name_obj = env_lookup(process, function, obj_new_keyword("name"));
  assert_or_set_error(name_obj, "no key 'name' on struct definition: ", function);
  char *name = name_obj->s;

  Obj *struct_size_obj = env_lookup(process, function, obj_new_keyword("size"));
  assert_or_set_error(struct_size_obj, "no key 'size' on struct definition: ", function);
  int struct_size = struct_size_obj->i;

  Obj *struct_member_count_obj = env_lookup(process, function, obj_new_keyword("member-count"));
  assert_or_set_error(struct_member_count_obj, "no key 'member-count' on struct definition: ", function);
  int member_count = struct_member_count_obj->i;
    
  Obj *offsets_obj = env_lookup(process, function, obj_new_keyword("member-offsets"));
  assert_or_set_error(offsets_obj, "no key 'member-offsets' on struct definition: ", function);
  assert_or_set_error(offsets_obj->tag == 'A', "offsets must be an array: ", function);
  Obj **offsets = offsets_obj->array;
    
  Obj *member_types_obj = env_lookup(process, function, obj_new_keyword("member-types"));
  assert_or_set_error(member_types_obj, "no key 'member-types' on struct definition: ", function);
  assert_or_set_error(member_types_obj->tag == 'A', "member-types must be an array: ", function);
  Obj **member_types = member_types_obj->array;
   
  //printf("Will create a %s of size %d and member count %d.\n", name, size, member_count);
  void *p = malloc(struct_size);
  Obj *new_struct = obj_new_ptr(p);

  shadow_stack_push(process, new_struct);
    
  if(!new_struct->meta) {
    new_struct->meta = obj_new_environment(NULL);
  }
  env_assoc(process, new_struct->meta, obj_new_keyword("type"), obj_new_keyword(name));

  assert_or_set_error(!(arg_count < member_count), "Too few args to struct constructor: ", obj_new_string(name));
  assert_or_set_error(!(arg_count > member_count), "Too many args to struct constructor: ", obj_new_string(name));
    
  for(int i = 0; i < arg_count; i++) {
    Obj *member_type = member_types[i];
    int offset = offsets[i]->i;
    if(args[i]->tag == 'V') {
      assert_or_set_error(obj_eq(process, member_type, type_float), "Can't assign float to a member of type ", obj_to_string(process, member_type));
      float *fp = (float*)(((char*)new_struct->void_ptr) + offset);
      float f = args[i]->f32;
      //printf("Setting member %d at offset %d to %f.\n", i, offset, f);
      *fp = f;
    }
    else if(args[i]->tag == 'I') {
      assert_or_set_error(obj_eq(process, member_type, type_int), "Can't assign int to a member of type ", obj_to_string(process, member_type));
      int *xp = (int*)(((char*)new_struct->void_ptr) + offset);
      int x = args[i]->i;
      *xp = x;
    }
    else if(args[i]->tag == 'B') {
      assert_or_set_error(obj_eq(process, member_type, type_bool), "Can't assign bool to a member of type ", obj_to_string(process, member_type));
      bool *xp = (bool*)(((char*)new_struct->void_ptr) + offset);
      bool x = args[i]->boolean;
      *xp = x;
    }
    else if(args[i]->tag == 'Q') {
      assert_or_set_error(!obj_eq(process, member_type, type_char), "Can't assign char to a member of type ", obj_to_string(process, member_type));
      assert_or_set_error(!obj_eq(process, member_type, type_int), "Can't assign int to a member of type ", obj_to_string(process, member_type));
      assert_or_set_error(!obj_eq(process, member_type, type_float), "Can't assign float to a member of type ", obj_to_string(process, member_type));
      assert_or_set_error(!obj_eq(process, member_type, type_string), "Can't assign string to a member of type ", obj_to_string(process, member_type));
      void **vp = (void**)(((char*)new_struct->void_ptr) + offset);
      *vp = args[i]->void_ptr;
    }
    else if(args[i]->tag == 'S') {
      assert_or_set_error(obj_eq(process, member_type, type_string), "Can't assign int to a member of type ", obj_to_string(process, member_type));
      char **sp = (char**)(((char*)new_struct->void_ptr) + offset);
      *sp = strdup(args[i]->s); // must strdup or the struct will ref Obj's on the stack that will get gc:ed
    }
    else if(args[i]->tag == 'T') {
      assert_or_set_error(obj_eq(process, member_type, type_char), "Can't assign char to a member of type ", obj_to_string(process, member_type));
      char *cp = (char*)(((char*)new_struct->void_ptr) + offset);
      *cp = args[i]->character;
    }
    else if(args[i]->tag == 'A') {
      //assert_or_set_error(obj_eq(member_type, type_array), "Can't assign array to a member of type ", obj_to_string(member_type));

      // TODO: use this code for sending arrays to normal FFI functions too!!!
      // TODO: use the SAME code for sending data to FFI and struct constructors.
      // TODO: check that we send the expected type to the constructor
        
      Array *a = obj_array_to_carp_array(process, args[i]);
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
      obj_string_mut_append(eval_error, obj_to_string(process, args[i])->s);
      obj_string_mut_append(eval_error, " (unhandled type).");
      return;
    }
  }
  shadow_stack_pop(process); // pop new_struct
  stack_push(process, new_struct);
}

void apply(Process *process, Obj *function, Obj **args, int arg_count) {
  if(function->tag == 'L') {

    //printf("Calling function "); obj_print_cout(function); printf(" with params: "); obj_print_cout(function->params); printf("\n");
    
    Obj *calling_env = obj_new_environment(function->env);
    bool allow_rest_args = true;
    env_extend_with_args(process, calling_env, function, arg_count, args, allow_rest_args);
    //printf("Lambda env: %s\n", obj_to_string(calling_env)->s);

    shadow_stack_push(process, function);
    shadow_stack_push(process, calling_env);
    
    eval_internal(process, calling_env, function->body);
    if(eval_error) {
      return;
    }
    
    Obj *pop1 = shadow_stack_pop(process);
    Obj *pop2 = shadow_stack_pop(process);
    assert(pop1 == calling_env);
    assert(pop2 == function);
  }
  else if(function->tag == 'P') {   
    Obj *result = function->primop((struct Process*)process, args, arg_count);
    stack_push(process, result);
  }
  else if(function->tag == 'F') {
    call_foreign_function(process, function, args, arg_count);
  }
  else if(function->tag == 'K') {
    if(arg_count != 1) {
      eval_error = obj_new_string("Args to keyword lookup must be a single arg.");
    }
    else if(args[0]->tag != 'E') {
      eval_error = obj_new_string("Arg 0 to keyword lookup must be a dictionary: ");
      obj_string_mut_append(eval_error, obj_to_string(process, args[0])->s);
    }
    else {
      Obj *value = env_lookup(process, args[0], function);
      if(value) {
        stack_push(process, value);
      } else {
        eval_error = obj_new_string("Failed to lookup keyword '");
        obj_string_mut_append(eval_error, obj_to_string(process, function)->s);
        obj_string_mut_append(eval_error, "'");
        obj_string_mut_append(eval_error, " in \n");
        obj_string_mut_append(eval_error, obj_to_string(process, args[0])->s);
        obj_string_mut_append(eval_error, "\n");
      }
    }
  } 
  else if(function->tag == 'E' && obj_eq(process, env_lookup(process, function, obj_new_keyword("struct")), lisp_true)) {
    //printf("Calling struct: %s\n", obj_to_string(process, function)->s);
    if(obj_eq(process, env_lookup(process, function, obj_new_keyword("generic")), lisp_true)) {
      printf("Calling generic struct constructor.\n");      
      //Obj *struct_name = env_lookup(process, function, obj_new_keyword("name"));
      Obj *carp_array = obj_new_array(arg_count);
      carp_array->array = args;      
      Obj *call_to_concretize_struct = obj_list(obj_new_symbol("concretize-struct-via-constructor-call"),
                                                //obj_new_keyword(struct_name->s),
                                                function,
                                                carp_array);      
      eval_internal(process, process->global_env, call_to_concretize_struct);
    } else {
      call_struct_constructor(process, function, args, arg_count);
    }
  } else {
    set_error("Can't call non-function: ", function);
  }
}

#define HEAD_EQ(str) (o->car->tag == 'Y' && strcmp(o->car->s, (str)) == 0)

void eval_list(Process *process, Obj *env, Obj *o) {
  assert(o);
  
  //printf("Evaling list %s\n", obj_to_string(o)->s);
  if(!o->car) {
    stack_push(process, o); // nil, empty list
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
      eval_internal(process, env, p->car);
      if(eval_error) { return; }
      p = p->cdr;
      if(p && p->car) {
        stack_pop(process); // remove result from form that is not last
      }
    }
  }
  else if(HEAD_EQ("let")) {
#if LABELED_DISPATCH
  dispatch_let:;
#endif
    Obj *let_env = obj_new_environment(env);
    shadow_stack_push(process, let_env);
    //Obj *p = o->cdr->car;
    assert_or_set_error(o->cdr->car, "No bindings in 'let' form: ", o);
    assert_or_set_error(o->cdr->car->tag == 'A', "Bindings in 'let' form must be an array: ", o);
    Obj *a = o->cdr->car;
    for(int i = 0; i < a->count; i += 2) {
      if(i + 1 == a->count) {
        set_error("Uneven nr of forms in let: ", o); // TODO: add error code for this kind of error, return error map instead
      }
      assert_or_set_error(a->array[i]->tag == 'Y', "Trying to bind to non-symbol in let form: ", a->array[i]);
      eval_internal(process, let_env, a->array[i + 1]);
      if(eval_error) { return; }
      Obj *value = stack_pop(process);
      env_extend(let_env, a->array[i], value);
      //printf("let %s to %s\n", obj_to_string(a->array[i])->s, obj_to_string(a->array[i + 1])->s);
      //obj_set_meta(value, obj_new_keyword("name"), a->array[i]); // TODO: only do this in certain situations
    }
    assert_or_set_error(o->cdr->cdr->car, "No body in 'let' form.", o);
    assert_or_set_error(o->cdr->cdr->cdr->car == NULL, "Too many body forms in 'let' form (use explicit 'do').", o);
    eval_internal(process, let_env, o->cdr->cdr->car);
    shadow_stack_pop(process); // let_env
  }
  else if(HEAD_EQ("not")) {
#if LABELED_DISPATCH
  dispatch_not:;
#endif
    Obj *p = o->cdr;
    while(p) {
      if(p->car) {
        eval_internal(process, env, p->car);
        if(eval_error) { return; }
        if(is_true(stack_pop(process))) {
          stack_push(process, lisp_false);
          return;
        }
      }
      p = p->cdr;
    }
    stack_push(process, lisp_true);
  }
  else if(HEAD_EQ("or")) {
#if LABELED_DISPATCH
  dispatch_or:;
#endif
    Obj *p = o->cdr;
    while(p) {
      if(p->car) {
        eval_internal(process, env, p->car);
        if(eval_error) { return; }
        if(is_true(stack_pop(process))) {
          stack_push(process, lisp_true);
          return;
        }
      }
      p = p->cdr;
    }
    stack_push(process, lisp_false);
  }
  else if(HEAD_EQ("and")) {
#if LABELED_DISPATCH
  dispatch_and:;
#endif
    Obj *p = o->cdr;
    while(p) {
      if(p->car) {
        eval_internal(process, env, p->car);
        if(eval_error) { return; }
        if(!is_true(stack_pop(process))) {
          stack_push(process, lisp_false);
          return;
        }
      }
      p = p->cdr;
    }
    stack_push(process, lisp_true);
  }
  else if(HEAD_EQ("quote")) {
#if LABELED_DISPATCH
  dispatch_quote:;
#endif
    if(o->cdr == nil) {
      stack_push(process, nil);
    } else {
      //assert_or_set_error(o->cdr->cdr->car, "Too many forms in 'quote' form: ", o);
      if(o->cdr->cdr->car) {
        printf("Too many forms in 'quote' form: %s\n", obj_to_string(process, o)->s);
      }
      stack_push(process, o->cdr->car);
    }
  }
  else if(HEAD_EQ("while")) {
#if LABELED_DISPATCH
  dispatch_while:;
#endif
    assert_or_set_error(o->cdr->car, "Too few body forms in 'while' form: ", o);
    assert_or_set_error(o->cdr->cdr->cdr->car == NULL, "Too many body forms in 'while' form (use explicit 'do').", o);
    eval_internal(process, env, o->cdr->car);
    if(eval_error) {
      return;
    }
    while(is_true(stack_pop(process))) {
      eval_internal(process, env, o->cdr->cdr->car);
      stack_pop(process);
      eval_internal(process, env, o->cdr->car);
      if(eval_error) {
        return;
      }
    }
    stack_push(process, nil);
  }
  else if(HEAD_EQ("if")) {
#if LABELED_DISPATCH
  dispatch_if:;
#endif
    assert_or_set_error(o->cdr->car, "Too few body forms in 'if' form: ", o);
    assert_or_set_error(o->cdr->cdr->car, "Too few body forms in 'if' form: ", o);
    assert_or_set_error(o->cdr->cdr->cdr->car, "Too few body forms in 'if' form: ", o);
    assert_or_set_error(o->cdr->cdr->cdr->cdr->car == NULL, "Too many body forms in 'if' form (use explicit 'do').", o);
    eval_internal(process, env, o->cdr->car);
    if(eval_error) {
      return;
    }
    else if(is_true(stack_pop(process))) {
      eval_internal(process, env, o->cdr->cdr->car);
    }
    else {
      eval_internal(process, env, o->cdr->cdr->cdr->car);
    }
  }
  else if(HEAD_EQ("match")) {
#if LABELED_DISPATCH
  dispatch_match:;
#endif
    eval_internal(process, env, o->cdr->car);
    if(eval_error) { return; }
    Obj *value = stack_pop(process);
    Obj *p = o->cdr->cdr;   
    match(process, env, value, p);
  }
  else if(HEAD_EQ("reset!")) {
#if LABELED_DISPATCH
  dispatch_reset:;
#endif
    assert_or_set_error(o->cdr->car->tag == 'Y', "Must use 'reset!' on a symbol.", o->cdr->car);
    Obj *pair = env_lookup_binding(process, env, o->cdr->car);
    if(!pair->car || pair->car->tag != 'Y') {
      printf("Can't reset! binding '%s', it's '%s'\n", o->cdr->car->s, obj_to_string(process, pair)->s);
      stack_push(process, nil);
      return;
    }
    
    eval_internal(process, env, o->cdr->cdr->car);
    if(eval_error) { return; }

    if(pair->cdr->tag == 'R' && pair->cdr->meta) {
      //pair->cdr->given_to_ffi = true; // needed?
      //printf("Resetting a ptr-to-global.\n");
      Obj *type_meta = env_lookup(process, pair->cdr->meta, obj_new_keyword("type"));
      if(type_meta && obj_eq(process, type_meta, type_int)) {
        int *ip = pair->cdr->void_ptr;
        *ip = stack_pop(process)->i;
      }
      else if(type_meta && obj_eq(process, type_meta, type_float)) {
        float *fp = pair->cdr->void_ptr;
        *fp = stack_pop(process)->f32;
      }
      else if(type_meta && obj_eq(process, type_meta, type_double)) {
        double *dp = pair->cdr->void_ptr;
        *dp = stack_pop(process)->f64;
      }
      else if(type_meta && obj_eq(process, type_meta, type_char)) {
        char *cp = pair->cdr->void_ptr;
        *cp = stack_pop(process)->character;
      }
      else if(type_meta && obj_eq(process, type_meta, type_bool)) {
        bool *bp = pair->cdr->void_ptr;
        *bp = stack_pop(process)->boolean;
      }
      else if(type_meta && obj_eq(process, type_meta, type_string)) {
        char **sp = pair->cdr->void_ptr;
        *sp = strdup(stack_pop(process)->s); // OBS! strdup!!! Without this the string will get GC:ed though...
      }
      else if(type_meta->tag == 'C' && type_meta->cdr->car && obj_eq(process, type_meta->car, obj_new_keyword("Array"))) {
        void **pp = pair->cdr->void_ptr;
        Obj *a = stack_pop(process);
        assert_or_set_error(a->tag == 'A', "Must reset! global to array: ", o);
        Array *carp_array = obj_array_to_carp_array(process, a);
        *pp = carp_array;
      }
      else {
        /* printf("No/invalid :type\n"); */
        /* pair->cdr = stack_pop(); */

        void **pp = pair->cdr->void_ptr;
        *pp = stack_pop(process)->void_ptr;
      }
    }
    else {
      pair->cdr = stack_pop(process);
    }
    stack_push(process, pair->cdr);
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
    stack_push(process, lambda);
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
    stack_push(process, macro);
  }
  else if(HEAD_EQ("def")) {
#if LABELED_DISPATCH
  dispatch_def:;
#endif
    assert_or_set_error(o->cdr, "Too few args to 'def': ", o);
    assert_or_set_error(o->cdr->car, "Can't assign to nil: ", o);
    assert_or_set_error(o->cdr->car->tag == 'Y', "Can't assign to non-symbol: ", o);
    Obj *key = o->cdr->car;
    eval_internal(process, env, o->cdr->cdr->car); // eval the second arg to 'def', the value to assign
    if(eval_error) { return; } // don't define it if there was an error
    Obj *val = stack_pop(process);
    global_env_extend(process, key, val);
    //printf("def %s to %s\n", obj_to_string(key)->s, obj_to_string(val)->s);
    //obj_set_meta(val, obj_new_keyword("name"), obj_to_string(key));
    stack_push(process, val);
  }
  else if(HEAD_EQ("def?")) {
#if LABELED_DISPATCH
  dispatch_defp:;
#endif
    //assert_or_set_error(o->cdr, "Too few args to 'def?': ", o);
    //assert_or_set_error(o->cdr->cdr, "Too few args to 'def?': ", o);
    eval_internal(process, env, o->cdr->car);
    if(eval_error) { return; }
    Obj *key = stack_pop(process);
    assert_or_set_error(key->tag == 'Y', "Can't call 'def?' on non-symbol: ", key);
    if(obj_eq(process, nil, env_lookup_binding(process, process->global_env, key))) {
      stack_push(process, lisp_false);
    } else {
      stack_push(process, lisp_true);
    }
  }
  else if(HEAD_EQ("ref")) {
#if LABELED_DISPATCH
  dispatch_ref:;
#endif
    assert_or_set_error(o->cdr, "Too few args to 'ref': ", o);
    eval_internal(process, env, o->cdr->car);
  }
  else if(HEAD_EQ("catch-error")) {
#if LABELED_DISPATCH
  dispatch_catch:;
#endif
    assert_or_set_error(o->cdr, "Too few args to 'catch-error': ", o);
    int shadow_stack_size_save = process->shadow_stack_pos;
    int stack_size_save = process->stack_pos;
    int function_trace_save = process->function_trace_pos;
    eval_internal(process, env, o->cdr->car);

    process->shadow_stack_pos = shadow_stack_size_save;
    process->stack_pos = stack_size_save + 1;
    process->function_trace_pos = function_trace_save;

    if(eval_error) {      
      stack_push(process, eval_error);
      eval_error = NULL;
      return;
    }
    else {
      stack_pop(process);
      stack_push(process, nil);
      return;
    }
  }
  else if(HEAD_EQ("macroexpand")) {
    assert_or_set_error(o->cdr, "Wrong argument count to 'macroexpand'.", nil);
    in_macro_expansion = true; // TODO: this is an ugly global variable to avoid threading of state 
    eval_internal(process, env, o->cdr->car);
    in_macro_expansion = false;
  }
  else {
#if LABELED_DISPATCH
  dispatch_function_evaluation:;
#endif
    
    shadow_stack_push(process, o);
    
    // Lambda, primop or macro   
    eval_internal(process, env, o->car);
    if(eval_error) { return; }
    
    Obj *function = stack_pop(process);
    assert_or_set_error(function, "Can't call NULL.", o);
    shadow_stack_push(process, function);
    
    bool eval_args = function->tag != 'M'; // macros don't eval their args
    Obj *p = o->cdr;
    int count = 0;
    
    while(p && p->car) {
      if(eval_error) {
        shadow_stack_pop(process);
        return;
      }
      
      if(eval_args) {
        eval_internal(process, env, p->car);
      }
      else {
        stack_push(process, p->car); // push non-evaled
      }
      count++;
      p = p->cdr;
    }

    if(eval_error) {
      shadow_stack_pop(process);
      return;
    }

    //printf("Popping args!\n");
    Obj **args = NULL;
    if(count > 0) {
      args = malloc(sizeof(Obj*) * count);
    }
    for(int i = 0; i < count; i++) {
      Obj *arg = stack_pop(process);
      args[count - i - 1] = arg;
      shadow_stack_push(process, arg);
    }

    if(function->tag == 'M') {
      Obj *calling_env = obj_new_environment(function->env);
      env_extend_with_args(process, calling_env, function, count, args, true);
      shadow_stack_push(process, calling_env);
      eval_internal(process, calling_env, function->body);
      if (eval_error) { free(args); return; }
      Obj *expanded = stack_pop(process);
      if(SHOW_MACRO_EXPANSION) {
        //printf("Meta of macro: %s\n", obj_to_string(function->meta)->s);
        printf("Expanded macro: %s\n", obj_to_string(process, expanded)->s);
      }
      shadow_stack_push(process, expanded);
      if(in_macro_expansion) {
        stack_push(process, expanded);
      } else {
        eval_internal(process, env, expanded);
      }
      if(eval_error) {
        return;
      }
      Obj *pop1 = shadow_stack_pop(process); // expanded
      Obj *pop2 = shadow_stack_pop(process); // calling_env
      assert(pop1 == expanded);
      assert(pop2 == calling_env);
    }
    else {
      if(process->function_trace_pos > STACK_SIZE - 1) {
        printf("Out of function trace stack.\n");
        stack_print(process);
        function_trace_print(process);
        exit(1);
      }

      if(LOG_FUNC_APPLICATION) {
        printf("evaluating form %s\n", obj_to_string(process, o)->s);
      }

      StackTraceCallSite call_site = { .caller = o, .callee = function };
      process->function_trace[process->function_trace_pos] = call_site;
      process->function_trace_pos++;

      //printf("apply start: "); obj_print_cout(function); printf("\n");
      apply(process, function, args, count);
      //printf("apply end\n");
      
      if(!eval_error) {
        process->function_trace_pos--;
      }
    }

    if(!eval_error) {
      //printf("time to pop!\n");
      for(int i = 0; i < count; i++) {
        shadow_stack_pop(process);
      }
      Obj *pop = shadow_stack_pop(process);
      assert(pop == function);
      
      Obj *oo = shadow_stack_pop(process); // o
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

void eval_internal(Process *process, Obj *env, Obj *o) {
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
    gc(process);
    obj_total_max += 1000;
    //printf("new obj_total_max = %d\n", obj_total_max);
  }
  else {
    //printf("%d/%d\n", obj_total, obj_total_max);
  }

  if(!o) {
    stack_push(process, nil);
  }
  else if(o->tag == 'C') {
    eval_list(process, env, o);
  }
  else if(o->tag == 'E') {
    Obj *new_env = obj_copy(o);
    obj_copy_meta(new_env, o);
    shadow_stack_push(process, new_env);
    Obj *p = new_env->bindings;
    while(p && p->car) {
      Obj *pair = p->car;
      eval_internal(process, env, pair->cdr);
      if(eval_error) {
        return;
      }
      //printf("Evaling env-binding %s, setting cdr to %s.\n", obj_to_string(pair)->s, obj_to_string(stack[stack_pos - 1])->s);
      pair->cdr = stack_pop(process);
      p = p->cdr;
    }
    stack_push(process, new_env);
    Obj *pop = shadow_stack_pop(process); // new_env
    assert(pop == new_env);
  }
  else if(o->tag == 'A') {
    Obj *new_array = obj_new_array(o->count);
    obj_copy_meta(new_array, o);
    shadow_stack_push(process, new_array);
    for(int i = 0; i < o->count; i++) {
      eval_internal(process, env, o->array[i]);
      if(eval_error) {
        return;
      }
      new_array->array[i] = stack_pop(process);
    }
    stack_push(process, new_array);
    Obj *pop = shadow_stack_pop(process); // new_array
    assert(pop == new_array);
  }
  else if(o->tag == 'Y') {
    Obj *result = env_lookup(process, env, o);
    if(!result) {
      char buffer[256];
      snprintf(buffer, 256, "Can't find '%s' in environment.", obj_to_string(process, o)->s);
      eval_error = obj_new_string(buffer);
      stack_push(process, nil);
    } else {
      stack_push(process, result);
    }
  }
  else {
    stack_push(process, o);
  }
}

Obj *eval(Process *process, Obj *env, Obj *form) {
  eval_error = NULL;
  //function_trace_pos = 0;
  eval_internal(process, env, form);
  Obj *result = stack_pop(process);
  return result;
}

void eval_text(Process *process, Obj *env, char *text, bool print, Obj *filename) {
  Obj *forms = read_string(process, env, text, filename);
  Obj *form = forms;
  stack_push(process, forms);
  while(form && form->car) {
    Obj *result = eval(process, env, form->car);
    if(eval_error) {
      Obj *lookup_message = NULL;
      if(eval_error->tag == 'E') {
        lookup_message = env_lookup(process, eval_error, obj_new_keyword("message"));
      }
      if(lookup_message) {
        printf("\e[31m%s\e[0m\n", obj_to_string_not_prn(process, lookup_message)->s);
      } else {
        printf("\e[31mERROR: %s\e[0m\n", obj_to_string_not_prn(process, eval_error)->s);
      }
      bool show_stacktrace = true;
      if(eval_error->tag == 'E') {
        Obj *lookup_show_stacktrace = env_lookup(process, eval_error, obj_new_keyword("show-stacktrace"));
        if(lookup_show_stacktrace && !is_true(lookup_show_stacktrace)) {
          show_stacktrace = false;
        }
      }
      if(show_stacktrace) {
        function_trace_print(process);
      }
      /* printf("\n"); */
      /* stack_print(); */
      eval_error = NULL;
      if(LOG_GC_POINTS) {
        printf("Running GC after error occured:\n");
      }
      gc(process);
      return;
    }
    if(print) {
      if(result) {
        obj_print(process, result);
      }
      else {
        printf("Result was NULL when evaling %s\n", obj_to_string(process, form->car)->s);
      }
      printf("\n");
    }
    form = form->cdr;
    if(GC_COLLECT_AFTER_EACH_FORM) {
      if(LOG_GC_POINTS) {
        printf("Running GC after evaluation of single form in eval_text:\n");
      }
      gc(process);
    }
  }
  stack_pop(process); // pop the 'forms' that was pushed above
}


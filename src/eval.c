#include "eval.h"
#include "env.h"
#include "assertions.h"
#include "reader.h"
#include "gc.h"
#include "primops.h"

#define LOG_EVAL 0
#define LOG_STACK 0
#define LOG_SHADOW_STACK 0
#define SHOW_MACRO_EXPANSION 0
#define LOG_FUNC_APPLICATION 0
#define GC_COLLECT_AFTER_EACH_FORM 0

#define STACK_TRACE_LEN 256
char function_trace[STACK_SIZE][STACK_TRACE_LEN];
int function_trace_pos;

void stack_print() {
  printf("----- STACK -----\n");
  for(int i = 0; i < stack_pos; i++) {
    printf("%d\t%s\n", i, obj_to_string(stack[i])->s);
  }
  printf("-----  END  -----\n\n");
}

void stack_push(Obj *o) {
  if(LOG_STACK) {
    printf("Pushing %s\n", obj_to_string(o)->s);
  }
  if(stack_pos >= STACK_SIZE) {
    printf("Stack overflow.");
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
    printf("Stack underflow.");
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
  for(int i = 0; i < stack_pos - 1; i++) {
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
  if(shadow_stack_pos >= STACK_SIZE) {
    printf("Shadow stack overflow.");
    shadow_stack_print();
    exit(1);
  }
  shadow_stack[shadow_stack_pos++] = o;
}

Obj *shadow_stack_pop() {
  if(shadow_stack_pos <= 0) {
    printf("Shadow stack underflow.");
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
    printf("%3d  %s\n", i, function_trace[i]);
  }
  printf(" ----------------------------------------------------------------\n");
}

bool obj_match(Obj *env, Obj *attempt, Obj *value);

bool obj_match_lists(Obj *env, Obj *attempt, Obj *value) {
  //printf("Matching list %s with %s\n", obj_to_string(attempt)->s, obj_to_string(value)->s);
  Obj *p1 = attempt;
  Obj *p2 = value;
  while(p1 && p1->car) {
    if(obj_eq(p1->car, ampersand) && p1->cdr && p1->cdr->car) {
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
    if(obj_eq(o, ampersand) && ((i + 1) < attempt->count)) {
      int rest_count = value->count - i;
      //printf("rest_count: %d\n", rest_count);
      Obj *rest = obj_new_array(rest_count);
      for(int j = 0; j < rest_count; j++) {
	rest->array[j] = value->array[i + j]; // copy the rest of the objects to a smaller array
      }
      //printf("rest: %s\n", obj_to_string(rest)->s);
      Obj *symbol_after_ampersand = attempt->array[i + 1];
      //printf("symbol_after_ampersand: %s\n", obj_to_string(symbol_after_ampersand)->s);
      bool matched_rest = obj_match(env, symbol_after_ampersand, rest);
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
    /* 	   obj_to_string(attempt)->s, */
    /* 	   attempt->tag, */
    /* 	   obj_to_string(value)->s, */
    /* 	   value->tag); */
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
      shadow_stack_pop(); // new_env
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

void apply(Obj *function, Obj **args, int arg_count) {
  if(function->tag == 'L') {

    //printf("Calling function "); obj_print_cout(function); printf(" with params: "); obj_print_cout(function->params); printf("\n");
    
    Obj *calling_env = obj_new_environment(function->env);
    env_extend_with_args(calling_env, function, arg_count, args);
    //printf("Lambda env: %s\n", obj_to_string(calling_env)->s);

    shadow_stack_push(function);
    shadow_stack_push(calling_env);
    eval_internal(calling_env, function->body);
    shadow_stack_pop();
    shadow_stack_pop();
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
     
    void **values = malloc(sizeof(void*) * arg_count);
	assert(values);

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
	  assert_or_set_error(args[i]->tag == 'I', "Invalid type of arg: ", args[i]);
	  values[i] = &args[i]->i;
	}
	else if(obj_eq(type_obj, type_bool)) {
	  assert_or_set_error(args[i]->tag == 'Y', "Invalid type of arg: ", args[i]);
	  bool b = is_true(args[i]);
	  values[i] = &b;
	}
	else if(obj_eq(type_obj, type_char)) {
	  assert_or_set_error(args[i]->tag == 'B', "Invalid type of arg: ", args[i]);
	  char b = args[i]->b;
	  values[i] = &b;
	}
	else if(obj_eq(type_obj, type_float)) {
	  assert_or_set_error(args[i]->tag == 'V', "Invalid type of arg: ", args[i]);
	  values[i] = &args[i]->f32;
	}
	else if(obj_eq(type_obj, type_string)) {
	  assert_or_set_error(args[i]->tag == 'S', "Invalid type of arg: ", args[i]);
	  args[i]->s = strdup(args[i]->s); // OBS! Duplicating string here. TODO: Think about if this is the correct thing to do!
	  values[i] = &args[i]->s;
	}
	else {
	  //printf("Calling function with parameter of type %s. Argument is of type %c.\n", obj_to_string(p->car)->s, args[i]->tag);
	  if(args[i]->tag == 'Q') {
	    values[i] = &args[i]->void_ptr;
	  }
	  else if(args[i]->tag == 'F') {
	    values[i] = &args[i]->funptr;
	  }
	  else if(args[i]->tag == 'L') {
	    set_error("Can't send argument of lambda type (tag 'L') to ffi function, you need to compile it to a C function using (bake ...) first:\n", args[i]);
	  }
	  else {
	    printf("INVALID ARG TYPE: %c\n", args[i]->tag);
	    set_error("Can't send argument of invalid type to foreign function taking parameter of type ", p->car);
	  }
	}
	p = p->cdr;
      }
      else {
	set_error("Too many arguments to ", function);
      }	
    }

    if(p && p->car) {
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
	//printf("c is null");
	obj_result = obj_new_string("");
      }
      else {      
	obj_result = obj_new_string(c);
      }
    }
    else if(obj_eq(return_type, type_int)) { 
      //printf("Returning int.\n");
      int result;
      ffi_call(function->cif, function->funptr, &result, values);
      obj_result = obj_new_int(result);
    }
    else if(obj_eq(return_type, type_bool)) { 
      //printf("Returning bool.\n");
      int result;
      ffi_call(function->cif, function->funptr, &result, values);
      obj_result = result ? lisp_true : lisp_false;
    }
    else if(obj_eq(return_type, type_char)) { 
      char result;
      ffi_call(function->cif, function->funptr, &result, values);
      obj_result = obj_new_char(result);
    }
    else if(obj_eq(return_type, type_float)) { 
      //printf("Returning float.\n");
      float result;
      ffi_call(function->cif, function->funptr, &result, values);
      obj_result = obj_new_float(result);
    }
    else if(obj_eq(return_type, type_void)) { 
      //printf("Returning void.\n");
      int result;
      ffi_call(function->cif, function->funptr, &result, values);
      obj_result = nil;
    }
    else if(return_type->tag == 'C' && obj_eq(function->return_type->car, type_ptr)) {
      void *result;
      ffi_call(function->cif, function->funptr, &result, values);
      //printf("Creating new void* with value: %p\n", result);
      obj_result = obj_new_ptr(result);
    }
    else {
      //set_error("Returning what? ", function->return_type);
      // Assume it's a user defined type:
      void *result;
      ffi_call(function->cif, function->funptr, &result, values);
      obj_result = obj_new_ptr(result);
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
    char *name = env_lookup(function, obj_new_keyword("name"))->s;
    int size = env_lookup(function, obj_new_keyword("size"))->i;
    int member_count = env_lookup(function, obj_new_keyword("member-count"))->i;

    Obj *offsets_obj = env_lookup(function, obj_new_keyword("member-offsets"));
    assert_or_set_error(offsets_obj->tag == 'A', "offsets must be an array: ", function);
    Obj **offsets = offsets_obj->array;

    Obj *member_types_obj = env_lookup(function, obj_new_keyword("member-types"));
    assert_or_set_error(member_types_obj->tag == 'A', "member-types must be an array: ", function);
    Obj **member_types = member_types_obj->array;
    
    //printf("Will create a %s of size %d and member count %d.\n", name, size, member_count);
    void *p = malloc(sizeof(size));
    Obj *new_struct = obj_new_ptr(p);
    assert_or_set_error(!(arg_count < member_count), "Too few args to struct constructor: ", obj_new_string(name));
    assert_or_set_error(!(arg_count > member_count), "Too many args to struct constructor: ", obj_new_string(name));
    for(int i = 0; i < arg_count; i++) {
      Obj *member_type = member_types[i];
      int offset = offsets[i]->i;
      if(args[i]->tag == 'V') {
	assert_or_set_error(obj_eq(member_type, type_float), "Can't assign float to a member of type ", obj_to_string(member_type));
	float *fp = new_struct->void_ptr + offset;
	float f = args[i]->f32;
	//printf("Setting member %d at offset %d to %f.\n", i, offset, f);
	*fp = f;
      }
      else if(args[i]->tag == 'I') {
	assert_or_set_error(obj_eq(member_type, type_int), "Can't assign int to a member of type ", obj_to_string(member_type));
	int *xp = new_struct->void_ptr + offset;
	int x = args[i]->i;
	*xp = x;
      }
      else if(args[i]->tag == 'Q') {
	void **vp = new_struct->void_ptr + offset;
	*vp = args[i]->void_ptr;
      }
      else {
	eval_error = obj_new_string("Can't set member ");
	//obj_string_mut_append(eval_error, );
        obj_string_mut_append(eval_error, " of struct ");
	obj_string_mut_append(eval_error, name);
	obj_string_mut_append(eval_error, " to ");
	obj_string_mut_append(eval_error, obj_to_string(args[i])->s);
	return;
      }
    }
    stack_push(new_struct);
  }
  else if(function->tag == 'E' && obj_eq(env_lookup(function, obj_new_keyword("struct-lookup")), lisp_true)) {
    if(arg_count != 1) {
      eval_error = obj_new_string("Invalid arg count to struct member lookup: ");
      char buffer[32];
      snprintf(buffer, 32, "%d", arg_count);
      obj_string_mut_append(eval_error, buffer);
      return;
    }

    /* Obj *struct_description = env_lookup(function, obj_new_keyword("struct-ref")); */
    /* assert_or_set_error(struct_description->tag == 'E', "struct-lookup doesn't have a struct-ref: ", function); */
    /* printf("%s\n", obj_to_string(struct_description)->s); */

    Obj *offset_obj = env_lookup(function, obj_new_keyword("member-offset"));
    assert_or_set_error(offset_obj->tag == 'I', "struct-lookup has invalid member-offset: ", function);
    int offset = offset_obj->i;

    //printf("Looking up member at offset %d.\n", offset);

    Obj *member_type = env_lookup(function, obj_new_keyword("member-type"));
    Obj *target_struct = args[0];

    Obj *lookup = nil;
    void *location = target_struct->void_ptr + offset;
    
    if(obj_eq(member_type, type_float)) {
      float *fp = location;
      float x = *fp;
      lookup = obj_new_float(x);
    }
    else if(obj_eq(member_type, type_int)) {
      int *xp = location;
      int x = *xp;
      lookup = obj_new_int(x);
    }
    else {
      void **pp = location;
      void *p = *pp;
      lookup = obj_new_ptr(p);
    }

    stack_push(lookup);
  }
  else {
    set_error("Can't call non-function: ", function);
  }
}

#define HEAD_EQ(str) (o->car->tag == 'Y' && strcmp(o->car->s, (str)) == 0)

void eval_list(Obj *env, Obj *o) {
  assert(o);
  //printf("Evaling list %s\n", obj_to_string(o)->s);
  if(!o->car) {
    stack_push(o); // nil, empty list
  }
  else if(HEAD_EQ("do")) {
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
    Obj *let_env = obj_new_environment(env);
    shadow_stack_push(let_env);
    Obj *p = o->cdr->car;
    assert_or_set_error(o->cdr->car, "No bindings in 'let' form: ", o);
    assert_or_set_error(o->cdr->car->tag == 'A', "Bindings in 'let' form must be an array: ", o);
    Obj *a = o->cdr->car;
    for(int i = 0; i < a->count; i += 2) {
      if(i + 1 == a->count) {
	set_error("Uneven nr of forms in let: ", o); // TODO: add error code for this kind of error, return error map instead
      }
      assert_or_set_error(a->array[i]->tag == 'Y', "Must bind to symbol in let form: ", p->car);
      eval_internal(let_env, a->array[i + 1]);
      if(eval_error) { return; }
      env_extend(let_env, a->array[i], stack_pop());
    }
    assert_or_set_error(o->cdr->cdr->car, "No body in 'let' form.", o);
    assert_or_set_error(o->cdr->cdr->cdr->car == NULL, "Too many body forms in 'let' form (use explicit 'do').", o);
    eval_internal(let_env, o->cdr->cdr->car);
    shadow_stack_pop(); // let_env
  }
  else if(HEAD_EQ("not")) {
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
    if(o->cdr == nil) {
      stack_push(nil);
    } else {
      stack_push(o->cdr->car);
    }
  }
  else if(HEAD_EQ("while")) {
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
  else if(HEAD_EQ("defstruct")) {
    assert_or_set_error(o->cdr->car, "Too few forms in 'defstruct' form: ", o);
    assert_or_set_error(o->cdr->cdr->car, "Too few forms in 'defstruct' form: ", o);
    assert_or_set_error(o->cdr->cdr->cdr->car == NULL, "Too many forms in 'defstruct' form: ", o);

    assert_or_set_error(o->cdr->car->tag == 'Y', "First argument to 'defstruct' form must be a symbol (it's the name of the struct): ", o);
    assert_or_set_error(o->cdr->cdr->car->tag == 'A', "Second argument to 'defstruct' form must be an array (with the members, i.e. [x :float, y :float]): ", o);

    char *name = o->cdr->car->s;
    Obj *types = o->cdr->cdr->car;

    Obj *struct_description = obj_new_environment(NULL);
    env_extend(struct_description, obj_new_keyword("name"), obj_new_string(name));

    int member_count = types->count / 2;

    Obj *member_types = obj_new_array(member_count);    
    Obj *offsets = obj_new_array(member_count);
    int offset = 0;
    bool generic = false;
    for(int i = 0; i < member_count; i++) {
      Obj *member_name = types->array[i * 2];
      assert_or_set_error(member_name->tag == 'Y', "Struct member name must be symbol: ", member_name);
      Obj *member_type = types->array[i * 2 + 1];
      member_types->array[i] = member_type;
      offsets->array[i] = obj_new_int(offset);
      int size = 0;
      if(obj_eq(member_type, type_float)) { size = sizeof(float); }
      else if(obj_eq(member_type, type_int)) { size = sizeof(int); }
      else if(obj_eq(member_type, type_char)) { size = sizeof(char); }
      else { size = sizeof(void*); }

      char fixed_member_name[256];
      fixed_member_name[0] = '#';
      snprintf(fixed_member_name + 1, 255, "%s", member_name->s);

      Obj *struct_member_lookup = obj_new_environment(NULL);
      env_extend(struct_member_lookup, obj_new_keyword("struct-lookup"), lisp_true);
      env_extend(struct_member_lookup, obj_new_keyword("struct-ref"), struct_description); // immediate access to the struct description
      env_extend(struct_member_lookup, obj_new_keyword("member-offset"), obj_new_int(offset));
      env_extend(struct_member_lookup, obj_new_keyword("member-name"), member_name);
      env_extend(struct_member_lookup, obj_new_keyword("member-type"), member_type);
      
      env_extend(env, obj_new_symbol(fixed_member_name), struct_member_lookup);

      offset += size;
    }

    env_extend(struct_description, obj_new_keyword("member-offsets"), offsets);
    env_extend(struct_description, obj_new_keyword("member-count"), obj_new_int(member_count));
    env_extend(struct_description, obj_new_keyword("member-types"), member_types);
    env_extend(struct_description, obj_new_keyword("size"), obj_new_int(offset));
    env_extend(struct_description, obj_new_keyword("generic"), generic ? lisp_true : lisp_false);
    env_extend(struct_description, obj_new_keyword("struct"), lisp_true);

    env_extend(env, obj_new_symbol(name), struct_description);
    
    stack_push(struct_description);
  }
  else if(HEAD_EQ("match")) {
    eval_internal(env, o->cdr->car);
    if(eval_error) { return; }
    Obj *value = stack_pop();
    Obj *p = o->cdr->cdr;   
    match(env, value, p);
  }
  else if(HEAD_EQ("reset!")) {
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
    stack_push(lambda);
  }
  else if(HEAD_EQ("macro")) {
    assert_or_set_error(o->cdr, "Macro form too short (no parameter list or body): ", o);
    assert_or_set_error(o->cdr->car, "No parameter list in macro: ", o);
    Obj *params = o->cdr->car;
    assert_or_set_error(o->cdr->cdr, "Macro form too short (no body): ", o);
    assert_or_set_error(o->cdr->cdr->car, "No body in macro: ", o);
    Obj *body = o->cdr->cdr->car;
    Obj *macro = obj_new_macro(params, body, env, o);
    stack_push(macro);
  }
  else if(HEAD_EQ("def")) {
    assert_or_set_error(o->cdr, "Too few args to 'def': ", o);
    assert_or_set_error(o->cdr->car, "Can't assign to nil: ", o);
    assert_or_set_error(o->cdr->car->tag == 'Y', "Can't assign to non-symbol: ", o);
    Obj *key = o->cdr->car;
    eval_internal(env, o->cdr->cdr->car); // eval the second arg to 'def', the value to assign
    if(eval_error) { return; } // don't define it if there was an error
    Obj *val = stack_pop();
    global_env_extend(key, val);
    //printf("def %s to %s\n", obj_to_string(key)->s, obj_to_string(val)->s);
    stack_push(val);
  }
  else if(HEAD_EQ("def?")) {
    eval_internal(env, o->cdr->car);
    if(eval_error) { return; }
    Obj *key = stack_pop();
    if(obj_eq(nil, env_lookup_binding(env, key))) {
      stack_push(lisp_false);
    } else {
      stack_push(lisp_true);
    }
  }
  else if(HEAD_EQ("ref")) {
    assert_or_set_error(o->cdr, "Too few args to 'ref': ", o);
    eval_internal(env, o->cdr->car);
  }
  else if(HEAD_EQ("catch-error")) {
    assert_or_set_error(o->cdr, "Too few args to 'catch-error': ", o);
    int shadow_stack_size_save = shadow_stack_pos;
    int stack_size_save = stack_pos;
    eval_internal(env, o->cdr->car);

    shadow_stack_pos = shadow_stack_size_save;
    stack_pos = stack_size_save + 1;

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
  else {
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
    Obj **args = malloc(sizeof(Obj*) * count);
    for(int i = 0; i < count; i++) {
      Obj *arg = stack_pop();
      args[count - i - 1] = arg;
      shadow_stack_push(arg);
    }

    if(function->tag == 'M') {
      Obj *calling_env = obj_new_environment(function->env);
      env_extend_with_args(calling_env, function, count, args);
      shadow_stack_push(calling_env);
      eval_internal(calling_env, function->body);
	  if (eval_error) { free(args); return; }
      Obj *expanded = stack_pop();
      if(SHOW_MACRO_EXPANSION) {
	printf("Expanded macro: %s\n", obj_to_string(expanded)->s);
      }
      shadow_stack_push(expanded);
      eval_internal(env, expanded);
      shadow_stack_pop(); // expanded
      shadow_stack_pop(); // calling_env
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
	  func_name = obj_to_string(function)->s;
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
	
	snprintf(function_trace[function_trace_pos], STACK_TRACE_LEN, "%-30s %s %d:%d", func_name, file, line, pos);
      }
      else {
	snprintf(function_trace[function_trace_pos], STACK_TRACE_LEN, "No meta data."); //"%s", obj_to_string(function)->s);
      }
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
      shadow_stack_pop();
      
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
    shadow_stack_push(new_env);
    Obj *p = new_env->bindings;
    while(p && p->car) {
      Obj *pair = p->car;
      eval_internal(env, pair->cdr);
      //printf("Evaling env-binding %s, setting cdr to %s.\n", obj_to_string(pair)->s, obj_to_string(stack[stack_pos - 1])->s);
      pair->cdr = stack_pop();
      p = p->cdr;
    }
    stack_push(new_env);
    shadow_stack_pop(); // new_env
  }
  else if(o->tag == 'A') {
    Obj *new_array = obj_new_array(o->count);
    shadow_stack_push(new_array);
    for(int i = 0; i < o->count; i++) {
      eval_internal(env, o->array[i]);
      new_array->array[i] = stack_pop();
    }
    stack_push(new_array);
    shadow_stack_pop(); // new_array
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
  function_trace_pos = 0;
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
      printf("\e[31mERROR: %s\e[0m\n", obj_to_string_not_prn(eval_error)->s);
      function_trace_print();
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

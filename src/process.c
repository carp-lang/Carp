#include "process.h"
#include "obj_string.h"
#include "env.h"
#include "repl.h"
#include "primops.h"

#define define(name, value) env_extend(process->global_env, obj_new_symbol(name), value);

#ifdef WIN32
#define PROMPT "CARP> "
#define PROMPT_UNFINISHED_FORM "   _> "
#else
#define PROMPT "λ> " // "\e[36mλ>\e[0m "
#define PROMPT_UNFINISHED_FORM "_> " // "\e[36m_>\e[0m "
#endif

Process *process_new() {
  Process *process = malloc(sizeof(Process));

  process->stack_pos = 0;
  pop_stacks_to_zero(process);

  process->global_env = obj_new_environment(NULL);

  nil = obj_new_cons(NULL, NULL);
  define("nil", nil);

  lisp_false = obj_new_bool(false);
  define("false", lisp_false);
  
  lisp_true = obj_new_bool(true);
  define("true", lisp_true);

  lisp_quote = obj_new_symbol("quote");
  define("quote", lisp_quote);

  ampersand = obj_new_symbol("&");
  define("&", ampersand);

  dotdotdot = obj_new_symbol("dotdotdot");
  define("dotdotdot", dotdotdot);

  lisp_NULL = obj_new_ptr(NULL);
  define("NULL", lisp_NULL);

  type_ref = obj_new_keyword("ref");
  define("type_ref", type_ref);

  type_int = obj_new_keyword("int");
  define("type-int", type_int); // without this it will get GC'd!

  type_bool = obj_new_keyword("bool");
  define("type-bool", type_bool);

  type_float = obj_new_keyword("float");
  define("type-float", type_float);

  type_double = obj_new_keyword("double");
  define("type-double", type_double);
  
  type_string = obj_new_keyword("string");
  define("type-string", type_string);

  type_symbol = obj_new_keyword("symbol");
  define("type-symbol", type_symbol);
  
  type_keyword = obj_new_keyword("keyword");
  define("type-keyword", type_keyword);
  
  type_foreign = obj_new_keyword("foreign");
  define("type-foreign", type_foreign);
  
  type_primop = obj_new_keyword("primop");
  define("type-primop", type_primop);
  
  type_env = obj_new_keyword("env");
  define("type-env", type_env);
  
  type_macro = obj_new_keyword("macro");
  define("type-macro", type_macro);

  type_lambda = obj_new_keyword("lambda");
  define("type-lambda", type_lambda);
  
  type_list = obj_new_keyword("list");
  define("type-list", type_list);

  type_void = obj_new_keyword("void");
  define("type-void", type_void);

  type_ptr = obj_new_keyword("ptr");
  define("type-ptr", type_ptr);

  type_char = obj_new_keyword("char");
  define("type-char", type_char);

  type_array = obj_new_keyword("array");
  define("type-array", type_array);

  type_ptr_to_global = obj_new_keyword("ptr-to-global");
  define("type-ptr-to-global", type_ptr_to_global);

  prompt = define("prompt", obj_new_string(PROMPT));
  prompt_unfinished_form = define("prompt-unfinished-form", obj_new_string(PROMPT_UNFINISHED_FORM));
  
  register_primop(process, "open", p_open_file);
  register_primop(process, "save", p_save_file);
  register_primop(process, "+", p_add);
  register_primop(process, "-", p_sub);
  register_primop(process, "*", p_mul);
  register_primop(process, "/", p_div);
  //register_primop(process, "mod", p_mod);
  register_primop(process, "=", p_eq);
  register_primop(process, "list", p_list);
  register_primop(process, "array", p_array);
  register_primop(process, "str", p_str);
  register_primop(process, "str-append!", p_str_append_bang);
  register_primop(process, "str-replace", p_str_replace);
  register_primop(process, "join", p_join);
  register_primop(process, "register", p_register);
  register_primop(process, "register-variable", p_register_variable);
  register_primop(process, "register-builtin", p_register_builtin);
  register_primop(process, "print", p_print);
  register_primop(process, "println", p_println);
  register_primop(process, "prn", p_prn);
  register_primop(process, "system", p_system);
  register_primop(process, "get", p_get);
  register_primop(process, "get-maybe", p_get_maybe);
  register_primop(process, "dict-set!", p_dict_set_bang);
  register_primop(process, "dict-remove!", p_dict_remove_bang);
  register_primop(process, "first", p_first);
  register_primop(process, "rest", p_rest);
  register_primop(process, "cons", p_cons);
  register_primop(process, "cons-last", p_cons_last);
  register_primop(process, "concat", p_concat);
  register_primop(process, "nth", p_nth);
  register_primop(process, "count", p_count);
  register_primop(process, "map", p_map);
  register_primop(process, "map-copy", p_map); // only matters when compiling to C
  register_primop(process, "map2", p_map2);
  register_primop(process, "filter", p_filter);
  register_primop(process, "reduce", p_reduce);
  register_primop(process, "apply", p_apply);
  register_primop(process, "type", p_type);
  register_primop(process, "<", p_lt);
  register_primop(process, "env", p_env);
  register_primop(process, "load-lisp", p_load_lisp);
  register_primop(process, "load-dylib", p_load_dylib);
  register_primop(process, "unload-dylib", p_unload_dylib);
  register_primop(process, "read", p_read);
  register_primop(process, "read-many", p_read_many);
  register_primop(process, "code", p_code);
  register_primop(process, "copy", p_copy);
  register_primop(process, "now", p_now);
  register_primop(process, "name", p_name);
  register_primop(process, "symbol", p_symbol);
  register_primop(process, "keyword", p_keyword);
  register_primop(process, "error", p_error);
  register_primop(process, "keys", p_keys);
  register_primop(process, "values", p_values);
  register_primop(process, "signature", p_signature);
  register_primop(process, "eval", p_eval);
  register_primop(process, "meta-set!", p_meta_set_BANG);
  register_primop(process, "meta-get", p_meta_get);
  register_primop(process, "meta-get-all", p_meta_get_all);
  register_primop(process, "array-to-list", p_array_to_list);
  register_primop(process, "array-of-size", p_array_of_size);
  register_primop(process, "array-set!", p_array_set_BANG);
  register_primop(process, "array-set", p_array_set);
  register_primop(process, "gc", p_gc);
  register_primop(process, "delete", p_delete);
  
  Obj *abs_args = obj_list(type_int);
  register_ffi_internal(process, "abs", (VoidFn)abs, abs_args, type_int, true);

  Obj *exit_args = obj_list(type_int);
  register_ffi_internal(process, "exit", (VoidFn)exit, exit_args, type_void, true);

  Obj *getenv_args = obj_list(type_string);
  register_ffi_internal(process, "getenv", (VoidFn)getenv, getenv_args, type_string, true);
  
  //printf("Global env: %s\n", obj_to_string(env)->s);

  return process;
}

void stack_print(Process *process) {
  printf("----- STACK -----\n");
  for(int i = 0; i < process->stack_pos; i++) {
    printf("%d\t%s\n", i, obj_to_string(process, process->stack[i])->s);
  }
  printf("-----  END  -----\n\n");
}

void stack_push(Process *process, Obj *o) {
  assert(o);
  if(LOG_STACK) {
    printf("Pushing %s\n", obj_to_string(process, o)->s);
  }
  if(process->stack_pos >= STACK_SIZE) {
    printf("Stack overflow:\n");
    stack_print(process);
    exit(1);
  }
  process->stack[process->stack_pos++] = o;
  if(LOG_STACK) {
    stack_print(process);
  }
}

Obj *stack_pop(Process *process) {
  if(eval_error) {
    return nil;
  }
  if(process->stack_pos <= 0) {
    printf("Stack underflow.\n");
    assert(false);
  }
  if(LOG_STACK) {
    printf("Popping %s\n", obj_to_string(process, process->stack[process->stack_pos - 1])->s);
  }
  Obj *o = process->stack[--process->stack_pos];
  if(LOG_STACK) {
    stack_print(process);
  }
  return o;
}

bool obj_eq(Process *process, Obj *a, Obj *b) {
  //printf("Comparing %s with %s.\n", obj_to_string(process, a)->s, obj_to_string(process, b)->s);

  if(a == b) {
    return true;
  }
  else if(a == NULL || b == NULL) {
    return false;
  }
  else if(a->tag != b->tag) {
    return false;
  }
  else if(a->tag == 'B') {
    return a->boolean == b->boolean;
  }
  else if(a->tag == 'S' || a->tag == 'Y' || a->tag == 'K') {
    return (strcmp(a->s, b->s) == 0);
  }
  else if(a->tag == 'Q') {
    return a->void_ptr == b->void_ptr;
  }
  else if(a->tag == 'I') {
    return a->i == b->i;
  }
  else if(a->tag == 'V') {
    return a->f32 == b->f32;
  }
  else if(a->tag == 'C') {
    Obj *pa = a;
    Obj *pb = b;
    while(1) {
      if(obj_eq(process, pa->car, pb->car)) {
	if(!pa->cdr && !pb->cdr) {
	  return true;
	}
	else if(pa->cdr && !pb->cdr) {
	  return false;
	}
	else if(!pa->cdr && pb->cdr) {
	  return false;
	}
	else {
	  pa = pa->cdr;
	  pb = pb->cdr;
	}
      }
      else {
	return false;
      }
    }
  }
  else if(a->tag == 'A') {
    if(a->count != b->count) {
      return false;
    }
    else {
      for(int i = 0; i < a->count; i++) {
	if(!obj_eq(process, a->array[i], b->array[i])) {
	  return false;
	}
      }
      return true;
    }
  }
  else if(a->tag == 'E') {
    if(!obj_eq(process, a->parent, b->parent)) { return false; }
    //printf("WARNING! Can't reliably compare dicts.\n");

    {
      Obj *pa = a->bindings;    
      while(pa && pa->cdr) {
	Obj *pair = pa->car;
	//printf("Will lookup %s\n", obj_to_string(process, pair->car)->s);
	Obj *binding = env_lookup_binding(process, b, pair->car);
	if(binding) {
	  //printf("Found binding: %s\n", obj_to_string(process, binding)->s);
	  bool eq = obj_eq(process, pair->cdr, binding->cdr);
	  if(!binding->car) {
	    //printf("binding->car was NULL\n");
	    return false;
	  }
	  else if(!eq) {
	    //printf("%s != %s\n", obj_to_string(process, pair->cdr)->s, obj_to_string(process, binding->cdr)->s);
	    return false;
	  }
	}
	else {
	  return false;
	}
	pa = pa->cdr;
      }
    }

    {
      Obj *pb = b->bindings;    
      while(pb && pb->cdr) {
	Obj *pair = pb->car;
	//printf("Will lookup %s\n", obj_to_string(process, pair->car)->s);
	Obj *binding = env_lookup_binding(process, a, pair->car);
	if(binding) {
	  //printf("Found binding: %s\n", obj_to_string(process, binding)->s);
	  bool eq = obj_eq(process, pair->cdr, binding->cdr);
	  if(!binding->car) {
	    //printf("binding->car was NULL\n");
	    return false;
	  }
	  else if(!eq) {
	    //printf("%s != %s\n", obj_to_string(process, pair->cdr)->s, obj_to_string(process, binding->cdr)->s);
	    return false;
	  }
	}
	else {
	  return false;
	}
	pb = pb->cdr;
      }
    }
    
    return true;
  }
  else {
    char buffer[512];
    snprintf(buffer, 512, "Can't compare %s with %s.\n", obj_to_string(process, a)->s, obj_to_string(process, b)->s);
    eval_error = obj_new_string(strdup(buffer));
    return false;
  }
}



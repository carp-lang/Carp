#include "repl.h"
#include "eval.h"
#include "gc.h"
#include "obj_string.h"
#include "reader.h"
#include "eval.h"
#include "env.h"
#include "primops.h"

#define MAX_INPUT_BUFFER_SIZE 2048
char input[MAX_INPUT_BUFFER_SIZE];

void repl(Obj *env) {
  while(1) {
    printf("\e[36mλ>\e[0m ");
    fgets(input, MAX_INPUT_BUFFER_SIZE, stdin);
    if(strcmp(input, "q\n") == 0) {
      break;
    }
    eval_text(env, input, true);
    while(stack_pos > 0) {
      //printf("°"); // Popping extra stack value
      stack_pop();
    }
    printf("\n");
    //assert(stack_pos == 0);
    //stack_print();
  }
  gc_sweep();
}

void env_new_global() {
  global_env = obj_new_environment(NULL);

  nil = obj_new_cons(NULL, NULL);
  define("nil", nil);

  lisp_false = obj_new_symbol("false");
  define("false", lisp_false);
  
  lisp_true = obj_new_symbol("true");
  define("true", lisp_true);

  lisp_quote = obj_new_symbol("quote");
  define("quote", lisp_quote);

  ampersand = obj_new_symbol("&");
  define("&", ampersand);

  lisp_NULL = obj_new_ptr(NULL);
  define("NULL", lisp_NULL);

  type_int = obj_new_keyword("int");
  define("type-int", type_int); // without this it will get GC'd!

  type_bool = obj_new_keyword("bool");
  define("type-bool", type_bool);

  type_float = obj_new_keyword("float");
  define("type-float", type_float);
  
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

  register_primop("open", p_open_file);
  register_primop("save", p_save_file);
  register_primop("+", p_add);
  register_primop("-", p_sub);
  register_primop("*", p_mul);
  register_primop("/", p_div);
  register_primop("mod", p_mod);
  register_primop("=", p_eq);
  register_primop("list", p_list);
  register_primop("str", p_str);
  register_primop("str-append!", p_str_append_bang);
  register_primop("str-replace", p_str_replace);
  register_primop("register", p_register);
  register_primop("register-variable", p_register_variable);
  register_primop("register-builtin", p_register_builtin);
  register_primop("print", p_print);
  register_primop("println", p_println);
  register_primop("prn", p_prn);
  register_primop("system", p_system);
  register_primop("get", p_get);
  register_primop("get-maybe", p_get_maybe);
  register_primop("dict-set!", p_dict_set_bang);
  register_primop("dict-remove!", p_dict_remove_bang);
  register_primop("first", p_first);
  register_primop("rest", p_rest);
  register_primop("cons", p_cons);
  register_primop("cons-last", p_cons_last);
  register_primop("concat", p_concat);
  register_primop("nth", p_nth);
  register_primop("count", p_count);
  register_primop("map", p_map);
  register_primop("map2", p_map2);
  register_primop("filter", p_filter);
  register_primop("reduce", p_reduce);
  register_primop("apply", p_apply);
  register_primop("type", p_type);
  register_primop("<", p_lt);
  register_primop("env", p_env);
  register_primop("load-lisp", p_load_lisp);
  register_primop("load-dylib", p_load_dylib);
  register_primop("unload-dylib", p_unload_dylib);
  register_primop("read", p_read);
  register_primop("read-many", p_read_many);
  register_primop("code", p_code);
  register_primop("copy", p_copy);
  register_primop("now", p_now);
  register_primop("name", p_name);
  register_primop("symbol", p_symbol);
  register_primop("error", p_error);
  register_primop("keys", p_keys);
  register_primop("values", p_values);
  register_primop("signature", p_signature);
  register_primop("eval", p_eval);
  register_primop("and", p_and);
  //register_primop("nullp", p_null_predicate);
  
  Obj *abs_args = obj_list(type_int);
  register_ffi_internal("abs", (VoidFn)abs, abs_args, type_int);

  Obj *exit_args = obj_list(type_int);
  register_ffi_internal("exit", (VoidFn)exit, exit_args, type_void);

  Obj *getenv_args = obj_list(type_string);
  register_ffi_internal("getenv", (VoidFn)getenv, getenv_args, type_string);
  
  //printf("Global env: %s\n", obj_to_string(env)->s);
}

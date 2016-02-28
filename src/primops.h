#pragma once

#include "obj.h"

#define define(name, value) env_extend(global_env, obj_new_symbol(name), value);

void register_primop(char *name, Primop primop);

Obj *p_open_file(Obj** args, int arg_count);
Obj *p_save_file(Obj** args, int arg_count);
Obj *p_add(Obj** args, int arg_count);
Obj *p_sub(Obj** args, int arg_count);
Obj *p_mul(Obj** args, int arg_count);
Obj *p_div(Obj** args, int arg_count);
Obj *p_mod(Obj** args, int arg_count);
Obj *p_eq(Obj** args, int arg_count);
Obj *p_list(Obj** args, int arg_count);
Obj *p_str(Obj** args, int arg_count);
Obj *p_str_append_bang(Obj** args, int arg_count);
Obj *p_str_replace(Obj** args, int arg_count);
Obj *p_copy(Obj** args, int arg_count);
Obj *p_print(Obj** args, int arg_count);
Obj *p_prn(Obj** args, int arg_count);
Obj *p_println(Obj** args, int arg_count);
Obj *p_system(Obj** args, int arg_count);
Obj *p_get(Obj** args, int arg_count);
Obj *p_get_maybe(Obj** args, int arg_count);
Obj *p_dict_set_bang(Obj** args, int arg_count);
Obj *p_dict_remove_bang(Obj** args, int arg_count);
Obj *p_rest(Obj** args, int arg_count);
Obj *p_cons(Obj** args, int arg_count);
Obj *p_cons_last(Obj** args, int arg_count);
Obj *p_concat(Obj** args, int arg_count);
Obj *p_nth(Obj** args, int arg_count);
Obj *p_count(Obj** args, int arg_count);
Obj *p_map(Obj** args, int arg_count);
Obj *p_map2(Obj** args, int arg_count);
Obj *p_register(Obj** args, int arg_count);
Obj *p_register_variable(Obj** args, int arg_count);
Obj *p_register_builtin(Obj** args, int arg_count);
Obj *p_first(Obj** args, int arg_count);
Obj *p_filter(Obj** args, int arg_count);
Obj *p_reduce(Obj** args, int arg_count);
Obj *p_apply(Obj** args, int arg_count);
Obj *p_type(Obj** args, int arg_count);
Obj *p_lt(Obj** args, int arg_count);
Obj *p_env(Obj** args, int arg_count);
Obj *p_load_lisp(Obj** args, int arg_count);
Obj *p_load_dylib(Obj** args, int arg_count);
Obj *p_unload_dylib(Obj** args, int arg_count);
Obj *p_read(Obj** args, int arg_count);
Obj *p_read_many(Obj** args, int arg_count);
Obj *p_code(Obj** args, int arg_count);
Obj *p_now(Obj** args, int arg_count);
Obj *p_name(Obj** args, int arg_count);
Obj *p_symbol(Obj** args, int arg_count);
Obj *p_keyword(Obj** args, int arg_count);
Obj *p_error(Obj** args, int arg_count);
Obj *p_keys(Obj** args, int arg_count);
Obj *p_values(Obj** args, int arg_count);
Obj *p_signature(Obj** args, int arg_count);
Obj *p_eval(Obj** args, int arg_count);
Obj *p_builtin_p(Obj** args, int arg_count);
Obj *p_meta_set_BANG(Obj** args, int arg_count);
Obj *p_meta_get(Obj** args, int arg_count);
Obj *p_array_to_list(Obj** args, int arg_count);
Obj *p_spork(Obj** args, int arg_count);
Obj *p_array_of_size(Obj** args, int arg_count);
//Obj *p_array(Obj** args, int arg_count);
Obj *p_array_set_BANG(Obj** args, int arg_count);
Obj *p_array_set(Obj** args, int arg_count);
//Obj *p_new(Obj** args, int arg_count);

Obj *register_ffi_internal(char *name, VoidFn funptr, Obj *args, Obj *return_type_obj, bool builtin);

ffi_cif *create_cif(Obj *args, int arg_count, Obj *return_type_obj, char *func_name);

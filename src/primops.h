#pragma once

#include "obj.h"

void register_primop(Process *process, char *name, Primop primop);

// PLEASE NOTE
// All primops must be very careful to put any temp variables on the shadow stack.
// All calls to the rest of the lisp system might trigger GC and remove the fresh locals!

Obj *p_open_file(Process *process, Obj** args, int arg_count);
Obj *p_save_file(Process *process, Obj** args, int arg_count);
Obj *p_add(Process *process, Obj** args, int arg_count);
Obj *p_sub(Process *process, Obj** args, int arg_count);
Obj *p_mul(Process *process, Obj** args, int arg_count);
Obj *p_div(Process *process, Obj** args, int arg_count);
//Obj *p_mod(Process *process, Obj** args, int arg_count);
Obj *p_eq(Process *process, Obj** args, int arg_count);
Obj *p_list(Process *process, Obj** args, int arg_count);
Obj *p_array(Process *process, Obj** args, int arg_count);
Obj *p_str(Process *process, Obj** args, int arg_count);
Obj *p_str_append_bang(Process *process, Obj** args, int arg_count);
Obj *p_join(Process *process, Obj** args, int arg_count);
Obj *p_str_replace(Process *process, Obj** args, int arg_count);
Obj *p_copy(Process *process, Obj** args, int arg_count);
Obj *p_print(Process *process, Obj** args, int arg_count);
Obj *p_prn(Process *process, Obj** args, int arg_count);
Obj *p_println(Process *process, Obj** args, int arg_count);
Obj *p_system(Process *process, Obj** args, int arg_count);
Obj *p_get(Process *process, Obj** args, int arg_count);
Obj *p_get_maybe(Process *process, Obj** args, int arg_count);
Obj *p_dict_set_bang(Process *process, Obj** args, int arg_count);
Obj *p_dict_remove_bang(Process *process, Obj** args, int arg_count);
Obj *p_rest(Process *process, Obj** args, int arg_count);
Obj *p_cons(Process *process, Obj** args, int arg_count);
Obj *p_cons_last(Process *process, Obj** args, int arg_count);
Obj *p_concat(Process *process, Obj** args, int arg_count);
Obj *p_nth(Process *process, Obj** args, int arg_count);
Obj *p_count(Process *process, Obj** args, int arg_count);
Obj *p_map(Process *process, Obj** args, int arg_count);
Obj *p_map2(Process *process, Obj** args, int arg_count);
Obj *p_register(Process *process, Obj** args, int arg_count);
Obj *p_register_variable(Process *process, Obj** args, int arg_count);
Obj *p_register_builtin(Process *process, Obj** args, int arg_count);
Obj *p_first(Process *process, Obj** args, int arg_count);
Obj *p_filter(Process *process, Obj** args, int arg_count);
Obj *p_reduce(Process *process, Obj** args, int arg_count);
Obj *p_apply(Process *process, Obj** args, int arg_count);
Obj *p_type(Process *process, Obj** args, int arg_count);
Obj *p_lt(Process *process, Obj** args, int arg_count);
Obj *p_env(Process *process, Obj** args, int arg_count);
Obj *p_load_lisp(Process *process, Obj** args, int arg_count);
Obj *p_load_dylib(Process *process, Obj** args, int arg_count);
Obj *p_unload_dylib(Process *process, Obj** args, int arg_count);
Obj *p_read(Process *process, Obj** args, int arg_count);
Obj *p_read_many(Process *process, Obj** args, int arg_count);
Obj *p_code(Process *process, Obj** args, int arg_count);
Obj *p_now(Process *process, Obj** args, int arg_count);
Obj *p_name(Process *process, Obj** args, int arg_count);
Obj *p_symbol(Process *process, Obj** args, int arg_count);
Obj *p_keyword(Process *process, Obj** args, int arg_count);
Obj *p_error(Process *process, Obj** args, int arg_count);
Obj *p_keys(Process *process, Obj** args, int arg_count);
Obj *p_values(Process *process, Obj** args, int arg_count);
Obj *p_signature(Process *process, Obj** args, int arg_count);
Obj *p_eval(Process *process, Obj** args, int arg_count);
Obj *p_builtin_p(Process *process, Obj** args, int arg_count);
Obj *p_meta_set_BANG(Process *process, Obj** args, int arg_count);
Obj *p_meta_get(Process *process, Obj** args, int arg_count);
Obj *p_meta_get_all(Process *process, Obj** args, int arg_count);
Obj *p_array_to_list(Process *process, Obj** args, int arg_count);
Obj *p_spork(Process *process, Obj** args, int arg_count);
Obj *p_array_of_size(Process *process, Obj** args, int arg_count);
//Obj *p_array(Process *process, Obj** args, int arg_count);
Obj *p_array_set_BANG(Process *process, Obj** args, int arg_count);
Obj *p_array_set(Process *process, Obj** args, int arg_count);
//Obj *p_new(Process *process, Obj** args, int arg_count);
Obj *p_gc(Process *process, Obj** args, int arg_count);
Obj *p_delete(Process *process, Obj** args, int arg_count);
Obj *p_stop(Process *process, Obj** args, int arg_count);
Obj *p_parallell(Process *process, Obj** args, int arg_count);
Obj *p_bytecode(Process *process, Obj** args, int arg_count);
Obj *p_bytecode_eval(Process *process, Obj** args, int arg_count);
Obj *p_lookup_in_substs_fast(Process *process, Obj** args, int arg_count);

Obj *register_ffi_internal(Process *process, char *name, VoidFn funptr, Obj *args, Obj *return_type_obj, bool builtin);

ffi_cif *create_cif(Process *process, Obj *args, int arg_count, Obj *return_type_obj, char *func_name);

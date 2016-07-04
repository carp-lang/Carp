#include "obj_string.h"
#include "env.h"
#include "eval.h"
#include "gc.h"
#include "obj_conversions.h"
#include "process.h"
#include "bytecode.h"

bool setting_print_lambda_body = true;

void obj_string_mut_append(Obj *string_obj, const char *s2) {
  assert(string_obj);
  assert(string_obj->tag == 'S');
  assert(string_obj->s);
  assert(s2);
  int string_obj_len = (int)strlen(string_obj->s);
  int s2_len = (int)strlen(s2);
  int total_length = (string_obj_len + s2_len);
  //printf("mut append '%s' (%d) << '%s' (%d)\n", string_obj->s, string_obj_len, s2, s2_len);
  char *s3 = realloc(string_obj->s, sizeof(char) * (total_length + 1));
  s3[total_length] = '\0';
  strncpy(s3 + string_obj_len, s2, s2_len);
  string_obj->s = s3;
  string_obj->hash = 0; // todo: calculate new hash instead!
}

Obj *concat_c_strings(char *a, const char *b) {
  Obj *s = obj_new_string(a);
  obj_string_mut_append(s, b);
  return s;
}

void print_generic_array_or_struct(Process *process, Obj *total, Obj *type_lookup, struct Obj *arg_to_str_obj) {
  assert(total);
  assert(total->tag == 'S');
  assert(type_lookup);
  assert(arg_to_str_obj);

  shadow_stack_push(process, total);
  shadow_stack_push(process, type_lookup);
  shadow_stack_push(process, arg_to_str_obj);

  Obj *reffed_arg_type = obj_list(obj_new_keyword("ref"), type_lookup); // HACK: ref needed when sending arrays into str
  Obj *args_type = obj_list(reffed_arg_type);
  Obj *signature = obj_list(obj_new_keyword("fn"), args_type, type_string);
  Obj *quoted_sig = obj_list(lisp_quote, signature);

  //printf("quoted_sig: %s\n", obj_to_string(quoted_sig)->s);

  Obj *generic_name_result = generic_name(process, "prn", quoted_sig);
  if(eval_error) {
    return;
  }
  shadow_stack_push(process, generic_name_result);

  bake_generic_primop_auto(process, "prn", quoted_sig);
  if(eval_error) {
    return;
  }

  // TODO: why this conversion?
  char *generic_name = obj_to_string_not_prn(process, generic_name_result)->s;
  //printf("generic_name 1: %s\n", generic_name);

  Obj *call_to_str = obj_list(obj_new_symbol(generic_name), (struct Obj *)arg_to_str_obj);

  //   OBS!!!
  //
  //   Calling obj_to_string on the call_to_str form will result in an infinite loop:
  //   printf("Call to str: %s\n", obj_to_string(call_to_str)->s);
  //
  //   DON'T DO IT!!!

  shadow_stack_push(process, call_to_str);

  Obj *array_to_string_result = NULL;
  if(BYTECODE_EVAL) {
    array_to_string_result = bytecode_sub_eval_form(process, process->global_env, call_to_str);
  }
  else {
    array_to_string_result = eval(process, process->global_env, call_to_str);
  }

  shadow_stack_push(process, array_to_string_result);
  if(eval_error) {
    printf("Error when calling str function for void ptr of type '%s':\n", obj_to_string(process, type_lookup)->s);
    printf("%s\n", obj_to_string(process, eval_error)->s);
    assert(false);
    stack_pop(process);
    obj_string_mut_append(total, "FAIL");
    return;
  }
  obj_string_mut_append(total, obj_to_string_not_prn(process, array_to_string_result)->s);

  Obj *pop1 = shadow_stack_pop(process);
  assert(pop1 == array_to_string_result);
  shadow_stack_pop(process);
  shadow_stack_pop(process);
  shadow_stack_pop(process);
  shadow_stack_pop(process);
  Obj *pop8 = shadow_stack_pop(process);
  assert(pop8 == total);

  return;
}

void add_indentation(Obj *total, int indent) {
  for(int i = 0; i < indent; i++) {
    obj_string_mut_append(total, " ");
  }
}

void obj_to_string_internal(Process *process, Obj *total, const Obj *o, bool prn, int indent) {
  assert(o);
  int x = indent;
  if(o->tag == 'C') {
    obj_string_mut_append(total, "(");
    x++;
    int save_x = x;
    const Obj *p = o;
    while(p && p->car) {
      obj_to_string_internal(process, total, p->car, true, x);
      if(p->cdr && p->cdr->tag != 'C') {
        obj_string_mut_append(total, " . ");
        obj_to_string_internal(process, total, o->cdr, true, x);
        break;
      }
      else if(p->cdr && p->cdr->car) {
        if(/* p->car->tag == 'C' ||  */ p->car->tag == 'E') {
          obj_string_mut_append(total, "\n");
          x = save_x;
          add_indentation(total, x);
        }
        else {
          obj_string_mut_append(total, " ");
          x++;
        }
      }
      p = p->cdr;
    }
    obj_string_mut_append(total, ")");
    x++;
  }
  else if(o->tag == 'A') {
    //printf("Will print Obj Array with count %d\n", o->count);
    shadow_stack_push(process, (struct Obj *)o);
    x++;
    //int save_x = x;
    obj_string_mut_append(total, "[");
    for(int i = 0; i < o->count; i++) {
      obj_to_string_internal(process, total, o->array[i], true, x);
      if(i < o->count - 1) {
        /* if(o->array[i]->car->tag == 'Q' || o->array[i]->car->tag == 'E') { */
        /*   obj_string_mut_append(total, "\n"); */
        /*   x = save_x; */
        /*   add_indentation(total, x); */
        /* } */
        /* else { */
        /*   obj_string_mut_append(total, " "); */
        /*   x++; */
        /* } */
        obj_string_mut_append(total, " ");
      }
    }
    obj_string_mut_append(total, "]");
    shadow_stack_pop(process);
    x++;
  }
  else if(o->tag == 'E') {
    shadow_stack_push(process, (struct Obj *)o);

    if(o == process->global_env) {
      obj_string_mut_append(total, "{ GLOBAL ENVIRONMENT }");
      return;
    }

    obj_string_mut_append(total, "{");
    x++;
    Obj *p = o->bindings;
    while(p && p->car) {
      char *key_s = obj_to_string(process, p->car->car)->s;
      obj_string_mut_append(total, key_s);
      obj_string_mut_append(total, " ");
      obj_to_string_internal(process, total, p->car->cdr, true, x + (int)strlen(key_s) + 1);
      p = p->cdr;
      if(p && p->car && p->car->car) {
        obj_string_mut_append(total, ", \n");
        add_indentation(total, x);
      }
    }
    obj_string_mut_append(total, "}");
    if(o->parent) {
      obj_string_mut_append(total, " -> \n");
      Obj *parent_printout = obj_to_string(process, o->parent);
      obj_string_mut_append(total, parent_printout->s);
    }
    shadow_stack_pop(process);
  }
  else if(o->tag == 'I') {
    static char temp[64];
    snprintf(temp, 64, "%d", o->i);
    obj_string_mut_append(total, temp);
  }
  else if(o->tag == 'V') {
    static char temp[64];
    snprintf(temp, 64, "%f", o->f32);
    obj_string_mut_append(total, temp);
    obj_string_mut_append(total, "f");
  }
  else if(o->tag == 'W') {
    static char temp[64];
    snprintf(temp, 64, "%f", o->f64);
    obj_string_mut_append(total, temp);
  }
  else if(o->tag == 'S') {
    if(prn) {
      obj_string_mut_append(total, "\"");
    }
    obj_string_mut_append(total, o->s);
    if(prn) {
      obj_string_mut_append(total, "\"");
    }
  }
  else if(o->tag == 'Y') {
    obj_string_mut_append(total, o->s);
  }
  else if(o->tag == 'K') {
    obj_string_mut_append(total, ":");
    obj_string_mut_append(total, o->s);
  }
  else if(o->tag == 'P') {
    obj_string_mut_append(total, "<primop:");
    static char temp[256];
    snprintf(temp, 256, "%p", o->primop);
    obj_string_mut_append(total, temp);
    if(o->meta) {
      Obj *name = env_lookup(process, o->meta, obj_new_keyword("name"));
      if(name) {
        obj_string_mut_append(total, ":");
        obj_string_mut_append(total, obj_to_string_not_prn(process, name)->s);
      }
    }
    obj_string_mut_append(total, ">");
  }
  else if(o->tag == 'D') {
    obj_string_mut_append(total, "<dylib:");
    static char temp[256];
    snprintf(temp, 256, "%p", o->primop);
    obj_string_mut_append(total, temp);
    obj_string_mut_append(total, ">");
  }
  else if(o->tag == 'Q') {
    shadow_stack_push(process, (struct Obj *)o);
    Obj *type_lookup;
    if(o->meta && (type_lookup = env_lookup(process, o->meta, obj_new_keyword("type")))) {
      if(type_lookup->tag == 'C' && type_lookup->cdr->car && obj_eq(process, type_lookup->car, obj_new_keyword("Array"))) {
        print_generic_array_or_struct(process, total, type_lookup, (struct Obj *)o);
      }
      else {
        print_generic_array_or_struct(process, total, type_lookup, (struct Obj *)o);
        /* obj_string_mut_append(total, "<ptr"); */
        /* obj_string_mut_append(total, obj_to_string(type_lookup)->s); */
        /* obj_string_mut_append(total, ">"); */
      }
    }
    else {
      obj_string_mut_append(total, "<ptr:");
      static char temp[256];
      snprintf(temp, 256, "%p", o->primop);
      obj_string_mut_append(total, temp);
      obj_string_mut_append(total, " of unknown type");
      obj_string_mut_append(total, ">");
    }
    shadow_stack_pop(process);
  }
  else if(o->tag == 'R') {
    shadow_stack_push(process, (struct Obj *)o);

    if(!o->void_ptr) {
      eval_error = obj_new_string("Pointer to global is NULL.\n");
      return;
    }

    Obj *type_lookup;
    //printf("o %p %p\n", o, o->void_ptr);

    if(o->void_ptr == NULL) {
      obj_string_mut_append(total, "NULL");
    }
    else if(o->meta && (type_lookup = env_lookup(process, o->meta, obj_new_keyword("type")))) {
      //printf("type %s\n", obj_to_string(type_lookup)->s);
      if(type_lookup->tag == 'C' && type_lookup->cdr->car && obj_eq(process, type_lookup->car, obj_new_keyword("Array"))) {
        void *dereffed = *(void **)o->void_ptr;
        assert(dereffed);
        Obj *x = primitive_to_obj(process, dereffed, type_lookup);
        shadow_stack_push(process, x);
        obj_string_mut_append(total, obj_to_string(process, x)->s);
        shadow_stack_pop(process); // x
      }
      else if(obj_eq(process, type_lookup, type_int)) {
        //int i = 123;
        void *dereffed = *(void **)o->void_ptr;
        assert(dereffed);
        Obj *x = primitive_to_obj(process, dereffed, type_int);
        obj_string_mut_append(total, obj_to_string(process, x)->s);
      }
      else if(obj_eq(process, type_lookup, type_float)) {
        //int i = 123;
        void *dereffed = *(void **)o->void_ptr;
        assert(dereffed);
        Obj *x = primitive_to_obj(process, dereffed, type_float);
        obj_string_mut_append(total, obj_to_string(process, x)->s);
      }
      else if(obj_eq(process, type_lookup, type_double)) {
        void *dereffed = *(void **)o->void_ptr;
        assert(dereffed);
        Obj *x = primitive_to_obj(process, dereffed, type_double);
        obj_string_mut_append(total, obj_to_string(process, x)->s);
      }
      else if(obj_eq(process, type_lookup, type_bool)) {
        void *dereffed = *(void **)o->void_ptr;
        // can't assert since false == NULL
        Obj *x = primitive_to_obj(process, dereffed, type_bool);
        obj_string_mut_append(total, obj_to_string(process, x)->s);
      }
      else if(obj_eq(process, type_lookup, type_string)) {
        void *dereffed = *(void **)o->void_ptr;
        assert(dereffed);
        Obj *x = primitive_to_obj(process, dereffed, type_string);
        obj_string_mut_append(total, x->s);
      }
      else if(obj_eq(process, type_lookup, type_char)) {
        void *dereffed = *(void **)o->void_ptr;
        assert(dereffed);
        Obj *x = primitive_to_obj(process, dereffed, type_char);
        obj_string_mut_append(total, obj_to_string(process, x)->s);
      }
      else {
        void *dereffed = *(void **)o->void_ptr;
        assert(dereffed);
        Obj *x = primitive_to_obj(process, dereffed, type_lookup);
        print_generic_array_or_struct(process, total, type_lookup, (struct Obj *)x);
        /* obj_string_mut_append(total, "<ptr"); */
        /* obj_string_mut_append(total, obj_to_string(type_lookup)->s); */
        /* obj_string_mut_append(total, ">"); */
      }
    }

    obj_string_mut_append(total, " ; ptr-to-global");

    shadow_stack_pop(process);
  }
  else if(o->tag == 'F') {
    obj_string_mut_append(total, "<ffi:");
    static char temp[256];
    snprintf(temp, 256, "%p", o->funptr);
    obj_string_mut_append(total, temp);
    if(o->meta) {
      Obj *name = env_lookup(process, o->meta, obj_new_keyword("name"));
      if(name) {
        obj_string_mut_append(total, ":");
        obj_string_mut_append(total, obj_to_string_not_prn(process, name)->s);
      }
    }
    else {
    }
    obj_string_mut_append(total, ">");
  }
  else if(o->tag == 'L') {
    if(setting_print_lambda_body) {
      obj_string_mut_append(total, "(fn");
      obj_string_mut_append(total, " ");
      obj_string_mut_append(total, obj_to_string(process, o->params)->s);
      obj_string_mut_append(total, " ");
      obj_string_mut_append(total, obj_to_string(process, o->body)->s);
      obj_string_mut_append(total, ")");
    }
    else {
      obj_string_mut_append(total, "<lambda>");
    }
  }
  else if(o->tag == 'M') {
    if(setting_print_lambda_body) {
      obj_string_mut_append(total, "(macro");
      obj_string_mut_append(total, " ");
      obj_string_mut_append(total, obj_to_string(process, o->params)->s);
      obj_string_mut_append(total, " ");
      obj_string_mut_append(total, obj_to_string(process, o->body)->s);
      obj_string_mut_append(total, ")");
    }
    else {
      obj_string_mut_append(total, "<macro>");
    }
  }
  else if(o->tag == 'T') {
    char s[2] = {o->character, '\0'};
    if(prn) {
      obj_string_mut_append(total, "\\");
    }
    obj_string_mut_append(total, s);
  }
  else if(o->tag == 'B') {
    if(o->boolean) {
      obj_string_mut_append(total, "true");
    }
    else {
      obj_string_mut_append(total, "false");
    }
  }
  else if(o->tag == 'X') {
    /* obj_string_mut_append(total, "--- Bytecode ---\n"); */
    /* obj_string_mut_append(total, o->bytecode); */
    /* obj_string_mut_append(total, "\nLiterals: "); */
    /* obj_string_mut_append(total, obj_to_string(process, o->bytecode_literals)->s); */
    /* obj_string_mut_append(total, "\n----------------"); */
    obj_string_mut_append(total, "(Bytecode: ");

    /* for(char *p = o->bytecode; p != '\0'; p++) { */
    /*   char buffer[16]; */
    /*   buffer[0] = *p; */
    /*   buffer[1] = '\0'; */
    /*   //printf("buffer = %s\n", buffer); */
    /*   obj_string_mut_append(total, buffer); */
    /* } */

    obj_string_mut_append(total, o->bytecode);

    obj_string_mut_append(total, " => ");
    obj_string_mut_append(total, obj_to_string(process, o->bytecode_literals)->s);
    obj_string_mut_append(total, ")");
  }
  else {
    printf("obj_to_string() can't handle type tag %c (%d).\n", o->tag, o->tag);
    assert(false);
  }
}

Obj *obj_to_string(Process *process, const Obj *o) {
  Obj *s = obj_new_string("");
  obj_to_string_internal(process, s, o, true, 0);
  return s;
}

Obj *obj_to_string_not_prn(Process *process, const Obj *o) {
  Obj *s = obj_new_string("");
  shadow_stack_push(process, s);
  obj_to_string_internal(process, s, o, false, 0);
  shadow_stack_pop(process);
  return s;
}

void obj_print(Process *process, Obj *o) {
  assert(o);
  Obj *s = obj_to_string(process, o);
  printf("%s", s->s);
}

void obj_print_not_prn(Process *process, Obj *o) {
  Obj *s = obj_to_string_not_prn(process, o);
  printf("%s", s->s);
}

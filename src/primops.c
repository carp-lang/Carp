#include "primops.h"

#ifdef WIN32
#include <time.h>
#else
#include <sys/time.h>
#endif

#include "assertions.h"
#include "obj_string.h"
#include "env.h"
#include "eval.h"
#include "reader.h"
#include "gc.h"
#include "obj_conversions.h"
#include "bytecode.h"
#include "../shared/types.h"

void register_primop(Process *process, char *name, Primop primop) {
  Obj *o = obj_new_primop(primop);
  env_extend(process->global_env, obj_new_symbol(name), o);
  o->meta = obj_new_environment(NULL);
  env_assoc(process, o->meta, obj_new_keyword("name"), obj_new_string(name));
}

Obj *open_file(Process *process, const char *filename) {
  assert(filename);

  char *buffer = 0;
  long length;
  FILE *f = fopen(filename, "rb");

  if(f) {
    fseek(f, 0, SEEK_END);
    length = ftell(f);
    fseek(f, 0, SEEK_SET);
    buffer = malloc(length + 1);
    if(buffer) {
      fread(buffer, 1, length, f);
      buffer[length] = '\0';
    }
    fclose(f);
  }
  else {
    set_error_and_return("Failed to open file: ", obj_new_string((char *)filename));
  }

  if(buffer) {
    return obj_new_string(buffer);
  }
  else {
    set_error_and_return("Failed to open buffer from file: ", obj_new_string((char *)filename));
  }
}

Obj *save_file(Process *process, const char *filename, const char *contents) {
  FILE *f = fopen(filename, "w");
  if(f) {
    fprintf(f, "%s", contents);
    fclose(f);
    return nil;
  }
  else {
    set_error_and_return("Failed to save file: ", obj_new_string((char *)filename));
  }
}

Obj *p_open_file(Process *process, Obj **args, int arg_count) {
  if(arg_count != 1) {
    return nil;
  }
  if(args[0]->tag != 'S') {
    return nil;
  }
  return open_file(process, args[0]->s);
}

Obj *p_save_file(Process *process, Obj **args, int arg_count) {
  if(arg_count != 2) {
    return nil;
  }
  if(args[0]->tag != 'S') {
    return nil;
  }
  if(args[1]->tag != 'S') {
    return nil;
  }
  return save_file(process, args[0]->s, args[1]->s);
}

Obj *p_add(Process *process, Obj **args, int arg_count) {
  if(arg_count == 0 || args[0]->tag == 'I') {
    int sum = 0;
    for(int i = 0; i < arg_count; i++) {
      if(args[i]->tag != 'I') {
        eval_error = obj_new_string("Args to add must be integers.\n");
        return nil;
      }
      sum += args[i]->i;
    }
    return obj_new_int(sum);
  }
  else if(args[0]->tag == 'V') {
    float sum = 0;
    for(int i = 0; i < arg_count; i++) {
      if(args[i]->tag != 'V') {
        eval_error = obj_new_string("Args to add must be floats.\n");
        return nil;
      }
      sum += args[i]->f32;
    }
    return obj_new_float(sum);
  }
  else if(args[0]->tag == 'W') {
    double sum = 0;
    for(int i = 0; i < arg_count; i++) {
      if(args[i]->tag != 'W') {
        eval_error = obj_new_string("Args to add must be doubles.\n");
        return nil;
      }
      sum += args[i]->f64;
    }
    return obj_new_double(sum);
  }
  else {
    eval_error = obj_new_string("Can't add non-numbers together.");
    return nil;
  }
}

Obj *p_sub(Process *process, Obj **args, int arg_count) {
  if(arg_count == 0 || args[0]->tag == 'I') {
    if(arg_count == 1) {
      return obj_new_int(-args[0]->i);
    }
    int sum = args[0]->i;
    for(int i = 1; i < arg_count; i++) {
      sum -= args[i]->i;
    }
    return obj_new_int(sum);
  }
  else if(args[0]->tag == 'V') {
    if(arg_count == 1) {
      return obj_new_int((int)-args[0]->f32);
    }
    float sum = args[0]->f32;
    for(int i = 1; i < arg_count; i++) {
      sum -= args[i]->f32;
    }
    return obj_new_float(sum);
  }
  else if(args[0]->tag == 'W') {
    if(arg_count == 1) {
      return obj_new_int((int)-args[0]->f64);
    }
    double sum = args[0]->f64;
    for(int i = 1; i < arg_count; i++) {
      sum -= args[i]->f64;
    }
    return obj_new_double(sum);
  }
  else {
    eval_error = obj_new_string("Can't subtract non-numbers.");
    return nil;
  }
}

Obj *p_mul(Process *process, Obj **args, int arg_count) {
  if(arg_count == 0) {
    return obj_new_int(1);
  }

  if(args[0]->tag == 'I') {
    int prod = args[0]->i;
    for(int i = 1; i < arg_count; i++) {
      prod *= args[i]->i;
    }
    return obj_new_int(prod);
  }
  else if(args[0]->tag == 'V') {
    float prod = args[0]->f32;
    for(int i = 1; i < arg_count; i++) {
      prod *= args[i]->f32;
    }
    return obj_new_float(prod);
  }
  else if(args[0]->tag == 'W') {
    double prod = args[0]->f64;
    for(int i = 1; i < arg_count; i++) {
      prod *= args[i]->f64;
    }
    return obj_new_double(prod);
  }
  else {
    eval_error = obj_new_string("Can't multiply non-numbers.");
    return nil;
  }
}

Obj *p_div(Process *process, Obj **args, int arg_count) {
  if(arg_count == 0) {
    return obj_new_int(1);
  }

  if(args[0]->tag == 'I') {
    int prod = args[0]->i;
    for(int i = 1; i < arg_count; i++) {
      prod /= args[i]->i;
    }
    return obj_new_int(prod);
  }
  else if(args[0]->tag == 'V') {
    float prod = args[0]->f32;
    for(int i = 1; i < arg_count; i++) {
      prod /= args[i]->f32;
    }
    return obj_new_float(prod);
  }
  else if(args[0]->tag == 'W') {
    double prod = args[0]->f64;
    for(int i = 1; i < arg_count; i++) {
      prod /= args[i]->f64;
    }
    return obj_new_double(prod);
  }
  else {
    eval_error = obj_new_string("Can't divide non-numbers.");
    return nil;
  }
}

/* Obj *p_mod(Obj** args, int arg_count) { */
/*   if(arg_count == 0) { */
/*     return obj_new_int(1); */
/*   } */
/*   int prod = args[0]->i; */
/*   for(int i = 1; i < arg_count; i++) { */
/*     prod %= args[i]->i; */
/*   } */
/*   return obj_new_int(prod); */
/* } */

Obj *p_eq(Process *process, Obj **args, int arg_count) {
  if(arg_count < 2) {
    printf("The function '=' requires at least 2 arguments.\n");
    return nil;
  }
  for(int i = 0; i < arg_count - 1; i++) {
    if(!obj_eq(process, args[i], args[i + 1])) {
      return lisp_false;
    }
  }
  return lisp_true;
}

Obj *p_list(Process *process, Obj **args, int arg_count) {
  if(arg_count == 0) {
    return nil; // TODO: don't use a hack like this
  }
  Obj *first = NULL;
  Obj *prev = NULL;
  for(int i = 0; i < arg_count; i++) {
    Obj *new = obj_new_cons(args[i], nil);
    if(!first) {
      first = new;
    }
    if(prev) {
      prev->cdr = new;
    }
    prev = new;
  }
  return first;
}

Obj *p_array(Process *process, Obj **args, int arg_count) {
  Obj *a = obj_new_array(arg_count);
  for(int i = 0; i < arg_count; i++) {
    a->array[i] = args[i];
  }
  return a;
}

Obj *p_dictionary(Process *process, Obj **args, int arg_count) {
  Obj *e = obj_new_environment(NULL);

  //printf("creating dictionary with %d args\n", arg_count);

  if(arg_count == 0) {
    e->bindings = nil;
  }
  else {
    if(arg_count % 2 == 1) {
      set_error_return_nil("Uneven nr of arguments to 'dictionary'. ", nil);
    }

    Obj *first = NULL;
    Obj *prev = NULL;

    for(int i = 0; i < arg_count; i += 2) {
      Obj *pair = obj_new_cons(args[i], args[i + 1]);
      Obj *new = obj_new_cons(pair, nil);
      if(!first) {
        first = new;
      }
      if(prev) {
        prev->cdr = new;
      }
      prev = new;
    }
    e->bindings = first;
  }

  //sprintf("Created dictionary:\n%s\n", obj_to_string(process, e)->s);

  e->hash = obj_hash(process, e);

  return e;
}

Obj *p_str(Process *process, Obj **args, int arg_count) {
  Obj *s = obj_new_string("");
  shadow_stack_push(process, s);
  for(int i = 0; i < arg_count; i++) {
    shadow_stack_push(process, args[i]);
  }
  for(int i = 0; i < arg_count; i++) {
    obj_string_mut_append(s, obj_to_string_not_prn(process, args[i])->s);
  }
  for(int i = 0; i < arg_count; i++) {
    shadow_stack_pop(process); // args
  }
  shadow_stack_pop(process);
  return s;
}

Obj *p_str_append_bang(Process *process, Obj **args, int arg_count) {
  if(arg_count != 2) {
    eval_error = obj_new_string("'str-append!' takes exactly two arguments");
    return nil;
  }
  if(args[0]->tag != 'S') {
    eval_error = obj_new_string("'str-append!' arg0 invalid");
    return nil;
  }
  if(args[1]->tag != 'S') {
    eval_error = obj_new_string("'str-append!' arg1 invalid");
    return nil;
  }
  Obj *s = args[0];
  obj_string_mut_append(s, args[1]->s);
  return s;
}

Obj *p_join(Process *process, Obj **args, int arg_count) {
  if(arg_count != 2) {
    eval_error = obj_new_string("'join' takes exactly two arguments");
    return nil;
  }
  if(args[0]->tag != 'S') {
    eval_error = obj_new_string("First arg to 'join' must be a string");
    return nil;
  }

  if(args[1]->tag == 'C') {
    Obj *s = obj_new_string("");
    shadow_stack_push(process, s);
    Obj *p = args[1];
    while(p && p->car) {
      obj_string_mut_append(s, obj_to_string_not_prn(process, p->car)->s);
      if(p->cdr && p->cdr->cdr) {
        obj_string_mut_append(s, args[0]->s);
      }
      p = p->cdr;
    }
    shadow_stack_pop(process);
    return s;
  }
  else if(args[1]->tag == 'A') {
    Obj *s = obj_new_string("");
    shadow_stack_push(process, s);
    Obj *a = args[1];
    for(int i = 0; i < a->count; i++) {
      obj_string_mut_append(s, obj_to_string_not_prn(process, a->array[i])->s);
      if(i < a->count - 1) {
        obj_string_mut_append(s, args[0]->s);
      }
    }
    shadow_stack_pop(process);
    return s;
  }
  else {
    eval_error = obj_new_string("Second arg to 'join' must be a list");
    return nil;
  }
}

char *str_replace(const char *str, const char *old, const char *new) {

  /* Adjust each of the below values to suit your needs. */

  /* Increment positions cache size initially by this number. */
  size_t cache_sz_inc = 16;
  /* Thereafter, each time capacity needs to be increased,
   * multiply the increment by this factor. */
  const size_t cache_sz_inc_factor = 3;
  /* But never increment capacity by more than this number. */
  const size_t cache_sz_inc_max = 1048576;

  char *pret, *ret = NULL;
  const char *pstr2, *pstr = str;
  size_t i, count = 0;
  ptrdiff_t *pos_cache = NULL;
  size_t cache_sz = 0;
  size_t cpylen, orglen, retlen, newlen, oldlen = strlen(old);

  /* Find all matches and cache their positions. */
  while((pstr2 = strstr(pstr, old)) != NULL) {
    count++;

    /* Increase the cache size when necessary. */
    if(cache_sz < count) {
      cache_sz += cache_sz_inc;
      pos_cache = realloc(pos_cache, sizeof(*pos_cache) * cache_sz);
      if(pos_cache == NULL) {
        goto end_repl_str;
      }
      cache_sz_inc *= cache_sz_inc_factor;
      if(cache_sz_inc > cache_sz_inc_max) {
        cache_sz_inc = cache_sz_inc_max;
      }
    }

    pos_cache[count - 1] = pstr2 - str;
    pstr = pstr2 + oldlen;
  }

  orglen = pstr - str + strlen(pstr);

  /* Allocate memory for the post-replacement string. */
  if(count > 0) {
    newlen = strlen(new);
    retlen = orglen + (newlen - oldlen) * count;
  }
  else
    retlen = orglen;
  ret = malloc(retlen + 1);
  if(ret == NULL) {
    goto end_repl_str;
  }

  if(count == 0) {
    /* If no matches, then just duplicate the string. */
    strcpy(ret, str);
  }
  else {
    /* Otherwise, duplicate the string whilst performing
     * the replacements using the position cache. */
    pret = ret;
    memcpy(pret, str, pos_cache[0]);
    pret += pos_cache[0];
    for(i = 0; i < count; i++) {
      memcpy(pret, new, newlen);
      pret += newlen;
      pstr = str + pos_cache[i] + oldlen;
      cpylen = (i == count - 1 ? orglen : pos_cache[i + 1]) - pos_cache[i] - oldlen;
      memcpy(pret, pstr, cpylen);
      pret += cpylen;
    }
    ret[retlen] = '\0';
  }

end_repl_str:
  /* Free the cache and return the post-replacement string,
   * which will be NULL in the event of an error. */
  free(pos_cache);
  return ret;
}

Obj *p_str_replace(Process *process, Obj **args, int arg_count) {
  if(arg_count != 3) {
    eval_error = obj_new_string("'str-replace' takes exactly three arguments");
    return nil;
  }
  if(args[0]->tag != 'S') {
    eval_error = obj_new_string("'str-replace' arg0 invalid: ");
    obj_string_mut_append(eval_error, obj_to_string(process, args[0])->s);
    return nil;
  }
  if(args[1]->tag != 'S') {
    eval_error = obj_new_string("'str-replace' arg1 invalid");
    return nil;
  }
  if(args[2]->tag != 'S') {
    eval_error = obj_new_string("'str-replace' arg2 invalid");
    return nil;
  }

  char *s = args[0]->s;
  char *lookup = args[1]->s;
  char *replacement = args[2]->s;
  char *replaced = str_replace(s, lookup, replacement);
  return obj_new_string(replaced);
}

Obj *p_copy(Process *process, Obj **args, int arg_count) {
  if(arg_count != 1) {
    eval_error = obj_new_string("'copy' takes exactly one argument");
    return nil;
  }
  Obj *a = args[0];

  if(!a) {
    set_error_return_nil("Trying to copy NULL", nil);
  }

  //printf("Will make a copy of: %s\n", obj_to_string(a)->s);
  Obj *b = obj_copy(process, a);
  return b;
}

Obj *p_print(Process *process, Obj **args, int arg_count) {
  for(int i = 0; i < arg_count; i++) {
    obj_print_not_prn(process, args[i]);
  }
  return nil;
}

Obj *p_prn(Process *process, Obj **args, int arg_count) {
  Obj *s = obj_new_string("");
  shadow_stack_push(process, s);
  for(int i = 0; i < arg_count; i++) {
    shadow_stack_push(process, args[i]);
  }
  for(int i = 0; i < arg_count; i++) {
    Obj *s2 = obj_to_string(process, args[i]);
    obj_string_mut_append(s, s2->s);
  }
  for(int i = 0; i < arg_count; i++) {
    shadow_stack_pop(process);
  }
  shadow_stack_pop(process); // s
  return s;
}

Obj *p_println(Process *process, Obj **args, int arg_count) {
  for(int i = 0; i < arg_count; i++) {
    obj_print_not_prn(process, args[i]);
    /* if(i < arg_count - 1) { */
    /*   printf(" "); */
    /* } */
  }
  printf("\n");
  return nil;
}

Obj *p_system(Process *process, Obj **args, int arg_count) {
  if(arg_count != 1) {
    printf("Wrong argument count to 'system'\n");
    return nil;
  }
  if(args[0]->tag != 'S') {
    printf("'system' takes a string as its argument\n");
    return nil;
  }
  system(args[0]->s);
  return nil;
}

Obj *p_get(Process *process, Obj **args, int arg_count) {
  if(arg_count != 2) {
    printf("Wrong argument count to 'get'\n");
    return nil;
  }
  if(args[0]->tag == 'E') {
    Obj *o = env_lookup(process, args[0], args[1]);
    if(o) {
      return o;
    }
    else {
      Obj *s = obj_new_string("Can't get key '");
      shadow_stack_push(process, s);
      obj_string_mut_append(s, obj_to_string(process, args[1])->s);
      obj_string_mut_append(s, "' in dict:\n");
      obj_string_mut_append(s, obj_to_string(process, args[0])->s);
      obj_string_mut_append(s, "");
      eval_error = s;
      shadow_stack_pop(process);
      return nil;
    }
  }
  else if(args[0]->tag == 'C') {
    if(args[1]->tag != 'I') {
      eval_error = obj_new_string("get requires arg 1 to be an integer\n");
      return nil;
    }
    int i = 0;
    int n = args[1]->i;
    Obj *p = args[0];
    while(p && p->car) {
      if(i == n) {
        return p->car;
      }
      p = p->cdr;
      i++;
    }
    eval_error = obj_new_string("Index ");
    obj_string_mut_append(eval_error, obj_to_string(process, obj_new_int(i))->s);
    obj_string_mut_append(eval_error, " out of bounds in");
    obj_string_mut_append(eval_error, obj_to_string(process, args[0])->s);
    return nil;
  }
  else {
    eval_error = obj_new_string("'get' requires arg 0 to be a dictionary or list: ");
    obj_string_mut_append(eval_error, obj_to_string(process, args[0])->s);
    return nil;
  }
}

Obj *p_get_maybe(Process *process, Obj **args, int arg_count) {
  if(arg_count != 2) {
    printf("Wrong argument count to 'get-maybe'\n");
    return nil;
  }
  if(args[0]->tag == 'E') {
    Obj *o = env_lookup(process, args[0], args[1]);
    if(o) {
      return o;
    }
    else {
      return nil;
    }
  }
  else if(args[0]->tag == 'C') {
    if(args[1]->tag != 'I') {
      eval_error = obj_new_string("get-maybe requires arg 1 to be an integer\n");
      return nil;
    }
    int i = 0;
    int n = args[1]->i;
    Obj *p = args[0];
    while(p && p->car) {
      if(i == n) {
        return p->car;
      }
      p = p->cdr;
      i++;
    }
    return nil;
  }
  else {
    set_error_return_nil("'get-maybe' requires arg 0 to be a dictionary or list:\n", args[0]);
  }
}

Obj *p_dict_set_bang(Process *process, Obj **args, int arg_count) {
  if(arg_count != 3) {
    printf("Wrong argument count to 'dict-set!'\n");
    return nil;
  }
  if(args[0]->tag == 'E') {
    return env_assoc(process, args[0], args[1], args[2]);
  }
  else if(args[0]->tag == 'C') {
    if(args[1]->tag != 'I') {
      eval_error = obj_new_string("dict-set! requires arg 1 to be an integer\n");
      return nil;
    }
    int i = 0;
    int n = args[1]->i;
    Obj *p = args[0];
    while(p && p->car) {
      if(i == n) {
        p->car = args[2];
        return nil;
      }
      p = p->cdr;
      i++;
    }
    eval_error = obj_new_string("Index ");
    obj_string_mut_append(eval_error, obj_to_string(process, obj_new_int(i))->s);
    obj_string_mut_append(eval_error, " out of bounds in");
    obj_string_mut_append(eval_error, obj_to_string(process, args[0])->s);
    return nil;
  }
  else {
    printf("'dict-set!' requires arg 0 to be a dictionary: %s\n", obj_to_string(process, args[0])->s);
    return nil;
  }
}

Obj *p_dict_remove_bang(Process *process, Obj **args, int arg_count) {
  if(arg_count != 2) {
    printf("Wrong argument count to 'dict-remove!'\n");
    return nil;
  }
  if(args[0]->tag != 'E') {
    printf("'dict-remove!' requires arg 0 to be a dictionary: %s\n", obj_to_string(process, args[0])->s);
    return nil;
  }

  Obj *prev = NULL;
  Obj *p = args[0]->bindings;
  while(p && p->car) {
    Obj *pair = p->car;
    if(obj_eq(process, pair->car, args[1])) {
      if(prev) {
        prev->cdr = p->cdr;
      }
      else {
        args[0]->bindings = p->cdr;
      }
      break;
    }
    else {
      prev = p;
      p = p->cdr;
    }
  }

  return args[0];
}

Obj *p_first(Process *process, Obj **args, int arg_count) {
  if(arg_count != 1) {
    set_error_return_nil("Wrong argument count to 'first'. ", nil);
  }
  if(args[0]->tag != 'C') {
    set_error_return_nil("'first' requires arg 0 to be a list: ", args[0]);
  }
  if(args[0]->car == NULL) {
    set_error_return_nil("Can't take first element of empty list. ", nil);
    return nil;
  }
  return args[0]->car;
}

Obj *p_rest(Process *process, Obj **args, int arg_count) {
  if(arg_count != 1) {
    printf("Wrong argument count to 'rest'\n");
    return nil;
  }
  if(args[0]->tag != 'C') {
    char buffer[512];
    snprintf(buffer, 512, "'rest' requires arg 0 to be a list: %s\n", obj_to_string(process, args[0])->s);
    eval_error = obj_new_string(strdup(buffer));
    return nil;
  }
  if(args[0]->cdr == NULL) {
    set_error_return_nil("Can't take rest of empty list. ", nil);
    return nil;
  }
  return args[0]->cdr;
}

Obj *p_cons(Process *process, Obj **args, int arg_count) {
  if(arg_count != 2) {
    printf("Wrong argument count to 'cons'\n");
    return nil;
  }
  if(args[1]->tag != 'C') {
    char buffer[512];
    snprintf(buffer, 512, "'cons' requires arg 1 to be a list: %s\n", obj_to_string(process, args[1])->s);
    eval_error = obj_new_string(strdup(buffer));
    return nil;
  }
  Obj *new_cons = obj_new_cons(args[0], args[1]);
  return new_cons;
}

Obj *p_cons_last(Process *process, Obj **args, int arg_count) {
  if(arg_count != 2) {
    printf("Wrong argument count to 'cons'\n");
    return nil;
  }
  if(args[0]->tag != 'C') {
    printf("'rest' requires arg 0 to be a list: %s\n", obj_to_string(process, args[1])->s);
    return nil;
  }
  Obj *new_list = obj_copy(process, args[0]);
  Obj *p = new_list;
  while(p->cdr) {
    p = p->cdr;
  }
  Obj *last = p;
  Obj *new_nil = obj_new_cons(NULL, NULL);
  last->car = args[1];
  last->cdr = new_nil;
  return new_list;
}

Obj *p_concat(Process *process, Obj **args, int arg_count) {
  if(arg_count == 0) {
    return nil;
  }

  for(int i = 0; i < arg_count; i++) {
    if(args[i]->tag != 'C') {
      eval_error = obj_new_string("'concat' requires all args to be lists\n");
      return nil;
    }
  }

  int i = 0;
  Obj *new = obj_copy(process, args[i]);

  while(!new->car) {
    ++i;
    if(i >= arg_count) {
      return nil;
    }
    new = args[i];
  }

  Obj *last = new;

  for(i++; i < arg_count; i++) {
    //printf("Will concat %s\n", obj_to_string(args[i])->s);
    if(!last->cdr) {
      // continue
    }
    else {
      while(last->cdr->cdr) {
        last = last->cdr;
      }
      Obj *o = args[i];
      if(o->car) {
        last->cdr = obj_copy(process, o);
      }
    }
  }
  return new;
}

Obj *p_nth(Process *process, Obj **args, int arg_count) {
  if(arg_count != 2) {
    eval_error = obj_new_string("Wrong argument count to 'nth'\n");
    return nil;
  }
  if(args[1]->tag != 'I') {
    set_error_return_nil("'nth' requires arg 1 to be an integer: ", args[1]);
  }
  if(args[0]->tag == 'C') {
    int i = 0;
    int n = args[1]->i;
    Obj *p = args[0];
    while(p && p->car) {
      if(i == n) {
        return p->car;
      }
      p = p->cdr;
      i++;
    }
    eval_error = obj_new_string("Index ");
    obj_string_mut_append(eval_error, obj_to_string(process, args[1])->s);
    obj_string_mut_append(eval_error, " out of bounds in ");
    obj_string_mut_append(eval_error, obj_to_string(process, args[0])->s);
    return nil;
  }
  else if(args[0]->tag == 'A') {
    Obj *a = args[0];
    int index = args[1]->i;
    if(index < 0 || index >= a->count) {
      set_error_return_nil("Index out of bounds in ", a);
    }
    else {
      return a->array[index];
    }
  }
  else {
    set_error_return_nil("'nth' requires arg 0 to be a list or array\n", args[0]);
  }
}

Obj *p_count(Process *process, Obj **args, int arg_count) {
  if(arg_count != 1) {
    set_error_return_nil("Wrong argument count to 'count'. ", nil);
  }
  if(args[0]->tag == 'C') {
    int i = 0;
    Obj *p = args[0];
    while(p && p->car) {
      p = p->cdr;
      i++;
    }
    return obj_new_int(i);
  }
  else if(args[0]->tag == 'A') {
    return obj_new_int(args[0]->count);
  }
  else {
    set_error_return_nil("'count' requires arg 0 to be a list or array: ", args[0]);
  }
}

bool is_callable(Obj *obj) {
  return obj->tag == 'P' || obj->tag != 'L' || obj->tag != 'F';
}

Obj *p_map(Process *process, Obj **args, int arg_count) {
  if(arg_count != 2) {
    eval_error = obj_new_string("Wrong argument count to 'map'.");
    return nil;
  }
  if(!is_callable(args[0])) {
    set_error_return_nil("'map' requires arg 0 to be a function or lambda: \n", args[0]);
  }
  Obj *f = args[0];
  if(args[1]->tag == 'C') {
    Obj *p = args[1];
    Obj *list = obj_new_cons(NULL, NULL);
    shadow_stack_push(process, list);
    Obj *prev = list;
    int shadow_count = 0;
    while(p && p->car) {
      Obj *arg[1] = {p->car};
      apply(process, f, arg, 1);
      prev->car = stack_pop(process);
      Obj *new = obj_new_cons(NULL, NULL);
      shadow_stack_push(process, new);
      shadow_count++;
      prev->cdr = new;
      prev = new;
      p = p->cdr;
    }
    for(int i = 0; i < shadow_count; i++) {
      shadow_stack_pop(process);
    }
    shadow_stack_pop(process); // list
    return list;
  }
  else if(args[1]->tag == 'A') {
    Obj *a = args[1];
    Obj *new_a = obj_new_array(a->count);
    shadow_stack_push(process, new_a);
    for(int i = 0; i < a->count; i++) {
      Obj *arg[1] = {a->array[i]};
      apply(process, f, arg, 1);
      new_a->array[i] = stack_pop(process);
    }
    shadow_stack_pop(process); // new_a
    return new_a;
  }

  Obj *type_lookup;
  Obj *o = args[1];
  if(o->meta && (type_lookup = env_lookup(process, o->meta, obj_new_keyword("type")))) {
    if(type_lookup->tag == 'C' && type_lookup->cdr->car && obj_eq(process, type_lookup->car, obj_new_keyword("Array"))) {
      Obj *inner_type = type_lookup->cdr->car;
      Array *a = o->void_ptr;
      Obj *new_a = obj_new_array(a->count);
      shadow_stack_push(process, new_a);
      for(int i = 0; i < a->count; i++) {
        Obj *arg[1];
        if(obj_eq(process, inner_type, type_string)) {
          arg[0] = obj_new_string(((char **)(a->data))[i]);
        }
        else if(obj_eq(process, inner_type, type_char)) {
          arg[0] = obj_new_char(((char *)(a->data))[i]);
        }
        else if(obj_eq(process, inner_type, type_float)) {
          arg[0] = obj_new_float(((float *)(a->data))[i]);
        }
        else if(obj_eq(process, inner_type, type_double)) {
          arg[0] = obj_new_double(((float *)(a->data))[i]);
        }
        else if(obj_eq(process, inner_type, type_int)) {
          arg[0] = obj_new_int(((int *)(a->data))[i]);
        }
        else {
          arg[0] = obj_new_ptr(((void **)(a->data))[i]);
          //set_error_return_nil("Map over void_ptr to array can't handle type: ", inner_type);
        }
        apply(process, f, arg, 1);
        new_a->array[i] = stack_pop(process);
      }
      shadow_stack_pop(process); // new_a
      return new_a;
    }
  }

  set_error_return_nil("'map' requires arg 1 to be a list or array: ", args[1]);
}

Obj *p_map2(Process *process, Obj **args, int arg_count) {
  if(arg_count != 3) {
    set_error_return_nil("Wrong argument count to 'map2'. ", nil);
  }
  if(!is_callable(args[0])) {
    set_error_return_nil("'map2' requires arg 0 to be a function or lambda: ", args[0]);
  }
  Obj *f = args[0];
  if(args[1]->tag == 'C' && args[2]->tag == 'C') {
    Obj *p = args[1];
    Obj *p2 = args[2];
    Obj *list = obj_new_cons(NULL, NULL);
    shadow_stack_push(process, list);
    Obj *prev = list;
    int shadow_count = 0;
    while(p && p->car && p2 && p2->car) {
      Obj *argz[2] = {p->car, p2->car};
      apply(process, f, argz, 2);
      prev->car = stack_pop(process);
      Obj *new = obj_new_cons(NULL, NULL);
      shadow_stack_push(process, new);
      shadow_count++;
      prev->cdr = new;
      prev = new;
      p = p->cdr;
      p2 = p2->cdr;
    }
    for(int i = 0; i < shadow_count; i++) {
      shadow_stack_pop(process);
    }
    shadow_stack_pop(process); // list
    return list;
  }
  else if(args[1]->tag == 'A' && args[2]->tag == 'A') {
    if(args[1]->count != args[2]->count) {
      eval_error = obj_new_string("Arrays to map2 are of different length.");
      return nil;
    }
    Obj *a = args[1];
    Obj *b = args[2];
    Obj *new_a = obj_new_array(a->count);
    shadow_stack_push(process, new_a);
    for(int i = 0; i < a->count; i++) {
      Obj *fargs[2] = {a->array[i], b->array[i]};
      apply(process, f, fargs, 2);
      new_a->array[i] = stack_pop(process);
    }
    shadow_stack_pop(process); // new_a
    return new_a;
  }
  else {
    eval_error = obj_new_string("'map2' requires both arg 1 and 2 to be lists or arrays:\n");
    obj_string_mut_append(eval_error, obj_to_string(process, args[1])->s);
    obj_string_mut_append(eval_error, "\n");
    obj_string_mut_append(eval_error, obj_to_string(process, args[2])->s);
    return nil;
  }
}

Obj *p_keys(Process *process, Obj **args, int arg_count) {
  if(arg_count != 1) {
    printf("Wrong argument count to 'keys'\n");
    return nil;
  }
  if(args[0]->tag != 'E') {
    printf("'keys' requires arg 0 to be a dictionary.\n");
    return nil;
  }
  Obj *p = args[0]->bindings;
  Obj *list = obj_new_cons(NULL, NULL);
  Obj *prev = list;
  while(p && p->car) {
    Obj *new = obj_new_cons(NULL, NULL);
    prev->car = p->car->car;
    prev->cdr = new;
    prev = new;
    p = p->cdr;
  }
  return list;
}

Obj *p_values(Process *process, Obj **args, int arg_count) {
  if(arg_count != 1) {
    printf("Wrong argument count to 'values'\n");
    return nil;
  }
  if(args[0]->tag != 'E') {
    printf("'values' requires arg 0 to be a dictionary.\n");
    return nil;
  }
  Obj *p = args[0]->bindings;
  Obj *list = obj_new_cons(NULL, NULL);
  Obj *prev = list;
  while(p && p->car) {
    Obj *new = obj_new_cons(NULL, NULL);
    prev->car = p->car->cdr;
    prev->cdr = new;
    prev = new;
    p = p->cdr;
  }
  return list;
}

Obj *p_signature(Process *process, Obj **args, int arg_count) {
  if(arg_count != 1) {
    eval_error = obj_new_string("Wrong argument count to 'signature'");
    return nil;
  }
  if(args[0]->tag == 'F') {
    Obj *a = obj_copy(process, args[0]->arg_types);
    Obj *b = args[0]->return_type;
    Obj *sig = obj_list(obj_new_keyword("fn"), a, b);
    return sig;
  }
  else if(args[0]->tag == 'P' || args[0]->tag == 'L') {
    if(!args[0]->meta) {
      return nil;
    }
    Obj *sig = env_lookup(process, args[0]->meta, obj_new_keyword("signature"));
    if(sig) {
      return sig;
    }
    else {
      return nil;
    }
  }
  else {
    eval_error = obj_new_string("'signature' requires arg 0 to be some kind of function.");
    return nil;
  }
}

Obj *p_null_predicate(Process *process, Obj **args, int arg_count) {
  if(arg_count != 1) {
    eval_error = obj_new_string("Wrong argument count to 'null?'");
    return nil;
  }
  if(args[0]->tag != 'Q') {
    eval_error = obj_new_string("Argument to 'null?' must be void pointer.");
    return nil;
  }
  if(args[0]->void_ptr == NULL) {
    return lisp_true;
  }
  else {
    return lisp_false;
  }
}

Obj *p_filter(Process *process, Obj **args, int arg_count) {
  if(arg_count != 2) {
    set_error_return_nil("Wrong argument count to 'filter'.", nil);
  }
  if(!is_callable(args[0])) {
    set_error_return_nil("'filter' requires arg 0 to be a function or lambda: ", args[0]);
  }
  Obj *f = args[0];
  if(args[1]->tag == 'C') {
    Obj *p = args[1];
    Obj *list = obj_new_cons(NULL, NULL);
    shadow_stack_push(process, list);
    Obj *prev = list;
    int shadow_count = 0;
    while(p && p->car) {
      Obj *arg[1] = {p->car};
      apply(process, f, arg, 1);
      Obj *result = stack_pop(process);
      if(is_true(result)) {
        Obj *new = obj_new_cons(NULL, NULL);
        shadow_stack_push(process, new);
        shadow_count++;
        prev->car = p->car;
        prev->cdr = new;
        prev = new;
      }
      p = p->cdr;
    }
    for(int i = 0; i < shadow_count; i++) {
      shadow_stack_pop(process);
    }
    shadow_stack_pop(process); // list
    return list;
  }
  else if(args[1]->tag == 'A') {
    Obj *a = args[1];
    Obj **temp = malloc(sizeof(Obj *) * a->count);
    int count = 0;
    for(int i = 0; i < a->count; i++) {
      Obj *arg[1] = {a->array[i]};
      apply(process, f, arg, 1);
      Obj *result = stack_pop(process);
      if(is_true(result)) {
        temp[count] = a->array[i];
        count++;
      }
    }
    Obj *a_new = obj_new_array(count);
    for(int i = 0; i < count; i++) {
      a_new->array[i] = temp[i];
    }
    free(temp);
    return a_new;
  }
  else {
    set_error_return_nil("'filter' requires arg 1 to be a list or array: ", args[1]);
  }
}

Obj *p_reduce(Process *process, Obj **args, int arg_count) {
  if(arg_count != 3) {
    printf("Wrong argument count to 'reduce'\n");
    return nil;
  }
  if(!is_callable(args[0])) {
    set_error_return_nil("'reduce' requires arg 0 to be a function or lambda: %s (%c)\n", args[0]);
  }
  Obj *f = args[0];
  Obj *total = args[1];
  if(args[2]->tag == 'C') {
    Obj *p = args[2];
    while(p && p->car) {
      Obj *args[2] = {total, p->car};
      apply(process, f, args, 2);
      total = stack_pop(process);
      p = p->cdr;
    }
    return total;
  }
  else if(args[2]->tag == 'A') {
    Obj *a = args[2];
    for(int i = 0; i < a->count; i++) {
      Obj *args[2] = {total, a->array[i]};
      apply(process, f, args, 2);
      total = stack_pop(process);
    }
    return total;
  }
  else {
    set_error_return_nil("'reduce' requires arg 2 to be a list or array: ", args[2]);
  }
}

Obj *p_apply(Process *process, Obj **args, int arg_count) {
  if(arg_count != 2) {
    printf("'apply' takes two arguments.\n");
    return nil;
  }
  if(args[0]->tag != 'P' && args[0]->tag != 'L' && args[0]->tag != 'F') {
    printf("'apply' requires arg 0 to be a function or lambda: %s (%c)\n", obj_to_string(process, args[0])->s, args[0]->tag);
    eval_error = obj_new_string("");
    return nil;
  }
  if(args[1]->tag != 'C') {
    printf("'apply' requires arg 1 to be a list: %s (%c)\n", obj_to_string(process, args[0])->s, args[0]->tag);
    eval_error = obj_new_string("");
    return nil;
  }
  Obj *p = args[1];
  int apply_arg_count = 0;
  while(p && p->car) {
    apply_arg_count++;
    p = p->cdr;
  }
  Obj **apply_args = NULL;
  if(apply_arg_count > 0) {
    apply_args = malloc(sizeof(Obj *) * apply_arg_count);
  }
  Obj *q = args[1];
  for(int i = 0; i < apply_arg_count; i++) {
    apply_args[i] = q->car;
    q = q->cdr;
  }
  apply(process, args[0], apply_args, apply_arg_count);
  free(apply_args);
  return stack_pop(process);
}

Obj *p_type(Process *process, Obj **args, int arg_count) {
  if(arg_count != 1) {
    printf("'type' takes one argument.\n");
    return nil;
  }
  if(args[0]->tag == 'S') {
    return type_string;
  }
  else if(args[0]->tag == 'I') {
    return type_int;
  }
  else if(args[0]->tag == 'V') {
    return type_float;
  }
  else if(args[0]->tag == 'W') {
    return type_double;
  }
  else if(args[0]->tag == 'C') {
    return type_list;
  }
  else if(args[0]->tag == 'L') {
    return type_lambda;
  }
  else if(args[0]->tag == 'P') {
    return type_primop;
  }
  else if(args[0]->tag == 'F') {
    return type_foreign;
  }
  else if(args[0]->tag == 'E') {
    return type_env;
  }
  else if(args[0]->tag == 'Y') {
    return type_symbol;
  }
  else if(args[0]->tag == 'K') {
    return type_keyword;
  }
  else if(args[0]->tag == 'Q') {
    return type_ptr;
  }
  else if(args[0]->tag == 'M') {
    return type_macro;
  }
  else if(args[0]->tag == 'T') {
    return type_char;
  }
  else if(args[0]->tag == 'A') {
    return type_array;
  }
  else if(args[0]->tag == 'B') {
    return type_bool;
  }
  else if(args[0]->tag == 'R') {
    return type_ptr_to_global;
  }
  else {
    printf("Unknown type tag: %c\n", args[0]->tag);
    //eval_error = obj_new_string("Unknown type.");
    return nil;
  }
}

Obj *p_lt(Process *process, Obj **args, int arg_count) {
  if(arg_count == 0) {
    return lisp_true;
  }
  if(args[0]->tag == 'I') {
    int smallest = args[0]->i;
    for(int i = 1; i < arg_count; i++) {
      assert_or_set_error_return_nil(args[i]->tag == 'I', "< for ints called with non-int: ", args[0]);
      if(smallest >= args[i]->i) {
        return lisp_false;
      }
      smallest = args[i]->i;
    }
    return lisp_true;
  }
  else if(args[0]->tag == 'V') {
    float smallest = args[0]->f32;
    for(int i = 1; i < arg_count; i++) {
      assert_or_set_error_return_nil(args[i]->tag == 'V', "< for floats called with non-float: ", args[0]);
      if(smallest >= args[i]->f32) {
        return lisp_false;
      }
      smallest = args[i]->f32;
    }
    return lisp_true;
  }
  else if(args[0]->tag == 'W') {
    double smallest = args[0]->f64;
    for(int i = 1; i < arg_count; i++) {
      assert_or_set_error_return_nil(args[i]->tag == 'W', "< for doubles called with non-double: ", args[0]);
      if(smallest >= args[i]->f64) {
        return lisp_false;
      }
      smallest = args[i]->f64;
    }
    return lisp_true;
  }
  else {
    eval_error = obj_new_string("Can't call < on non-numbers.");
    return lisp_false;
  }
}

/*
  int current_timestamp() {
  struct timeval te;
  gettimeofday(&te, NULL); // get current time
  long long milliseconds = te.tv_sec * 1000LL + te.tv_usec / 1000; // calculate milliseconds
  return milliseconds;
  }
*/

Obj *p_now(Process *process, Obj **args, int arg_count) {
  if(arg_count != 0) {
    printf("Wrong argument count to 'now'\n");
    return nil;
  }
  return obj_new_int(carp_millitime());
}

Obj *p_name(Process *process, Obj **args, int arg_count) {
  if(arg_count != 1) {
    eval_error = obj_new_string("Wrong arg count to 'name'.");
    return nil;
  }
  if(args[0]->tag != 'S' && args[0]->tag != 'Y' && args[0]->tag != 'K') {
    Obj *s = obj_new_string("Argument to 'name' must be string, keyword or symbol: ");
    shadow_stack_push(process, s);
    obj_string_mut_append(s, obj_to_string(process, args[0])->s);
    eval_error = s;
    shadow_stack_pop(process);
    return nil;
  }
  return obj_new_string(args[0]->s);
}

Obj *p_symbol(Process *process, Obj **args, int arg_count) {
  if(arg_count != 1) {
    eval_error = obj_new_string("Wrong arg count to 'symbol'.");
    return nil;
  }
  if(args[0]->tag != 'S') {
    Obj *s = obj_new_string("Argument to 'symbol' must be string: ");
    shadow_stack_push(process, s);
    obj_string_mut_append(s, obj_to_string(process, args[0])->s);
    eval_error = s;
    shadow_stack_pop(process);
    return nil;
  }
  return obj_new_symbol(args[0]->s);
}

Obj *p_keyword(Process *process, Obj **args, int arg_count) {
  if(arg_count != 1) {
    eval_error = obj_new_string("Wrong arg count to 'keyword'.");
    return nil;
  }
  if(args[0]->tag != 'S') {
    Obj *s = obj_new_string("Argument to 'keyword' must be string: ");
    shadow_stack_push(process, s);
    obj_string_mut_append(s, obj_to_string(process, args[0])->s);
    eval_error = s;
    shadow_stack_pop(process);
    return nil;
  }
  return obj_new_keyword(args[0]->s);
}

Obj *p_error(Process *process, Obj **args, int arg_count) {
  if(arg_count != 1) {
    eval_error = obj_new_string("Wrong argument count to 'error'\n");
    return nil;
  }
  eval_error = args[0];
  return nil;
}

Obj *p_env(Process *process, Obj **args, int arg_count) {
#if BYTECODE_EVAL
  return process->frames[process->frame].env;
#else
  return process->global_env->bindings;
#endif
}

Obj *p_def_QMARK(Process *process, Obj **args, int arg_count) {
  if(arg_count != 1) {
    eval_error = obj_new_string("Wrong argument count to 'def?'\n");
    return nil;
  }
  if(args[0]->tag != 'Y') {
    eval_error = obj_new_string("Must send symbol to 'def?'\n");
    return nil;
  }
  Obj *binding = env_lookup_binding(process, process->global_env, args[0]);
  if(binding && binding->car && binding->cdr) {
    //printf("Binding exists: %s\n", obj_to_string(process, binding)->s);
    return lisp_true;
  }
  else {
    return lisp_false;
  }
}

Obj *p_load_lisp(Process *process, Obj **args, int arg_count) {
  Obj *file_string = open_file(process, args[0]->s);
  shadow_stack_push(process, file_string);
  if(file_string->tag == 'S') {
    Obj *forms = read_string(process, process->global_env, file_string->s, args[0]);
    shadow_stack_push(process, forms);
    Obj *form = forms;
    while(form && form->car) {
#if BYTECODE_EVAL
      /* Obj *discarded_result = */ bytecode_sub_eval_form(process, process->global_env, form->car);
#else
      eval_internal(process, process->global_env, form->car);
      /*Obj *discarded_result = */ stack_pop(process);
#endif
      if(eval_error) {
        return nil;
      }
      form = form->cdr;
    }
    shadow_stack_pop(process); // forms
  }
  shadow_stack_pop(process); // file_string
  return nil;
}

Obj *p_load_dylib(Process *process, Obj **args, int arg_count) {
  char *filename = args[0]->s;
  carp_library_t handle = carp_load_library(filename);
  if(!handle) {
    set_error_and_return("Failed to open dylib: ", args[0]);
    return nil;
  }
  const char *load_error;
  if((load_error = carp_get_load_library_error()) != NULL) {
    set_error_and_return("Failed to load dylib: ", args[0]);
  }
  //printf("dlopen %p\n", handle);
  return obj_new_dylib(handle);
}

Obj *p_unload_dylib(Process *process, Obj **args, int arg_count) {
  //assert_or_return_nil(arg_count == 1, "'unload-dylib' must take one argument.");
  //assert_or_return_nil(args[0]->tag, "'unload-dylib' must take dylib as argument.", args[0]);
  if(!(args[0]->tag == 'D')) {
    set_error_and_return("unload-dylib takes a dylib as argument: ", args[0]);
    return nil;
  }
  carp_library_t handle = args[0]->dylib;
  if(!handle) {
    return obj_new_symbol("no handle to unload");
  }
  //printf("dlclose %p\n", handle);
  int result = carp_unload_library(handle);
  if(result) {
    eval_error = obj_new_string(carp_get_load_library_error());
    return nil;
  }
  else {
    return obj_new_keyword("done");
  }
}

Obj *p_read(Process *process, Obj **args, int arg_count) {
  if(arg_count != 1) {
    set_error_and_return("'read' takes one argument: ", args[0]);
    return nil;
  }
  if(args[0]->tag != 'S') {
    set_error_and_return("'read' takes a string as argument: ", args[0]);
    return nil;
  }
  Obj *forms = read_string(process, process->global_env, args[0]->s, obj_new_string("p_read"));
  return forms->car;
}

Obj *p_read_many(Process *process, Obj **args, int arg_count) {
  if(arg_count != 1) {
    set_error_and_return("'read-many' takes one argument: ", args[0]);
    return nil;
  }
  if(args[0]->tag != 'S') {
    set_error_and_return("'read-many' takes a string as argument: ", args[0]);
    return nil;
  }
  Obj *forms = read_string(process, process->global_env, args[0]->s, obj_new_string("p_read_many"));
  return forms;
}

Obj *p_eval(Process *process, Obj **args, int arg_count) {
  if(arg_count != 1) {
    eval_error = obj_new_string("Wrong argument count to 'eval'");
    return nil;
  }
  shadow_stack_push(process, args[0]);

#if BYTECODE_EVAL

  Obj *result = bytecode_sub_eval_form(process, process->global_env, args[0]);
  shadow_stack_pop(process);
  return result;

#else

  eval_internal(process, process->global_env, args[0]);
  Obj *result = stack_pop(process);
  shadow_stack_pop(process);
  return result;

#endif
}

Obj *p_code(Process *process, Obj **args, int arg_count) {
  if(args[0]->tag != 'L' && args[0]->tag != 'M') {
    set_error_and_return("'code' must take lambda/macro as argument: ", args[0]);
  }
  if(!args[0]->code) {
    set_error_and_return("code of lambda/macro is NULL: ", args[0]);
  }
  Obj *code = args[0]->code;
  return code;
}

ffi_type *lisp_type_to_ffi_type(Process *process, Obj *type_obj) {

  // Is it a ref type? (borrowed)
  if(type_obj->tag == 'C' && type_obj->car && type_obj->cdr && type_obj->cdr->car && obj_eq(process, type_obj->car, type_ref)) {
    type_obj = type_obj->cdr->car; // the second element of the list
    //printf("Found ref type, inner type is: %s\n", obj_to_string(type_obj)->s);
  }

  if(obj_eq(process, type_obj, type_string)) {
    return &ffi_type_pointer;
  }
  else if(obj_eq(process, type_obj, type_int)) {
    return &ffi_type_sint;
  }
  else if(obj_eq(process, type_obj, type_float)) {
    return &ffi_type_float;
  }
  else if(obj_eq(process, type_obj, type_double)) {
    return &ffi_type_double;
  }
  else if(obj_eq(process, type_obj, type_void)) {
    return &ffi_type_uint;
  }
  else if(obj_eq(process, type_obj, type_bool)) {
    return &ffi_type_uint;
  }
  else if(obj_eq(process, type_obj, type_char)) {
    return &ffi_type_schar;
  }
  else {
    return &ffi_type_pointer; // Assume it's a user defined type
    /* error = obj_new_string("Unhandled return type for foreign function: "); */
    /* obj_string_mut_append(error, obj_to_string(type_obj)->s); */
    /* return NULL; */
  }
}

char *lispify(char *name) {
  char *s0 = str_replace(name, "_", "-");
  char *s1 = str_replace(s0, "BANG", "!");
  char *s2 = str_replace(s1, "QMARK", "?");
  char *s3 = str_replace(s2, "PTR", "*");
  char *s4 = str_replace(s3, "LT", "<");
  char *s5 = str_replace(s4, "GT", ">");
  char *s6 = str_replace(s5, "EQ", "=");
  free(s0);
  free(s1);
  free(s2);
  free(s3);
  free(s4);
  free(s5);
  return s6;
}

ffi_type **make_arg_type_array(Process *process, Obj *args, int arg_count, char *func_name) {
  ffi_type **arg_types_c_array = malloc(sizeof(ffi_type *) * (arg_count + 1));

  Obj *p = args;
  for(int i = 0; i < arg_count; i++) {
    ffi_type *arg_type = lisp_type_to_ffi_type(process, p->car);
    if(!arg_type) {
      free(arg_types_c_array);
      char buffer[512];
      snprintf(buffer, 512, "Arg %d for function %s has invalid type: %s\n", i, func_name, obj_to_string(process, p->car)->s);
      eval_error = obj_new_string(strdup(buffer));
      return NULL;
    }
    arg_types_c_array[i] = arg_type;
    p = p->cdr;
  }
  arg_types_c_array[arg_count] = NULL; // ends with a NULL so we don't need to store arg_count
  return arg_types_c_array;
}

ffi_cif *create_cif(Process *process, Obj *args, int arg_count, Obj *return_type_obj, char *func_name) {
  ffi_type **arg_types_c_array = make_arg_type_array(process, args, arg_count, func_name);

  if(!arg_types_c_array) {
    return NULL;
  }

  ffi_type *return_type = lisp_type_to_ffi_type(process, return_type_obj);

  if(!return_type) {
    free(arg_types_c_array);
    return NULL;
  }

  //printf("Registering %s with %d args\n", func_name, arg_count);

  ffi_cif *cif = malloc(sizeof(ffi_cif));
  int init_result = ffi_prep_cif(cif,
                                 FFI_DEFAULT_ABI,
                                 arg_count,
                                 return_type,
                                 arg_types_c_array);

  if(init_result != FFI_OK) {
    printf("Registration of foreign function %s failed.\n", func_name);
    return NULL;
  }

  return cif;
}

Obj *register_ffi_internal(Process *process, char *name, VoidFn funptr, Obj *args, Obj *return_type_obj, bool builtin) {

  if(!funptr) {
    printf("funptr for %s is NULL\n", name);
    return nil;
  }

  int arg_count = 0;
  Obj *p = args;
  while(p && p->car) {
    p = p->cdr;
    arg_count++;
  }
  //printf("Arg count for %s: %d\n", name, arg_count);

  ffi_cif *cif = create_cif(process, args, arg_count, return_type_obj, name);
  if(!cif) {
    return nil;
  }

  //printf("Registration of '%s' OK.\n", name);

  Obj *ffi = obj_new_ffi(name, cif, funptr, args, return_type_obj);

  if(!ffi->meta) {
    ffi->meta = obj_new_environment(NULL);
  }
  env_assoc(process, ffi->meta, obj_new_keyword("name"), obj_new_string(name));

  char *lispified_name = lispify(name);
  //printf("Registering %s\n", lispified_name);

  global_env_extend(process, obj_new_symbol(lispified_name), ffi);

  return ffi;
}

Obj *register_ffi_variable_internal(Process *process, char *name, void *varptr, Obj *var_type_obj) {

  if(!varptr) {
    printf("varptr for %s is NULL\n", name);
    return nil;
  }

  /* Obj *new_variable_value; */

  /* if(obj_eq(process, var_type_obj, type_int)) { */
  /*   int *i = varptr; */
  /*   new_variable_value = obj_new_int(*i); */
  /* } */
  /* else { */
  /*   eval_error = obj_new_string("Invalid variable type."); */
  /*   return nil; */
  /* } */

  /* Obj *ovar = primitive_to_obj(varptr, var_type_obj); */
  /* printf("ovar = %s\n", obj_to_string(ovar)->s); */

  Obj *variable_ptr = obj_new_ptr_to_global(varptr);
  obj_set_meta(variable_ptr, obj_new_keyword("type"), var_type_obj);

  char *lispified_name = lispify(name);
  //printf("Registering variable %s\n", lispified_name);
  global_env_extend(process, obj_new_symbol(lispified_name), variable_ptr);

  return variable_ptr;
}

// (register <dylib> <function-name> <arg-types> <return-type>)
Obj *p_register(Process *process, Obj **args, int arg_count) {
  if(arg_count != 4 || args[0]->tag != 'D' || args[1]->tag != 'S' || args[2]->tag != 'C') {
    printf("Args to register must be: (handle, function-name, argument-types, return-type)");
    printf("Arg count: %d\n", arg_count);
    printf("Args %c %c %c %c\n", args[0]->tag, args[1]->tag, args[2]->tag, args[3]->tag);
    return nil;
  }
  carp_library_t handle = args[0]->dylib;
  char *name = args[1]->s;

  VoidFn f = carp_find_symbol(handle, name);

  if(!f) {
    printf("Failed to load dynamic C function with name '%s' from %s\n", name, obj_to_string(process, args[0])->s);
    return nil;
  }

  return register_ffi_internal(process, name, f, args[2], args[3], false);
}

Obj *p_register_variable(Process *process, Obj **args, int arg_count) {
  if(arg_count != 3 || args[0]->tag != 'D' || args[1]->tag != 'S') {
    printf("Args to register-variable must be: (handle, variable-name, type)");
    printf("Arg count: %d\n", arg_count);
    printf("Args %c %c %c\n", args[0]->tag, args[1]->tag, args[2]->tag);
    return nil;
  }

  carp_library_t handle = args[0]->dylib;
  char *name = args[1]->s;

  void *variable = carp_find_symbol(handle, name);

  if(!variable) {
    printf("Failed to load dynamic C variable with name '%s' from %s\n", name, obj_to_string(process, args[0])->s);
    return nil;
  }

  return register_ffi_variable_internal(process, name, variable, args[2]);
}

Obj *p_register_builtin(Process *process, Obj **args, int arg_count) {
  if(arg_count != 3 || args[0]->tag != 'S' || args[1]->tag != 'C') {
    printf("Args to register-builtin must be: (function-name, argument-types, return-type)\n");
    printf("Arg count: %d\n", arg_count);
    printf("Args %c %c %c\n", args[0]->tag, args[1]->tag, args[2]->tag);
    return nil;
  }
  char *name = args[0]->s;
  VoidFn f = carp_find_symbol(NULL, name);

  if(!f) {
    printf("Failed to load dynamic C function with name '%s' from executable.\n", name);
    return nil;
  }

  return register_ffi_internal(process, name, f, args[1], args[2], true);
}

Obj *p_meta_set_BANG(Process *process, Obj **args, int arg_count) {
  if(arg_count != 3) {
    eval_error = obj_new_string("Invalid argument to meta-set!");
    return nil;
  }
  Obj *o = args[0];
  if(!o->meta) {
    o->meta = obj_new_environment(NULL);
  }
  env_assoc(process, o->meta, args[1], args[2]);
  return o;
}

Obj *p_meta_get(Process *process, Obj **args, int arg_count) {
  if(arg_count != 2) {
    eval_error = obj_new_string("Invalid argument to meta-get");
    return nil;
  }
  Obj *o = args[0];
  if(o->meta) {
    Obj *lookup = env_lookup(process, o->meta, args[1]);
    if(lookup) {
      return lookup;
    }
    else {
      return nil;
    }
  }
  else {
    return nil;
  }
}

Obj *p_meta_get_all(Process *process, Obj **args, int arg_count) {
  if(arg_count != 1) {
    eval_error = obj_new_string("Invalid argument to meta-get-all");
    return nil;
  }
  Obj *o = args[0];
  if(o->meta) {
    return o->meta;
  }
  else {
    return nil;
  }
}

Obj *p_array_to_list(Process *process, Obj **args, int arg_count) {
  Obj *a = args[0];
  assert_or_set_error_return_nil(a->tag == 'A', "array-to-list must take array as argument: ", args[0]);
  Obj *list = obj_new_cons(NULL, NULL);
  Obj *prev = list;
  for(int i = 0; i < a->count; i++) {
    Obj *new = obj_new_cons(NULL, NULL);
    prev->car = a->array[i];
    prev->cdr = new;
    prev = new;
  }
  return list;
}

/* Obj *p_array(Process *process, Obj** args, int arg_count) { */
/*   Obj *new_array = obj_new_array(arg_count); */
/*   for(int i = 0; i < arg_count; i++) { */
/*     new_array->array[i] = args[i]; */
/*   } */
/*   return new_array; */
/* } */

Obj *p_array_of_size(Process *process, Obj **args, int arg_count) {
  int array_count = args[0]->i;
  Obj *new_array = obj_new_array(array_count);
  for(int i = 0; i < array_count; i++) {
    new_array->array[i] = nil;
  }
  return new_array;
}

Obj *p_array_set_BANG(Process *process, Obj **args, int arg_count) {
  assert_or_set_error_return_nil(arg_count == 3, "array-set! must take 3 arguments: ", args[0]);
  Obj *a = args[0];
  assert_or_set_error_return_nil(a->tag == 'A', "array-set! must take an array as first arg: ", args[0]);
  Obj *i = args[1];
  assert_or_set_error_return_nil(i->tag == 'I', "array-set! must take an int as second arg: ", args[1]);
  Obj *o = args[2];
  a->array[i->i] = o;
  return nil;
}

Obj *p_array_set(Process *process, Obj **args, int arg_count) {
  assert_or_set_error_return_nil(arg_count == 3, "array-set must take 3 arguments: ", args[0]);
  Obj *a = args[0];
  assert_or_set_error_return_nil(a->tag == 'A', "array-set must take an array as first arg: ", args[0]);
  Obj *i = args[1];
  assert_or_set_error_return_nil(i->tag == 'I', "array-set must take an int as second arg: ", args[1]);
  Obj *o = args[2];
  Obj *new_array = obj_copy(process, a);
  new_array->array[i->i] = o;
  return new_array;
}

/* Obj *p_new(Process *process, Obj** args, int arg_count) { */
/*   // This is used for baking of struct constructors */
/*   eval_error = obj_new_string("The primop 'new' should never be called in dynamic code."); */
/*   return nil; */
/* } */

Obj *p_gc(Process *process, Obj **args, int arg_count) {
  gc(process);
  return nil;
}

Obj *p_delete(Process *process, Obj **args, int arg_count) {
  // no op?
  return nil;
}

Obj *p_stop(Process *process, Obj **args, int arg_count) {
  process->dead = true;
  return nil;
}

Obj *p_parallell(Process *process, Obj **args, int arg_count) {
  parallell = process_clone(process);
  process_eval(parallell, args[0]);
  return nil;
}

Obj *p_bytecode(Process *process, Obj **args, int arg_count) {
  assert_or_set_error_return_nil(arg_count == 1, "bytecode must take 1 arguments. ", nil);
  Obj *bytecode = form_to_bytecode(process, process->global_env, args[0], false);
  return bytecode;
}

Obj *p_bytecode_eval(Process *process, Obj **args, int arg_count) {
  assert_or_set_error_return_nil(arg_count == 1, "eval-bytecode must take 1 arguments. ", nil);
  Obj *bytecode = args[0];
  return bytecode_eval_bytecode_in_env(process, bytecode, process->global_env, NULL);
}

Obj *p_lookup_in_substs_fast(Process *process, Obj **args, int arg_count) {
  assert_or_set_error_return_nil(arg_count == 2, "lookup-in-substs-fast must take 2 arguments. ", nil);
  Obj *substs = args[0];
  Obj *b = args[1];

  if(substs->tag == 'K' && strcmp(substs->s, "fail")) {
    return substs;
  }
  else if(substs->tag == 'E') {
    if(b->tag == 'C') {
      Obj *list = obj_new_cons(NULL, NULL);
      shadow_stack_push(process, list);
      Obj *prev = list;
      int shadow_count = 0;
      Obj *p = b;
      while(p && p->car) {
        Obj *args[2] = {substs, p->car};
        prev->car = p_lookup_in_substs_fast(process, args, 2);
        Obj *new = obj_new_cons(NULL, NULL);
        shadow_stack_push(process, new);
        shadow_count++;
        prev->cdr = new;
        prev = new;
        p = p->cdr;
      }
      for(int i = 0; i < shadow_count; i++) {
        shadow_stack_pop(process);
      }
      shadow_stack_pop(process); // list
      return list;
    }
    else {
      Obj *result = NULL;
      shadow_stack_push(process, substs);
      shadow_stack_push(process, b);
      Obj *lookup = env_lookup(process, substs, b);
      if(lookup) {
        if(obj_eq(process, b, lookup)) {
          result = lookup;
        }
        else {
          if(lookup->tag == 'S') {
            Obj *args[2] = {substs, lookup};
            result = p_lookup_in_substs_fast(process, args, 2);
          }
          else {
            result = lookup;
          }
        }
      }
      else {
        result = b;
      }
      shadow_stack_pop(process);
      shadow_stack_pop(process);
      return result;
    }
  }
  else {
    set_error_return_nil("First arg to lookup-in-substs-fast must be a dictionary: ", substs);
  }
  assert(false);
}

void replace_from_right_in_list(Process *process, Obj *list, Obj *existing, Obj *new_value) {
  Obj *listp = list;
  while(listp && listp->car) {
    Obj *value = listp->car;

    if(value->tag == 'C') {
      replace_from_right_in_list(process, value, existing, new_value);
    }
    else if(obj_eq(process, value, existing)) {
      listp->car = new_value;
    }
    else {
      // do nothing
    }

    listp = listp->cdr;
  }
}

Obj *p_replace_subst_from_right_fast(Process *process, Obj **args, int arg_count) {
  assert_or_set_error_return_nil(arg_count == 3, "replace-substs-from-right-fast must take 3 arguments. ", nil);
  assert_or_set_error_return_nil(args[0]->tag == 'E', "First argument to lookup-in-substs-fast must be dictionary. ", args[0]);

  Obj *mut_substs = obj_copy(process, args[0]); // COPY!
  Obj *existing = args[1];
  Obj *new_value = args[2];

  shadow_stack_push(process, mut_substs);

  Obj *bindings = mut_substs->bindings;
  Obj *p = bindings;
  while(p && p->car) {
    Obj *pair = p->car;
    Obj *value = pair->cdr;

    if(value->tag == 'C') {
      replace_from_right_in_list(process, value, existing, new_value);
    }
    else if(obj_eq(process, value, existing)) {
      pair->cdr = new_value;
    }
    else {
      // do nothing
    }

    p = p->cdr;
  }

  shadow_stack_pop(process); // mut_substs

  return mut_substs;
}

Obj *p_types_exactly_eq(Process *process, Obj **args, int arg_count) {
  assert_or_set_error_return_nil(arg_count == 2, "types-exactly-eq? must take 2 arguments. ", nil);
  Obj *a = args[0];
  Obj *b = args[1];
  if(a->tag == 'C' && b->tag == 'C') {
    Obj *p = a;
    Obj *p2 = b;
    while(p && p->car) {
      if(!p2 || !p2->car) {
        return lisp_false;
      }
      Obj *inner_args[2] = {p->car, p2->car};
      Obj *result = p_types_exactly_eq(process, inner_args, 2);
      if(result == lisp_false) {
        return lisp_false;
      }
      p = p->cdr;
      p2 = p2->cdr;
    }
    return lisp_true;
  }
  else if(a->tag == 'K' && strcmp(a->s, "any") == 0) {
    return lisp_true;
  }
  else if(b->tag == 'K' && strcmp(b->s, "any") == 0) {
    return lisp_true;
  }
  else {
    return obj_eq(process, a, b) ? lisp_true : lisp_false;
  }
}

Obj *p_extend_substitutions_fast(Process *process, Obj **args, int arg_count) {
  assert_or_set_error_return_nil(arg_count == 3, "extend-substitutions-fast must take 3 arguments. ", nil);

  Obj *substs = args[0];
  Obj *lhs = args[1];
  Obj *value = args[2];

  if(substs->tag == 'K' && strcmp(substs->s, "fail")) {
    printf("FAIL\n");
    return substs;
  }
  else {
    Obj *result = NULL;
    //printf("substs: %s\n", obj_to_string(process, substs)->s);
    Obj *lookup_args[2] = {substs, value};
    Obj *lookup = p_lookup_in_substs_fast(process, lookup_args, 2);

    shadow_stack_push(process, lookup);
    shadow_stack_push(process, lhs);
    shadow_stack_push(process, value);

    /* printf("Will extend %s with %s, lookup: %s, substs:\n%s\n\n", */
    /*        obj_to_string(process, lhs)->s, */
    /*        obj_to_string(process, value)->s, */
    /*        obj_to_string(process, lookup)->s, */
    /*        obj_to_string(process, substs)->s); */

    if(lhs->tag == 'S') {
      //printf("lhs is a typevar\n");
      Obj *existing = env_lookup(process, substs, lhs);
      if(existing == NULL) {
        //printf("not existing\n");
        Obj *new_substs = env_assoc(process, substs, lhs, lookup);
        Obj *replace_args[3] = {new_substs, existing, lookup};
        result = p_replace_subst_from_right_fast(process, replace_args, 3);
      }
      else {
        //printf("lhs exists\n");

        if(existing->tag == 'C') {
          //printf("existing is a list\n");

          if(lookup->tag == 'C') {
            //printf("lookup is a list\n");

            Obj *final_substs = substs;
            Obj *p1 = existing;
            Obj *p2 = lookup;
            while(p1 && p1->car) {
              assert(p2);
              assert(p2->car);

              if(final_substs->tag == 'K' && strcmp(final_substs->s, "fail") == 0) {
                result = final_substs;
                break;
              }

              Obj *extend_args[3] = {final_substs, p1->car, p2->car};
              final_substs = p_extend_substitutions_fast(process, extend_args, 3);

              p1 = p1->cdr;
              p2 = p2->cdr;
            }
            result = final_substs;
          }
          else {
            //printf("lookup is not a list\n");
            result = substs;
          }
        }
        else if(existing->tag == 'S') {
          //printf("existing is a typevar\n");
          Obj *replace_args[3] = {substs, existing, lookup};
          result = p_replace_subst_from_right_fast(process, replace_args, 3);
        }
        else if(lookup->s) {
          result = substs;
        }
        else if(lhs->tag == 'K' && strcmp(lhs->s, "any") == 0) {
          result = substs;
        }
        else {
          //printf("existing is a not a typevar or list\n");
          // The existing binding is not a typevar, must match exactly or the unification will fail
          Obj *exactly_eq_args[2] = {existing, lookup};
          Obj *are_eq = p_types_exactly_eq(process, exactly_eq_args, 2);
          if(are_eq == lisp_true) {
            result = substs;
          }
          else {
            //printf("FAIL in typevar, else\n");
            result = obj_new_keyword("fail");
          }
        }
      }
    }
    else {
      // lhs is not a typevar
      //printf("lhs is not a typevar: %s\n", obj_to_string(process, lhs)->s);

      if(lhs->tag == 'C') {
        //printf("lhs is a list\n");

        if(lookup->tag == 'C') {
          //printf("lookup is a list\n");

          Obj *final_substs = substs;
          Obj *p1 = lhs;
          Obj *p2 = lookup;

          /* printf("substs: %s\n", obj_to_string(process, final_substs)->s); */
          /* printf("p1: %s\n", obj_to_string(process, p1)->s); */
          /* printf("p2: %s\n", obj_to_string(process, p2)->s); */

          while(p1 && p1->car) {
            assert(p2);
            assert(p2->car);

            if(final_substs->tag == 'K' && strcmp(final_substs->s, "fail") == 0) {
              result = final_substs;
              break;
            }

            // (extend-substitutions-fast {"b" (list "t0" "t1")} (list :int "x") "b")

            Obj *extend_args[3] = {final_substs, p1->car, p2->car};
            final_substs = p_extend_substitutions_fast(process, extend_args, 3);

            //shadow_stack_push(process, final_substs);
            //printf("new final substs: %s %c\n", obj_to_string(process, final_substs)->s, final_substs->tag);

            p1 = p1->cdr;
            p2 = p2->cdr;
          }
          result = final_substs;
        }
        else {
          //printf("lookup is NOT a list\n");
          result = substs;
        }
      }
      else {
        //printf("lhs is NOT a list\n");

        if(obj_eq(process, lhs, lookup)) {
          result = substs;
        }
        else if(lookup->tag == 'S') {
          result = substs; // WHY IS THIS CASE NECESSARY?! make it not so...
        }
        else if(lookup->tag == 'K' && strcmp(lookup->s, "any") == 0) {
          result = substs;
        }
        else if(lhs->tag == 'K' && strcmp(lhs->s, "any") == 0) {
          result = substs;
        }
        else {
          //printf("FAIL in not typevar, not list\n");
          result = obj_new_keyword("fail");
        }
      }
    }

    shadow_stack_pop(process); // value
    shadow_stack_pop(process); // lhs
    shadow_stack_pop(process); // lookup

    //printf("result = %s\n", obj_to_string(process, result)->s);
    return result;
  }
}

Obj *sort_internal(Process *process, Obj *f, Obj *xs) {
  Obj *left = obj_new_cons(NULL, NULL);
  Obj *right = obj_new_cons(NULL, NULL);
  Obj *left_prev = left;
  Obj *right_prev = right;
  Obj *sorted = NULL;

  if(!xs->car) {
    printf("nil\n");
    return nil;
  }
  else if(xs->cdr == NULL) {
    //printf("will print\n");
    printf("single value: %s\n", obj_to_string(process, xs->car)->s);
    //printf("did print\n");
    return xs->car;
  }

  shadow_stack_push(process, left);
  shadow_stack_push(process, right);

  Obj *mid = xs->car;
  //printf("mid: %s\n", obj_to_string(process, xs->car)->s);

  Obj *p = xs->cdr;
  while(p && p->car) {

    Obj *arg[2] = {p->car, mid};
    apply(process, f, arg, 2);
    Obj *result = stack_pop(process);
    Obj *new = obj_new_cons(NULL, NULL);

    printf("p->car: %s\n", obj_to_string(process, p->car)->s);

    if(is_true(result)) {
      left_prev->car = p->car;
      left_prev->cdr = new;
      left_prev = new;
    }
    else {
      right_prev->car = p->car;
      right_prev->cdr = new;
      right_prev = new;
    }

    p = p->cdr;
  }

  shadow_stack_push(process, left);
  shadow_stack_push(process, right);

  /* printf("left: %s\n", obj_to_string(process, left)->s); */
  /* printf("right: %s\n", obj_to_string(process, right)->s); */

  Obj *sorted_left = sort_internal(process, f, left);
  Obj *sorted_right = sort_internal(process, f, right);

  /* printf("sorted_left: %s\n", obj_to_string(process, sorted_left)->s); */
  /* printf("sorted_right: %s\n", obj_to_string(process, sorted_right)->s); */

  //printf("%c %c\n", sorted_left->tag, sorted_right->tag);

  shadow_stack_push(process, sorted_left);
  shadow_stack_push(process, sorted_right);

  Obj *args[3] = {sorted_left, obj_new_cons(mid, nil), sorted_right};

  assert_or_set_error_return_nil(sorted_left->tag == 'C', "sorted left is not list: ", sorted_left);
  assert_or_set_error_return_nil(sorted_right->tag == 'C', "sorted right is not list: ", sorted_right);

  sorted = p_concat(process, args, 3);

  shadow_stack_pop(process);
  shadow_stack_pop(process);
  shadow_stack_pop(process);
  shadow_stack_pop(process);
  shadow_stack_pop(process);
  shadow_stack_pop(process);

  assert(sorted);
  return sorted;
}

Obj *p_sort_by(Process *process, Obj **args, int arg_count) {
  if(arg_count != 2) {
    eval_error = obj_new_string("Wrong argument count to 'sort-by'.");
    return nil;
  }
  if(!is_callable(args[0])) {
    set_error_return_nil("'sort-by' requires arg 0 to be a function or lambda: \n", args[0]);
  }
  Obj *f = args[0];
  Obj *xs = args[1];
  Obj *result = sort_internal(process, f, xs);
  return result;
}

Obj *p_hash(Process *process, Obj **args, int arg_count) {
  if(arg_count != 1) {
    eval_error = obj_new_string("Wrong argument count to 'hash'.");
    return nil;
  }
  return obj_new_int(obj_hash(process, args[0]));
}

/* shadow_stack_push(process, list); */

/* int shadow_count = 0; */
/* Obj *p = b; */
/* while(p && p->car) { */
/*   Obj *args[2] = { substs, p->car }; */
/*   prev->car = p_lookup_in_substs_fast(process, args, 2); */
/*   Obj *new = obj_new_cons(NULL, NULL); */
/*   shadow_stack_push(process, new); */
/*   shadow_count++; */
/*   prev->cdr = new; */
/*   prev = new; */
/*   p = p->cdr; */
/* } */

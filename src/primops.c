#include "primops.h"

#include <sys/time.h>
#include <dlfcn.h>

#include "assertions.h"
#include "obj_string.h"
#include "env.h"
#include "eval.h"
#include "reader.h"

Obj *open_file(const char *filename) {
  assert(filename);
  
  char * buffer = 0;
  long length;
  FILE * f = fopen (filename, "rb");

  if(f) {
    fseek (f, 0, SEEK_END);
    length = ftell (f);
    fseek (f, 0, SEEK_SET);
    buffer = malloc (length + 1);
    if (buffer)	{
      fread (buffer, 1, length, f);
      buffer[length] = '\0';
    }
    fclose (f);
  } else {
    set_error_and_return("Failed to open file: ", obj_new_string((char*)filename));
  }

  if (buffer) {
    return obj_new_string(buffer);
  } else {
    set_error_and_return("Failed to open buffer from file: ", obj_new_string((char*)filename));
  }
}

Obj *save_file(const char *filename, const char *contents) {
  FILE * f = fopen (filename, "w");
  if(f) {
    fprintf(f, "%s", contents);
    fclose(f);
    return obj_new_symbol("success");
  } else {
    set_error_and_return("Failed to save file: ", obj_new_string((char*)filename));
  }
}

Obj *p_open_file(Obj** args, int arg_count) {
  if(arg_count != 1) { return nil; }
  if(args[0]->tag != 'S') { return nil; }
  return open_file(args[0]->s);
}

Obj *p_save_file(Obj** args, int arg_count) {
  if(arg_count != 2) { return nil; }
  if(args[0]->tag != 'S') { return nil; }
  if(args[1]->tag != 'S') { return nil; }
  return save_file(args[0]->s, args[1]->s);
}

Obj *p_add(Obj** args, int arg_count) {
  if(arg_count == 0 || args[0]->tag == 'I') {
    int sum = 0;
    for(int i = 0; i < arg_count; i++) {
      if(args[i]->tag != 'I') {
	printf("Args to add must be integers.\n");
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
	printf("Args to add must be floats.\n");
	return nil;
      }
      sum += args[i]->f32;
    }
    return obj_new_float(sum);
  }
  else {
    error = obj_new_string("Can't add non-numbers together.");
    return nil;
  }
}

Obj *p_sub(Obj** args, int arg_count) {
  if(arg_count == 0 || args[0]->tag == 'I') {
    if(arg_count == 1) { return obj_new_int(-args[0]->i); }
    int sum = args[0]->i;
    for(int i = 1; i < arg_count; i++) {
      sum -= args[i]->i;
    }
    return obj_new_int(sum);
  }
  else if(args[0]->tag == 'V') {
    if(arg_count == 1) {
      return obj_new_int(-args[0]->f32);
    }
    float sum = args[0]->f32;
    for(int i = 1; i < arg_count; i++) {
      sum -= args[i]->f32;
    }
    return obj_new_float(sum);
  }
  else {
    error = obj_new_string("Can't subtract non-numbers.");
    return nil;
  }
}

Obj *p_mul(Obj** args, int arg_count) {
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
  else {
    error = obj_new_string("Can't multiply non-numbers.");
    return nil;
  }
}

Obj *p_div(Obj** args, int arg_count) {
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
  else {
    error = obj_new_string("Can't divide non-numbers.");
    return nil;
  }
}

Obj *p_mod(Obj** args, int arg_count) {
  if(arg_count == 0) {
    return obj_new_int(1);
  }
  int prod = args[0]->i;
  for(int i = 1; i < arg_count; i++) {
    prod %= args[i]->i;
  }
  return obj_new_int(prod);
}

Obj *p_eq(Obj** args, int arg_count) {
  if(arg_count < 2) {
    printf("The function '=' requires at least 2 arguments.\n");
    return nil;
  }
  for(int i = 0; i < arg_count - 1; i++) {
    if(!obj_eq(args[i], args[i + 1])) {
      return lisp_false;
    }
  }
  return lisp_true;
}

Obj *p_list(Obj** args, int arg_count) {
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

Obj *p_str(Obj** args, int arg_count) {
  Obj *s = obj_new_string("");
  for(int i = 0; i < arg_count; i++) {
    obj_string_mut_append(s, obj_to_string_not_prn(args[i])->s);
  }
  return s;
}

Obj *p_str_append_bang(Obj** args, int arg_count) {
  if(arg_count != 2) {
    error = obj_new_string("'str-append!' takes exactly two arguments");
    return nil;
  }
  if(args[0]->tag != 'S') {
    error = obj_new_string("'str-append!' arg0 invalid");
    return nil;
  }
  if(args[1]->tag != 'S') {
    error = obj_new_string("'str-append!' arg1 invalid");
    return nil;
  }
  Obj *s = args[0];
  obj_string_mut_append(s, args[1]->s);
  return s;
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
	while ((pstr2 = strstr(pstr, old)) != NULL) {
		count++;

		/* Increase the cache size when necessary. */
		if (cache_sz < count) {
			cache_sz += cache_sz_inc;
			pos_cache = realloc(pos_cache, sizeof(*pos_cache) * cache_sz);
			if (pos_cache == NULL) {
				goto end_repl_str;
			}
			cache_sz_inc *= cache_sz_inc_factor;
			if (cache_sz_inc > cache_sz_inc_max) {
				cache_sz_inc = cache_sz_inc_max;
			}
		}

		pos_cache[count-1] = pstr2 - str;
		pstr = pstr2 + oldlen;
	}

	orglen = pstr - str + strlen(pstr);

	/* Allocate memory for the post-replacement string. */
	if (count > 0) {
		newlen = strlen(new);
		retlen = orglen + (newlen - oldlen) * count;
	} else	retlen = orglen;
	ret = malloc(retlen + 1);
	if (ret == NULL) {
		goto end_repl_str;
	}

	if (count == 0) {
		/* If no matches, then just duplicate the string. */
		strcpy(ret, str);
	} else {
		/* Otherwise, duplicate the string whilst performing
		 * the replacements using the position cache. */
		pret = ret;
		memcpy(pret, str, pos_cache[0]);
		pret += pos_cache[0];
		for (i = 0; i < count; i++) {
			memcpy(pret, new, newlen);
			pret += newlen;
			pstr = str + pos_cache[i] + oldlen;
			cpylen = (i == count-1 ? orglen : pos_cache[i+1]) - pos_cache[i] - oldlen;
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

Obj *p_str_replace(Obj** args, int arg_count) {
  if(arg_count != 3) {
    error = obj_new_string("'str-replace' takes exactly three arguments");
    return nil;
  }
  if(args[0]->tag != 'S') {
    error = obj_new_string("'str-replace' arg0 invalid: ");
    obj_string_mut_append(error, obj_to_string(args[0])->s);
    return nil;
  }
  if(args[1]->tag != 'S') {
    error = obj_new_string("'str-replace' arg1 invalid");
    return nil;
  }
  if(args[2]->tag != 'S') {
    error = obj_new_string("'str-replace' arg2 invalid");
    return nil;
  }

  char *s = args[0]->s;
  char *lookup = args[1]->s;
  char *replacement = args[2]->s;
  char *replaced = str_replace(s, lookup, replacement);
  return obj_new_string(replaced);
}

Obj *p_copy(Obj** args, int arg_count) {
  if(arg_count != 1) {
    error = obj_new_string("'copy' takes exactly one argument");
    return nil;
  }
  Obj *a = args[0];
  //printf("Will make a copy of: %s\n", obj_to_string(a)->s);
  Obj *b = obj_copy(a);
  return b;
}

Obj *p_print(Obj** args, int arg_count) {
  for(int i = 0; i < arg_count; i++) {
    obj_print_not_prn(args[i]);
  }
  return nil;
}

Obj *p_prn(Obj** args, int arg_count) {
  Obj *s = obj_new_string("");
  for(int i = 0; i < arg_count; i++) {
    Obj *s2 = obj_to_string(args[i]);
    obj_string_mut_append(s, s2->s);
  }
  return s;
}

Obj *p_println(Obj** args, int arg_count) {
  for(int i = 0; i < arg_count; i++) {
    obj_print_not_prn(args[i]);
    /* if(i < arg_count - 1) { */
    /*   printf(" "); */
    /* } */
  }
  printf("\n");
  return nil;
}

Obj *p_system(Obj** args, int arg_count) {
  if(arg_count != 1) { printf("Wrong argument count to 'system'\n"); return nil; }
  if(args[0]->tag != 'S') { printf("'system' takes a string as its argument\n"); return nil; }
  system(args[0]->s);
  return obj_new_symbol("success");
}

Obj *p_get(Obj** args, int arg_count) {
  if(arg_count != 2) { printf("Wrong argument count to 'get'\n"); return nil; }
  if(args[0]->tag == 'E') {
    Obj *o = env_lookup(args[0], args[1]);
    if(o) {
      return o;
    } else {
      Obj *s = obj_new_string("Can't get key '");
      obj_string_mut_append(s, obj_to_string(args[1])->s);
      obj_string_mut_append(s, "' in dict '");
      obj_string_mut_append(s, obj_to_string(args[0])->s);
      obj_string_mut_append(s, "'");
      error = s;
      return nil;
    }
  }
  else if(args[0]->tag == 'C') {
    if(args[1]->tag != 'I') {
      error = obj_new_string("get requires arg 1 to be an integer\n");
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
    error = obj_new_string("Index ");
    obj_string_mut_append(error, obj_to_string(obj_new_int(i))->s);
    obj_string_mut_append(error, " out of bounds in");
    obj_string_mut_append(error, obj_to_string(args[0])->s);
    return nil;
  }
  else {
    error = obj_new_string("'get' requires arg 0 to be a dictionary or list: ");
    obj_string_mut_append(error, obj_to_string(args[0])->s);
    return nil;
  }
}

Obj *p_get_maybe(Obj** args, int arg_count) {
  if(arg_count != 2) { printf("Wrong argument count to 'get-maybe'\n"); return nil; }
  if(args[0]->tag == 'E') {
    Obj *o = env_lookup(args[0], args[1]);
    if(o) {
      return o;
    } else {
      return nil;
    }
  }
  else if(args[0]->tag == 'C') {
    if(args[1]->tag != 'I') { printf("get-maybe requires arg 1 to be an integer\n"); return nil; }
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
    printf("'get-maybe' requires arg 0 to be a dictionary or list: %s\n", obj_to_string(args[0])->s);
    return nil;
  }
}

Obj *p_dict_set_bang(Obj** args, int arg_count) {
  if(arg_count != 3) { printf("Wrong argument count to 'dict-set!'\n"); return nil; }
  if(args[0]->tag != 'E') {
    printf("'dict-set!' requires arg 0 to be a dictionary: %s\n", obj_to_string(args[0])->s);
    return nil;
  }
  Obj *pair = env_lookup_binding(args[0], args[1]);
  if(pair && pair->car && pair->cdr) {
    pair->cdr = args[2];
  }
  else {
    //printf("Pair not found, will add new key.\n");
    Obj *new_pair = obj_new_cons(args[1], args[2]);
    Obj *new_cons = obj_new_cons(new_pair, args[0]->bindings);
    args[0]->bindings = new_cons;
  }
  return args[0];
}

Obj *p_dict_remove_bang(Obj** args, int arg_count) {
  if(arg_count != 2) { printf("Wrong argument count to 'dict-remove!'\n"); return nil; }
  if(args[0]->tag != 'E') {
    printf("'dict-remove!' requires arg 0 to be a dictionary: %s\n", obj_to_string(args[0])->s);
    return nil;
  }

  Obj *prev = NULL;
  Obj *p = args[0]->bindings;
  while(p && p->car) {
    Obj *pair = p->car;
    if(obj_eq(pair->car, args[1])) {
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

Obj *p_first(Obj** args, int arg_count) {
  if(arg_count != 1) { printf("Wrong argument count to 'first'\n"); return nil; }
  if(args[0]->tag != 'C') { printf("'first' requires arg 0 to be a list: %s\n", obj_to_string(args[0])->s); return nil; }
  if(args[0]->car == NULL) {
    printf("Can't take first element of empty list.\n");
    return nil;
  }
  return args[0]->car;
}

Obj *p_rest(Obj** args, int arg_count) {
  if(arg_count != 1) { printf("Wrong argument count to 'rest'\n"); return nil; }
  if(args[0]->tag != 'C') {
    char buffer[512];
    snprintf(buffer, 512, "'rest' requires arg 0 to be a list: %s\n", obj_to_string(args[0])->s);
    error = obj_new_string(strdup(buffer));
    return nil;
  }
  if(args[0]->cdr == NULL) {
    printf("Can't take rest of empty list.\n");
    return nil;
  }
  return args[0]->cdr;
}

Obj *p_cons(Obj** args, int arg_count) {
  if(arg_count != 2) { printf("Wrong argument count to 'cons'\n"); return nil; }
  if(args[1]->tag != 'C') {
    char buffer[512];
    snprintf(buffer, 512, "'cons' requires arg 1 to be a list: %s\n", obj_to_string(args[0])->s);
    error = obj_new_string(strdup(buffer));
    return nil;
  }
  Obj *new_cons = obj_new_cons(args[0], args[1]);
  return new_cons;
}

Obj *p_cons_last(Obj** args, int arg_count) {
  if(arg_count != 2) { printf("Wrong argument count to 'cons'\n"); return nil; }
  if(args[0]->tag != 'C') { printf("'rest' requires arg 0 to be a list: %s\n", obj_to_string(args[1])->s); return nil; }
  Obj *new_list = obj_copy(args[0]);
  Obj *p = new_list;
  while(p && p->cdr) { p = p->cdr; }
  Obj *last = p;
  Obj *new_nil = obj_new_cons(NULL, NULL);
  last->car = args[1];
  last->cdr = new_nil;
  return new_list;
}

Obj *p_concat(Obj** args, int arg_count) {
  if(arg_count == 0) {
    return nil;
  }
  
  for(int i = 0; i < arg_count; i++) {
    if(args[0]->tag != 'C') { error = obj_new_string("'concat' requires all args to be lists\n"); return nil; }
  }

  int i = 0;
  Obj *new = obj_copy(args[i]);

  while(!new->car) {
    new = args[++i];
    if(i >= arg_count) {
      return nil;
    }
  }
  
  Obj *last = new;

  for(i++; i < arg_count; i++) {
    //printf("Will concat %s\n", obj_to_string(args[i])->s);
    if(!last->cdr) {
      // continue
    } else {
      while(last->cdr->cdr) {
	last = last->cdr;
      }
      Obj *o = args[i];
      if(o->car) {
	last->cdr = obj_copy(o);
      }
    }
  }
  return new;
}

Obj *p_nth(Obj** args, int arg_count) {
  if(arg_count != 2) { printf("Wrong argument count to 'nth'\n"); return nil; }
  if(args[0]->tag != 'C') { printf("'nth' requires arg 0 to be a list\n"); return nil; }
  if(args[1]->tag != 'I') { printf("'nth' requires arg 1 to be an integer\n"); return nil; }
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
  printf("Index %d out of bounds in %s\n", n, obj_to_string(args[0])->s);
  return nil;
}

Obj *p_count(Obj** args, int arg_count) {
  if(arg_count != 1) { printf("Wrong argument count to 'count'\n"); return nil; }
  if(args[0]->tag != 'C') { printf("'count' requires arg 0 to be a list: %s\n", obj_to_string(args[0])->s); return nil; }
  int i = 0;
  Obj *p = args[0];
  while(p && p->car) {
    p = p->cdr;
    i++;
  }
  return obj_new_int(i);
}

bool is_callable(Obj *obj) {
  return obj->tag == 'P' || obj->tag != 'L' || obj->tag != 'F';
}

Obj *p_map(Obj** args, int arg_count) {
  //printf("map start\n");
  if(arg_count != 2) { printf("Wrong argument count to 'map'\n"); return nil; }
  if(!is_callable(args[0])) { printf("'map' requires arg 0 to be a function or lambda: %s\n", obj_to_string(args[0])->s); return nil; }
  if(args[1]->tag != 'C') { printf("'map' requires arg 1 to be a list\n"); return nil; }
  Obj *f = args[0];
  Obj *p = args[1];
  Obj *list = obj_new_cons(NULL, NULL);
  shadow_stack_push(list);
  Obj *prev = list;
  int shadow_count = 0;
  while(p && p->car) {
    Obj *arg[1] = { p->car };
    apply(f, arg, 1);
    prev->car = stack_pop();
    Obj *new = obj_new_cons(NULL, NULL);
    shadow_stack_push(new);
    shadow_count++;
    prev->cdr = new;
    prev = new;
    p = p->cdr;
  }
  for(int i = 0; i < shadow_count; i++) {
    shadow_stack_pop();
  }
  shadow_stack_pop(); // list
  //printf("map end\n");
  return list;
}

Obj *p_map2(Obj** args, int arg_count) {
  if(arg_count != 3) { printf("Wrong argument count to 'map2'\n"); return nil; }
  if(!is_callable(args[0])) { printf("'map2' requires arg 0 to be a function or lambda: %s\n", obj_to_string(args[0])->s); return nil; }
  if(args[1]->tag != 'C') { printf("'map2' requires arg 1 to be a list\n"); return nil; }
  if(args[2]->tag != 'C') { printf("'map2' requires arg 2 to be a list\n"); return nil; }
  Obj *f = args[0];
  Obj *p = args[1];
  Obj *p2 = args[2];
  Obj *list = obj_new_cons(NULL, NULL);
  shadow_stack_push(list);
  Obj *prev = list;
  int shadow_count = 0;
  while(p && p->car && p2 && p2->car) {
    Obj *argz[2] = { p->car, p2->car };
    apply(f, argz, 2);
    prev->car = stack_pop();
    Obj *new = obj_new_cons(NULL, NULL);
    shadow_stack_push(new);
    shadow_count++;
    prev->cdr = new;
    prev = new;
    p = p->cdr;
    p2 = p2->cdr;
  }
  for(int i = 0; i < shadow_count; i++) {
    shadow_stack_pop();
  }
  shadow_stack_pop(); // list
  return list;
}

Obj *p_keys(Obj** args, int arg_count) {
  if(arg_count != 1) { printf("Wrong argument count to 'keys'\n"); return nil; }
  if(args[0]->tag != 'E') { printf("'keys' requires arg 0 to be a dictionary.\n"); return nil; }
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

Obj *p_values(Obj** args, int arg_count) {
  if(arg_count != 1) { printf("Wrong argument count to 'values'\n"); return nil; }
  if(args[0]->tag != 'E') { printf("'values' requires arg 0 to be a dictionary.\n"); return nil; }
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

Obj *p_signature(Obj** args, int arg_count) {
  if(arg_count != 1) { error = obj_new_string("Wrong argument count to 'signature'"); return nil; }
  if(args[0]->tag != 'F') { error = obj_new_string("'signature' requires arg 0 to be a foreign function."); return nil; }
  Obj *a = obj_copy(args[0]->arg_types);
  Obj *b = args[0]->return_type;
  Obj *sig = obj_list(obj_new_keyword("arrow"), a, b);
  return sig;
}

Obj *p_null_predicate(Obj** args, int arg_count) {
  if(arg_count != 1) { error = obj_new_string("Wrong argument count to 'null?'"); return nil; }
  if(args[0]->tag != 'Q') { error = obj_new_string("Argument to 'null?' must be void pointer."); return nil; }
  if(args[0]->void_ptr == NULL) {
    return lisp_true;
  } else {
    return lisp_false;
  }
}

Obj *p_and(Obj** args, int arg_count) {
  for(int i = 0; i < arg_count; i++) {
    if(!is_true(args[i])) {
      return lisp_false;
    }
  }
  return lisp_true;
}

Obj *p_filter(Obj** args, int arg_count) {
  if(arg_count != 2) { printf("Wrong argument count to 'filter'\n"); return nil; }
  if(!is_callable(args[0])) { printf("'filter' requires arg 0 to be a function or lambda: %s\n", obj_to_string(args[0])->s); return nil; }
  if(args[1]->tag != 'C') { printf("'filter' requires arg 1 to be a list\n"); return nil; }
  Obj *f = args[0];
  Obj *p = args[1];
  Obj *list = obj_new_cons(NULL, NULL);
  shadow_stack_push(list);
  Obj *prev = list;
  int shadow_count = 0;
  while(p && p->car) {
    Obj *arg[1] = { p->car };
    apply(f, arg, 1);
    Obj *result = stack_pop();
    if(is_true(result)) {
      Obj *new = obj_new_cons(NULL, NULL);
      shadow_stack_push(new);
      shadow_count++;
      prev->car = p->car;
      prev->cdr = new;
      prev = new;
    }
    p = p->cdr;
  }
  for(int i = 0; i < shadow_count; i++) {
    shadow_stack_pop();
  }
  shadow_stack_pop(); // list
  return list;
}

Obj *p_reduce(Obj** args, int arg_count) {
  if(arg_count != 3) { printf("Wrong argument count to 'reduce'\n"); return nil; }
  if(!is_callable(args[0])) { printf("'reduce' requires arg 0 to be a function or lambda: %s (%c)\n", obj_to_string(args[0])->s, args[0]->tag); return nil; }
  if(args[2]->tag != 'C') { printf("'reduce' requires arg 2 to be a list\n"); return nil; }
  Obj *f = args[0];
  Obj *total = args[1];
  Obj *p = args[2]; 
  while(p && p->car) {
    Obj *args[2] = { total, p->car };
    apply(f, args, 2);
    total = stack_pop();
    p = p->cdr;
  }
  return total;
}

Obj *p_apply(Obj** args, int arg_count) {
  if(arg_count != 2) { printf("'apply' takes two arguments.\n"); return nil; }
  if(args[0]->tag != 'P' && args[0]->tag != 'L') {
    printf("'apply' requires arg 0 to be a function or lambda: %s (%c)\n", obj_to_string(args[0])->s, args[0]->tag);
    return nil;
  }
  if(args[1]->tag != 'C') {
    printf("'apply' requires arg 1 to be a list: %s (%c)\n", obj_to_string(args[0])->s, args[0]->tag);
    return nil;
  }
  Obj *p = args[1];
  int apply_arg_count = 0;
  while(p && p->car) {
    apply_arg_count++;
    p = p->cdr;
  }
  Obj *apply_args[apply_arg_count];
  Obj *q = args[1];
  for(int i = 0; i < apply_arg_count; i++) {
    apply_args[i] = q->car;
    q = q->cdr;
  }
  apply(args[0], apply_args, apply_arg_count);
  return stack_pop();
}

Obj *p_type(Obj** args, int arg_count) {
  if(arg_count != 1) { printf("'type' takes one argument.\n"); return nil; }
  if(args[0]->tag == 'S') {
    return type_string;
  }
  else if(args[0]->tag == 'I') {
    return type_int;
  }
  else if(args[0]->tag == 'V') {
    return type_float;
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
  else {
    printf("Unknown type tag: %c\n", args[0]->tag);
    //error = obj_new_string("Unknown type.");
    return nil;
  }
}

Obj *p_lt(Obj** args, int arg_count) {
  if(arg_count == 0) { return lisp_true; }
  if(args[0]->tag == 'I') {
    int smallest = args[0]->i;
    for(int i = 1; i < arg_count; i++) {
      if(smallest >= args[i]->i) { return lisp_false; }
      smallest = args[i]->i;
    }
    return lisp_true;
  }
  else if(args[0]->tag == 'V') {
    float smallest = args[0]->f32;
    for(int i = 1; i < arg_count; i++) {
      if(smallest >= args[i]->f32) { return lisp_false; }
      smallest = args[i]->f32;
    }
    return lisp_true;
  }
  else {
    error = obj_new_string("Can't call < on non-numbers.");
    return lisp_false;
  }
}

int current_timestamp() {
    struct timeval te;
    gettimeofday(&te, NULL); // get current time
    long long milliseconds = te.tv_sec * 1000LL + te.tv_usec / 1000; // calculate milliseconds
    return milliseconds;
}

Obj *p_now(Obj** args, int arg_count) {
  if(arg_count != 0) { printf("Wrong argument count to 'now'\n"); return nil; }
  return obj_new_int(current_timestamp());
}

Obj *p_name(Obj** args, int arg_count) {
  if(arg_count != 1) {
    error = obj_new_string("Wrong arg count to 'name'.");
    return nil;
  }
  if(args[0]->tag != 'S' && args[0]->tag != 'Y' && args[0]->tag != 'K') {
    Obj *s = obj_new_string("Argument to 'name' must be string, keyword or symbol: ");
    obj_string_mut_append(s, obj_to_string(args[0])->s);
    error = s;
    return nil;
  }
  return obj_new_string(args[0]->s);
}

Obj *p_symbol(Obj** args, int arg_count) {
  if(arg_count != 1) {
    error = obj_new_string("Wrong arg count to 'symbol'.");
    return nil;
  }
  if(args[0]->tag != 'S') {
    Obj *s = obj_new_string("Argument to 'symbol' must be string: ");
    obj_string_mut_append(s, obj_to_string(args[0])->s);
    error = s;
    return nil;
  }
  return obj_new_symbol(args[0]->s);
}

Obj *p_error(Obj** args, int arg_count) {
  if(arg_count != 1) { error = obj_new_string("Wrong argument count to 'error'\n"); return nil; }
  error = args[0];
  return nil;
}

Obj *p_env(Obj** args, int arg_count) {
  return global_env;
}

Obj *p_load_lisp(Obj** args, int arg_count) {
  Obj *file_string = open_file(args[0]->s);
  shadow_stack_push(file_string);
  if(file_string->tag == 'S') {
    Obj *forms = read_string(global_env, file_string->s);
    shadow_stack_push(forms);
    Obj *form = forms;
    while(form && form->car) {
      eval_internal(global_env, form->car);
      if(error) { return nil; }
      Obj *result = stack_pop();
      form = form->cdr;
    }
    shadow_stack_pop(); // forms
  }
  shadow_stack_pop(); // file_string
  return nil;
}

Obj *p_load_dylib(Obj** args, int arg_count) {
  char *filename = args[0]->s;
  void *handle = dlopen (filename, RTLD_LAZY);
  if (!handle) {
    set_error_and_return("Failed to open dylib: ", args[0]);
    return nil;
  }
  char *load_error;
  if ((load_error = dlerror()) != NULL)  {
    set_error_and_return("Failed to load dylib: ", args[0]);
  }
  //printf("dlopen %p\n", handle);
  return obj_new_dylib(handle);
}

Obj *p_unload_dylib(Obj** args, int arg_count) {
  //assert_or_return_nil(arg_count == 1, "'unload-dylib' must take one argument.");
  //assert_or_return_nil(args[0]->tag, "'unload-dylib' must take dylib as argument.", args[0]);
  if (!(args[0]->tag == 'D')) {
    set_error_and_return("unload-dylib takes a dylib as argument: ", args[0]);
    return nil;
  }
  void *handle = args[0]->dylib;
  if(!handle) {
    return obj_new_symbol("no handle to unload");
  }
  //printf("dlclose %p\n", handle);
  int result = dlclose(handle);
  if(result) {
    error = obj_new_string(dlerror());
    return nil;
  }
  else {
    return obj_new_symbol("dylib successfully unloaded");
  }
}

Obj *p_read(Obj** args, int arg_count) {
  //assert_or_return_nil(args[0], "No argument to 'read'.", args[0]);
  //assert_or_return_nil(args[0]->tag == 'S', "'read' must take a string as an argument.", args[0]);
  Obj *forms = read_string(global_env, args[0]->s);
  return forms->car;
}

Obj *p_read_many(Obj** args, int arg_count) {
  Obj *forms = read_string(global_env, args[0]->s);
  return forms;
}

Obj *p_eval(Obj** args, int arg_count) {
  if(arg_count != 1) { error = obj_new_string("Wrong argument count to 'eval'"); return nil; }
  eval_internal(global_env, args[0]);
  Obj *result = stack_pop();
  return result;
}

Obj *p_code(Obj** args, int arg_count) {  
  if(args[0]->tag != 'L' && args[0]->tag != 'M') {
    set_error_and_return("'code' must take lambda/macro as argument: ", args[0]);
  }
  if(!args[0]->code) {
    set_error_and_return("code of lambda/macro is NULL: ", args[0]);
  }
  Obj *code = args[0]->code;
  return code;
}

ffi_type *lisp_type_to_ffi_type(Obj *type_obj) {
  if(obj_eq(type_obj, type_string)) {
    return &ffi_type_pointer;
  }
  else if(obj_eq(type_obj, type_int)) {
    return &ffi_type_uint;
  }
  else if(obj_eq(type_obj, type_float)) {
    return &ffi_type_float;
  }
  else if(obj_eq(type_obj, type_void)) {
    return &ffi_type_uint;
  }
  else if(obj_eq(type_obj, type_bool)) {
    return &ffi_type_uint;
  }
  else if(type_obj->tag == 'C' && obj_eq(type_obj->car, type_ptr)) {
    return &ffi_type_pointer;
  }
  else {
    error = obj_new_string("Unhandled return type for foreign function.");
    return NULL;
  }
}

char *lispify(char *name) {
  char *s0 = str_replace(name, "_", "-");
  char *s1 = str_replace(s0, "BANG", "!");
  char *s2 = str_replace(s1, "QMARK", "?");
  free(s0);
  free(s1);
  return s2;
}

Obj *register_ffi_internal(char *name, VoidFn funptr, Obj *args, Obj *return_type_obj) {

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
  
  ffi_type **arg_types_c_array = malloc(sizeof(ffi_type) * (arg_count + 1));

  p = args;
  for(int i = 0; i < arg_count; i++) {
    ffi_type *arg_type = lisp_type_to_ffi_type(p->car);
    if(!arg_type) {
      char buffer[512];
      snprintf(buffer, 512, "Arg %d for function %s has invalid type: %s\n", i, name, obj_to_string(p->car)->s);
      error = obj_new_string(strdup(buffer));
      return nil;
    }
    arg_types_c_array[i] = arg_type;
    p = p->cdr;
  }
  arg_types_c_array[arg_count] = NULL; // ends with a NULL so we don't need to store arg_count

  ffi_type *return_type = lisp_type_to_ffi_type(return_type_obj);

  if(!return_type) {
    return nil;
  }

  ffi_cif *cif = malloc(sizeof(ffi_cif));
  int init_result = ffi_prep_cif(cif,
				 FFI_DEFAULT_ABI,
				 arg_count,
				 return_type,
				 arg_types_c_array);
  
  if (init_result != FFI_OK) {
    printf("Registration of foreign function %s failed.\n", name);
    return nil;
  }

  //printf("Registration of '%s' OK.\n", name);
  
  Obj *ffi = obj_new_ffi(cif, funptr, args, return_type_obj);

  char *lispified_name = lispify(name);
  //printf("Registering %s\n", lispified_name);
  
  global_env_extend(obj_new_symbol(lispified_name), ffi);

  return ffi;
}

Obj *register_ffi_variable_internal(char *name, void *varptr, Obj *var_type_obj) {

  if(!varptr) {
    printf("varptr for %s is NULL\n", name);
    return nil;
  }
  
  Obj *new_variable_value;

  if(obj_eq(var_type_obj, type_int)) {
    int *i = varptr;
    new_variable_value = obj_new_int(*i);
  }
  else {
    error = obj_new_string("Invalid variable type.");
    return nil;
  }

  char *lispified_name = lispify(name);
  //printf("Registering variable %s\n", lispified_name);  
  global_env_extend(obj_new_symbol(lispified_name), new_variable_value);

  return new_variable_value;
}

// (register <dylib> <function-name> <arg-types> <return-type>)
Obj *p_register(Obj** args, int arg_count) {
  if(arg_count != 4 || args[0]->tag != 'D' || args[1]->tag != 'S' || args[2]->tag != 'C') {
    printf("Args to register must be: (handle, function-name, argument-types, return-type)");
    printf("Arg count: %d\n", arg_count);
    printf("Args %c %c %c %c\n", args[0]->tag, args[1]->tag, args[2]->tag, args[3]->tag);
    return nil;
  }
  void *handle = args[0]->dylib;
  char *name = args[1]->s;
  
  VoidFn f = dlsym(handle, name);

  if(!f) {
    printf("Failed to load dynamic C function with name '%s' from %s\n", name, obj_to_string(args[0])->s);
    return nil;
  }
  
  return register_ffi_internal(name, f, args[2], args[3]);
}

Obj *p_register_variable(Obj** args, int arg_count) {
  if(arg_count != 3 || args[0]->tag != 'D' || args[1]->tag != 'S') {
    printf("Args to register-variable must be: (handle, variable-name, type)");
    printf("Arg count: %d\n", arg_count);
    printf("Args %c %c %c\n", args[0]->tag, args[1]->tag, args[2]->tag);
    return nil;
  }
  
  void *handle = args[0]->dylib;
  char *name = args[1]->s;
  
  void *variable = dlsym(handle, name);

  if(!variable) {
    printf("Failed to load dynamic C variable with name '%s' from %s\n", name, obj_to_string(args[0])->s);
    return nil;
  }
  
  return register_ffi_variable_internal(name, variable, args[2]);
}

Obj *p_register_builtin(Obj** args, int arg_count) {
  if(arg_count != 3 || args[0]->tag != 'S' || args[1]->tag != 'C') {
    printf("Args to register-builtin must be: (function-name, argument-types, return-type)\n");
    printf("Arg count: %d\n", arg_count);
    printf("Args %c %c %c\n", args[0]->tag, args[1]->tag, args[2]->tag);
    return nil;
  }
  char *name = args[0]->s;
  VoidFn f = dlsym(RTLD_DEFAULT, name);

  if(!f) {
    printf("Failed to load dynamic C function with name '%s' from executable.\n", name);
    return nil;
  }
  
  return register_ffi_internal(name, f, args[1], args[2]);
}


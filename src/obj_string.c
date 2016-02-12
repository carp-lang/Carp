#include "obj_string.h"

bool setting_print_lambda_body = true;

void obj_string_mut_append(Obj *string_obj, const char *s2) {
  assert(string_obj);
  assert(string_obj->tag == 'S');
  int string_obj_len = strlen(string_obj->s);
  int s2_len = strlen(s2);
  int total_length = (string_obj_len + s2_len);
  char *s3 = realloc(string_obj->s, sizeof(char) * (total_length + 1));
  s3[total_length] = '\0';
  strncpy(s3 + string_obj_len, s2, s2_len);
  string_obj->s = s3;
}

Obj *concat_c_strings(char *a, const char *b) {
  Obj *s = obj_new_string(a);
  obj_string_mut_append(s, b);
  return s;
}

void add_indentation(Obj *total, int indent) {
  for(int i = 0; i < indent; i++) {
    obj_string_mut_append(total, " ");
  }
}

void obj_to_string_internal(Obj *total, const Obj *o, bool prn, int indent) {
  assert(o);
  int x = indent;
  if(o->tag == 'C') {
    obj_string_mut_append(total, "(");
    x++;
    int save_x = x;
    const Obj *p = o;
    while(p && p->car) {
      obj_to_string_internal(total, p->car, true, x);
      if(p->cdr && p->cdr->tag != 'C') {
      	obj_string_mut_append(total, " . ");
      	obj_to_string_internal(total, o->cdr, true, x);
      	break;
      }
      else if(p->cdr && p->cdr->car) {
	if(/* p->car->tag == 'C' ||  */p->car->tag == 'E') {
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
    obj_string_mut_append(total, "[");
    for(int i = 0; i < o->count; i++) {
      obj_to_string_internal(total, o->array[i], true, x);
      if(i < o->count - 1) {
	obj_string_mut_append(total, " ");
      }
    }
    obj_string_mut_append(total, "]");
  }
  else if(o->tag == 'E') {
    obj_string_mut_append(total, "{");
    x++;
    Obj *p = o->bindings;
    while(p && p->car) {
      char *key_s = obj_to_string(p->car->car)->s;
      obj_string_mut_append(total, key_s);
      obj_string_mut_append(total, " ");
      obj_to_string_internal(total, p->car->cdr, true, x + strlen(key_s) + 1);
      p = p->cdr;
      if(p && p->car && p->car->car) {
	obj_string_mut_append(total, ", \n");
	add_indentation(total, x);
      }
    }
    obj_string_mut_append(total, "}");
    if(o->parent) {
      obj_string_mut_append(total, " -> \n");
      Obj *parent_printout = obj_to_string(o->parent);
      obj_string_mut_append(total, parent_printout->s);
    }
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
    obj_string_mut_append(total, "<ptr:");
    static char temp[256];
    snprintf(temp, 256, "%p", o->primop);
    obj_string_mut_append(total, temp);
    obj_string_mut_append(total, ">");
  }
  else if(o->tag == 'F') {
    obj_string_mut_append(total, "<ffi:");
    static char temp[256];
    snprintf(temp, 256, "%p", o->funptr);
    obj_string_mut_append(total, temp);
    obj_string_mut_append(total, ">");
  }
  else if(o->tag == 'L') {
    if(setting_print_lambda_body) {
      obj_string_mut_append(total, "(fn");
      obj_string_mut_append(total, " ");
      obj_string_mut_append(total, obj_to_string(o->params)->s);
      obj_string_mut_append(total, " ");
      obj_string_mut_append(total, obj_to_string(o->body)->s);
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
      obj_string_mut_append(total, obj_to_string(o->params)->s);
      obj_string_mut_append(total, " ");
      obj_string_mut_append(total, obj_to_string(o->body)->s);
      obj_string_mut_append(total, ")");
    }
    else {
      obj_string_mut_append(total, "<macro>");
    }
  }
  else if(o->tag == 'B') {
    char s[2] = { o->b, '\0' };
    if(prn) {
      obj_string_mut_append(total, "\\");
    }
    obj_string_mut_append(total, s);
  }
  else {
    printf("obj_to_string() can't handle type tag %c (%d).\n", o->tag, o->tag);
    assert(false);
  }
}

Obj *obj_to_string(const Obj *o) {
  Obj *s = obj_new_string("");
  obj_to_string_internal(s, o, true, 0);
  return s;
}

Obj *obj_to_string_not_prn(const Obj *o) {
  Obj *s = obj_new_string("");
  obj_to_string_internal(s, o, false, 0);
  return s;
}

void obj_print(Obj *o) {
  assert(o);
  Obj *s = obj_to_string(o);
  printf("%s", s->s);
}

void obj_print_not_prn(Obj *o) {
  Obj *s = obj_to_string_not_prn(o);
  printf("%s", s->s);
}



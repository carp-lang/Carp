#include "reader.h"
#include <ctype.h>
#include "env.h"
#include "obj_string.h"

int read_line_nr;
int read_line_pos;
int read_pos = 0;
#define CURRENT s[read_pos]

bool is_ok_in_symbol(char c, bool initial) {
  if(isdigit(c) && initial) {
    return false;
  }
  else if(c == '\'' && initial) {
    return true;
  }
  else if(c == '!' || c == '?' || c == '<' || c == '>' || c == '=' || c == '%' ||
          c == '+' || c == '*' || c == '/' || c == '-' || c == '_' || c == '#') {
    return true;
  }
  else if(isalpha(c) || isdigit(c)) {
    return true;
  }
  else {
    return false;
  }
}

bool is_whitespace(char c) {
  return c == ' ' || c == '\r' || c == '\t' || c == '\n' || c == ',';
}

void hit_new_line() {
  read_line_nr++;
  read_line_pos = 0;
}

void skip_whitespace(char *s) {
  while(is_whitespace(CURRENT)) {
    if(CURRENT == '\n') {
      hit_new_line();
    }
    read_pos++;
  }
  if(CURRENT == ';') {
    while(CURRENT != '\n' && CURRENT != '\0') {
      read_pos++;
    }
    hit_new_line();
    read_pos++;
    skip_whitespace(s);
  }
}

void print_read_pos() {
  printf("Line: %d, pos: %d.\n", read_line_nr, read_line_pos);
}

Obj *read_internal(Process *process, Obj *env, char *s, Obj *filename) {
  skip_whitespace(s);

  if(CURRENT == ')' || CURRENT == ']') {
    read_pos++;
    printf("Too many parenthesis at the end.\n");
    print_read_pos();
    return nil;
  }
  else if(CURRENT == '(') {
    Obj *list = obj_new_cons(NULL, NULL);
    obj_set_line_info(process, list, read_line_nr, read_line_pos, filename);
    Obj *prev = list;
    read_pos++;
    while(1) {
      skip_whitespace(s);
      if(CURRENT == '\0') {
        printf("Missing parenthesis at the end.\n");
        print_read_pos();
        return nil;
      }
      if(CURRENT == ')') {
        read_pos++;
        break;
      }
      Obj *o = read_internal(process, env, s, filename);
      Obj *new = obj_new_cons(NULL, NULL);
      prev->car = o;
      prev->cdr = new;
      prev = new;
    }
    return list;
  }
  else if(CURRENT == '[') {
//const int max_count = 512; // MSVC thinks that this is not a constant. What the ...
#define MAX_COUNT 512
    Obj *temp[MAX_COUNT];
    int count = 0;
    int line = read_line_nr;
    int pos = read_line_pos;

    read_pos++;
    while(1) {
      skip_whitespace(s);
      if(CURRENT == '\0') {
        printf("Missing ']' at the end of array.\n");
        print_read_pos();
        return nil;
      }
      if(CURRENT == ']') {
        read_pos++;
        break;
      }
      Obj *o = read_internal(process, env, s, filename);
      temp[count] = o;
      count++;
      if(count >= MAX_COUNT) {
        eval_error = obj_new_string("Can't read more than 512 values in literal. Please talk to the creator of this language about this.");
      }
    }

    Obj *new_array = obj_new_array(count);
    for(int i = 0; i < count; i++) {
      new_array->array[i] = temp[i];
    }
    obj_set_line_info(process, new_array, line, pos, filename);
    return new_array;
  }
  else if(CURRENT == '{') {
    int line = read_line_nr;
    int pos = read_line_pos;

    Obj *list = obj_new_cons(NULL, NULL);
    obj_set_line_info(process, list, read_line_nr, read_line_pos, filename);
    Obj *prev = list;
    read_pos++;
    while(1) {
      skip_whitespace(s);
      if(CURRENT == '\0') {
        printf("Missing '}' at the end.\n");
        print_read_pos();
        return nil;
      }
      if(CURRENT == '}') {
        read_pos++;
        break;
      }
      Obj *o = read_internal(process, env, s, filename);
      Obj *new = obj_new_cons(NULL, NULL);
      prev->car = o;
      prev->cdr = new;
      prev = new;
    }
    Obj *call_to_dict = obj_new_cons(obj_new_symbol("dictionary"), list);
    //printf("Read dictionary literal: %s\n", obj_to_string(process, call_to_dict)->s);
    obj_set_line_info(process, call_to_dict, line, pos, filename);
    return call_to_dict;
  }
  else if(CURRENT == '&') {
    int line = read_line_nr, pos = read_line_pos;
    read_pos++;
    Obj *inner = read_internal(process, env, s, filename);
    Obj *cons2 = obj_new_cons(inner, nil);
    Obj *cons1 = obj_new_cons(obj_new_symbol("ref"), cons2);
    obj_set_line_info(process, cons1, line, pos, filename);
    return cons1;
  }
  else if(CURRENT == '.' && s[read_pos + 1] == '.' && s[read_pos + 2] == '.') {
    read_pos += 3;
    return dotdotdot;
  }
  else if(CURRENT == '\\') {
    read_pos++;
    char b = CURRENT;
    read_pos++;
    return obj_new_char(b);
  }
  else if(isdigit(CURRENT) || (CURRENT == '-' && isdigit(s[read_pos + 1]))) {
    int line = read_line_nr, pos = read_line_pos;
    int negator = 1;
    if(CURRENT == '-') {
      negator = -1;
      read_pos++;
    }
    bool has_period = false;
    bool is_double = false;
    bool is_float = false;
    char scratch[32];
    int i = 0;
    while(isdigit(CURRENT)) {
      scratch[i++] = CURRENT;
      read_pos++;
      if(CURRENT == '.' && !has_period) {
        scratch[i++] = CURRENT;
        has_period = true;
        read_pos++;
      }
      if(CURRENT == 'f') {
        is_float = true;
        read_pos++;
        break;
      }
      if(CURRENT == 'd') {
        is_double = true;
        read_pos++;
        break;
      }
    }
    scratch[i] = '\0';
    if(has_period && !is_float) {
      double x = atof(scratch) * negator;
      Obj *new_double = obj_new_double(x);
      obj_set_line_info(process, new_double, line, pos, filename);
      return new_double;
    }
    else if(is_float) {
      float x = (float)atof(scratch) * negator;
      Obj *new_float = obj_new_float(x);
      obj_set_line_info(process, new_float, line, pos, filename);
      return new_float;
    }
    else {
      int num = atoi(scratch) * negator;
      Obj *new_int = obj_new_int(num);
      obj_set_line_info(process, new_int, line, pos, filename);
      return new_int;
    }
  }
  else if(CURRENT == '@') {
    read_pos++;
    Obj *inner = read_internal(process, env, s, filename);
    Obj *cons2 = obj_new_cons(inner, nil);
    Obj *cons1 = obj_new_cons(obj_new_symbol("copy"), cons2);
    return cons1;
  }
  else if(CURRENT == '\'') {
    read_pos++;
    Obj *inner = read_internal(process, env, s, filename);
    Obj *cons2 = obj_new_cons(inner, nil);
    Obj *cons1 = obj_new_cons(lisp_quote, cons2);
    return cons1;
  }
  else if(CURRENT == '`') {
    read_pos++;
    Obj *inner = read_internal(process, env, s, filename);
    Obj *cons2 = obj_new_cons(inner, nil);
    Obj *cons1 = obj_new_cons(obj_new_symbol("quasiquote"), cons2);
    //printf("Read quasiquote.\n");
    return cons1;
  }
  else if(CURRENT == '~') {
    read_pos++;
    if(CURRENT == '@') {
      read_pos++;
      Obj *sym = read_internal(process, env, s, filename);
      Obj *cons2 = obj_new_cons(sym, nil);
      Obj *cons1 = obj_new_cons(obj_new_symbol("dequote-splicing"), cons2);
      return cons1;
    }
    else {
      Obj *sym = read_internal(process, env, s, filename);
      Obj *cons2 = obj_new_cons(sym, nil);
      Obj *cons1 = obj_new_cons(obj_new_symbol("dequote"), cons2);
      return cons1;
    }
  }
  else if(CURRENT == '^') {
    read_pos++;
    Obj *key_symbol = read_internal(process, env, s, filename);

    if(key_symbol->tag != 'Y') {
      eval_error = obj_new_string("Invalid key for meta data.");
      return nil;
    }

    Obj *key = obj_new_keyword(key_symbol->s);
    Obj *value = read_internal(process, env, s, filename);
    Obj *form = read_internal(process, env, s, filename);
    Obj *head = obj_new_symbol("meta-set!");

    Obj *new_form = obj_list(head, form, key, value);

    return new_form;
  }
  else if(is_ok_in_symbol(CURRENT, true)) {
    int line = read_line_nr, pos = read_line_pos;
    char name[512];
    int i = 0;
    while(is_ok_in_symbol(CURRENT, false)) {
      name[i++] = CURRENT;
      read_pos++;
    }
    name[i] = '\0';
    Obj *symbol = obj_new_symbol(name);
    obj_set_line_info(process, symbol, line, pos, filename);
    symbol->hash = obj_hash(process, symbol);
    return symbol;
  }
  else if(CURRENT == ':') {
    int line = read_line_nr, pos = read_line_pos;
    read_pos++;
    char name[512];
    int i = 0;
    while(is_ok_in_symbol(CURRENT, false)) {
      name[i++] = CURRENT;
      read_pos++;
    }
    name[i] = '\0';

    Obj *new_keyword = obj_new_keyword(name);
    obj_set_line_info(process, new_keyword, line, pos, filename);
    new_keyword->hash = obj_hash(process, new_keyword);
    return new_keyword;
  }
  else if(CURRENT == '"') {
    read_pos++;
    int line = read_line_nr;
    int pos = read_line_pos;
    char str[512];
    int i = 0;
    while(CURRENT != '"') {
      if(CURRENT == '\0') {
        printf("Missing quote in string\n");
        break;
      }
      else if(CURRENT == '\\') {
        read_pos++;
        if(CURRENT == 'n') {
          str[i++] = '\n';
        }
        else if(CURRENT == '"') {
          str[i++] = '"';
        }
        else if(CURRENT == '\\') {
          str[i++] = '\\';
        }
        else {
          printf("Can't read '%c' after backslash (%d)\n", CURRENT, CURRENT);
          read_pos++;
          return nil;
        }
        read_pos++;
      }
      else {
        str[i++] = CURRENT;
        read_pos++;
      }
    }
    str[i] = '\0';
    read_pos++;
    Obj *new_string = obj_new_string(str);
    obj_new_string(str);
    obj_set_line_info(process, new_string, line, pos, filename);
    new_string->hash = obj_hash(process, new_string);
    return new_string;
  }
  else if(CURRENT == 0) {
    return nil;
  }
  else {
    printf("Can't read '%c' (%d)\n", CURRENT, CURRENT);
    read_pos++;
    return nil;
  }
}

Obj *read_string(Process *process, Obj *env, char *s, Obj *filename) {
  read_line_nr = 1;
  read_line_pos = 0;
  read_pos = 0;
  Obj *top_forms = NULL;
  Obj *prev = NULL;
  while(s[read_pos] != '\0') {
    Obj *o = read_internal(process, env, s, filename);
    Obj *cons = obj_new_cons(NULL, NULL);
    cons->car = o;
    if(!top_forms) {
      top_forms = cons;
    }
    if(prev) {
      prev->cdr = cons;
    }
    prev = cons;
    skip_whitespace(s);
  }
  return top_forms;
}

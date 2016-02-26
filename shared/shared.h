#ifndef SHARED_H
#define SHARED_H

#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
//#include <pthread.h>
//#include <unistd.h>
#include "platform.h"

typedef int unknown;
typedef void* typevar;
typedef void* any;

typedef char* string;

int intsqrt(int x) { return (int)sqrt(x); }
float itof(int x) { return (float)x; }

#ifndef max
int max(int x, int y) {
  return x > y ? x : y;
}
#endif

string itos(int x) {
  char *s = malloc(sizeof(char) * 32);
  snprintf(s, 32, "%d", x);
  return s;
}

bool nullQMARK(void *p) {
  return p == NULL;
}

bool not(bool x) {
  return !x;
}

void panic(string msg) {
  printf("Error: %s\n", msg);
  exit(1);
}



void print(string msg) {
  printf("%s", msg);
}

void println(string msg) {
  printf("%s\n", msg);
}

int* fake() {
  return (int*)123;
}

void fake2(string *s) {
  
}

void eat_string(char *s) {
  free(s);
}

void eat_void(void *nothing) {
  // nothing!
}

char *string_copy(char *s) {
  return strdup(s);
}

char *string_append(char *s1, char *s2) {
  char *new_str = malloc(strlen(s1) + strlen(s2) + 1);
  new_str[0] = '\0';
  strcat(new_str, s1);
  strcat(new_str, s2);
  return new_str;
}

bool file_existsQMARK(char *filename) {
  FILE *f = fopen(filename, "r");
  return f != NULL;
}

typedef string* string_array;

string_array string_array_new(int size) {
  string_array a = calloc(size + 1, sizeof(string));
  for(int i = 0; i < size; i++) {
    a[i] = strdup("");
  }
  return a;
}

int string_array_count(string_array array) {
  int i = 0;
  string_array p = array;
  while(*p) {
    i++;
    p++;
  }
  return i;
}

string string_array_get(string_array array, int pos) {
  return strdup(array[pos]);
}

string_array string_array_set(string_array array, int pos, string new_value) {
  array[pos] = strdup(new_value);
  return array;
}

typedef string (*string_to_string_fn)(string);

string_array string_array_map(string_to_string_fn f, string_array array) {
  string_array p = array;
  while(*p) {
    string old_string = *p;
    string new_string = f(old_string);
    *p = new_string;
    p++;
  }
  return array;
}

int inc(x) { return x + 1; }
int dec(x) { return x - 1; }

void async(void *f) {
  printf("Async starting.\n");
  carp_thread_t th = carp_thread_create(f, "Async");
  carp_thread_destroy(th);
  printf("Async done.\n");
}

int last_index_of(string s, char c) {
  int len = (int)strlen(s);
  for(int i = len - 1; i >= 0; i--) {
    if(s[i] == c) {
      return i;
    }
  }
  return -1;
}

string substring(string s, int index) {
  if(index >= strlen(s)) {
    panic("substring out of bounds");
  }
  const char *sub = s + index;
  return strdup(sub);
}

string file_path_component(string s) {
  int i = last_index_of(s, '/');
  return substring(s, i + 1);
}

string get_input() {
  char in[1024];
  fgets(in, 1024, stdin);
  return strdup(in);
}

void call(void *f()) {
  f();
}

void call1(void *f(int)) {
  f(1);
}

void calls(void *f(char*)) {
  f("hejsan");
}

void printret(int (*f)()) {
  int x = f();
  printf("ret = %d\n", x);
}

typedef struct {
  float x;
  float y;
} FauxVec2;

FauxVec2 *position() {
  FauxVec2 *v2 = malloc(sizeof(FauxVec2));
  v2->x = 100.0f;
  v2->y = 200.0f;
  return v2;
}

typedef struct {
  int count;
  void *data;
} Array;


string str__RefTo_ArrayOf_int_stringsfdfsf(Array *a) {
  char buffer[1024];
  int pos = 0;
  int count = a->count;
  buffer[pos++] = '[';
  for(int i = 0; i < count; i++) {
    int **casted_data = a->data;
    int *element = casted_data[i];
    //str__int_string(*element);
    if(i < count - 1) {
      buffer[pos++] = ',';
      buffer[pos++] = ' ';
    }
  }
  buffer[pos++] = ']';
  buffer[pos++] = '\0';
  return strdup(buffer);
}


#endif

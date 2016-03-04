#pragma once

#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include "types.h"
#include "platform.h"

#ifdef WIN32
#define EXPORT __declspec(dllexport)
#else
#define EXPORT
#endif

typedef int unknown;
typedef void* typevar;
typedef void* any;

typedef char* string;

EXPORT int intsqrt(int x) { return (int)sqrt(x); }
EXPORT float itof(int x) { return (float)x; }

#ifdef max
#undef max
#endif
EXPORT int max(int x, int y) {
  return x > y ? x : y;
}

EXPORT string itos(int x) {
  char *s = malloc(sizeof(char) * 32);
  snprintf(s, 32, "%d", x);
  return s;
}

EXPORT bool nullQMARK(void *p) {
  return p == NULL;
}

EXPORT bool not(bool x) {
  return !x;
}

EXPORT void panic(string msg) {
  printf("Error: %s\n", msg);
  exit(1);
}

EXPORT void print(string msg) {
  printf("%s", msg);
}

EXPORT void println(string msg) {
  assert(msg);
  printf("%s\n", msg);
}

EXPORT int* fake() {
  return (int*)123;
}

EXPORT void fake2(string *s) {
  
}

EXPORT void eat_string(char *s) {
  free(s);
}

EXPORT void eat_void(void *nothing) {
  // nothing!
}

EXPORT char *string_copy(char *s) {
  return strdup(s);
}

EXPORT char *string_append(char *s1, char *s2) {
  char *new_str = malloc(strlen(s1) + strlen(s2) + 1);
  new_str[0] = '\0';
  strcat(new_str, s1);
  strcat(new_str, s2);
  return new_str;
}

EXPORT bool file_existsQMARK(char *filename) {
  FILE *f = fopen(filename, "r");
  bool result = f != NULL;
  if(result) {
    fclose(f);
  }
  return result;
}

typedef string* string_array;

EXPORT string_array string_array_new(int size) {
  string_array a = calloc(size + 1, sizeof(string));
  for(int i = 0; i < size; i++) {
    a[i] = strdup("");
  }
  return a;
}

EXPORT int string_array_count(string_array array) {
  int i = 0;
  string_array p = array;
  while(*p) {
    i++;
    p++;
  }
  return i;
}

EXPORT string string_array_get(string_array array, int pos) {
  return strdup(array[pos]);
}

EXPORT string_array string_array_set(string_array array, int pos, string new_value) {
  array[pos] = strdup(new_value);
  return array;
}

typedef string (*string_to_string_fn)(string);

EXPORT string_array string_array_map(string_to_string_fn f, string_array array) {
  string_array p = array;
  while(*p) {
    string old_string = *p;
    string new_string = f(old_string);
    *p = new_string;
    p++;
  }
  return array;
}

EXPORT int inc(x) { return x + 1; }
EXPORT int dec(x) { return x - 1; }

EXPORT void async(void *f) {
  printf("Async starting.\n");
  carp_thread_t th = carp_thread_create(f, "Async");
  carp_thread_destroy(th);
  printf("Async done.\n");
}

EXPORT int last_index_of(string s, char c) {
  int len = (int)strlen(s);
  for(int i = len - 1; i >= 0; i--) {
    if(s[i] == c) {
      return i;
    }
  }
  return -1;
}

EXPORT string substring(string s, int index) {
  if(index >= strlen(s)) {
    panic("substring out of bounds");
  }
  const char *sub = s + index;
  return strdup(sub);
}

EXPORT string file_path_component(string s) {
  int i = last_index_of(s, '/');
  return substring(s, i + 1);
}

EXPORT string get_input() {
  char in[1024];
  fgets(in, 1024, stdin);
  return strdup(in);
}

EXPORT void call(void *f()) {
  f();
}

EXPORT void call1(void *f(int)) {
  f(1);
}

EXPORT void calls(void *f(char*)) {
  f("hejsan");
}

EXPORT void printret(int (*f)()) {
  int x = f();
  printf("ret = %d\n", x);
}

EXPORT int mod(int x, int y) {
  return x % y;
}

#ifdef WIN32
EXPORT void sleep(int millis) {
	carp_sleep(millis);
}
#endif

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

EXPORT CARP_PLATFORM platform() {
	return carp_get_platform();
}


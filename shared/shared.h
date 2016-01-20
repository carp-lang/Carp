#ifndef SHARED_H
#define SHARED_H

#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>

typedef int unknown;
typedef void* typevar;
typedef void* any;

typedef char* string;

int intsqrt(int x) { return sqrt(x); }
float itof(int x) { return (float)x; }

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

char *string_copy(char *s) {
  return strdup(s);
}

char *str_append(char *s1, char *s2) {
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
  return array[pos];
}

string_array string_array_set(string_array array, int pos, string new_value) {
  array[pos] = strdup(new_value);
  return array;
}

#endif

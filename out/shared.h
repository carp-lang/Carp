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

#endif

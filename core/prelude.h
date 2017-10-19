#ifndef PRELUDE_H
#define PRELUDE_H

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <math.h>
#include <time.h>
#include <assert.h>
#include <limits.h>

void *logged_malloc(size_t size) {
    void *ptr = malloc(size);
    printf("MALLOC: %p (%ld bytes)\n", ptr, size);
    return ptr;
}

void logged_free(void *ptr) {
    printf("FREE: %p\n", ptr);
    free(ptr);
}

#define LOG_MEMORY 0

#if LOG_MEMORY
#define CARP_MALLOC(size) logged_malloc(size)
#define CARP_FREE(ptr) logged_free(ptr)
#else
#define CARP_MALLOC(size) malloc(size)
#define CARP_FREE(ptr) free(ptr)
#endif

typedef char* string;

// Array
typedef struct {
    int len;
    void *data;
} Array;

bool not(bool b) {
    return !b;
}

int Int__PLUS_(x, y)   { return x + y; }
int Int__MINUS_(x, y)  { return x - y; }
int Int__MUL_(x, y)    { return x * y; }
int Int__DIV_(x, y)    { return x / y; }
bool Int__EQ_(x, y)     { return x == y; }
bool Int__DIV__EQ_(x, y) { return x != y; }
bool Int__LT_(x, y)    { return x < y; }
bool Int__GT_(x, y)    { return x > y; }

int Int_inc(int x) { return x + 1; }
int Int_dec(int x) { return x - 1; }

int Int_copy(int *x) { return *x; }
float Float_copy(float *x) { return *x; }
double Double_copy(double *x) { return *x; }

#define Double__PLUS_(x, y) ((x) + (y))
#define Double__MINUS_(x, y) ((x) - (y))
#define Double__MUL_(x, y) ((x) * (y))
#define Double__DIV_(x, y) ((x) / (y))
#define Double__LT_(x, y) ((x) < (y))
#define Double__GT_(x, y) ((x) > (y))

#define Float__PLUS_(x, y) ((x) + (y))
#define Float__MINUS_(x, y) ((x) - (y))
#define Float__MUL_(x, y) ((x) * (y))
#define Float__DIV_(x, y) ((x) / (y))

#define and(x, y) ((x) && (y))
#define or(x, y) ((x) || (y))

void IO_println(string *s) { puts(*s); }
void IO_print(string *s) { printf("%s", *s); }

string IO_get_MINUS_line() {
    size_t size = 1024;
    char *buffer = CARP_MALLOC(size);
    getline(&buffer, &size, stdin);
    return buffer;
}

int Int_from_MINUS_string(string *s) {
    return atoi(*s);
}

int Int_mod(int x, int divider) {
    return x % divider;
}

void Int_seed(int seed) {
    srand(seed);
}

int Int_random() {
    return rand();
}

int Int_random_MINUS_between(int lower, int upper) {
    int diff = upper - lower;
    return lower + (rand() % diff);
}

string Int_str(int x) {
    char *buffer = CARP_MALLOC(64);
    snprintf(buffer, 64, "%d", x);
    return buffer;
}

bool Int_mask(int a, int b) {
    return a & b;
}

void String_delete(string s) {
    CARP_FREE(s);
}

string String_copy(string *s) {
    char *ptr = strdup(*s);
    #if LOG_MEMORY
    printf("STRDUP: %p\n", ptr);
    #endif
    return ptr;
}

bool String__EQ_(string *a, string *b) {
    return strcmp(*a, *b) == 0;
}

bool String__DIV__EQ_(string *a, string *b) {
    return strcmp(*a, *b) == 0;
}
string String_append(string a, string b) {
    int la = strlen(a);
    int lb = strlen(b);
    int total = la + lb + 1;
    string buffer = CARP_MALLOC(total);
    snprintf(buffer, total, "%s%s", a, b);
    CARP_FREE(a);
    CARP_FREE(b);
    return buffer;
}

int String_count(string *s) {
    return strlen(*s);
}

// Replace with 'copy' later:
string String_duplicate(string *s) {
    return strdup(*s);
}

char* String_cstr(string *s) {
    return *s;
}

string String_str(string *s) {
    int n = strlen(*s) + 4;
    string buffer = malloc(n);
    snprintf(buffer, n, "@\"%s\"", *s);
    return buffer;
}

Array String_chars(string *s) {
    Array chars;
    chars.len = strlen(*s);
    chars.data = strdup(*s);
    return chars;
}

string String_from_MINUS_chars(Array a) {
    string s = malloc(a.len);
    snprintf(s, a.len, "%s", a.data);
    return s;
}

string Char_str(char c) {
    char *buffer = CARP_MALLOC(3);
    snprintf(buffer, 3, "\\%c", c);
    return buffer;
}

int exmod__bleh(int x) {
    return x * 1000;
}

// Double.toInt : Double -> Int
int Double_to_MINUS_int(double x) {
    return (int)x;
}

double Double_from_MINUS_int(int x) {
    return (double)x;
}

double Double_sin(double x) {
    return sin(x);
}

double Double_cos(double x) {
    return cos(x);
}

double Double_sqrt(double x) {
    return sqrt(x);
}

double Double_atan2(double x, double y) {
    return atan2(x, y);
}

string Double_str(double x) {
    char *buffer = CARP_MALLOC(32);
    snprintf(buffer, 32, "%g", x);
    return buffer;
}

int Float_to_MINUS_int(double x) {
    return (int)x;
}

float Float_random_MINUS_between(float lower, float upper) {
    float diff = upper - lower;
    float r = ((float)(rand() % INT_MAX)) / ((float)INT_MAX);
    return lower + diff * r;
}

string Float_str(float x) {
    char *buffer = CARP_MALLOC(32);
    snprintf(buffer, 32, "%gf", x);
    return buffer;
}

// Bool
bool Bool__EQ_(bool a, bool b) {
  return a == b;
}

bool Bool__DIV__EQ_(bool a, bool b) {
  return a != b;
}

string Bool_str(bool b) {
    if(b) {
        return strdup("true");
    } else {
        return strdup("false");
    }
}

void System_exit(int code) {
    exit(code);
}

void System_CARP_FREE__string_MUL_(void *p) {
    CARP_FREE(p);
}

int System_time() {
    return time(0);
}

void System_srand(int x) {
    srand(x);
}

#endif

#ifndef PRELUDE_H
#define PRELUDE_H

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <math.h>
#include <time.h>
#include <assert.h>

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

bool not(bool b) {
    return !b;
}

int Int__PLUS_(x, y)  { return x + y; }
int Int__MINUS_(x, y) { return x - y; }
int Int__MUL_(x, y)   { return x * y; }
int Int__DIV_(x, y)   { return x / y; } 
int Int__EQ_(x, y)    { return x == y; } 
int Int__LT_(x, y)    { return x < y; } 
int Int__GT_(x, y)    { return x > y; } 

int Int_inc(int x) { return x + 1; }
int Int_dec(int x) { return x - 1; }

#define Double__PLUS_(x, y) ((x) + (y))
#define Double__MINUS_(x, y) ((x) - (y))
#define Double__MUL_(x, y) ((x) * (y))
#define Double__DIV_(x, y) ((x) / (y))

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

string str(int x) {
    char *buffer = CARP_MALLOC(64);
    snprintf(buffer, 64, "%d", x);
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
    return str(x);
}

bool Int_mask(int a, int b) {
    return a & b;
}

void String_delete(string s) {
    CARP_FREE(s);
}

string String_copy(string *s) {
    return strdup(*s);
}

bool String__EQ_(string *a, string *b) {
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

string Char_str(char c) {
    char *buffer = CARP_MALLOC(2);
    snprintf(buffer, 2, "%c", c);
    return buffer;
}

int exmod__bleh(int x) {
    return x * 1000;
}

// Double.toInt : Double -> Int
int Double_toInt(double x) {
    return (int)x;
}

double Double_fromInt(int x) {
    return (double)x;
}

double Double_sin(double x) {
    return sin(x);
}

double Double_cos(double x) {
    return cos(x);
}

int Float_toInt(double x) {
    return (int)x;
}

// Array
typedef struct {
    int len;
    void *data;
} Array;

Array Array_range(int start, int end) {
    Array a;
    int len = end - start;
    a.len = len;
    a.data = CARP_MALLOC(sizeof(int) * len);
    for(int i = 0; i < len; ++i) {
        ((int*)a.data)[i] = start + i;
    }
    return a;
}

string Array_str__int(Array *aRef) {
    Array a = *aRef;
    string buffer = CARP_MALLOC(1024);
    string b = buffer;
    sprintf(b, "["); b += 1;
    for(int i = 0; i < a.len; ++i) {
        string temp = malloc(32);
        snprintf(temp, 32, "%d", ((int*)a.data)[i]);
        sprintf(b, "%s", temp);
        free(temp);
        b += strlen(temp);
        if(i < a.len - 1) {
            sprintf(b, " "); b += 1;            
        }
    }
    sprintf(b, "]"); b += 1;
    *b = '\0';
    return buffer;        
}

string Array_str__string(Array *aRef) {
    Array a = *aRef;
    string buffer = CARP_MALLOC(1024);
    string b = buffer;
    sprintf(b, "["); b += 1;
    for(int i = 0; i < a.len; ++i) {
        char *temp = ((string*)a.data)[i];
        sprintf(b, "%s", temp);
        b += strlen(temp);
        if(i < a.len - 1) {
            sprintf(b, " "); b += 1;            
        }
    }
    sprintf(b, "]"); b += 1;
    *b = '\0';
    return buffer;        
}

string Array_str__bool(Array a) {
    string buffer = CARP_MALLOC(1024);
    string b = buffer;
    sprintf(b, "["); b += 1;
    for(int i = 0; i < a.len; ++i) {
        string temp = malloc(32);
        snprintf(temp, 32, "%s", (((int*)a.data)[i] ? "true" : "false"));
        sprintf(b, "%s", temp);
        free(temp);
        b += strlen(temp);
        if(i < a.len - 1) {
            sprintf(b, " "); b += 1;            
        }
    }
    sprintf(b, "]"); b += 1;
    *b = '\0';
    return buffer;        
}

int Array_sum(Array *aRef) {
    Array a = *aRef;
    int sum = 0;
    for(int i = 0; i < a.len; ++i) {
        sum += ((int*)a.data)[i];
    }
    return sum;
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

#endif

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

typedef struct {
    size_t size;  // size of the allocated data buffer including the NUL-terminator.
    char *data;
} string;

#ifdef LOG_MEMORY
int malloc_balance_counter = 0;

void *logged_malloc(size_t size) {
    void *ptr = malloc(size);
    printf("MALLOC: %p (%ld bytes)\n", ptr, size);
    malloc_balance_counter++;
    return ptr;
}

void logged_free(void *ptr) {
    printf("FREE: %p\n", ptr);
    free(ptr);
    malloc_balance_counter--;
    if(malloc_balance_counter == 0) {
        printf("malloc is balanced! (this should be the last thing you see)\n");
    }
    else if(malloc_balance_counter < 0) {
        printf("malloc is %d, that is bad!\n", malloc_balance_counter);
    }
}

#define CARP_MALLOC(size) logged_malloc(size)
#define CARP_FREE(ptr) logged_free(ptr)

#else

#define CARP_MALLOC(size) malloc(size)
#define CARP_FREE(ptr) free(ptr)

#endif

void string_constructor(string *s, size_t size) {
    assert(size >= 1); // At least one char is needed for the NUL-terminator.
    s->data = CARP_MALLOC(size);
    s->size = size;
}

void string_destructor(string *s) {
    CARP_FREE(s->data);
}

string string_from_cstr(const char *s) {
    string ret;
    string_constructor(&ret, strlen(s) + 1);
    memcpy(ret.data, s, ret.size);
    return ret;
}


// Array
typedef struct {
    size_t len;
    void *data;
} Array;

bool not(bool b) {
    return !b;
}

int Int__PLUS_(int x, int y)   { return x + y; }
int Int__MINUS_(int x, int y)  { return x - y; }
int Int__MUL_(int x, int y)    { return x * y; }
int Int__DIV_(int x, int y)    { return x / y; }
bool Int_safe_MINUS_add(int x, int y, int* res) { return __builtin_sadd_overflow(x, y, res); }
bool Int_safe_MINUS_sub(int x, int y, int* res) { return __builtin_ssub_overflow(x, y, res); }
bool Int_safe_MINUS_mul(int x, int y, int* res) { return __builtin_smul_overflow(x, y, res); }
bool Int__EQ_(int x, int y)     { return x == y; }
bool Int__DIV__EQ_(int x, int y) { return x != y; }
bool Int__LT_(int x, int y)    { return x < y; }
bool Int__GT_(int x, int y)    { return x > y; }

int Int_inc(int x) { return x + 1; }
int Int_dec(int x) { return x - 1; }
int Int_abs(int x) { return abs(x); }
int Int_bit_MINUS_shift_MINUS_left(int x, int y) { return x << y; }
int Int_bit_MINUS_shift_MINUS_right(int x, int y) { return x >> y; }
int Int_bit_MINUS_and(int x, int y) { return x & y; }
int Int_bit_MINUS_or(int x, int y) { return x | y; }
int Int_bit_MINUS_xor(int x, int y) { return x ^ y; }
int Int_bit_MINUS_not(int x) { return ~x; }

long Long__PLUS_(long x, long y)   { return x + y; }
long Long__MINUS_(long x, long y)  { return x - y; }
long Long__MUL_(long x, long y)    { return x * y; }
long Long__DIV_(long x, long y)    { return x / y; }
bool Long_safe_MINUS_add(long x, long y, long* res) { return __builtin_saddl_overflow(x, y, res); }
bool Long_safe_MINUS_sub(long x, long y, long* res) { return __builtin_ssubl_overflow(x, y, res); }
bool Long_safe_MINUS_mul(long x, long y, long* res) { return __builtin_smull_overflow(x, y, res); }
bool Long__EQ_(long x, long y)     { return x == y; }
bool Long__LT_(long x, long y)    { return x < y; }
bool Long__GT_(long x, long y)    { return x > y; }

long Long_inc(long x) { return x + 1; }
long Long_dec(long x) { return x - 1; }
long Long_abs(long x) { return labs(x); }
long Long_bit_MINUS_shift_MINUS_left(long x, long y) { return x << y; }
long Long_bit_MINUS_shift_MINUS_right(long x, long y) { return x >> y; }
long Long_bit_MINUS_and(long x, long y) { return x & y; }
long Long_bit_MINUS_or(long x, long y) { return x | y; }
long Long_bit_MINUS_xor(long x, long y) { return x ^ y; }
long Long_bit_MINUS_not(long x) { return ~x; }

int Int_copy(int *x) { return *x; }
int Long_copy(long *x) { return *x; }
float Float_copy(float *x) { return *x; }
double Double_copy(double *x) { return *x; }

double Double__PLUS_(double x, double y) { return x + y; }
double Double__MINUS_(double x, double y) { return x - y; }
double Double__MUL_(double x, double y) { return x * y; }
double Double__DIV_(double x, double y) { return x / y; }
bool Double__LT_(double x, double y) { return x < y; }
bool Double__GT_(double x, double y) { return x > y; }
bool Double__EQ_(double x, double y) { return x == y; }
double Double_neg(double x) { return -x; }

float Float__PLUS_(float x, float y) { return x + y; }
float Float__MINUS_(float x, float y) { return x - y; }
float Float__MUL_(float x, float y) { return x * y; }
float Float__DIV_(float x, float y) { return x / y; }
bool Float__LT_(float x, float y) { return x < y; }
bool Float__GT_(float x, float y) { return x > y; }
bool Float__EQ_(float x, float y) { return x == y; }
double Float_neg(float x) { return -x; }

bool and(bool x, bool y) { return x && y; }
bool or(bool x, bool y) { return x || y; }

void IO_println(string *s) { puts(s->data); }
void IO_print(string *s) { printf("%s", s->data); }

string IO_get_MINUS_line() {
    size_t buffer_size = 1024;  // including NUL-terminator
    char *buffer = CARP_MALLOC(buffer_size);
    getline(&buffer, &buffer_size, stdin);
    string s;
    s.data = buffer;
    s.size = buffer_size;
    return s;
}

int Int_from_MINUS_string(string *s) {
    return atoi(s->data);
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
    string s;
    string_constructor(&s, 64);
    snprintf(s.data, 64, "%d", x);
    s.size = strlen(s.data) + 1;
    return s;
}

long Long_from_MINUS_string(string *s) {
    return atol(s->data);
}

long Long_mod(long x, long divider) {
    return x % divider;
}

void Long_seed(long seed) {
    srand(seed);
}

long Long_random() {
    return rand();
}

long Long_random_MINUS_between(long lower, long upper) {
    long diff = upper - lower;
    return lower + (rand() % diff);
}

string Long_str(long x) {
    string s;
    string_constructor(&s, 64);
    snprintf(s.data, 64, "%ldl", x);
    s.size = strlen(s.data) + 1;
    return s;
}

int Long_to_MINUS_int(long a) {
  return (int) a;
}

long Long_from_MINUS_int(int a) {
  return (long) a;
}

void String_delete(string s) {
    string_destructor(&s);
}

string String_copy(string *s) {
    string ret;
    string_constructor(&ret, s->size);
    memcpy(ret.data, s->data, s->size);
    return ret;
}

bool String__EQ_(string *a, string *b) {
    return strcmp(a->data, b->data) == 0;
}

bool String__DIV__EQ_(string *a, string *b) {
    return strcmp(a->data, b->data) == 0;
}
string String_append(string a, string b) {
    size_t la = a.size - 1;
    size_t lb = b.size - 1;
    size_t total = la + lb + 1;
    string buffer;
    string_constructor(&buffer, total);
    // TODO(philix): Implement String_append more efficiently
    snprintf(buffer.data, total, "%s%s", a.data, b.data);
    buffer.size = strlen(buffer.data) + 1;
    string_destructor(&a);
    string_destructor(&b);
    return buffer;
}

int String_count(string *s) {
    assert(s->size >= 1);
    return s->size - 1;
}

// Replace with 'copy' later:
string String_duplicate(string *s) {
    return String_copy(s);
}

char* String_cstr(const string *s) {
    // TODO(philix): Carp doesn't have to repeat C++'s mistake of requiring
    // NUL-terminators in its string type
    return s->data;
}

string String_str(string *s) {
    size_t n = String_count(s) + 4;
    string buffer;
    string_constructor(&buffer, n);
    // TODO(philix): Implement String_str more efficiently
    snprintf(buffer.data, n, "@\"%s\"", s->data);
    buffer.size = strlen(buffer.data) + 1;
    return buffer;
}

Array String_chars(string *s) {
    string copy = String_copy(s);
    Array chars;
    chars.len = String_count(&copy);
    chars.data = copy.data;
    return chars;
}

string String_from_MINUS_chars(Array a) {
    string s;
    string_constructor(&s, a.len+1);
    memcpy(s.data, a.data, a.len);
    s.data[a.len] = '\0';
    return s;
}

bool Char__EQ_(char a, char b) {
  return a == b;
}

string Char_str(char c) {
    string s;
    string_constructor(&s, 3);
    s.data[0] = '\\';
    s.data[1] = c;
    s.data[2] = '\0';
    return s;
}

int Char_to_MINUS_int(char c) {
  return (int)c;
}

char Char_from_MINUS_int(int i) {
  return (char)i;
}

char Char_copy(char *c) {
  return *c;
}

// Double.toInt : Double -> Int
int Double_to_MINUS_int(double x) {
    return (int)x;
}

double Double_from_MINUS_int(int x) {
    return (double)x;
}

float Double_to_MINUS_float(double x) {
    return (float)x;
}

double Double_from_MINUS_float(float x) {
    return (double)x;
}

double Double_abs(double x) {
    return fabs(x);
}

double Double_acos(double x) {
    return acos(x);
}

double Double_asin(double x) {
    return asin(x);
}

double Double_atan(double x) {
    return atan(x);
}

double Double_atan2(double y, double x) {
    return atan2(y, x);
}

double Double_cos(double x) {
    return cos(x);
}

double Double_cosh(double x) {
    return cosh(x);
}

double Double_sin(double x) {
    return sin(x);
}

double Double_sinh(double x) {
    return sinh(x);
}

double Double_tanh(double x) {
    return tanh(x);
}

double Double_exp(double x) {
    return exp(x);
}

double Double_frexp(double x, int* exponent) {
    return frexp(x, exponent);
}

double Double_ldexp(double x, int exponent) {
    return ldexp(x, exponent);
}

double Double_log(double x) {
    return log(x);
}

double Double_log10(double x) {
    return log10(x);
}

double Double_modf(double x, double* integer) {
    return modf(x, integer);
}

double Double_pow(double x, double y) {
    return pow(x, y);
}

double Double_sqrt(double x) {
    return sqrt(x);
}

double Double_ceil(double x) {
    return ceil(x);
}

double Double_floor(double x) {
    return floor(x);
}

double Double_mod(double x, double y) {
    return fmod(x, y);
}

string Double_str(double x) {
    string s;
    string_constructor(&s, 32);
    snprintf(s.data, 32, "%g", x);
    s.size = strlen(s.data) + 1;
    return s;
}

int Float_to_MINUS_int(double x) {
    return (int)x;
}

float Float_random_MINUS_between(float lower, float upper) {
    float diff = upper - lower;
    float r = ((float)(rand() % INT_MAX)) / ((float)INT_MAX);
    return lower + diff * r;
}

float Float_abs(float x) {
    return fabs(x);
}

float Float_acos(float x) {
    return acos(x);
}

float Float_asin(float x) {
    return asin(x);
}

float Float_atan(float x) {
    return atan(x);
}

float Float_atan2(float y, float x) {
    return atan2(y, x);
}

float Float_cos(float x) {
    return cos(x);
}

float Float_cosh(float x) {
    return cosh(x);
}

float Float_sin(float x) {
    return sin(x);
}

float Float_sinh(float x) {
    return sinh(x);
}

float Float_tanh(float x) {
    return tanh(x);
}

float Float_exp(float x) {
    return exp(x);
}

float Float_frexp(float x, int* exponent) {
    return frexp(x, exponent);
}

float Float_ldexp(float x, int exponent) {
    return ldexp(x, exponent);
}

float Float_log(float x) {
    return log(x);
}

float Float_log10(float x) {
    return log10(x);
}

float Float_modf(float x, float* integer) {
    return modf(x, (double*) integer);
}

float Float_pow(float x, float y) {
    return pow(x, y);
}

float Float_sqrt(float x) {
    return sqrt(x);
}

float Float_ceil(float x) {
    return ceil(x);
}

float Float_floor(float x) {
    return floor(x);
}

float Float_mod(float x, float y) {
    return fmod(x, y);
}

string Float_str(float x) {
    string s;
    string_constructor(&s, 32);
    snprintf(s.data, 32, "%gf", x);
    s.size = strlen(s.data) + 1;
    return s;
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
        return string_from_cstr("true");
    } else {
        return string_from_cstr("false");
    }
}

void System_exit(int code) {
    exit(code);
}

void System_free(void *p) {
    CARP_FREE(p);
}

int System_time() {
    return time(0);
}

void System_srand(int x) {
    srand(x);
}

string IO_read_MINUS_file(string *filename) {
    string empty_s;
    string ret;
    char *buffer = 0;
    long length;
    FILE *f = fopen(filename->data, "rb");

    if(f) {
        fseek (f, 0, SEEK_END);
        length = ftell (f);
        fseek (f, 0, SEEK_SET);
        buffer = CARP_MALLOC (length + 1);
        if (buffer)	{
            fread (buffer, 1, length, f);
            buffer[length] = '\0';
        }
        fclose (f);
    } else {
        printf("Failed to open file: %s\n", filename->data);
        string_constructor(&empty_s, 1);
        return empty_s;
    }

    if (buffer) {
        string_constructor(&ret, length + 1);
        memcpy(ret.data, buffer, length + 1);
        return ret;
    } else {
        printf("Failed to open buffer from file: %s\n", filename->data);
        string_constructor(&empty_s, 1);
        return empty_s;
    }
}

#endif

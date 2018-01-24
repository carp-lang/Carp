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

#ifndef _WIN32
#include <unistd.h>
#endif

typedef char* string;

#ifdef LOG_MEMORY
long malloc_balance_counter = 0;
bool log_memory_balance = false;

void *logged_malloc(size_t size) {
    void *ptr = malloc(size);
    if(log_memory_balance) {
        printf("MALLOC: %p (%ld bytes)\n", ptr, size);
    }
    malloc_balance_counter++;
    return ptr;
}

void logged_free(void *ptr) {
    if(log_memory_balance) {
        printf("FREE: %p\n", ptr);
    }
    free(ptr);
    malloc_balance_counter--;
    /* if(malloc_balance_counter == 0) { */
    /*     printf("malloc is balanced! (this should be the last thing you see)\n"); */
    /* } */
    /* else if(malloc_balance_counter < 0) { */
    /*     printf("malloc is %ld, that is bad!\n", malloc_balance_counter); */
    /* } */
}

void Debug_log_MINUS_memory_MINUS_balance_BANG_(bool value) {
    log_memory_balance = value;
}

#define CARP_MALLOC(size) logged_malloc(size)
#define CARP_FREE(ptr) logged_free(ptr)

long Debug_memory_MINUS_balance() {
    return malloc_balance_counter;
}

void Debug_reset_MINUS_memory_MINUS_balance_BANG_() {
    malloc_balance_counter = 0;
}

#else

#define CARP_MALLOC(size) malloc(size)
#define CARP_FREE(ptr) free(ptr)

long Debug_memory_MINUS_balance() {
    printf("Error - calling 'memory-balance' without compiling with LOG_MEMORY enabled.\n");
    exit(1);
    return 0;
}

void Debug_reset_MINUS_memory_MINUS_balance_BANG_() {
    printf("Error - calling 'reset-memory-balance!' without compiling with LOG_MEMORY enabled.\n");
    exit(1);
}

void Debug_log_MINUS_memory_MINUS_balance_BANG_(bool value) {
    printf("Error - calling 'log-memory-balance!' without compiling with LOG_MEMORY enabled.\n");
    exit(1);
}

#endif

// Array
typedef struct {
    size_t len;
    void *data;
} Array;

bool not(bool b) {
    return !b;
}

bool and(bool x, bool y) { return x && y; }
bool or(bool x, bool y) { return x || y; }

void String_delete(string s) {
    CARP_FREE(s);
}

string String_copy(string *s) {
    size_t len = strlen(*s) + 1;
    string ptr = CARP_MALLOC(len);

    if (ptr == NULL) {
      return NULL;
    }

    return (string) memcpy(ptr, *s, len);
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
    return String_copy(s);
}

char* String_cstr(string *s) {
    return *s;
}

string String_str(string *s) {
    int n = strlen(*s) + 1;
    string buffer = CARP_MALLOC(n);
    snprintf(buffer, n, "%s", *s);
    return buffer;
}

string String_prn(string *s) {
    int n = strlen(*s) + 4;
    string buffer = CARP_MALLOC(n);
    snprintf(buffer, n, "@\"%s\"", *s);
    return buffer;
}

char String_char_MINUS_at(string* s, int i) {
  return (*s)[i];
}

string String_format(string *str, string *s) {
    int size = snprintf(NULL, 0, *str, *s)+1;
    string buffer = CARP_MALLOC(size);
    snprintf(buffer, size, *str, *s);
    return buffer;
}

Array String_chars(string *s) {
    Array chars;
    chars.len = strlen(*s);
    chars.data = String_copy(s);
    return chars;
}

string String_from_MINUS_chars(Array a) {
    string s = CARP_MALLOC(a.len+1);
    memmove(s, a.data, a.len);
    s[a.len] = '\0';
    return s;
}

string String_tail(string* s) {
  int len = strlen(*s);
  string news = CARP_MALLOC(len);
  memcpy(news, (*s)+1, len-1);
  news[len-1] = '\0';
  return news;
}

string String_empty() {
    string s = CARP_MALLOC(1);
    s[0] = '\0';
    return s;
}

#endif

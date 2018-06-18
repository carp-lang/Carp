#ifndef PRELUDE_H
#define PRELUDE_H

#include <assert.h>
#include <stdbool.h>
#include <stddef.h>

typedef char* String;
typedef char* Pattern;

// Array
typedef struct {
    size_t len;
    size_t capacity;
    void *data;
} Array;

// Lambdas
typedef struct {
    void *callback;
    void *env;
    void (*delete)(void*);
} Lambda;

typedef void* LambdaEnv

bool not(bool b) {
    return !b;
}

bool and(bool x, bool y) { return x && y; }
bool or(bool x, bool y) { return x || y; }

#endif

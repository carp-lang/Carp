#ifndef PRELUDE_H
#define PRELUDE_H

#include <assert.h>
#include "carp_stdbool.h"
#include <stddef.h>
#include <sys/wait.h>
#include <signal.h>

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
    void *delete;
    void *copy;
} Lambda;

typedef void* LambdaEnv;

bool not(bool b) {
    return !b;
}

bool and(bool x, bool y) { return x && y; }
bool or(bool x, bool y) { return x || y; }

#endif

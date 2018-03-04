#ifndef PRELUDE_H
#define PRELUDE_H

#include <assert.h>
#include <stdbool.h>
#include <stddef.h>

typedef char* string;
typedef char* pattern;

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

#endif

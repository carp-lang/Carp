#ifndef BANANA_H
#define BANANA_H

// This is an example external struct
typedef struct {
    double price;
    int size;
} Banana;

// This is an external function that can be registered
int magic() {
    return 123456789;
}

#endif

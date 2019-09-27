#pragma once
#include <stdlib.h>
#include "core.h"

#include "carp_stdbool.h"

#ifdef LOG_MEMORY

#include <stdio.h>

Long malloc_balance_counter = 0;
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

Long Debug_memory_MINUS_balance() {
    return malloc_balance_counter;
}

void Debug_reset_MINUS_memory_MINUS_balance_BANG_() {
    malloc_balance_counter = 0;
}

#else

#ifdef CHECK_ALLOCATIONS
void* CARP_MALLOC(size_t size) {
  void* res = malloc(size);
  if (!res) abort();
  return res;
}
#else
#define CARP_MALLOC(size) malloc(size)
#endif

#define CARP_FREE(ptr) free(ptr)

#include <stdio.h>

Long Debug_memory_MINUS_balance() {
    printf("Error - calling 'memory-balance' without compiling with LOG_MEMORY enabled (--log-memory).\n");
    exit(1);
    return 0;
}

void Debug_reset_MINUS_memory_MINUS_balance_BANG_() {
    printf("Error - calling 'reset-memory-balance!' without compiling with LOG_MEMORY enabled (--log-memory).\n");
    exit(1);
}

void Debug_log_MINUS_memory_MINUS_balance_BANG_(bool value) {
    printf("Error - calling 'log-memory-balance!' without compiling with LOG_MEMORY enabled (--log-memory).\n");
    exit(1);
}

#endif

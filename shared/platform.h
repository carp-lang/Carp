#pragma once

#if defined(__APPLE__)

#include <stdlib.h>
#include <assert.h>
#include <pthread.h>
#include <sys/time.h>
#include <dlfcn.h>

/* Init/shutdown */

typedef struct carp_library* carp_library_t;

static void carp_platform_init() {
}

static void carp_platform_shutdown() {
}

/* --- Threads --- */

typedef struct carp_thread* carp_thread_t;
typedef void(*carp_thread_routine)(void* arg);

struct carp_thread {
    pthread_t thread;
};

typedef struct thread_arg_wrapper {
    carp_thread_routine tr;
    void* arg;
} thread_arg_wrapper;

static void* thread_proc_wrapper(void* arg) {
    thread_arg_wrapper* argw = (thread_arg_wrapper*)arg;
    argw->tr(argw->arg);
    free(argw);
    return 0;
}

static carp_thread_t carp_thread_create(carp_thread_routine thread_routine, void* arg) {
    carp_thread_t thread = malloc(sizeof(struct carp_thread));
    assert(thread);
    thread_arg_wrapper* argw = malloc(sizeof(thread_arg_wrapper));
    assert(argw);
    argw->arg = arg;
    argw->tr = thread_routine;
    pthread_create(&thread->thread, NULL, thread_proc_wrapper, argw);
    return thread;
}

static void carp_thread_destroy(carp_thread_t thread) {
    free(thread);
}

/* --- Timing --- */

static int carp_millitime() {
    struct timeval te;
    gettimeofday(&te, NULL); // get current time
    long long milliseconds = te.tv_sec * 1000LL + te.tv_usec / 1000; // calculate milliseconds
    return milliseconds;
}

/* --- Libraries --- */

struct carp_library {
    void* handle;
};

static carp_library_t carp_load_library(const char* name) {
    void* handle = dlopen(name, RTLD_LAZY);
    if (handle == NULL) {
        return NULL;
    }
    return (carp_library_t)handle;
}

static int carp_unload_library(carp_library_t lib) {
    return dlclose((void*)lib);
}

static void* carp_find_symbol(carp_library_t lib, const char * name) {
    if (lib != NULL) {
        return dlsym((void*)lib, name);
    }
    return dlsym(RTLD_DEFAULT, name);
}

static char* carp_get_load_library_error() {
    return dlerror();
}

#endif


///* Init/shutdown */
//
//void carp_platform_init();
//void carp_platform_shutdown();
//
///* --- Threads --- */
//
//typedef struct carp_thread* carp_thread_t;
//typedef void(*carp_thread_routine)(void* arg);
//
//carp_thread_t carp_thread_create(carp_thread_routine thread_routine, void* arg);
//
///* It is safe to call carp_thread_destroy before the thread finishes, or even starts, if a fire and forget thread is desired. 
//   But carp_thread_destroy must be called or resources will leak. */
//void carp_thread_destroy(carp_thread_t thread);
//
///* --- Timing --- */
//
//int carp_millitime();
//
///* --- Libraries --- */
//
//typedef struct carp_library* carp_library_t;
//
//carp_library_t carp_load_library(const char* name);
//
//int carp_unload_library(carp_library_t lib);
//
///* Pass NULL to search all loaded libraries */
//void* carp_find_symbol(carp_library_t lib, const char* name);
//
///* On Windows, this function just gets the last error for the calling thread
//   so to get any error from a failed library loading, this needs to be called
//   immediately after the load call  */
//char* carp_get_load_library_error();

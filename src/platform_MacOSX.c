#include "platform.h"

#include <stdlib.h>
#include <assert.h>
#include <pthread.h>
#include <sys/time.h>
#include <dlfcn.h>

void carp_platform_init() {
}

void carp_platform_shutdown() {
}

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

carp_thread_t carp_thread_create(carp_thread_routine thread_routine, void* arg) {
	carp_thread_t thread = malloc(sizeof(struct carp_thread));
	assert(thread);
    thread_arg_wrapper* argw = malloc(sizeof(thread_arg_wrapper));
    assert(argw);
    argw->arg = arg;
    argw->tr = thread_routine;
    pthread_create(&thread->thread, NULL, thread_proc_wrapper, argw);
    return thread;
}

void carp_thread_destroy(carp_thread_t thread) {
	free(thread);
}

int carp_millitime() {
    struct timeval te;
    gettimeofday(&te, NULL); // get current time
    long long milliseconds = te.tv_sec * 1000LL + te.tv_usec / 1000; // calculate milliseconds
    return milliseconds;
}

struct carp_library {
    void* handle;
};

carp_library_t carp_load_library(const char* name) {
    void* handle = dlopen(name, RTLD_LAZY);
	if (handle == NULL) {
		return NULL;
	}
	return (carp_library_t)handle;
}

int carp_unload_library(carp_library_t lib) {
    return dlclose((void*)lib);
}

void * carp_find_symbol(carp_library_t lib, const char * name) {
	if (lib != NULL) {
        return dlsym((void*)lib, name);
	}
    return dlsym(RTLD_DEFAULT, name);
}

char* carp_get_load_library_error() {
	return dlerror();
}


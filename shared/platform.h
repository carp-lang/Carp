#pragma once

#include "types.h"


#if defined (__APPLE__) || defined(__linux__)

#include <stdlib.h>
#include <assert.h>
#include <pthread.h>
#include <sys/time.h>

// The symbols RTLD_DEFAULT and RTLD_NEXT are defined by <dlfcn.h> only
// when _GNU_SOURCE was defined before including it. (http://linux.die.net/man/3/dlopen)

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#ifndef __USE_GNU
#define __USE_GNU
#endif

#include <dlfcn.h>
#include <unistd.h>

/* Init/shutdown */

void carp_platform_init() {
}

void carp_platform_shutdown() {
}

/* --- Threads --- */

struct carp_thread {
    pthread_t thread;
};

typedef struct thread_arg_wrapper {
    carp_thread_routine tr;
    void* arg;
} thread_arg_wrapper;

void* thread_proc_wrapper(void* arg) {
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

/* --- Timing --- */

int carp_millitime() {
    struct timeval te;
    gettimeofday(&te, NULL); // get current time
    long long milliseconds = te.tv_sec * 1000LL + te.tv_usec / 1000; // calculate milliseconds
    return milliseconds;
}

/* --- Libraries --- */

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

void* carp_find_symbol(carp_library_t lib, const char * name) {
    if (lib != NULL) {
        return dlsym((void*)lib, name);
    }
    return dlsym(RTLD_DEFAULT, name);
}

char* carp_get_load_library_error() {
    return dlerror();
}

CARP_PLATFORM carp_get_platform() {
#ifdef __APPLE__
	return CARP_PLATFORM_OSX;
#elif defined __linux__
	return CARP_PLATFORM_LINUX;
#elif defined WIN32
	return CARP_PLATFORM_WINDOWS;
#endif
	return CARP_PLATFORM_UNKNOWN;
}

#endif // __APPLE__

#ifdef WIN32

#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#include <assert.h>
#include <stdlib.h>

/* Support code for library symbol search */

typedef struct module_list {
	HMODULE module;
	struct module_list* next;
}*module_list_t;

LARGE_INTEGER carp_perf_freq;
HMODULE carp_main_module = INVALID_HANDLE_VALUE;
HMODULE carp_msvcrt_module = INVALID_HANDLE_VALUE;
module_list_t carp_loaded_modules = NULL;

module_list_t new_module_list_node() {
	module_list_t lst = malloc(sizeof(struct module_list));
	lst->module = INVALID_HANDLE_VALUE;
	lst->next = NULL;
	return lst;
}

void add_module_to_list(module_list_t lst, HMODULE module) {
	while (lst->module != INVALID_HANDLE_VALUE) {
		if (lst->next == NULL) {
			lst->next = new_module_list_node();
		}
		lst = lst->next;
	}
	lst->module = module;
}

void remove_module_from_list(module_list_t lst, HMODULE module) {
	while (lst->module != module) {
		if (lst->next == NULL) {
			return; // not found
		}
		lst = lst->next;
	}
	lst->module = INVALID_HANDLE_VALUE;
}

void free_all_modules_and_destroy_module_list(module_list_t lst) {
	while (lst) {
		if (lst->module != INVALID_HANDLE_VALUE) {
			FreeLibrary(lst->module);
		}
		module_list_t tmp = lst;
		lst = lst->next;
		free(tmp);
	}
}

/* Init/shutdown */

void carp_platform_init() {
	QueryPerformanceFrequency(&carp_perf_freq);
	carp_main_module = GetModuleHandle(NULL);
	carp_msvcrt_module = LoadLibrary("msvcrt.dll");
	carp_loaded_modules = new_module_list_node();
	add_module_to_list(carp_loaded_modules, carp_msvcrt_module);
}

void carp_platform_shutdown() {
	free_all_modules_and_destroy_module_list(carp_loaded_modules);
	carp_main_module = INVALID_HANDLE_VALUE;
	carp_msvcrt_module = INVALID_HANDLE_VALUE;
	carp_loaded_modules = NULL;
}

/* --- Threads --- */

struct carp_thread {
	HANDLE handle;
};

typedef struct thread_arg_wrapper {
	carp_thread_routine tr;
	void* arg;
} thread_arg_wrapper;

DWORD WINAPI thread_proc_wrapper(LPVOID p) {
	thread_arg_wrapper* argw = (thread_arg_wrapper*)p;
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
	thread->handle = CreateThread(NULL, 0, thread_proc_wrapper, argw, 0, 0);
	return thread;
}

void carp_thread_destroy(carp_thread_t thread) {
	CloseHandle(thread->handle);
	free(thread);
}

/* --- Timing --- */

int carp_millitime() {
	LARGE_INTEGER pt;
	QueryPerformanceCounter(&pt);
	return (int)(((double)pt.QuadPart) / ((double)carp_perf_freq.QuadPart) * 1000);
}

/* --- Libraries --- */

struct carp_library {
	HMODULE module;
};

carp_library_t carp_load_library(const char* name) {
	HMODULE module = LoadLibrary(name);
	if (module == NULL) {
		return NULL;
	}
	SetLastError(0);
	add_module_to_list(carp_loaded_modules, module);
	carp_library_t lib = malloc(sizeof(struct carp_library));
	lib->module = module;
	return lib;
}

int carp_unload_library(carp_library_t lib) {
	remove_module_from_list(carp_loaded_modules, lib->module);
	BOOL result = FreeLibrary(lib->module);
	free(lib);
	return !result;
}

void* carp_find_symbol(carp_library_t lib, const char * name) {
	if (lib != NULL) {
		assert(lib->module != INVALID_HANDLE_VALUE);
		return GetProcAddress(lib->module, name);
	}
	void* addr = GetProcAddress(carp_main_module, name);
	if (addr != NULL) {
		return addr;
	}
	module_list_t lst = carp_loaded_modules;
	while (lst) {
		if (lst->module != INVALID_HANDLE_VALUE) {
			void* addr = GetProcAddress(lst->module, name);
			if (addr != NULL) {
				return addr;
			}
		}
		lst = lst->next;
	}
	return NULL;
}

char error_buf[2048];

char* carp_get_load_library_error() {
	DWORD error = GetLastError();
	if (error == 0) {
		return NULL;
	}
	assert(sizeof(TCHAR) == 1); // If wide chars are used, we have to convert to utf8
	FormatMessage(
		FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
		NULL,
		error,
		MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
		(LPSTR)&error_buf,
		sizeof(error_buf) - 1,
		NULL);
	return error_buf;
}

void carp_sleep(int millis) {
	Sleep(millis);
}

CARP_PLATFORM carp_get_platform() {
	return CARP_PLATFORM_WINDOWS;
}

#endif

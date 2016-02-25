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

#if defined(WIN32)

#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#include <assert.h>
#include <stdlib.h>

/* Support code for library symbol search */

typedef struct module_list {
	HMODULE module;
	struct module_list* next;
}*module_list_t;

static LARGE_INTEGER perf_freq;
static HMODULE main_module = INVALID_HANDLE_VALUE;
static module_list_t loaded_modules = NULL;

static module_list_t new_module_list_node() {
	module_list_t lst = malloc(sizeof(struct module_list));
	lst->module = INVALID_HANDLE_VALUE;
	lst->next = NULL;
	return lst;
}

static void add_module_to_list(module_list_t lst, HMODULE module) {
	while (lst->module != INVALID_HANDLE_VALUE) {
		if (lst->next == NULL) {
			lst->next = new_module_list_node();
		}
		lst = lst->next;
	}
	lst->module = module;
}

static void remove_module_from_list(module_list_t lst, HMODULE module) {
	while (lst->module != module) {
		if (lst->next == NULL) {
			return; // not found
		}
		lst = lst->next;
	}
	lst->module = INVALID_HANDLE_VALUE;
}

static void free_all_modules_and_destroy_module_list(module_list_t lst) {
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

static void carp_platform_init() {
	QueryPerformanceFrequency(&perf_freq);
	main_module = GetModuleHandle(NULL);
	loaded_modules = new_module_list_node();
}

static void carp_platform_shutdown() {
	main_module = INVALID_HANDLE_VALUE;
	free_all_modules_and_destroy_module_list(loaded_modules);
	loaded_modules = NULL;
}

/* --- Threads --- */

typedef struct carp_thread* carp_thread_t;
typedef void(*carp_thread_routine)(void* arg);

struct carp_thread {
	HANDLE handle;
};

typedef struct thread_arg_wrapper {
	carp_thread_routine tr;
	void* arg;
} thread_arg_wrapper;

static DWORD WINAPI thread_proc_wrapper(LPVOID p) {
	thread_arg_wrapper* argw = (thread_arg_wrapper*)p;
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
	thread->handle = CreateThread(NULL, 0, thread_proc_wrapper, argw, 0, 0);
	return thread;
}

static void carp_thread_destroy(carp_thread_t thread) {
	CloseHandle(thread->handle);
	free(thread);
}

/* --- Timing --- */

static int carp_millitime() {
	LARGE_INTEGER pt;
	QueryPerformanceCounter(&pt);
	return (int)(((double)pt.QuadPart) / ((double)perf_freq.QuadPart) * 1000);
}

/* --- Libraries --- */

typedef struct carp_library* carp_library_t;

struct carp_library {
	HMODULE module;
};

static carp_library_t carp_load_library(const char* name) {
	HMODULE module = LoadLibrary(name);
	if (module == NULL) {
		return NULL;
	}
	SetLastError(0);
	add_module_to_list(loaded_modules, module);
	carp_library_t lib = malloc(sizeof(struct carp_library));
	lib->module = module;
	return lib;
}

static int carp_unload_library(carp_library_t lib) {
	remove_module_from_list(loaded_modules, lib->module);
	BOOL result = FreeLibrary(lib->module);
	free(lib);
	return (int)result;
}

static void* carp_find_symbol(carp_library_t lib, const char * name) {
	if (lib != NULL) {
		assert(lib->module != INVALID_HANDLE_VALUE);
		return GetProcAddress(lib->module, name);
	}
	void* addr = GetProcAddress(main_module, name);
	if (addr != NULL) {
		return addr;
	}
	module_list_t lst = loaded_modules;
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

static char error_buf[2048];

static char* carp_get_load_library_error() {
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

#endif

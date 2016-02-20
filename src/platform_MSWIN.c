#pragma once

#include "platform.h"

#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#include <assert.h>
#include <stdlib.h>

typedef struct module_list {
	HMODULE module;
	struct module_list* next;
}* module_list_t;

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

static void free_module_and_remove_from_list(module_list_t lst, HMODULE module) {
	while (lst->module != module) {
		if (lst->next == NULL) {
			return; // not found
		}
		lst = lst->next;
	}
	FreeLibrary(lst->module);
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

void carp_platform_init() {
	QueryPerformanceFrequency(&perf_freq);
	main_module = GetModuleHandle(NULL);
	loaded_modules = new_module_list_node();
}

void carp_platform_shutdown() {
	CloseHandle(main_module);
	main_module = INVALID_HANDLE_VALUE;
	free_all_modules_and_destroy_module_list(loaded_modules);
	loaded_modules = NULL;
}

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

//uint64_t carp_nanotime() {
int carp_millitime() {
	LARGE_INTEGER pt;
	QueryPerformanceCounter(&pt);
	return (int)(((double)pt.QuadPart) / ((double)perf_freq.QuadPart) * 1000);
}

struct carp_library {
	HMODULE module;
};

carp_library_t carp_load_library(const char* name) {
	HMODULE module = LoadLibrary(name);
	if (module == NULL) {
		return NULL;
	}
	add_module_to_list(loaded_modules, module);
	carp_library_t lib = malloc(sizeof(struct carp_library));
	lib->module = module;
	return lib;
}

void carp_unload_library(carp_library_t lib) {
	free_module_and_remove_from_list(loaded_modules, lib->module);
	free(lib);
}

void* carp_find_function(const char* name) {
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

const char* carp_get_load_library_error() {

}


#pragma once

#ifdef bool
#undef bool
#endif

typedef int bool;
#define true 1
#define false 0

typedef struct carp_thread* carp_thread_t;
typedef void(*carp_thread_routine)(void* arg);

typedef struct carp_library* carp_library_t;

/* Init/shutdown */

void carp_platform_init();

void carp_platform_shutdown();

/* --- Threads --- */

carp_thread_t carp_thread_create(carp_thread_routine thread_routine, void* arg);

void carp_thread_destroy(carp_thread_t thread);

/* --- Timing --- */

int carp_millitime();

/* --- Libraries --- */

carp_library_t carp_load_library(const char* name);

int carp_unload_library(carp_library_t lib);

void* carp_find_symbol(carp_library_t lib, const char * name);

char* carp_get_load_library_error();

/* -- misc -- */

void carp_sleep(int millis);

typedef enum CARP_PLATFORM {
	CARP_PLATFORM_OSX = 0,
	CARP_PLATFORM_WINDOWS = 1,
	CARP_PLATFORM_LINUX = 2,
	CARP_PLATFORM_UNKNOWN = 100
} CARP_PLATFORM;

CARP_PLATFORM carp_get_platform();

typedef struct {
  int count;
  void *data;
} Array;


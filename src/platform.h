#pragma once

#ifndef WIN32
# include <inttypes.h>
#else
typedef signed char int8_t;
typedef unsigned char uint8_t;
typedef signed short int16_t;
typedef unsigned short uint16_t;
typedef signed __int32 int32_t;
typedef unsigned __int32 uint32_t;
typedef signed __int64 int64_t;
typedef unsigned __int64 uint64_t;
#endif

/* Init/shutdown */

void carp_platform_init();
void carp_platform_shutdown();

/* --- Threads --- */

typedef struct carp_thread* carp_thread_t;
typedef void(*carp_thread_routine)(void* arg);

carp_thread_t carp_thread_create(carp_thread_routine thread_routine, void* arg);

/* It is safe to call carp_thread_destroy before the thread finishes, or even starts, if a fire and forget thread is desired. 
   But carp_thread_destroy must be called or resources will leak. */
void carp_thread_destroy(carp_thread_t thread);

/* --- Timing --- */

int carp_millitime();

/* --- Libraries --- */

typedef struct carp_library* carp_library_t;

carp_library_t carp_load_library(const char* name);

int carp_unload_library(carp_library_t lib);

/* Pass NULL to search all loaded libraries */
void* carp_find_symbol(carp_library_t lib, const char* name);

/* On Windows, this function just gets the last error for the calling thread
   so to get any error from a failed library loading, this needs to be called
   immediately after the load call  */
char* carp_get_load_library_error();

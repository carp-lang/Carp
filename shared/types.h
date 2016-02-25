#pragma once

typedef struct carp_thread* carp_thread_t;
typedef void(*carp_thread_routine)(void* arg);

typedef struct carp_library* carp_library_t;

void carp_sleep(int millis);

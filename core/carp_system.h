#include <time.h>

#ifndef _WIN32
#include <unistd.h>
#endif

#include <carp_memory.h>

void System_free(void *p) {
    CARP_FREE(p);
}

int System_time() {
    return time(0);
}

#ifdef _WIN32
void System_sleep_MINUS_seconds(int t) {
    // TODO!
}

void System_sleep_MINUS_micros(int t) {
    // TODO!
}

double System_nanotime() {
    return 0;
}
#else
void System_sleep_MINUS_seconds(int t) {
    sleep(t);
}

void System_sleep_MINUS_micros(int t) {
    usleep(t);
}

double System_nanotime() {
  struct timespec tv;
  clock_gettime(CLOCK_REALTIME, &tv);
  return 1000000000 * tv.tv_sec + tv.tv_nsec;
}
#endif

void System_system(String *command) {
    system(*command);
}

Array System_args;

String* System_get_MINUS_arg(int idx) {
    assert(idx < System_args.len);
    return &(((String*)System_args.data)[idx]);
}

int System_get_MINUS_args_MINUS_len() {
    return System_args.len;
}

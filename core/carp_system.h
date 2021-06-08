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

int System_system(const String *command) {
    return system(*command);
}

Array System_args;

#include <errno.h>
#include <string.h>
String System_error_text() {
    return String_from_MINUS_cstr( strerror(errno) );
}


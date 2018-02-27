#include <time.h>
#include <unistd.h>

#include <carp_memory.h>

void System_exit(int code) {
    exit(code);
}

void System_free(void *p) {
    CARP_FREE(p);
}

int System_time() {
    return time(0);
}

void System_srand(int x) {
    srand(x);
}

void System_sleep_MINUS_seconds(int t) {
    sleep(t);
}

void System_sleep_MINUS_micros(int t) {
    usleep(t);
}


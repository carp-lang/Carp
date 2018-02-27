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

void System_seed_MINUS_random(int x) {
    srand(x);
}

void System_sleep_MINUS_seconds(int t) {
    sleep(t);
}

void System_sleep_MINUS_micros(int t) {
    usleep(t);
}

int Int_random() {
    return rand();
}

int Int_random_MINUS_between(int lower, int upper) {
    int diff = upper - lower;
    return lower + (rand() % diff);
}

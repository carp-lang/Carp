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

void System_seed_MINUS_random(int x) {
    srand(x);
}

#ifndef _WIN32
void System_sleep_MINUS_seconds(int t) {
    sleep(t);
}

void System_sleep_MINUS_micros(int t) {
    usleep(t);
}
#endif

int Int_random() {
    return rand();
}

int Int_random_MINUS_between(int lower, int upper) {
    int diff = upper - lower;
    return lower + (rand() % diff);
}

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

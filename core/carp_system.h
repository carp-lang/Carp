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

uint64_t System_nanotime() {
    return 0;
}
#else
void System_sleep_MINUS_seconds(int t) {
    sleep(t);
}

void System_sleep_MINUS_micros(int t) {
    usleep(t);
}

uint64_t System_nanotime() {
    struct timespec tv;
    clock_gettime(CLOCK_REALTIME, &tv);
    return (uint64_t)1000000000 * (uint64_t)tv.tv_sec + (uint64_t)tv.tv_nsec;
}
#endif

int System_system(const String *command) {
    return system(*command);
}

Array System_args;

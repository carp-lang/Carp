#include <sys/time.h>

// TODO: use timespecs for better accuracy!
double get_MINUS_time_MINUS_elapsed() {
  struct timeval tv;
  gettimeofday(&tv, NULL);
  return 1000000 * tv.tv_sec + tv.tv_usec;
}

#include <time.h>

#ifdef __MACH__
#include <mach/clock.h>
#include <mach/mach.h>
#endif

double get_MINUS_time_MINUS_elapsed() {
  struct timespec tv;

#ifdef __MACH__
  // OS X does not have clock_gettime, use clock_get_time
  // https://gist.github.com/jbenet/1087739
  clock_serv_t cclock;
  mach_timespec_t mts;
  host_get_clock_service(mach_host_self(), CALENDAR_CLOCK, &cclock);
  clock_get_time(cclock, &mts);
  mach_port_deallocate(mach_task_self(), cclock);
  tv.tv_sec = mts.tv_sec;
  tv.tv_nsec = mts.tv_nsec;
#else
  clock_gettime(CLOCK_REALTIME, &tv);
#endif

  return 1000000000 * tv.tv_sec + tv.tv_nsec;
}

#include "time.h"
#include "_syscall.h"

#if defined(__NR_time)
time_t time(time_t *timer) {
  time_t ret;
  SYSCALL_RET(__NR_time, ret, "r"(timer));
  return ret;
}

#elif defined(__NR_clock_gettime)
#include "stddef.h"  // NULL

time_t time(time_t *timer) {
  struct timespec ts;
  clock_gettime(CLOCK_REALTIME, &ts);
  if (timer != NULL)
    *timer = ts.tv_sec;
  return ts.tv_sec;
}
#endif

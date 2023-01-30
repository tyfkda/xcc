#include "time.h"

#if !defined(__WASM)
#include "_syscall.h"
#endif

#if defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wunused-parameter"
#endif

#if defined(__NR_time)
time_t time(time_t *timer) {
  time_t ret;
  SYSCALL_RET(__NR_time, ret);
  return ret;
}

#elif defined(__NR_clock_gettime) || defined(__WASM)
#include "stddef.h"  // NULL

time_t time(time_t *timer) {
  struct timespec ts;
  clock_gettime(CLOCK_REALTIME, &ts);
  if (timer != NULL)
    *timer = ts.tv_sec;
  return ts.tv_sec;
}
#endif

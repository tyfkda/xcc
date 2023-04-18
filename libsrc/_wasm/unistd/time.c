#include "time.h"
#include "stddef.h"  // NULL

time_t time(time_t *timer) {
  struct timespec ts;
  clock_gettime(CLOCK_REALTIME, &ts);
  if (timer != NULL)
    *timer = ts.tv_sec;
  return ts.tv_sec;
}

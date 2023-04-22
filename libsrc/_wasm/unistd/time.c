#include "time.h"
#include "stddef.h"  // NULL
#include "stdint.h"

time_t time(time_t *timer) {
  uint64_t t;
  clock_time_get(CLOCK_REALTIME, 100000000ULL, &t);
  uint64_t sec = t / 1000000000ULL;
  if (timer != NULL)
    *timer = sec;
  return sec;
}

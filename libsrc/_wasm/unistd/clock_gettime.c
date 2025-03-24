#include "time.h"
#include "errno.h"
#include "stddef.h"  // NULL
#include "stdint.h"
#include "../wasi.h"

int clock_gettime(clockid_t clk_id, struct timespec *tp) {
  if (tp == NULL) {
    errno = EINVAL;
    return -1;
  }

  uint64_t time;
  clock_time_get(clk_id, 1, &time);
  tp->tv_sec = time / 1000000000ULL;
  tp->tv_nsec = time % 1000000000ULL;
  return 0;
}

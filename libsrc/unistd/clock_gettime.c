#include "time.h"
#include "_syscall.h"

#if defined(__NR_clock_gettime)
int clock_gettime(clockid_t clk_id, struct timespec *tp) {
  int ret;
  SYSCALL_RET(__NR_clock_gettime, ret);
  SET_ERRNO(ret);
  return ret;
}
#endif

#if !defined(__WASM)
#include "time.h"
#include "_syscall.h"

#if defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wunused-parameter"
#endif

#if defined(__NR_clock_gettime)
int clock_gettime(clockid_t clk_id, struct timespec *tp) {
  int ret;
  SYSCALL_RET(__NR_clock_gettime, ret);
  return ret;
}
#endif

#endif

#include "math.h"
#include "stdint.h"  // int64_t
#include "_ieee.h"

#ifndef __NO_FLONUM
#undef isfinite
int isfinite(double x) {
  union { double d; int64_t q; } u;
  u.d = x;
  return (u.q & EXPO_MASK) != EXPO_MASK;
}
#endif

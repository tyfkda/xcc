#include "math.h"
#include "stdint.h"  // int64_t
#include "_ieee.h"

#ifndef __NO_FLONUM
#undef finite
extern inline int isfinite(double x);

int finite(double x) {
  union { double d; int64_t q; } u;
  u.d = x;
  return (u.q & EXPO_MASK) != EXPO_MASK;
}

#if defined(__APPLE__)
int __isfinited(double x)  { return finite(x); }
#endif
#endif

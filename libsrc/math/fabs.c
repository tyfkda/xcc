#include "math.h"
#include "stdint.h"  // int64_t
#include "_ieee.h"

#ifndef __NO_FLONUM
double fabs(double x) {
  union { double d; int64_t q; } u;
  u.d = x;
  u.q &= ~SIGN_MASK;
  return u.d;
}
#endif

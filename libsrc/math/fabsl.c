#include "math.h"
#include "assert.h"
#include "stdint.h"  // int64_t
#include "_ieee.h"

#ifndef __NO_FLONUM
long double fabsl(long double x) {
  union { long double ld; int64_t q; } u;
  u.ld = x;
  u.q &= ~SIGN_MASK;
  return u.ld;
}
#endif

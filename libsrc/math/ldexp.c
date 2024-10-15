#include "math.h"
#include "stdint.h"  // int64_t
#include "_ieee.h"

#ifndef __NO_FLONUM
double ldexp(double x, int exp) {
  if (!isfinite(x) || x == 0) {
    return x;
  }
  int64_t q = *(int64_t*)&x;
  exp += GET_BIASED_EXPO(q);
  if (exp >= ((1 << EXPO_BIT) - 1))
    return x < 0 ? -HUGE_VAL : HUGE_VAL;
  else if (exp < 0)
    return 0;
  int64_t r = (q & (FRAC_MASK | SIGN_MASK)) | ((int64_t)exp << EXPO_POS);
  return *(double*)&r;
}
#endif

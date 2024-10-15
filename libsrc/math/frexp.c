#include "math.h"
#include "stdint.h"  // int64_t
#include "_ieee.h"

#ifndef __NO_FLONUM
double frexp(double x, int *p) {
  if (!isfinite(x) || x == 0) {
    *p = 0;
    return x;
  }
  int64_t q = *(int64_t*)&x;
  *p = GET_EXPO(q);
  int64_t r = (q & (FRAC_MASK | SIGN_MASK)) | ((int64_t)EXPO_BIAS << EXPO_POS);
  return *(double*)&r;
}
#endif

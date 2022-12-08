#include "math.h"
#include "stdint.h"  // int64_t
#include "_ieee.h"

#ifndef __NO_FLONUM
int isfinite(double x) {
  int64_t q = *(int64_t*)&x;
  return (q & EXPO_MASK) != EXPO_MASK;
}
#endif

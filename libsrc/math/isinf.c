#include "math.h"
#include "stdint.h"  // int64_t
#include "_ieee.h"

#ifndef __NO_FLONUM
#undef isinf
int isinf(double x) {
  int64_t q = *(int64_t*)&x;
  return (q & NAN_MASK) == EXPO_MASK;
}
#endif

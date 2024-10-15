#include "math.h"
#include "stdint.h"  // int64_t
#include "_ieee.h"

#ifndef __NO_FLONUM
extern inline int isnan(double x);

int __isnan(double x) {
  union { double d; int64_t q; } u;
  u.d = x;
  return (u.q & NAN_MASK) == NAN_MASK;
}
#endif

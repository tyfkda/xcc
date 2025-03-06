#include "math.h"
#include "stdint.h"  // int64_t
#include "_ieee.h"

#ifndef __NO_FLONUM
float fabsf(float x) {
  union { float f; int32_t l; } u;
  u.f = x;
  u.l &= (int32_t)~(1 << 31);
  return u.f;
}
#endif

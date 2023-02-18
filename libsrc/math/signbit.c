#include "math.h"
#include "stdint.h"  // int64_t
#include "_ieee.h"

#ifndef __NO_FLONUM
#undef signbit
int signbit(double x) {
  if (x != 0)
    return x < 0;
  return ((*(uint64_t*)&x) >> (EXPO_POS + EXPO_BIT));
}
#endif

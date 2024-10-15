#include "math.h"
#include "stdint.h"  // int64_t
#include "_ieee.h"

#ifndef __NO_FLONUM
extern inline int signbit(double x);

#if defined(__APPLE__) || defined(__riscv)
#define __signbit  __signbitd
#endif

int __signbit(double x) {
  return ((*(uint64_t*)&x) >> (EXPO_POS + EXPO_BIT));
}
#endif

#include "math.h"
#include "stdint.h"  // int64_t
#include "_ieee.h"

#ifndef __NO_FLONUM
double ceil(double x) {
#if defined(__wasm)
#define S(x)   S_(x)
#define S_(x)  #x
#define OP_LOCAL_GET      32   // 0x20
#define OP_F64_CEIL       155  // 0x9b
  (void)x;
  __asm volatile(
      S(OP_LOCAL_GET) ",0,"  // local.get 0
      S(OP_F64_CEIL));       // f64.ceil
#else
  int64_t q = *(int64_t*)&x;
  int e = GET_BIASED_EXPO(q);
  if (e <= EXPO_BIAS + FRAC_BIT && e != 0) {
    if (e <= EXPO_BIAS)
      return q > 0 ? 1.0 : 0.0;

    int64_t one = (int64_t)1 << ((EXPO_BIAS + FRAC_BIT + 1) - e);
    if (q < 0) {
      int64_t r = q & -one;
      return *(double*)&r;
    } else {
      int64_t frac = q & (one - 1);
      if (frac != 0) {
        int64_t r = (q & -one) + one;
        return *(double*)&r;
      }
    }
  }
  return x;
#endif
}
#endif

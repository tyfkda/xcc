#include "math.h"

#ifndef __NO_FLONUM
double sqrt(double x) {
#if defined(__wasm)
#define S(x)   S_(x)
#define S_(x)  #x
#define OP_LOCAL_GET      32   // 0x20
#define OP_F64_SQRT       159  // 0x9f
  __asm volatile(
      S(OP_LOCAL_GET) ",0,"  // local.get 0
      S(OP_F64_SQRT));       // f64.sqrt
#elif defined(__x86_64__) && !defined(__GNUC__)
  __asm volatile("sqrtsd %xmm0, %xmm0");
#elif defined(__aarch64__) && !defined(__GNUC__)
  __asm volatile("fsqrt d0, d0");
#elif defined(__riscv) && !defined(__GNUC__)
  __asm volatile("fsqrt.d fa0, fa0");
#else
  if (x < 0)
    return NAN;
  if (!isfinite(x))  // NAN or HUGE_VAL
    return x;

  double l = 0, r = x >= 1 ? x : 1.0;
  for (int i = 0; i < 32; ++i) {
    double m = (l + r) * 0.5;
    double mm = m * m;
    if (mm < x)
      l = m;
    else
      r = m;
  }
  return (l + r) * 0.5;
#endif
}
#endif

#include "math.h"
#include "assert.h"

#ifndef __NO_FLONUM
double sqrt(double x) {
#if defined(__x86_64__)
  __asm("sqrtsd %xmm0, %xmm0");
#elif defined(__aarch64__)
  __asm("fsqrt d0, d0");
#else
  if (x <= 0)
    return x;

  double l = 0, r = x;
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

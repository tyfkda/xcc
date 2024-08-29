#include "math.h"
#include "float.h"

#ifndef __NO_FLONUM
extern int __isnan(double);
extern int __isinf(double);

#if defined(__riscv)
#define __fpclassify  __fpclassifyd
#endif

int __fpclassify(double x) {
  if (!finite(x))
    return __isnan(x) ? FP_NAN : FP_INFINITE;
  if (x == 0.0)
    return FP_ZERO;
  if (x < DBL_MIN && x > -DBL_MIN)
    return FP_SUBNORMAL;
  return FP_NORMAL;
}
#endif

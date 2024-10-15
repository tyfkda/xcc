#include "math.h"

#ifndef __NO_FLONUM
double log2(double x) {
  if (x <= 0)
    return x < 0 ? NAN : -HUGE_VAL;
  if (!isfinite(x))
    return x;

  return log(x) * (1.0 / M_LN2);
}
#endif

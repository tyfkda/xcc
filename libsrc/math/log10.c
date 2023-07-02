#include "math.h"

#ifndef __NO_FLONUM
#define LOG_10  2.302585092994046

double log10(double x) {
  if (x <= 0)
    return x < 0 ? NAN : -HUGE_VAL;
  if (!isfinite(x))
    return x;

  return log(x) * (1.0 / LOG_10);
}
#endif

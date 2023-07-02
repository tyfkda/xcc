#include "math.h"

#ifndef __NO_FLONUM
double atanh(double x) {
  if (x > 1 || x < -1)
    return NAN;
  return 0.5 * log((1 + x) / (1 - x));
}
#endif

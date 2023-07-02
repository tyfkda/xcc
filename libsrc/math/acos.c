#include "math.h"

#ifndef __NO_FLONUM
double acos(double x) {
  if (x < -1 || x > 1 || !isfinite(x))
    return NAN;
  return M_PI_2 - asin(x);
}
#endif

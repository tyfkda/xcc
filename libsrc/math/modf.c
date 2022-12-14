#include "math.h"

#ifndef __NO_FLONUM
double modf(double x, double *pint) {
  if (!isfinite(x)) {
    *pint = x;
    return isnan(x) ? x : 0.0;
  }

  double i = x >= 0 ? floor(x) : ceil(x);
  *pint = i;
  return x - i;
}
#endif

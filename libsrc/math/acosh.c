#include "math.h"

#ifndef __NO_FLONUM
double acosh(double x) {
  if (x < 1)
    return NAN;
  return log(x + sqrt(x * x - 1));
}
#endif

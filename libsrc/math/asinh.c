#include "math.h"

#ifndef __NO_FLONUM
double asinh(double x) {
  return log(x + sqrt(x * x + 1));
}
#endif

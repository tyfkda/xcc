#include "math.h"

#ifndef __NO_FLONUM
double fabs(double x) {
  return x >= 0 ? x : -x;
}
#endif

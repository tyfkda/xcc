#include "math.h"

#ifndef __NO_FLONUM
double sinh(double x) {
  // (e^x - e^-x) / 2
  return (exp(x) - exp(-x)) * 0.5;
}
#endif

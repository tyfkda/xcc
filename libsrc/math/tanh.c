#include "math.h"

#ifndef __NO_FLONUM
double tanh(double x) {
  return sinh(x) / cosh(x);
}
#endif

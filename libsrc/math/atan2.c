#include "math.h"
#include "stdbool.h"

#ifndef __NO_FLONUM
double atan2(double y, double x) {
  if (fabs(x) >= fabs(y)) {
    if (x == 0)
      return signbit(x) == 0 ? y : copysign(M_PI, y);
    double t = atan(y / x);
    if (x < 0)
      t = (t <= 0 ? M_PI : -M_PI) + t;
    return t;
  } else {
    double t = atan(x / y);
    t = (y >= 0 ? M_PI / 2 : -M_PI / 2) - t;
    return t;
  }
}
#endif

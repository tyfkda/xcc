#include "math.h"
#include "stdbool.h"

#ifndef __NO_FLONUM
double atan2(double y, double x) {
  double ax = fabs(x), ay = fabs(y);
  if (ax >= ay) {
    if (ax == 0)
      return signbit(ax) == 0 ? 0.0 : M_PI;
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

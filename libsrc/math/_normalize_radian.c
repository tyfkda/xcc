#include "math.h"

#ifndef __NO_FLONUM
double _normalize_radian(double x) {
  double y = fmod(x, 2 * M_PI);
  if (y > M_PI)
    return y - 2 * M_PI;
  if (y < -M_PI)
    return y + 2 * M_PI;
  return y;
}
#endif

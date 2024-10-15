#include "math.h"

#ifndef __NO_FLONUM
double log(double x) {
  if (x <= 0)
    return x < 0 ? NAN : -HUGE_VAL;
  if (!isfinite(x))
    return x;

  int n;
  x = frexp(x, &n);
  // 0.5 <= x < 1.0

  double y = (x - 1) / (x + 1);
  double yy = y * y;
  double total = 0;
  for (int i = 1; i <= 15; ++i) {
    total += y / (i * 2 - 1);
    y *= yy;
  }
  return 2 * total + n * M_LN2;
}
#endif

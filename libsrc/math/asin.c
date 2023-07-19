#include "math.h"

#ifndef __NO_FLONUM
double asin(double x) {
  if (x < -1 || x > 1 || !isfinite(x))
    return NAN;
  if (x < 0)
    return -asin(-x);

  if (x <= 0.5) {
    double v = x;
    double k = x;
    double xx = x * x;
    for (int i = 0; i < 12; ++i) {
      int n = i + 1;
      k *= xx * (double)(2 * n - 1) / (double)(2 * n);
      v += k / (2 * n + 1);
    }
    return v;
  } else {
    // Derived from half angle formula of sin.
    return M_PI_2 - 2 * asin(sqrt((1 - x) * 0.5));
  }
}
#endif

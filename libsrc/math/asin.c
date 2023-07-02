#include "math.h"

#ifndef __NO_FLONUM
double asin(double x) {
  if (x < -1 || x > 1 || !isfinite(x))
    return NAN;
  if (x < 0)
    return -asin(-x);

  double v;
  if (x < 0.55) {
    v = x;
    double k = x;
    double xx = x * x;
    for (int i = 0; i < 12; ++i) {
      int n = i + 1;
      k *= xx * (double)(2 * n - 1) / (double)(2 * n);
      v += k / (2 * n + 1);
    }
  } else {
    static const double TABLE[] = {
      M_SQRT2,
      1.0 / 6 / M_SQRT2,
      3.0 / 80 / M_SQRT2,
      5.0 / 448 / M_SQRT2,
      35.0 / 9216 / M_SQRT2,
      63.0 / 45056 / M_SQRT2,
      231.0 / 425984 / M_SQRT2,
      143.0 / 655360 / M_SQRT2,
      6435.0 / 71303168 / M_SQRT2,
      12155.0 / 318767104 / M_SQRT2,
      46189.0 / 2818572288 / M_SQRT2,
    };
    x = 1 - x;
    v = M_PI_2;
    double xx = sqrt(x);
    for (int i = 0; i < (int)(sizeof(TABLE) / sizeof(*TABLE)); ++i) {
      v -= xx * TABLE[i];
      xx *= x;
    }
  }
  return v;
}
#endif

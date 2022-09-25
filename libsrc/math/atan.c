#include "math.h"
#include "stdbool.h"

#ifndef __NO_FLONUM
static const double TAN225 = 0.414213562373095;  // tan(22.5 degree)

double atan(double x) {
  bool neg = x < 0;
  x = fabs(x);
  bool inv = false;
  if (x > 1) {
    x = 1.0 / x;
    inv = true;
  }
  bool diag = false;
  if (x > TAN225) {
    x = (1 - x) / (1 + x);
    diag = true;
  }

  double t = 0;
  {
    double _xx = -x * x;
    double k = x;
    for (int i = 0; i < 16; ++i) {
      t += k / (i * 2 + 1);
      k *= _xx;
    }
  }

  if (diag)
    t = (M_PI / 4) - t;
  if (inv)
    t = (M_PI / 2) - t;
  return neg ? -t : t;
}
#endif

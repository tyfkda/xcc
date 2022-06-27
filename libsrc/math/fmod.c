#include "math.h"

double fmod(double x, double m) {
  m = fabs(m);
  if (x >= 0)
    return x - floor(x / m) * m;
  else
    return x - ceil(x / m) * m;
}

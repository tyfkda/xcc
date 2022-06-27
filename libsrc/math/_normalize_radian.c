#include "math.h"

double normalize_radian(double x) {
  double y = fmod(x, 2 * M_PI);
  if (y > M_PI)
    return y - 2 * M_PI;
  if (y < -M_PI)
    return y + 2 * M_PI;
  return y;
}

#include "math.h"

#ifndef __NO_FLONUM
extern double normalize_radian(double x);

double cos(double x) {
  static const double TABLE[] = {
     1 / 1.0,
    -1 / (2.0 * 1),
     1 / (4.0 * 3 * 2 * 1),
    -1 / (6.0 * 5 * 4 * 3 * 2 * 1),
     1 / (8.0 * 7 * 6 * 5 * 4 * 3 * 2 * 1),
    -1 / (10.0 * 9 * 8 * 7 * 6 * 5 * 4 * 3 * 2 * 1),
     1 / (12.0 * 11 * 10 * 9 * 8 * 7 * 6 * 5 * 4 * 3 * 2 * 1),
    -1 / (14.0 * 13 * 12 * 11 * 10 * 9 * 8 * 7 * 6 * 5 * 4 * 3 * 2 * 1),
     1 / (16.0 * 15 * 14 * 13 * 12 * 11 * 10 * 9 * 8 * 7 * 6 * 5 * 4 * 3 * 2 * 1),
    -1 / (18.0 * 17 * 16 * 15 * 14 * 13 * 12 * 11 * 10 * 9 * 8 * 7 * 6 * 5 * 4 * 3 * 2 * 1),
  };

  x = normalize_radian(x);

  double v = 0;
  double xx = x * x;
  x = 1;
  for (int i = 0; i < sizeof(TABLE) / sizeof(*TABLE); ++i) {
     v += x * TABLE[i];
    x *= xx;
  }
  return v;
}
#endif

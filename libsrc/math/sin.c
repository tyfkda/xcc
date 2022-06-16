#include "math.h"

extern double normalize_radian(double x);

double sin(double x) {
  static const double TABLE[] = {
     1 / 1.0,
    -1 / (3.0 * 2 * 1),
     1 / (5.0 * 4 * 3 * 2 * 1),
    -1 / (7.0 * 6 * 5 * 4 * 3 * 2 * 1),
     1 / (9.0 * 8 * 7 * 6 * 5 * 4 * 3 * 2 * 1),
    -1 / (11.0 * 10 * 9 * 8 * 7 * 6 * 5 * 4 * 3 * 2 * 1),
     1 / (13.0 * 12 * 11 * 10 * 9 * 8 * 7 * 6 * 5 * 4 * 3 * 2 * 1),
    -1 / (15.0 * 14 * 13 * 12 * 11 * 10 * 9 * 8 * 7 * 6 * 5 * 4 * 3 * 2 * 1),
     1 / (17.0 * 16 * 15 * 14 * 13 * 12 * 11 * 10 * 9 * 8 * 7 * 6 * 5 * 4 * 3 * 2 * 1),
    -1 / (19.0 * 18 * 17 * 16 * 15 * 14 * 13 * 12 * 11 * 10 * 9 * 8 * 7 * 6 * 5 * 4 * 3 * 2 * 1),
  };

  x = normalize_radian(x);

  double v = 0;
  double xx = x * x;
  for (int i = 0; i < sizeof(TABLE) / sizeof(*TABLE); ++i) {
    v += x * TABLE[i];
    x *= xx;
  }
  return v;
}

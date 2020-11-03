#ifndef __NO_FLONUM
#include <math.h>

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

  double v = 0;
  double xx = x * x;
  for (int i = 0; i < sizeof(TABLE) / sizeof(*TABLE); ++i) {
    v += x * TABLE[i];
    x *= xx;
  }
  return v;
}

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

  double v = 0;
  double xx = x * x;
  x = 1;
  for (int i = 0; i < sizeof(TABLE) / sizeof(*TABLE); ++i) {
     v += x * TABLE[i];
    x *= xx;
  }
  return v;
}

double sqrt(double _) {
  __asm("sqrtsd %xmm0, %xmm0");
}

double fabs(double x) {
  //return x >= 0 ? x : -x;
  if (x >= 0.0)
    return x;
  return -x;
}

double drand48(void) {
  static unsigned int X = 1;
  static const unsigned int A = 214013;
  static const unsigned int C = 2531011;
  X = X * A + C;
  return (X >> 16) / 65535.0;
}

double erand48(unsigned short xsubi[3]) {
  return drand48();
}
#endif

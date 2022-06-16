#include "math.h"
#include "stdbool.h"

double pow(double base, double x) {
  bool neg = x < 0;
  x = fabs(x);
  unsigned int n = x;  // TODO: Care about overflow
  double r = (x - n) * log(base);

  double result = 1;
  double y = 1;
  for (int i = 1; i <= 15; ++i) {
    y *= r / i;
    result += y;
  }

  double poe = base;
  for (; n > 0; n >>= 1, poe *= poe) {
    if (n & 1)
      result *= poe;
  }
  return neg ? 1.0 / result : result;
}

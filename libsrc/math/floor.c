#include "math.h"

double floor(double x) {
  // TODO:
  if (x >= 0) {
    unsigned long l = x;
    return l;
  } else {
    unsigned long l = -x;
    return x + l < 0 ? -l - 1 : l;
  }
}

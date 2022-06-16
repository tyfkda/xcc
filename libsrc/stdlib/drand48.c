#include "stdlib.h"

double drand48(void) {
  // TODO: Implement
  static unsigned int X = 1;
  static const unsigned int A = 214013;
  static const unsigned int C = 2531011;
  X = X * A + C;
  return (X >> 16) / 65535.0;
}

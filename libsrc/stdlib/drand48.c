#include "stdlib.h"

#ifndef __NO_FLONUM
double drand48(void) {
  // TODO: Implement
  static unsigned int x = 1;
  static const unsigned int A = 214013;
  static const unsigned int C = 2531011;
  x = x * A + C;
  return (x >> 16) / 65535.0;
}
#endif

#include "stdlib.h"

#ifndef __NO_FLONUM
double drand48(void) {
  return lrand48() * (1.0 / (1UL << 31));
}
#endif

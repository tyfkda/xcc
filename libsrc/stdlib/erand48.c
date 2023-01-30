#include "stdlib.h"

#ifndef __NO_FLONUM
double erand48(unsigned short xsubi[3]) {
  return nrand48(xsubi) * (1.0 / (1UL << 31));
}
#endif

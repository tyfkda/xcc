#include "math.h"

#ifndef __NO_FLONUM
double sqrt(double _) {
  __asm("sqrtsd %xmm0, %xmm0");
}
#endif

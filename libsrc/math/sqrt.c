#include "math.h"

double sqrt(double _) {
  __asm("sqrtsd %xmm0, %xmm0");
}

#include "math.h"
#include "stdint.h"  // int64_t
#include "_ieee.h"

double round(double x) {
  return x >= 0 ? floor(x + 0.5) : ceil(x - 0.5);
}

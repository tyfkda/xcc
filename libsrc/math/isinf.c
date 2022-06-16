#include "math.h"
#include "stdint.h"  // int64_t
#include "_ieee.h"

int isinf(double x) {
  int64_t *q = (int64_t*)&x;
  int e = ((int)((*q) >> (EXP_POS - 1))) & ((1 << (EXP_BIT + 1)) - 1);
  return e == (((1 << EXP_BIT) - 1) << 1);
}

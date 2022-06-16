#include "math.h"
#include "stdint.h"  // int64_t
#include "_ieee.h"

double frexp(double x, int *p) {
  if (x == 0 || !isfinite(x)) {
    *p = 0;
    return x;
  }
  int64_t *q = (int64_t*)&x;
  int e = ((int)((*q) >> EXP_POS)) & ((1 << EXP_BIT) - 1);
  *p = e - EXP_BIOS;
  *q = (*q & ~((((int64_t)1 << EXP_BIT) - 1) << EXP_POS)) | ((int64_t)EXP_BIOS << EXP_POS);
  return x;
}

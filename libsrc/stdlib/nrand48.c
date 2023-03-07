#include "stdlib.h"

#include "stdint.h"  // uint64_t

// [0, 1<<31)
long nrand48(unsigned short xsubi[3]) {
  #define A  0x5deece66d
  #define C  0x0b
  uint64_t x = xsubi[0] | ((uint64_t)xsubi[1] << 16) | ((uint64_t)xsubi[2] << 32);
  x = x * A + C;
  xsubi[0] = x;
  xsubi[1] = x >> 16;
  xsubi[2] = x >> 32;
  return (long)(x >> (48 - 31)) & ((1L << 31) - 1);
}

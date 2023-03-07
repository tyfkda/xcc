#include "stdlib.h"

// static unsigned short xsubi[3] = {0x2e16, 0xea27, 0x6c4c};  // {1, 0, 0}?
static unsigned short xsubi[3] = {0xdeec, 0x1053, 0x1db2};

// [0, 1<<31)
long lrand48(void) {
  long val = nrand48(xsubi);
  return val;
}

void srand48(long seedval) {
  xsubi[0] = 0x330e;
  xsubi[1] = seedval;
  xsubi[2] = seedval >> 16;
}

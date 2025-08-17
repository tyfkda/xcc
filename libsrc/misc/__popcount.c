int __popcount(unsigned int x) {
  x -= (x >> 1) & 0x55555555U;
  x = (x & 0x33333333U) + ((x >> 2) & 0x33333333U);
  x = (x + (x >>  4)) & 0x0f0f0f0fU;
  x = (x + (x >>  8));
  x = (x + (x >> 16)) & 0x3fU;
  return x;
}

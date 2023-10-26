#include "string.h"

int memcmp(const void *buf1, const void *buf2, size_t n) {
  const unsigned char *p = buf1;
  const unsigned char *q = buf2;
  int d = 0;
  for (size_t i = 0; i < n; ++i, ++p, ++q) {
    d = (int)*(unsigned char*)p - (int)*(unsigned char*)q;
    if (d != 0)
      break;
  }
  return d;
}

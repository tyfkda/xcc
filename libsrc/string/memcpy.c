#include "string.h"

void *memcpy(void *dst, const void *src, size_t n) {
  const char *s = src;
  char *d = dst;
  while (n-- > 0)
    *d++ = *s++;
  return dst;
}

#include "string.h"

void *memccpy(void *dst, const void *src, int c, size_t n) {
  char *b = memchr(src, c, n);
  if (b != NULL)
    n = b - (char *)src;
  return memcpy(dst, src, n);
}

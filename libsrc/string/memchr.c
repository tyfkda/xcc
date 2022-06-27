#include "string.h"

void *memchr(const void *buf, int c, size_t n) {
  for (const char *p = buf, *e = p + n; p < e; ++p) {
    if (*p == c)
      return p;
  }
  return NULL;
}

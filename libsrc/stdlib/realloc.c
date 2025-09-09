#include "_malloc.h"
#include "stdlib.h"
#include "string.h"  // memcpy

void *realloc(void *p, size_t size) {
  if (size <= 0) {
    free(p);
    return NULL;
  }

  if (p == NULL)
    return malloc(size);

  void *buf = malloc(size);
  if (buf != NULL) {
    Header *h = (Header *)p - 1;
    size_t s = (h->s.size - 1) * sizeof(Header);
    memcpy(buf, p, size > s ? s : size);
    free(p);
  }
  return buf;
}

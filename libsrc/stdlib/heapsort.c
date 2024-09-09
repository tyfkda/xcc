#include "stdlib.h"
#include "alloca.h"
#include "string.h"  // memcpy

#define SIZE_THRESHOLD  (64)

int heapsort(void *base, size_t n, size_t size, int (*compare)(const void *, const void *)) {
  if (n < 2)
    return 0;  // TODO: Set errno and return -1.

  char *tmp = size <= SIZE_THRESHOLD ? alloca(size) : malloc(size);
  char *top = base;

  for (size_t k = n / 2; ;) {
    size_t i = k, j;
    char *parent = top + i * size;
    memcpy(tmp, parent, size);
    while ((j = 2 * i + 1) < n) {
      char *child = top + j * size, *child2;
      if (j + 1 < n && compare(child, child2 = child + size) < 0) {
        ++j;
        child = child2;
      }
      if (compare(tmp, child) >= 0)
        break;
      memcpy(parent, child, size);
      i = j;
      parent = child;
    }
    memcpy(parent, tmp, size);
    if (k == 0)
      break;
    --k;
  }
  while (n > 0) {
    // Purge the top element.
    char *last = top + (n - 1) * size;
    memcpy(tmp, last, size);
    memcpy(last, top, size);
    --n;

    char *parent = top;
    size_t i = 0, j;
    while ((j = 2 * i + 1) < n) {
      char *child = top + j * size, *child2;
      if (j + 1 < n && compare(child, child2 = child + size) < 0) {
        ++j;
        child = child2;
      }
      if (compare(tmp, child) >= 0)
        break;
      memcpy(parent, child, size);
      i = j;
      parent = child;
    }
    memcpy(parent, tmp, size);
  }

  if (size > SIZE_THRESHOLD)
    free(tmp);

  return 0;
}

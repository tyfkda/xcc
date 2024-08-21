#include "stdlib.h"

void *bsearch(const void *key, const void *base, size_t nmemb, size_t size,
              int (*compare)(const void*, const void*)) {
  while (nmemb > 0) {
    size_t mid = nmemb >> 1;
    char *p = (char*)base + mid * size;
    int cmp = compare(key, (void*)p);
    if (cmp < 0) {
      nmemb = mid;
    } else if (cmp > 0) {
      base = p + size;
      nmemb -= mid + 1;
    } else {
      return p;
    }
  }
  return NULL;
}

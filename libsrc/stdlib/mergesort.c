#include "stdlib.h"

#include "errno.h"
#include "string.h"  // memcpy

static void merge(void *base, size_t left, size_t mid, size_t right, size_t size,
                  int (*compare)(const void *, const void *), void *work) {
  size_t offset = left * size;
  char *dst = base + offset;
  char *lp = work + offset;
  char *lend = work + (mid + 1) * size;
  char *rp = lend;
  char *rend = work + (right + 1) * size;

  memcpy(lp, dst, rend - lp);

  for (;;) {
    if (compare(lp, rp) <= 0) {
      memcpy(dst, lp, size);
      dst += size;
      lp += size;
      if (lp >= lend)
        break;
    } else {
      memcpy(dst, rp, size);
      dst += size;
      rp += size;
      if (rp >= rend)
        break;
    }
  }

  if (lp < lend)
    memcpy(dst, lp, lend - lp);
  else if (rp < rend)
    memcpy(dst, rp, rend - rp);
}

static void recur(char *base, size_t left, size_t right, size_t size,
                  int (*compare)(const void *, const void *), char *work) {
  if (left >= right)
    return;

  size_t mid = left + (right - left) / 2;
  recur(base, left, mid, size, compare, work);
  recur(base, mid + 1, right, size, compare, work);

  merge(base, left, mid, right, size, compare, work);
}

int mergesort(void *base, size_t nmemb, size_t size, int (*compare)(const void *, const void *)) {
  char *work = malloc(size * nmemb);
  if (work == NULL)
    return -ENOMEM;
  recur(base, 0, nmemb - 1, size, compare, work);
  free(work);
  return 0;
}

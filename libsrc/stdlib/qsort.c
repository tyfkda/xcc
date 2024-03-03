#include "stdlib.h"
#include "alloca.h"
#include "string.h"  // memcpy

void qsort(void *base, size_t nmemb, size_t size, int (*compare)(const void *, const void *)) {
  if (nmemb <= 1)
    return;

  char *a = base;
  const char *px;

  px = &a[(nmemb >> 1) * size];
  size_t i = 0;
  size_t j = nmemb - 1;
  char *tmp = alloca(size);
  for (;;) {
    while (compare(&a[i * size], px) < 0)
      ++i;
    while (compare(px, &a[j * size]) < 0)
      --j;
    if (i >= j)
      break;

    char *pi = &a[i * size];
    char *pj = &a[j * size];
    memcpy(tmp, pi, size);
    memcpy(pi, pj, size);
    memcpy(pj, tmp, size);

    if (px == pi)
      px = pj;
    else if (px == pj)
      px = pi;
    ++i;
    --j;
  }
  if (i > 1)
    qsort(a, i, size, compare);
  if (j + 2 < nmemb)
    qsort(&a[(j + 1) * size], nmemb - j - 1, size, compare);
}

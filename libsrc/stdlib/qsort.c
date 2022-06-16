#include "stdlib.h"

void qsort(void *base, size_t nmemb, size_t size, int (*compare)(const void *, const void *)) {
  if (nmemb <= 1)
    return;

  char *a = base;
  const char *px;

  px = &a[(nmemb >> 1) * size];
  int i = 0;
  int j = nmemb - 1;
  for (;;) {
    while (compare(&a[i * size], px) < 0)
      ++i;
    while (compare(px, &a[j * size]) < 0)
      --j;
    if (i >= j)
      break;

    char *pi = &a[i * size];
    char *pj = &a[j * size];
    for (size_t k = 0; k < size; ++k) {
      char t = pi[k];
      pi[k] = pj[k];
      pj[k] = t;
    }
    if (px == pi)
      px = pj;
    else if (px == pj)
      px = pi;
    ++i;
    --j;
  }
  if (i > 1)
    qsort(a, i, size, compare);
  if ((size_t)(j + 2) < nmemb)
    qsort(&a[(j + 1) * size], nmemb - j - 1, size, compare);
}

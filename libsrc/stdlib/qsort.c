#include "stdlib.h"

void qsort(void *base, size_t nmemb, size_t size, int (*compare)(const void *, const void *)) {
  if (nmemb <= 1)
    return;

  char *a = base;

  size_t mid = nmemb >> 1;
  const char *px = &a[mid * size];
  size_t i = 0;
  size_t j = nmemb - 1;
  for (;;) {
    while (i < mid && compare(&a[i * size], px) < 0)
      ++i;
    while (j > mid && compare(px, &a[j * size]) < 0)
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
    if (px == pi) {
      px = pj;
      mid = j;
      ++i;
    } else if (px == pj) {
      px = pi;
      mid = i;
      --j;
    } else {
      ++i;
      --j;
    }
  }
  if (i > 1)
    qsort(a, i, size, compare);
  if ((size_t)(j + 2) < nmemb)
    qsort(&a[(j + 1) * size], nmemb - j - 1, size, compare);
}

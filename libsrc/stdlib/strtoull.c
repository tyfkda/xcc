#include "stdlib.h"

extern unsigned long long strtoull_sub(const char *p, char **pp, int base);

unsigned long long strtoull(const char *p, char **pp, int base) {
  const char *orig = p;
  if (*p == '+')
    ++p;
  char *q;
  unsigned long long result = strtoull_sub(p, &q, base);
  if (q == p)
    q = (char*)orig;

  if (pp != 0)
    *pp = q;

  return result;
}

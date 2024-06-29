#include "stdlib.h"

extern unsigned long long _strtoull_sub(const char *p, char **pp, int base, unsigned long long max);

unsigned long strtoul(const char *p, char **pp, int base) {
  const char *orig = p;
  if (*p == '+')
    ++p;
  char *q;
  unsigned long result = _strtoull_sub(p, &q, base, -1UL);
  if (pp != 0)
    *pp = q == p ? (char*)orig : q;
  return result;
}

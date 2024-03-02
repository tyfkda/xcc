#include "stdlib.h"
#include "ctype.h"  // isspace

extern unsigned long long _strtoull_sub(const char *p, char **pp, int base, unsigned long long max);

unsigned long long strtoull(const char *p, char **pp, int base) {
  const char *orig = p;

  for (; isspace(*p); ++p)
    ;

  if (*p == '+')
    ++p;
  char *q;
  unsigned long long result = _strtoull_sub(p, &q, base, -1ULL);
  if (pp != 0)
    *pp = q == p ? (char*)orig : q;
  return result;
}

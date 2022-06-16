#include "string.h"

extern unsigned long strtoul_sub(const char *p, char **pp, int base);

unsigned long strtoul(const char *p, char **pp, int base) {
  const char *orig = p;
  if (*p == '+')
    ++p;
  char *q;
  unsigned long result = strtoul_sub(p, &q, base);
  if (q == p)
    q = (char*)orig;

  if (pp != 0)
    *pp = q;

  return result;
}

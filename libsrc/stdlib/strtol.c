#include "stdlib.h"
#include "ctype.h"  // isspace
#include "limits.h"  // CHAR_BIT
#include "stdbool.h"

extern bool _parse_sign(const char **pp);
extern unsigned long long _strtoull_sub(const char *p, char **pp, int base, unsigned long long max);

long strtol(const char *p, char **pp, int base) {
  const char *orig = p;

  for (; isspace(*p); ++p)
    ;

  bool neg = _parse_sign(&p);
  char *q;
  long result;
  if (!neg) {
    result = (long)_strtoull_sub(p, &q, base, (1ULL << (sizeof(long) * CHAR_BIT - 1)) - 1);
  } else {
    result = -(long)_strtoull_sub(p, &q, base, 1ULL << (sizeof(long) * CHAR_BIT - 1));
  }
  if (pp != 0)
    *pp = q == p ? (char*)orig : q;
  return result;
}

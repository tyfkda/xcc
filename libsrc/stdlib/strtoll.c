#include "stdlib.h"
#include "ctype.h"  // isspace
#include "limits.h"  // CHAR_BIT
#include "stdbool.h"

extern bool _parse_sign(const char **pp);
extern unsigned long long _strtoull_sub(const char *p, char **pp, int base, unsigned long long max);

long long strtoll(const char *p, char **pp, int base) {
  const char *orig = p;

  for (; isspace(*p); ++p)
    ;

  bool neg = _parse_sign(&p);
  char *q;
  long long result;
  if (!neg) {
    result = _strtoull_sub(p, &q, base, (1ULL << (sizeof(long long) * CHAR_BIT - 1)) - 1);
  } else {
    result = -_strtoull_sub(p, &q, base, 1ULL << (sizeof(long long) * CHAR_BIT - 1));
  }
  if (pp != 0)
    *pp = q == p ? (char*)orig : q;
  return result;
}

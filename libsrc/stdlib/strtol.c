#include "stdlib.h"
#include "ctype.h"  // isspace
#include "limits.h"  // CHAR_BIT
#include "stdbool.h"

bool parse_sign(const char **pp);
unsigned long long strtoull_sub(const char *p, char **pp, int base, unsigned long long max);

long strtol(const char *p, char **pp, int base) {
  const char *orig = p;

  for (; isspace(*p); ++p)
    ;

  bool neg = parse_sign(&p);
  char *q;
  long result;
  if (!neg) {
    result = (long)strtoull_sub(p, &q, base, (1ULL << (sizeof(long) * CHAR_BIT - 1)) - 1);
  } else {
    result = -(long)strtoull_sub(p, &q, base, 1ULL << (sizeof(long) * CHAR_BIT - 1));
  }
  if (pp != 0)
    *pp = q == p ? (char*)orig : q;
  return result;
}

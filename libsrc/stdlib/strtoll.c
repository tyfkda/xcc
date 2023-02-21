#include "stdlib.h"
#include "ctype.h"  // isspace
#include "limits.h"  // CHAR_BIT
#include "stdbool.h"

extern unsigned long long strtoull_sub(const char *p, char **pp, int base, unsigned long long max);

bool parse_sign(const char **pp) {
  const char *p = *pp;
  char c = *p;
  bool negative = c == '-';
  if (c == '+' || c == '-')
    *pp = p + 1;
  return negative;
}

long long strtoll(const char *p, char **pp, int base) {
  const char *orig = p;

  for (; isspace(*p); ++p)
    ;

  bool neg = parse_sign(&p);
  char *q;
  long long result;
  if (!neg) {
    result = strtoull_sub(p, &q, base, (1ULL << (sizeof(long long) * CHAR_BIT - 1)) - 1);
  } else {
    result = -strtoull_sub(p, &q, base, 1ULL << (sizeof(long long) * CHAR_BIT - 1));
  }
  if (pp != 0)
    *pp = q == p ? (char*)orig : q;
  return result;
}

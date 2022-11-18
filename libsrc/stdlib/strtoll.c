#include "stdlib.h"
#include "ctype.h"  // tolower
#include "stdbool.h"

bool parse_sign(const char **pp) {
  const char *p = *pp;
  char c = *p;
  bool negative = c == '-';
  if (c == '+' || c == '-')
    *pp = p + 1;
  return negative;
}

unsigned long long strtoull_sub(const char *p, char **pp, int base) {
  char digimax = '0' + (base <= 10 ? base : 10);
  char hexmax = 'a' - 10 + base;
  unsigned long long result = 0;
  for (;; ++p) {
    char c = *p;
    int n;
    if ('0' <= c && c < digimax)
      n = c - '0';
    else {
      c = tolower(c);
      if ('a' <= c && c < hexmax)
        n = c - 'a' + 10;
      else
        break;
    }
    result = result * base + n;
  }

  if (pp != 0)
    *pp = (char*)p;

  return result;
}

long long strtoll(const char *p, char **pp, int base) {
  const char *orig = p;
  bool neg = parse_sign(&p);
  char *q;
  long long result = strtoull_sub(p, &q, base);
  if (q == p)
    q = (char*)orig;
  if (neg)
    result = -result;

  if (pp != 0)
    *pp = q;

  return result;
}

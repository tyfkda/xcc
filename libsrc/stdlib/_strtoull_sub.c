#include "ctype.h"  // isspace, tolower

unsigned long long _strtoull_sub(const char *p, char **pp, int base, unsigned long long max) {
  char digimax = '0' + (base <= 10 ? base : 10);
  char hexmax = 'a' - 10 + base;
  unsigned long long premax = max / base;
  unsigned long long result = 0;
  for (;; ++p) {
    char c = *p;
    unsigned int n;
    if ('0' <= c && c < digimax)
      n = c - '0';
    else {
      c = tolower(c);
      if ('a' <= c && c < hexmax)
        n = c - 'a' + 10;
      else
        break;
    }
    if (result > premax) {
      result = max;
      continue;
    }

    unsigned long long r = result * base;
    unsigned long long d = max - r;
    result = n < d ? r + n : max;
  }

  if (pp != 0)
    *pp = (char*)p;

  return result;
}

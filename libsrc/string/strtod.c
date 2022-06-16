#include "string.h"
#include "stdbool.h"

bool parse_sign(const char **pp);

#ifndef __NO_FLONUM
static double ipow(double base, long x) {
  bool neg = false;
  if (x < 0) {
    neg = true;
    x = -x;
  }
  double result = 1;
  double a = base;
  for (; x > 0; x >>= 1, a *= a) {
    if ((x & 1) != 0)
      result *= a;
  }
  return neg ? 1.0 / result : result;
}

static double strtod_i(const char *p, const char **pp) {
  double result = 0;
  for (;; ++p) {
    char c = *p;
    if (!(c >= '0' && c <= '9'))
      break;
    result = result * 10 + (c - '0');
  }
  *pp = p;
  return result;
}

double strtod(const char* /*restrict*/ p, char ** /*restrict*/ pp) {
  const char *orig = p;
  bool neg = parse_sign(&p);

  static const struct {
    const char *str;
    double pos, neg;
  } CONST[] = {
    {"infinity", 1.0 / 0.0, -1.0 / 0.0},
    {"inf", 1.0 / 0.0, -1.0 / 0.0},
    {"nan", 0.0 / 0.0, 0.0 / 0.0},
  };
  for (int i = 0, n = sizeof(CONST) / sizeof(*CONST); i < n; ++i) {
    const char *str = CONST[i].str;
    size_t len = strlen(str);
    if (strncmp(p, str, len) == 0) {
      p += len;
      char c = *p;
      if (pp != 0)
        *pp = (char*)p;
      return neg ? CONST[i].neg : CONST[i].pos;
    }
  }

  const char *op = p;
  double result = strtod_i(p, &p);
  if (*p == '.') {
    const char *q = p + 1;
    double frac = strtod_i(q, &p);
    result += frac * ipow(10, q - p);
  }
  if (*p == 'e' || *p == 'E') {
    const char *q = p + 1;
    bool neg2 = parse_sign(&q);
    double order = strtod_i(q, &p);
    if (q == p) {
      // Error.
    } else {
      result *= ipow(10, neg2 ? -order : order);
    }
  }
  if (p == op)
    p = orig;
  if (neg)
    result = -result;

  if (pp != 0)
    *pp = (char*)p;

  return result;
}
#endif

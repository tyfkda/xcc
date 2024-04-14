#include "stdlib.h"
#include "ctype.h"  // isspace, tolower
#include "stdbool.h"
#include "string.h"
#include "stdint.h"
#include "../math/_ieee.h"

extern bool _parse_sign(const char **pp);

#ifndef __NO_FLONUM
static double ipow(double base, long x) {
  double result = 1;
  double a = base;
  for (; x > 0; x >>= 1, a *= a) {
    if ((x & 1) != 0)
      result *= a;
  }
  return result;
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

static double strtod_i2(const char *p, const char **pp) {
  const char *q = p, *r = p;
  for (;;) {
    char c = *q;
    if (!(c >= '0' && c <= '9'))
      break;
    ++q;
    if (c != '0')
      r = q;
  }

  double result = 0;
  for (size_t order = r - p; r > p; --order) {
    char c = *(--r);
    result += (c - '0') / ipow(10, order);
  }
  *pp = q;
  return result;
}

#define strtod_h(p, pp)  strtoull(p, (char**)(pp), 16)

static double strtod_hex(const char *p, const char **pp) {
  double result = strtod_h(p, &p);
  if (*p == '.') {
    const char *q = p + 1;
    unsigned long long frac = strtod_h(q, &p);
    long order = p - q;
    result += frac * ipow(1.0f / 16, order);
  }
  if (tolower(*p) == 'p') {
    const char *q = p + 1;
    bool neg2 = _parse_sign(&q);
    double order = strtod_i(q, &p);
    if (q == p) {
      // Error.
    } else {
      double k = ipow(2, order);
      if (neg2)
        result /= k;
      else
        result *= k;
    }
  }
  *pp = p;
  return result;
}

double strtod(const char * restrict p_, char ** restrict pp) {
  const char *p = p_;
  const char *orig = p;

  for (; isspace(*p); ++p)
    ;

  bool neg = _parse_sign(&p);

  static const struct {
    const char *str;
    double pos, neg;
  } CONST[] = {
    {"infinity", 1.0 / 0.0, -1.0 / 0.0},
    {"inf", 1.0 / 0.0, -1.0 / 0.0},
    {"nan", 0.0 / 0.0, 0.0 / 0.0},
  };
  for (int i = 0; i < (int)(sizeof(CONST) / sizeof(*CONST)); ++i) {
    const char *str = CONST[i].str;
    size_t len = strlen(str);
    if (strncmp(p, str, len) == 0) {
      p += len;
      if (pp != 0)
        *pp = (char*)p;
      return neg ? CONST[i].neg : CONST[i].pos;
    }
  }

  const char *op;
  double result;
  if (p[0] == '0' && tolower(p[1]) == 'x') {
    op = p += 2;
    result = strtod_hex(p, &p);
  } else {
    op = p;
    result = strtod_i(p, &p);
    if (*p == '.') {
      const char *q = p + 1;
      double frac = strtod_i2(q, &p);
      result += frac;
    }
    if (tolower(*p) == 'e') {
      const char *q = p + 1;
      bool neg2 = _parse_sign(&q);
      double order = strtod_i(q, &p);
      if (q == p) {
        // Error.
      } else {
        double k = ipow(10, order);
        if (neg2)
          result /= k;
        else
          result *= k;
      }
    }
  }
  if (p == op)
    p = orig;
  if (neg)
    result = -result;

  if (pp != NULL)
    *pp = (char*)p;

  return result;
}
#endif

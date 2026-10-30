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


// Decimal text to double, correctly rounded for the usual case. The digits are gathered into one integer, then scaled by a single
// power of ten; when the integer is at most 2^53 and the power is at most
// 10^22, both are exact doubles, so the one multiply or divide rounds
// correctly (Clinger's fast path). The old digit-by-digit sum read 0.15 as
// 0.15000000000000002. Longer or more extreme numbers use the old approximation.
static double strtod_dec(const char *p, const char **pp) {
  unsigned long long mantissa = 0;
  int digits = 0, exp10 = 0;
  bool any = false;
  for (; *p >= '0' && *p <= '9'; ++p) {
    any = true;
    if (digits < 19) {
      mantissa = mantissa * 10 + (unsigned long long)(*p - '0');
      if (mantissa != 0)
        ++digits;
    } else {
      ++exp10;   // past 19 digits: keep the size, drop the digit
    }
  }
  if (*p == '.') {
    const char *q = p + 1;
    for (; *q >= '0' && *q <= '9'; ++q) {
      any = true;
      if (digits < 19) {
        mantissa = mantissa * 10 + (unsigned long long)(*q - '0');
        if (mantissa != 0)
          ++digits;
        --exp10;
      }
    }
    if (any)
      p = q;
  }
  if (!any) {
    *pp = p;
    return 0;
  }
  if (tolower(*p) == 'e') {
    const char *q = p + 1;
    bool neg2 = _parse_sign(&q);
    const char *start = q;
    long e = 0;
    for (; *q >= '0' && *q <= '9'; ++q)
      if (e < 100000)
        e = e * 10 + (*q - '0');
    if (q != start) {
      exp10 += (int)(neg2 ? -e : e);
      p = q;
    }
  }
  *pp = p;

  double m = (double)mantissa;
  if (mantissa <= (1ULL << 53) && exp10 >= -22 && exp10 <= 22)
    return exp10 < 0 ? m / ipow(10, -exp10) : m * ipow(10, exp10);
  if (exp10 < 0) {
    // Divide in steps so 10^-exp10 never overflows to infinity.
    for (; exp10 < -300; exp10 += 300)
      m /= ipow(10, 300);
    return m / ipow(10, -exp10);
  }
  return m * ipow(10, exp10);
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

double strtod(const char *restrict p_, char **restrict pp) {
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
    result = strtod_dec(p, &p);
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

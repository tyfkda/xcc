#include "stdio.h"

#include "ctype.h"
#include "limits.h"
#include "stdarg.h"
#include "stdbool.h"
#include "stdint.h"  // uintptr_t
#include "string.h"
#include "sys/types.h"

#include "_file.h"  // _fputc

#define PRINTF_BUFSIZ  (32)
#define MIN(a, b)  ((a) < (b) ? (a) : (b))
#define SIGN(p)  ((p) ? '+' : '-')
#define STATIC_ASSERT(cond)  char __dummy[(cond) ? 0 : -1]; (void)__dummy

static const char kHexDigits[] = "0123456789abcdef";
static const char kUpperHexDigits[] = "0123456789ABCDEF";

static int putnstr(FILE *fp, int n, const char *s) {
  int i;
  for (i = 0; i < n && s[i] != '\0'; ++i)
    ;
  return fwrite(s, 1, i, fp);
}

static void putpadding(FILE *fp, int m, char padding) {
  if (m <= 0)
    return;
  char buf[16];
  int n = MIN(m, (int)sizeof(buf));
  memset(buf, padding, n);
  do {
    int i = MIN(m, n);
    fwrite(buf, 1, i, fp);
    m -= i;
  } while (m > 0);
}

static int snprintullong(FILE *fp, unsigned long long x, int base, const char *digits, int order,
                         int padding) {
  char buf[32];
  STATIC_ASSERT(sizeof(buf) >= (sizeof(long long) * CHAR_BIT + 2) / 3);
  unsigned int i = 0, o = 0;

  do {
    buf[sizeof(buf) - (++i)] = digits[x % base];
    x /= base;
  } while (x != 0);

  if (i < (unsigned int)order) {
    unsigned int d = (unsigned int)order - i;
    putpadding(fp, d, padding);
    o += d;
  }
  return o + fwrite(&buf[sizeof(buf) - i], 1, i, fp);
}

static char *snprintullong2(char *bufend, unsigned long long x, int base, const char *digits) {
  char *p = bufend;
  *(--p) = '\0';
  do {
    *(--p) = digits[x % base];
    x /= base;
  } while (x != 0);
  return p;
}

static int snprintstr(FILE *fp, const char *s, int order, int suborder, bool leftalign,
                      char padding) {
  size_t len = strlen(s);
  if (suborder > 0)
    len = MIN(len, (unsigned int)suborder);
  if (order <= 0 || len >= (unsigned int)order) {
    putnstr(fp, len, s);
    return len;
  } else {
    if (leftalign) {
      putnstr(fp, len, s);
      putpadding(fp, order - len, padding);
    } else {
      putpadding(fp, order - len, padding);
      putnstr(fp, len, s);
    }
    return order;
  }
}

static int sprintsign(FILE *fp, bool negative, bool force, int *porder) {
  int o = 0;
  if (negative) {
    _fputc('-', fp);
    ++o;
  } else if (force) {
    _fputc('+', fp);
    ++o;
  }
  if (*porder > 1 && o > 0)
    *porder -= o;
  return o;
}

#ifndef __NO_FLONUM
#include "math.h"  // isfinite, signbit

static long double pow10(int order) {
  long double a = 1, x = 10;
  for (; order > 0; order >>= 1) {
    if ((order & 1) != 0)
      a *= x;
    x *= x;
  }
  return a;
}

// Float conversions (%f %F %e %E %g %G) following C99: field width, the
// '-', '0', '+' and '#' flags, and precision (including .0).

// The exact rounding error of the product p = a * b (Dekker's two-product,
// for 53-bit doubles: long double is a double on this target). Its sign says
// whether the true product is above or below p.
static long double product_error(long double a, long double b, long double p) {
  const long double split = 134217729.0;  // 2^27 + 1
  long double t = split * a;
  long double ahi = t - (t - a), alo = a - ahi;
  t = split * b;
  long double bhi = t - (t - b), blo = b - bhi;
  return ((ahi * bhi - p) + ahi * blo + alo * bhi) + alo * blo;
}

// Appends `x` (0 <= x < 1e18) with `prec` decimals to `p`; returns the end.
// Rounds to nearest using the exact value of x, and true ties to even, as glibc does.
// Digits past the 18th decimal are written as zeros.
static char *put_fixed(char *p, long double x, int prec, bool alt) {
  int extra = 0;
  if (prec > 18) {
    extra = (prec > 300 ? 300 : prec) - 18;
    prec = 18;
  }
  unsigned long long int_part = (unsigned long long)x;
  long double one = pow10(prec);
  long double part = x - (long double)int_part;
  long double scaled = part * one;
  unsigned long long frac = (unsigned long long)scaled;
  long double rest = scaled - (long double)frac;
  if (rest == 0.5) {
    // Looks like a tie, but the multiplication may have rounded: 0.05 * 10 is
    // just over 0.5 exactly, so it rounds up; 2.5 really is a tie, so it goes to even.
    long double err = product_error(part, one, scaled);
    if (err > 0 || (err == 0 && ((prec > 0 ? frac : int_part) & 1) != 0))
      ++frac;
  } else if (rest > 0.5) {
    ++frac;
  }
  if ((long double)frac >= one) {
    ++int_part;
    frac -= (unsigned long long)one;
  }
  char digits[24];
  int n = 0;
  do {
    digits[n++] = '0' + (char)(int_part % 10);
    int_part /= 10;
  } while (int_part != 0);
  while (n > 0)
    *p++ = digits[--n];
  if (prec > 0 || alt)
    *p++ = '.';
  for (int i = prec - 1; i >= 0; --i) {
    p[i] = '0' + (char)(frac % 10);
    frac /= 10;
  }
  p += prec;
  for (; extra > 0; --extra)
    *p++ = '0';
  return p;
}

// Splits x > 0 into m * 10^e with 1 <= m < 10.
static int decimal_exponent(long double *px) {
  long double x = *px;
  int e = 0;
  if (x >= 10) {
    while (x >= pow10(e + 1))
      ++e;
    x /= pow10(e);
  } else if (x < 1) {
    while (x * pow10(-e) < 1)
      --e;
    x *= pow10(-e);
  }
  *px = x;
  return e;
}

static char *put_scientific(char *p, long double x, int prec, bool alt, bool upper, int *pexp) {
  int e = 0;
  if (x != 0) {
    e = decimal_exponent(&x);
    // Rounding can carry to 10.0: renormalise.
    long double one = pow10(prec > 18 ? 18 : prec);
    if ((long double)(unsigned long long)(x * one + 0.5) >= 10 * one) {
      x /= 10;
      ++e;
    }
  }
  p = put_fixed(p, x, prec, alt);
  *p++ = upper ? 'E' : 'e';
  *p++ = e < 0 ? '-' : '+';
  int ae = e < 0 ? -e : e;
  if (ae >= 100)
    *p++ = '0' + (char)(ae / 100);
  *p++ = '0' + (char)((ae / 10) % 10);
  *p++ = '0' + (char)(ae % 10);
  if (pexp != NULL)
    *pexp = e;
  return p;
}

static char *strip_zeros(char *start, char *p) {
  char *dot = NULL;
  for (char *q = start; q < p; ++q)
    if (*q == '.')
      dot = q;
  if (dot == NULL)
    return p;
  while (p > dot + 1 && p[-1] == '0')
    --p;
  if (p == dot + 1)
    --p;
  return p;
}

static int printflonum(FILE *fp, long double x, int conv, int width, int prec, bool sign,
                       bool leftalign, char padding, bool alt) {
  char body[400];   // sign + up to 18 digits + '.' + at most 300 decimals + exponent
  char *p = body;
  bool upper = conv == 'F' || conv == 'E' || conv == 'G';
  bool neg = signbit(x);
  if (neg)
    x = -x;
  if (neg)
    *p++ = '-';
  else if (sign)
    *p++ = '+';
  char *digits = p;
  if (!isfinite(x)) {
    const char *s = isnan(x) ? (upper ? "NAN" : "nan") : (upper ? "INF" : "inf");
    while (*s != '\0')
      *p++ = *s++;
    padding = ' ';
  } else if (conv == 'e' || conv == 'E') {
    p = put_scientific(p, x, prec < 0 ? 6 : prec, alt, upper, NULL);
  } else if (conv == 'g' || conv == 'G') {
    int P = prec < 0 ? 6 : prec == 0 ? 1 : prec;
    int e = 0;
    char probe[400];
    if (x != 0)
      put_scientific(probe, x, P - 1, false, upper, &e);
    if (e < -4 || e >= P) {
      p = put_scientific(p, x, P - 1, alt, upper, NULL);
      if (!alt) {
        // Strip zeros from the mantissa only.
        char *ech = p;
        while (ech > digits && *ech != 'e' && *ech != 'E')
          --ech;
        char tail[8];
        int tn = (int)(p - ech);
        for (int i = 0; i < tn; ++i)
          tail[i] = ech[i];
        p = strip_zeros(digits, ech);
        for (int i = 0; i < tn; ++i)
          *p++ = tail[i];
      }
    } else {
      p = put_fixed(p, x, P - 1 - e, alt);
      if (!alt)
        p = strip_zeros(digits, p);
    }
  } else if (x >= 1e18) {
    p = put_scientific(p, x, prec < 0 ? 6 : prec, alt, upper, NULL);   // too big for the fixed path
  } else {
    p = put_fixed(p, x, prec < 0 ? 6 : prec, alt);
  }

  int len = (int)(p - body);
  int pad = width > len ? width - len : 0;
  if (leftalign) {
    putnstr(fp, len, body);
    putpadding(fp, pad, ' ');
  } else if (padding == '0') {
    putnstr(fp, (int)(digits - body), body);   // the sign goes before the zeros
    putpadding(fp, pad, '0');
    putnstr(fp, (int)(p - digits), digits);
  } else {
    putpadding(fp, pad, ' ');
    putnstr(fp, len, body);
  }
  return len + pad;
}
#endif

int vfprintf(FILE *fp, const char *fmt_, va_list ap) {
  char buf[PRINTF_BUFSIZ];
#define bufend  (buf + sizeof(buf))
  STATIC_ASSERT(sizeof(buf) >= (sizeof(long long) * CHAR_BIT + 2) / 3);

  const unsigned char *fmt = (const unsigned char*)fmt_;
  int c, i;
  int o;

  for (i = o = 0; fmt[i] != '\0'; i++) {
    c = fmt[i];
    if (c != '%') {
      _fputc(c, fp);
      ++o;
      continue;
    }

    // Handle '%'
    char padding = ' ';
    int order = 0, suborder = 0;
    bool has_prec = false, alt = false;
    bool sign = false;
    bool leftalign = false;
    bool sizez = false;
    int nlong = 0;
    c = fmt[++i];

    for (;;) {
      if (c == '+') sign = true;
      else if (c == '-') leftalign = true;
      else if (c == '#') alt = true;
      else break;
      c = fmt[++i];
    }
    if (c == '0') {
      padding = '0';
      c = fmt[++i];
    }
    if (c >= '1' && c <= '9') {
      order = c - '0';
      while (c = fmt[++i], c >= '0' && c <= '9')
        order = order * 10 + (c - '0');
    } else if (c == '*') {
      order = va_arg(ap, int);
      c = fmt[++i];
    }
    if (c == '.') {
      has_prec = true;
      c = fmt[++i];
      if (isdigit(c)) {
        do {
          suborder = suborder * 10 + (c - '0');
        } while (isdigit(c = fmt[++i]));
      } else if (c == '*') {
        suborder = va_arg(ap, int);
        c = fmt[++i];
      }
    }

    if (tolower(c) == 'l') {
      nlong = 1;
      c = fmt[++i];
      if (tolower(c) == 'l') {
        nlong = 2;
        c = fmt[++i];
      }
    } else if (c == 'z') {
      sizez = true;
      c = fmt[++i];
    }

    switch (c) {
    case 'd': case 'i':
      {
        long long x;
        if (sizez) {
          x = va_arg(ap, ssize_t);
        } else {
          switch (nlong) {
          case 0:  x = va_arg(ap, int); break;
          case 1:  x = va_arg(ap, long); break;
          default: x = va_arg(ap, long long); break;  // case 2:
          }
        }
        bool negative = x < 0;
        unsigned long long ux = negative ? -x : x;
        if (negative || order > 0 || sign)
          o += sprintsign(fp, negative, sign, &order);
        char *p = snprintullong2(bufend, ux, 10, kHexDigits);
        if (suborder > order) {
          order = suborder;
          padding = '0';
        }
        o += snprintstr(fp, p, order, suborder, leftalign, padding);
      }
      break;
    case 'u':
      {
        unsigned long long x;
        if (sizez) {
          x = va_arg(ap, size_t);
        } else {
          switch (nlong) {
          case 0:  x = va_arg(ap, unsigned int); break;
          case 1:  x = va_arg(ap, unsigned long); break;
          default: x = va_arg(ap, unsigned long long); break;  // case 2:
          }
        }
        char *p = snprintullong2(bufend, x, 10, kHexDigits);
        if (sign)
          *(--p) = '+';
        o += snprintstr(fp, p, order, suborder, leftalign, padding);
      }
      break;
    case 'x': case 'X': case 'o':
      {
        const char *digits = c == 'X' ? kUpperHexDigits : kHexDigits;
        int base = c == 'o' ? 8 : 16;
        unsigned long long x;
        if (sizez) {
          x = va_arg(ap, size_t);
        } else {
          switch (nlong) {
          case 0:  x = va_arg(ap, unsigned int); break;
          case 1:  x = va_arg(ap, unsigned long); break;
          default: x = va_arg(ap, unsigned long long); break;  // case 2:
          }
        }
        o += snprintullong(fp, x, base, digits, order, padding);
      }
      break;
    case 'p':
      {
        void *ptr = va_arg(ap, void*);
        char *p = snprintullong2(bufend, (uintptr_t)ptr, 16, kHexDigits);
        p -= 2;
        p[0] = '0';
        p[1] = 'x';
        o += snprintstr(fp, p, order, suborder, leftalign, padding);
      }
      break;
    case 's':
      {
        const char *s = va_arg(ap, const char*);
        if (s == NULL)
          s = "(null)";
        o += snprintstr(fp, s, order, suborder, leftalign, ' ');
      }
      break;
    case 'c':
      _fputc(va_arg(ap, unsigned int), fp);
      ++o;
      break;
    case '%':
      _fputc(c, fp);
      ++o;
      break;
#ifndef __NO_FLONUM
    case 'f': case 'F': case 'e': case 'E': case 'g': case 'G':
      {
        long double x;
        switch (nlong) {
        case 0:  x = va_arg(ap, double); break;
        default: x = va_arg(ap, long double); break;
        }
        o += printflonum(fp, x, c, order, has_prec ? suborder : -1, sign, leftalign, padding, alt);
      }
      break;
#endif
    default:
      // Unknown % sequence.  Print it to draw attention.
      _fputc('%', fp);
      ++o;
      if (c != '\0') {
        _fputc(c, fp);
        ++o;
      }
      break;
    }
  }

  return o;
}

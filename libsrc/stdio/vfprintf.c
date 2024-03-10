#include "stdio.h"

#include "ctype.h"
#include "limits.h"
#include "stdarg.h"
#include "stdbool.h"
#include "stdint.h"  // uintptr_t
#include "string.h"
#include "sys/types.h"

#include "_file.h"  // FPUTC

#define PRINTF_BUFSIZ  (32)
#define MIN(a, b)  ((a) < (b) ? (a) : (b))
#define SIGN(p)  ((p) ? '+' : '-')
#define STATIC_ASSERT(cond)  char __dummy[(cond) ? 0 : -1]; (void)__dummy

static char kHexDigits[] = "0123456789abcdef";
static char kUpperHexDigits[] = "0123456789ABCDEF";

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

static int snprintullong(FILE *fp, unsigned long long x,
                         int base, const char* digits, int order, int padding) {
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

static int snprintstr(FILE *fp, const char* s,
                      int order, int suborder, bool leftalign, char padding) {
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
    FPUTC('-', fp);
    ++o;
  } else if (force) {
    FPUTC('+', fp);
    ++o;
  }
  if (*porder > 1 && o > 0)
    *porder -= o;
  return o;
}

#ifndef __NO_FLONUM
#include "math.h"  // isfinite

static long double pow10(int order) {
  long double a = 1, x = 10;
  for (; order > 0; order >>= 1) {
    if ((order & 1) != 0)
      a *= x;
    x *= x;
  }
  return a;
}

static int printfloat(FILE *fp, long double x,
                      int order, int suborder, bool sign, bool leftalign, char padding) {
  if (!isfinite(x)) {
    const char *s = isnan(x) ? "nan" : x > 0 ? "inf" : "-inf";
    return snprintstr(fp, s, order, suborder, leftalign, ' ');
  }

  int o = sprintsign(fp, x < 0, sign, &order);
  x = x < 0 ? -x : x;

  unsigned long long int_part = x;
  suborder = suborder > 0 ? suborder : 6;
  long double one = pow10(suborder);
  unsigned long long fraction = (x - int_part) * one + 0.5;
  if (fraction >= one) {
    ++int_part;
    fraction -= one;
  }
  o += snprintullong(fp, int_part, 10, kHexDigits, order, padding);
  FPUTC('.', fp); ++o;
  o += snprintullong(fp, fraction, 10, kHexDigits, suborder, '0');
  return o;
}

static int normalize_float(long double *px) {
  long double x = *px;
  int exp = 0;
  if (x >= 1e6) {
    do {
      x /= 10;
      ++exp;
    } while (x >= 10);
  }
  if (x > 0 && x <= 1e-4) {
    do {
      x *= 10;
      --exp;
    } while (x < 1.0);
  }
  *px = x;
  return exp;
}

static int printscientific(FILE *fp, long double x, int order, int suborder, bool sign,
                           bool leftalign, char padding) {
  if (!isfinite(x)) {
    const char *s = isnan(x) ? "nan" : x > 0 ? "inf" : "-inf";
    return snprintstr(fp, s, order, suborder, leftalign, ' ');
  }

  int o = sprintsign(fp, x < 0, sign, &order);
  x = x < 0 ? -x : x;

  int e = normalize_float(&x);
  unsigned long long int_part = x;
  suborder = suborder > 0 ? suborder : 6;
  long double one = pow10(suborder);
  unsigned long long fraction = (x - int_part) * one + 0.5;
  if (fraction >= one) {
    ++int_part;
    fraction -= one;
  }
  while (fraction % 10 == 0 && suborder > 0) {
    fraction /= 10;
    --suborder;
  }
  o += snprintullong(fp, int_part, 10, kHexDigits, order, padding);
  if (fraction != 0) {
    FPUTC('.', fp); ++o;
    o += snprintullong(fp, fraction, 10, kHexDigits, suborder, '0');
  }

  if (e != 0) {
    FPUTC('e', fp); ++o;
    o += sprintsign(fp, e < 0, true, &order);
    e = e < 0 ? -e : e;
    o += snprintullong(fp, e, 10, kHexDigits, 2, '0');
  }
  return o;
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
      FPUTC(c, fp);
      ++o;
      continue;
    }

    // Handle '%'
    char padding = ' ';
    int order = 0, suborder = 0;
    bool sign = false;
    bool leftalign = false;
    int nlong = 0;
    c = fmt[++i];

    if (c == '+') {
      sign = true;
      c = fmt[++i];
    } else if (c == '-') {
      leftalign = true;
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
    }

    switch (c) {
    case 'd':
      {
        long long x;
        switch (nlong) {
        case 0:  x = va_arg(ap, int); break;
        case 1:  x = va_arg(ap, long); break;
        default: x = va_arg(ap, long long); break;  // case 2:
        }
        bool negative = x < 0;
        unsigned long long ux = negative ? -x : x;
        if (negative || order > 0 || sign)
          o += sprintsign(fp, negative, sign, &order);
        char *p = snprintullong2(bufend, ux, 10, kHexDigits);
        o += snprintstr(fp, p, order, suborder, leftalign, padding);
      }
      break;
    case 'u':
      {
        unsigned long long x;
        switch (nlong) {
        case 0:  x = va_arg(ap, unsigned int); break;
        case 1:  x = va_arg(ap, unsigned long); break;
        default: x = va_arg(ap, unsigned long long); break;  // case 2:
        }
        char *p = snprintullong2(bufend, x, 10, kHexDigits);
        if (sign)
          *(--p) = '+';
        o += snprintstr(fp, p, order, suborder, leftalign, padding);
      }
      break;
    case 'z':
      {
        switch (fmt[i + 1]) {
        case 'u':
          {
            ++i;
            size_t x = va_arg(ap, size_t);
            o += snprintullong(fp, x, 10, kHexDigits, order, padding);
          }
          break;
        case 'd':
          {
            ++i;
            ssize_t x = va_arg(ap, ssize_t);
            bool negative = x < 0;
            size_t ux = negative ? -x : x;
            if (negative || order > 0 || sign)
              o += sprintsign(fp, negative, sign, &order);
            char *p = snprintullong2(bufend, ux, 10, kHexDigits);
            o += snprintstr(fp, p, order, suborder, leftalign, padding);
          }
          break;
        default: break;  // TODO: error
        }
      }
      break;
    case 'x': case 'X':
      {
        const char *digits = c == 'x' ? kHexDigits : kUpperHexDigits;
        unsigned long long x;
        switch (nlong) {
        case 0:  x = va_arg(ap, unsigned int); break;
        case 1:  x = va_arg(ap, unsigned long); break;
        default: x = va_arg(ap, unsigned long long); break;  // case 2:
        }
        o += snprintullong(fp, x, 16, digits, order, padding);
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
      FPUTC(va_arg(ap, unsigned int), fp);
      ++o;
      break;
    case '%':
      FPUTC(c, fp);
      ++o;
      break;
#ifndef __NO_FLONUM
    case 'f':
      {
        long double x;
        switch (nlong) {
        case 0:  x = va_arg(ap, double); break;
        default: x = va_arg(ap, long double); break;
        }
        o += printfloat(fp, x, order, suborder, sign, leftalign, padding);
      }
      break;
    case 'g':
      {
        long double x;
        switch (nlong) {
        case 0:  x = va_arg(ap, double); break;
        default: x = va_arg(ap, long double); break;
        }
        o += printscientific(fp, x, order, suborder, sign, leftalign, padding);
      }
      break;
#endif
    default:
      // Unknown % sequence.  Print it to draw attention.
      FPUTC('%', fp);
      ++o;
      if (c != '\0') {
        FPUTC(c, fp);
        ++o;
      }
      break;
    }
  }

  return o;
}

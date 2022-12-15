#include "stdio.h"

#include "ctype.h"
#include "limits.h"
#include "stdarg.h"
#include "stdbool.h"
#include "stdint.h"  // uintptr_t
#include "string.h"

#include "_file.h"  // FPUTC

#ifndef __NO_FLONUM
#include "math.h"
#endif

#define PRINTF_BUFSIZ  (32)
#define MIN(a, b)  ((a) < (b) ? (a) : (b))
#define SIGN(p)  ((p) ? '+' : '-')
#define STATIC_ASSERT(cond)  char __dummy[(cond) ? 0 : -1]; (void)__dummy

static char kHexDigits[] = "0123456789abcdef";
static char kUpperHexDigits[] = "0123456789ABCDEF";

#ifndef __NO_FLONUM
static double pow10(int order) {
  double a = 1, x = 10;
  for (; order > 0; order >>= 1) {
    if ((order & 1) != 0)
      a *= x;
    x *= x;
  }
  return a;
}
#endif

static void putnstr(FILE *fp, int n, const char *s) {
  int c;
  for (; (c = *s++) != '\0' && n > 0; --n)
    FPUTC(c, fp);
}

static void putpadding(FILE *fp, int m, char padding) {
  for (; m > 0; --m)
    FPUTC(padding, fp);
}

// Output is not '\0' terminated.
static int snprintullong(FILE *fp, unsigned long long x,
                         int base, const char* digits, int order, int padding) {
  char buf[32];
  unsigned int i, o;

  i = 0;
  do {
    buf[i++] = digits[x % base];
    x /= base;
  } while (x != 0);

  if (i < (unsigned int)order) {
    memset(buf + i, padding, order - i);
    i = order;
  }

  for (o = 0; i > 0; ++o)
    FPUTC(buf[--i], fp);

  return o;
}

char *snprintullong2(char *buf, unsigned long long x, int base, const char *digits) {
  char *p = buf + PRINTF_BUFSIZ;
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

static int sprintsign(FILE *fp, int negative, int force, int *porder) {
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

int vfprintf(FILE *fp, const char *fmt_, va_list ap) {
  char buf[PRINTF_BUFSIZ];
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

    if (c == 'l') {
      nlong = 1;
      c = fmt[++i];
      if (c == 'l') {
        nlong = 2;
        c = fmt[++i];
      }
    }

    switch (c) {
    case 'd': {
      long long x;
      switch (nlong) {
      case 0:  x = va_arg(ap, int); break;
      case 1:  x = va_arg(ap, long); break;
      default: x = va_arg(ap, long long); break;  // case 2:
      }
      unsigned long long ux = x < 0 ? -x : x;
      char *p = snprintullong2(buf, ux, 10, kHexDigits);
      if (x < 0 || sign)
        *(--p) = SIGN(x >= 0);
      o += snprintstr(fp, p, order, suborder, leftalign, padding);
    } break;
    case 'u': {
      unsigned long long x;
      switch (nlong) {
      case 0:  x = va_arg(ap, unsigned int); break;
      case 1:  x = va_arg(ap, unsigned long); break;
      default: x = va_arg(ap, unsigned long long); break;  // case 2:
      }
      char *p = snprintullong2(buf, x, 10, kHexDigits);
      if (sign)
        *(--p) = '+';
      o += snprintstr(fp, p, order, suborder, leftalign, padding);
    } break;
    case 'x': case 'X': {
      const char *digits = c == 'x' ? kHexDigits : kUpperHexDigits;
      unsigned long long x;
      switch (nlong) {
      case 0:  x = va_arg(ap, unsigned int); break;
      case 1:  x = va_arg(ap, unsigned long); break;
      default: x = va_arg(ap, unsigned long long); break;  // case 2:
      }
      char *p = snprintullong2(buf, x, 16, digits);
      o += snprintstr(fp, p, order, suborder, leftalign, padding);
    } break;
    case 'p': {
      void *ptr = va_arg(ap, void*);
      char *p = snprintullong2(buf, (uintptr_t)ptr, 16, kHexDigits);
      p -= 2;
      p[0] = '0';
      p[1] = 'x';
      o += snprintstr(fp, p, order, suborder, leftalign, padding);
    } break;
    case 's': {
      const char *s = va_arg(ap, const char*);
      if (s == NULL)
        s = "(null)";
      o += snprintstr(fp, s, order, suborder, leftalign, ' ');
    } break;
    case 'c':
      FPUTC(va_arg(ap, unsigned int), fp);
      ++o;
      break;
    case '%':
      FPUTC(c, fp);
      ++o;
      break;
#ifndef __NO_FLONUM
    case 'f': {
      double x = va_arg(ap, double);
      if (!isfinite(x)) {
        const char *s = isnan(x) ? "nan" : x > 0 ? "inf" : "-inf";
        o += snprintstr(fp, s, order, suborder, leftalign, ' ');
      } else {
        o += sprintsign(fp, x < 0, sign, &order);
        x = x < 0 ? -x : x;

        long intPart = x >= 0 ? (long)x : -(long)(-x);
        o += snprintullong(fp, intPart, 10, kHexDigits, order, padding);
        FPUTC('.', fp);
        ++o;
        suborder = suborder > 0 ? suborder : 6;
        unsigned long fraction = (unsigned long)((x - intPart) * pow10(suborder));
        o += snprintullong(fp, fraction, 10, kHexDigits,
                           suborder, '0');
      }
    } break;
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

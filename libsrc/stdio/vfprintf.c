#include "ctype.h"
#include "stdarg.h"
#include "stdbool.h"
#include "stdint.h"  // uintptr_t
#include "stdio.h"
#include "string.h"

#include "_file.h"

#define MIN(a, b)  ((a) < (b) ? (a) : (b))

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
  do{
    buf[i++] = digits[x % base];
    x /= base;
  }while(x != 0);

  if (i < (unsigned int)order) {
    memset(buf + i, padding, order - i);
    i = order;
  }

  for (o = 0; i > 0; ++o)
    FPUTC(buf[--i], fp);

  return o;
}

static int snprintstr(FILE *fp, const char* s,
                      int order, int suborder, int leftalign) {
  int o = 0;
  if(s == NULL)
    s = "(null)";
  size_t len = strlen(s);
  if (suborder > 0)
    len = MIN(len, (unsigned int)suborder);
  if (order <= 0 || len >= (unsigned int)order) {
    putnstr(fp, len, s);
    o += len;
  } else {
    if (leftalign) {
      putnstr(fp, len, s);
      putpadding(fp, order - len, ' ');
    } else {
      putpadding(fp, order - len, ' ');
      putnstr(fp, len, s);
    }
    o += order;
  }
  return o;
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

// Only understands %d, %x, %X, %p, %s, %c, %f and "+-0~9".
// '\0' is not put at the end if the buffer is smaller than output.
int vfprintf(FILE *fp, const char *fmt_, va_list ap) {
  const unsigned char *fmt = (const unsigned char*)fmt_;
  int c, i;
  int o;

  for(i = o = 0; fmt[i] != '\0'; i++){
    c = fmt[i];
    if(c != '%'){
      FPUTC(c, fp);
      ++o;
      continue;
    }

    // Handle '%'
    char padding = ' ';
    int order = 0, suborder = 0;
    int sign = 0;
    int leftalign = 0;
    int nlong = 0;
    c = fmt[++i];
    if (c == '+') {
      sign = 1;
      c = fmt[++i];
    } else if (c == '-') {
      leftalign = 1;
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

    if(c == 'l'){
      nlong = 1;
      c = fmt[++i];
      if(c == 'l'){
        nlong = 2;
        c = fmt[++i];
      }
    }
    if(c == 'd'){
      long long x;
      switch (nlong) {
      case 0:  x = va_arg(ap, int); break;
      case 1:  x = va_arg(ap, long); break;
      default: x = va_arg(ap, long long); break;  // case 2:
      }
      o += sprintsign(fp, x < 0, sign, &order);
      unsigned long long ux = x < 0 ? -x : x;
      o += snprintullong(fp, ux, 10, kHexDigits, order, padding);
    } else if(tolower(c) == 'x') {
      const char *digits = c == 'x' ? kHexDigits : kUpperHexDigits;
      unsigned long long x;
      switch (nlong) {
      case 0:  x = va_arg(ap, unsigned int); break;
      case 1:  x = va_arg(ap, unsigned long); break;
      default: x = va_arg(ap, unsigned long long); break;  // case 2:
      }
      o += snprintullong(fp, x, 16, digits, order, padding);
    } else if(c == 'p') {
      void *ptr = va_arg(ap, void*);
      order -= 2;
      if (order < 0)
        order = 0;
      if (order == 0 || padding != ' ') {
        o += snprintstr(fp, "0x", 0, 0, 0);
        o += snprintullong(fp, (uintptr_t)ptr, 16, kHexDigits, order, padding);
      } else {
        int oo = 0;
        for (uintptr_t x = (uintptr_t)ptr; x >= 0x10; x >>= 4)
          ++oo;
        if (order > oo) {
          putpadding(fp, order - oo, padding);
          o += order - oo;
        }
        snprintstr(fp, "0x", 0, 0, 0);
        snprintullong(fp, (uintptr_t)ptr, 16, kHexDigits, 0, padding);
        o += 2 + oo;
      }
    } else if(c == 's'){
      // ("%5", "foo")         = "  foo"
      // ("%-5", "foo")        = "foo  "
      // ("%5", "foobarbaz")   = "foobarbaz"
      // ("%.3", "foobarbaz")  = "foo"
      // ("%5.7", "foobarbaz") = "foobarb"
      // ("%5.3", "foobarbaz") = "  foo"

      const char *s = va_arg(ap, const char*);
      o += snprintstr(fp, s, order, suborder, leftalign);
    } else if(c == 'c'){
      FPUTC(va_arg(ap, unsigned int), fp);
      ++o;
    } else if(c == '%'){
      FPUTC(c, fp);
      ++o;
#ifndef __NO_FLONUM
    } else if(c == 'f'){
      double x = va_arg(ap, double);

      o += sprintsign(fp, x < 0, sign, &order);
      x = x < 0 ? -x : x;

      long intPart = x >= 0 ? (long)x : -(long)(-x);
      o += snprintullong(fp, intPart, 10, kHexDigits,
                         order, padding);
      FPUTC('.', fp);
      ++o;
      suborder = suborder > 0 ? suborder : 6;
      unsigned long fraction = (unsigned long)((x - intPart) * pow10(suborder));
      o += snprintullong(fp, fraction, 10, kHexDigits,
                          suborder, '0');
#endif
    } else {
      // Unknown % sequence.  Print it to draw attention.
      FPUTC('%', fp);
      ++o;
      if (c != '\0') {
        FPUTC(c, fp);
        ++o;
      }
    }
  }

  return o;
}

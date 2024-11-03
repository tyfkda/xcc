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

static int printnonfinite(FILE *fp, long double x,
                          int order, int suborder, bool sign, bool leftalign) {
  int o = sprintsign(fp, signbit(x), sign, &order);
  return o + snprintstr(fp, isnan(x) ? "nan" : "inf", order, suborder, leftalign, ' ');
}

static int printfloat(FILE *fp, long double x,
                      int order, int suborder, bool sign, bool leftalign, char padding) {
  if (!isfinite(x))
    return printnonfinite(fp, x, order, suborder, sign, leftalign);

  bool neg = signbit(x);
  if (neg)
    x = -x;
  int o = sprintsign(fp, neg, sign, &order);

  unsigned long long int_part = x;
  suborder = suborder > 0 ? suborder : 6;
  long double one = pow10(suborder);
  unsigned long long fraction = (x - int_part) * one + 0.5;
  if (fraction >= one) {
    ++int_part;
    fraction -= one;
  }
  o += snprintullong(fp, int_part, 10, kHexDigits, order, padding);
  _fputc('.', fp); ++o;
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
  if (!isfinite(x))
    return printnonfinite(fp, x, order, suborder, sign, leftalign);

  bool neg = signbit(x);
  if (neg)
    x = -x;
  int o = sprintsign(fp, neg, sign, &order);

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
    _fputc('.', fp); ++o;
    o += snprintullong(fp, fraction, 10, kHexDigits, suborder, '0');
  }

  if (e != 0) {
    _fputc('e', fp); ++o;
    o += sprintsign(fp, e < 0, true, &order);
    e = e < 0 ? -e : e;
    o += snprintullong(fp, e, 10, kHexDigits, 2, '0');
  }
  return o;
}
#endif

struct printf_info {
  wchar_t spec;  // Format letter
  wchar_t pad;   // Padding
  int width;
  int prec;      // Precision
  unsigned char left;      // -n
  unsigned char showsign;  // +
  unsigned char nlong;
};

typedef int (*printf_function)(FILE *stream, const struct printf_info *info,
                               const void *const *args);

int fmt_d(FILE *stream, const struct printf_info *info, const void *const *args) {
  long long x;
  switch (info->nlong) {
  case 0:  x = *(int*)args[0]; break;
  case 1:  x = *(long*)args[0]; break;
  default: x = *(long long*)args[0]; break;  // case 2:
  }

  char buf[PRINTF_BUFSIZ];
#define bufend  (buf + sizeof(buf))

  bool negative = x < 0;
  unsigned long long ux = negative ? -x : x;
  int order = info->width;
  int o = 0;
  if (negative || order > 0 || info->showsign)
    o += sprintsign(stream, negative, info->showsign, &order);
  char *p = snprintullong2(bufend, ux, 10, kHexDigits);
  o += snprintstr(stream, p, order, info->prec, info->left, info->pad);
  return o;
#undef bufend
}

int fmt_u(FILE *stream, const struct printf_info *info, const void *const *args) {
  unsigned long long x;
  switch (info->nlong) {
  case 0:  x = *(unsigned int*)args[0]; break;
  case 1:  x = *(unsigned long*)args[0]; break;
  default: x = *(unsigned long long*)args[0]; break;  // case 2:
  }

  char buf[PRINTF_BUFSIZ];
#define bufend  (buf + sizeof(buf))

  char *p = snprintullong2(bufend, x, 10, kHexDigits);
  if (info->showsign)
    *(--p) = '+';
  return snprintstr(stream, p, info->width, info->prec, info->left, info->pad);
#undef bufend
}

int fmt_x(FILE *stream, const struct printf_info *info, const void *const *args) {
  unsigned long long x;
  switch (info->nlong) {
  case 0:  x = *(unsigned int*)args[0]; break;
  case 1:  x = *(unsigned long*)args[0]; break;
  default: x = *(unsigned long long*)args[0]; break;  // case 2:
  }

  const char *digits = info->spec == 'x' ? kHexDigits : kUpperHexDigits;
  return snprintullong(stream, x, 16, digits, info->width, info->pad);
}
#define fmt_X  fmt_x

int fmt_p(FILE *stream, const struct printf_info *info, const void *const *args) {
  void *x = *(void**)args[0];

  char buf[PRINTF_BUFSIZ];
#define bufend  (buf + sizeof(buf))

  char *p = snprintullong2(bufend, (uintptr_t)x, 16, kHexDigits);
  p -= 2;
  p[0] = '0';
  p[1] = 'x';
  return snprintstr(stream, p, info->width, info->prec, info->left, info->pad);
#undef bufend
}

int fmt_s(FILE *stream, const struct printf_info *info, const void *const *args) {
  const char *s = (const char*)args[0];
  return snprintstr(stream, s, info->width, info->prec, info->left, ' ');
}

int fmt_c(FILE *stream, const struct printf_info *info, const void *const *args) {
  int c = *(int*)args[0];
  _fputc(c, stream);
  return 1;
}

int fmt_percent(FILE *stream, const struct printf_info *info, const void *const *args) {
  _fputc('%', stream);
  return 1;
}

#ifndef __NO_FLONUM
int fmt_g(FILE *stream, const struct printf_info *info, const void *const *args) {
  long double x;
  switch (info->nlong) {
  case 0:  x = *(double*)args[0]; break;
  default: x = *(long double*)args[0]; break;
  }
  if (info->spec == 'f')
    return printfloat(stream, x, info->width, info->prec, info->showsign, info->left, info->pad);
  else
    return printscientific(stream, x, info->width, info->prec, info->showsign, info->left, info->pad);
}
#define fmt_f  fmt_g
#endif

static printf_function table[('z' + 1) - 'A'] = {
  ['d' - 'A'] = fmt_d,
  ['u' - 'A'] = fmt_u,
  ['x' - 'A'] = fmt_x,
  ['X' - 'A'] = fmt_X,
  ['p' - 'A'] = fmt_p,
  ['s' - 'A'] = fmt_s,
  ['c' - 'A'] = fmt_c,
  // ['%' - 'A'] = fmt_percent,
#ifndef __NO_FLONUM
  ['f' - 'A'] = fmt_f,
  ['g' - 'A'] = fmt_g,
#endif
};

int vfprintf(FILE *fp, const char *fmt_, va_list ap) {
  (void)table;

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

    bool sizez = false;
    // Handle '%'
    struct printf_info info;
    info.pad = ' ';
    info.width = 0;
    info.prec = 0;
    info.left = false;
    info.showsign = false;
    info.nlong = 0;

    c = fmt[++i];

    if (c == '+') {
      info.showsign = true;
      c = fmt[++i];
    } else if (c == '-') {
      info.left = true;
      c = fmt[++i];
    }
    if (c == '0') {
      info.pad = '0';
      c = fmt[++i];
    }
    if (c >= '1' && c <= '9') {
      int width = c - '0';
      while (c = fmt[++i], c >= '0' && c <= '9')
        width = width * 10 + (c - '0');
      info.width = width;
    } else if (c == '*') {
      info.width = va_arg(ap, int);
      c = fmt[++i];
    }
    if (c == '.') {
      c = fmt[++i];
      if (isdigit(c)) {
        int prec = 0;
        do {
          prec = prec * 10 + (c - '0');
        } while (isdigit(c = fmt[++i]));
        info.prec = prec;
      } else if (c == '*') {
        info.prec = va_arg(ap, int);
        c = fmt[++i];
      }
    }

    if (tolower(c) == 'l') {
      info.nlong = 1;
      c = fmt[++i];
      if (tolower(c) == 'l') {
        info.nlong = 2;
        c = fmt[++i];
      }
    } else if (c == 'z') {
      sizez = true;
      c = fmt[++i];
    }

    info.spec = c;
    switch (c) {
    case 'd':
      {
        void *args[1];
        union {
          int i;
          long l;
          long long ll;
        } x;
        if (sizez) {
          x.l = va_arg(ap, ssize_t);
          info.nlong = 1;
          args[0] = &x.l;
        } else {
          switch (info.nlong) {
          case 0:   x.i = va_arg(ap, int); args[0] = &x.i; break;
          case 1:   x.l = va_arg(ap, long); args[0] = &x.l; break;
          default:  x.ll = va_arg(ap, long long); args[0] = &x.ll; break;  // case 2:
          }
        }
        o += fmt_d(fp, &info, (const void**)args);
      }
      break;
    case 'u':
      {
        void *args[1];
        union {
          unsigned int i;
          unsigned long l;
          unsigned long long ll;
        } x;
        if (sizez) {
          x.l = va_arg(ap, size_t);
          info.nlong = 1;
          args[0] = &x.l;
        } else {
          switch (info.nlong) {
          case 0:   x.i = va_arg(ap, unsigned int); args[0] = &x.i; break;
          case 1:   x.l = va_arg(ap, unsigned long); args[0] = &x.l; break;
          default:  x.ll = va_arg(ap, unsigned long long); args[0] = &x.ll; break;  // case 2:
          }
        }
        o += fmt_u(fp, &info, (const void**)args);
      }
      break;
    case 'x': case 'X':
      {
        void *args[1];
        union {
          unsigned int i;
          unsigned long l;
          unsigned long long ll;
        } x;
        if (sizez) {
          x.l = va_arg(ap, size_t);
          info.nlong = 1;
          args[0] = &x.l;
        } else {
          switch (info.nlong) {
          case 0:   x.i = va_arg(ap, unsigned int); args[0] = &x.i; break;
          case 1:   x.l = va_arg(ap, unsigned long); args[0] = &x.l; break;
          default:  x.ll = va_arg(ap, unsigned long long); args[0] = &x.ll; break;  // case 2:
          }
        }
        o += fmt_x(fp, &info, (const void**)args);
      }
      break;
    case 'p':
      {
        void *ptr = va_arg(ap, void*);
        void *args[1];
        args[0] = &ptr;
        o += fmt_p(fp, &info, (const void**)args);
      }
      break;
    case 's':
      {
        const char *s = va_arg(ap, const char*);
        if (s == NULL)
          s = "(null)";
        void *args[1];
        args[0] = (void*)s;
        o += fmt_s(fp, &info, (const void**)args);
      }
      break;
    case 'c':
      {
        int c = va_arg(ap, unsigned int);
        void *args[1];
        args[0] = &c;
        o += fmt_c(fp, &info, (const void**)args);
      }
      break;
    // case '%':
    //   o += fmt_percent(fp, &info, NULL);
    //   break;
#ifndef __NO_FLONUM
    case 'f':
    case 'g':
      {
        void *args[1];
        union {
          double d;
          long double ld;
        } x;
        switch (info.nlong) {
        case 0:   x.d = va_arg(ap, double); args[0] = &x.d; break;
        default:  x.ld = va_arg(ap, long double); args[0] = &x.ld; break;
        }
        o += fmt_f(fp, &info, (const void**)args);
      }
      break;
#endif
    default:
      _fputc('%', fp);
      ++o;
      if (c != '%') {
        // Unknown % sequence.  Print it to draw attention.
        if (c != '\0') {
          _fputc(c, fp);
          ++o;
        }
      }
      break;
    }
  }

  return o;
}

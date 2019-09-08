#include "sprintf.h"

#include "stdarg.h"
#include "stdint.h"  // uintptr_t
//#include "stdio.h"
#include "string.h"

#define NULL  ((void*)0)
#define MIN(a, b)  ((a) < (b) ? (a) : (b))

static char kHexDigits[] = "0123456789abcdef";

static int
putstr(char *out, int o, int n, const char *s)
{
  while (*s != '\0' && o < n)
    out[o++] = *s++;
  return o;
}

static int
putpadding(char *out, int o, int n, int m, char padding)
{
  if (m > n - o)
    m = n - o;
  for (; m > 0; --m)
    out[o++] = padding;
  return o;
}

// Output is not '\0' terminated.
static int
snprintuint(char *out, unsigned int n, unsigned int x,
            int base, int order, int padding)
{
  const char* digits = kHexDigits;
  char buf[16];
  int i, o;

  i = 0;
  do{
    buf[i++] = digits[x % base];
    x /= base;
  }while(x != 0);

  if (i < order) {
    memset(buf + i, padding, order - i);
    i = order;
  }

  for (o = 0; --i >= 0 && o < n; ++o)
    out[o] = buf[i];

  return o;
}

static int
snprintulong(char *out, unsigned int n, unsigned long x,
             int base, int order, int padding)
{
  const char* digits = kHexDigits;
  char buf[32];
  int i, o;

  i = 0;
  do{
    buf[i++] = digits[x % base];
    x /= base;
  }while(x != 0);

  if (i < order) {
    memset(buf + i, padding, order - i);
    i = order;
  }

  for (o = 0; --i >= 0 && o < n; ++o)
    out[o] = buf[i];

  return o;
}

static int
snprintstr(char *out, unsigned int n, const char* s,
           int order, int suborder, int leftalign)
{
  int o = 0;
  if(s == NULL)
    s = "(null)";
  int len = strlen(s);
  if (suborder > 0)
    len = MIN(len, suborder);
  if (order <= 0 || len >= order) {
    o = putstr(out, o, MIN(n, o + len), s);
  } else {
    if (leftalign) {
      o = putstr(out, o, MIN(n, o + len), s);
      o = putpadding(out, o, n, order - len, ' ');
    } else {
      o = putpadding(out, o, n, order - len, ' ');
      o = putstr(out, o, MIN(n, o + len), s);
    }
  }
  return o;
}

static int
sprintsign(char *out, int negative, int force, int *porder)
{
  int o = 0;
  if (negative) {
    out[o++] = '-';
  } else if (force) {
    out[o++] = '+';
  }
  if (*porder > 1 && o > 0)
    *porder -= o;
  return o;
}

// Only understands %d, %x, %X, %p, %s, %c, %f and "+-0~9".
// '\0' is not put at the end if the buffer is smaller than output.
size_t
vsnprintf(char *out, size_t n, const char *fmt_, va_list ap)
{
  const unsigned char *fmt = (const unsigned char*)fmt_;
  int c, i;
  int o;

  for(i = o = 0; fmt[i] != '\0' && o < n; i++){
    c = fmt[i];
    if(c != '%'){
      out[o++] = c;
      continue;
    }

    // Handle '%'
    char padding = ' ';
    int order = 0, suborder = 0;
    int sign = 0;
    int leftalign = 0;
    int bLong = 0;
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
      while ((c = fmt[++i]) >= '0' && c <= '9')
        order = order * 10 + (c - '0');
    }
    if (c == '.') {
      while ((c = fmt[++i]) >= '0' && c <= '9') {
        suborder = suborder * 10 + (c - '0');
      }
    }

    if(c == 'l'){
      bLong = 1;
      c = fmt[++i];
    }
    if(c == 'd'){
      if (bLong) {
        long x = va_arg(ap, long);
        o += sprintsign(out + o, x < 0, sign, &order);
        unsigned long ux = x < 0 ? -x : x;
        o += snprintulong(out + o, n - o, ux, 10, order, padding);
      } else {
        int x = va_arg(ap, int);
        o += sprintsign(out + o, x < 0, sign, &order);
        unsigned int ux = x < 0 ? -x : x;
        o += snprintuint(out + o, n - o, ux, 10, order, padding);
      }
    } else if(c == 'x') {
      o += snprintuint(out + o, n - o, va_arg(ap, int), 16,
                       order, padding);
    } else if(c == 'p') {
      o += snprintstr(out + o, n - o, "0x", 0, 0, 0);
      o += snprintulong(out + o, n - o, (uintptr_t)va_arg(ap, void*), 16,
                        order - 2, '0');
    } else if(c == 's'){
      // ("%5", "foo")         = "  foo"
      // ("%-5", "foo")        = "foo  "
      // ("%5", "foobarbaz")   = "foobarbaz"
      // ("%.3", "foobarbaz")  = "foo"
      // ("%5.7", "foobarbaz") = "foobarb"
      // ("%5.3", "foobarbaz") = "  foo"

      const char *s = va_arg(ap, const char*);
      o += snprintstr(out + o, n - o, s, order, suborder, leftalign);
    } else if(c == 'c'){
      out[o++] = va_arg(ap, unsigned int);
    } else if(c == '%'){
      out[o++] = c;
    } else {
      // Unknown % sequence.  Print it to draw attention.
      out[o++] = '%';
      if (o >= n)
        break;
      if (c != '\0')
        out[o++] = c;
    }
  }

  if (o < n)
    out[o] = '\0';
  return o;
}

size_t
snprintf(char *out, size_t n, const char *fmt, ...)
{
  va_list ap;
  int len;
  va_start(ap, fmt);
  len = vsnprintf(out, n, fmt, ap);
  va_end(ap);
  return len;
}

size_t
sprintf(char *out, const char *fmt, ...)
{
  va_list ap;
  int len;
  va_start(ap, fmt);
  //len = vsnprintf(out, (size_t)-1, fmt, ap);
  len = vsnprintf(out, 0x7fffffff, fmt, ap);  // unsigned がまだ扱えないため。longにできないのは不明。
  va_end(ap);
  return len;
}

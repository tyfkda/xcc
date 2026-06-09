#include "stdio.h"
#include "ctype.h"
#include "stdarg.h"
#include "stdbool.h"
#include "stdint.h"  // SIZE_MAX
#include "string.h"

#ifndef __NO_FLONUM
#include "math.h"
#endif

typedef struct {
  va_list ap;
} VaList;

// ctypes ones might not use as function pointer, so implement them.

static int fn_isdigit(int c) {
  return '0' <= c && c <= '9';
}
static int fn_isxdigit(int c) {
  return ('0' <= c && c <= '9') || ('A' <= c && c <= 'F') || ('a' <= c && c <= 'f');
}

static int fn_isoctal(int c) {
  return '0' <= c && c <= '7';
}

static bool read_llong(FILE *fp, int base, long long *p) {
  int c = fgetc(fp);
  bool negative = false;
  switch (c) {
  case '-':
    negative = true;
    // Fallthrough.
  case '+':
    c = fgetc(fp);
    break;
  default:
    break;
  }

  int (*isvalid)(int) = base == 16 ? fn_isxdigit : base == 8 ? fn_isoctal : fn_isdigit;

  bool result = false;
  if (isvalid(c)) {
    long long x = 0;
    do {
      int v = 0;
      c = toupper(c);
      if ('0' <= c && c <= '9')
        v = c - '0';
      else /*if ('A' <= c && c <= 'F')*/
        v = c - ('A' - 10);
      x = x * base + v;
      c = fgetc(fp);
    } while (isvalid(c));
    *p = negative ? -x : x;
    result = true;
  }
  ungetc(c, fp);
  return result;
}

static bool read_store_llong(FILE *fp, int base, int long_count, VaList *va) {
  long long x;
  if (!read_llong(fp, base, &x))
    return false;

  switch (long_count) {
  case 0:  { int *p = va_arg(va->ap, int*); *p = x; } break;
  case 1:  { long *p = va_arg(va->ap, long*); *p = x; } break;
  default:  { long long *p = va_arg(va->ap, long long*); *p = x; } break;
  }
  return true;
}

#ifndef __NO_FLONUM
static long double ipow(long double base, long x) {
  long double result = 1;
  long double a = base;
  for (; x > 0; x >>= 1, a *= a) {
    if ((x & 1) != 0)
      result *= a;
  }
  return result;
}

static bool read_ldouble(FILE *fp, long double *p) {
  int c = fgetc(fp);
  bool negative = false;
  switch (c) {
  case '-':
    negative = true;
    // Fallthrough.
  case '+':
    c = fgetc(fp);
    break;
  default:
    break;
  }

  bool result = false;
  long double ld;
  switch (toupper(c)) {
  case 'I':  // inf?
    if (toupper(c = fgetc(fp)) == 'N' && toupper(c = fgetc(fp)) == 'F') {
      ld = INFINITY;
      result = true;
      c = EOF;
    }
    break;
  case 'N':  // nan?
    if (toupper(c = fgetc(fp)) == 'A' && toupper(c = fgetc(fp)) == 'N') {
      ld = NAN;
      result = true;
      c = EOF;
    }
    break;

  default:
    {
      long long x = 0, y = 0;
      int suborder = 0;
      if (isdigit(c)) {
        do {
          x = x * 10 + (c - '0');
          c = fgetc(fp);
        } while (isdigit(c));
        result = true;
      }
      if (c == '.') {
        for (;;) {
          c = fgetc(fp);
          if (!isdigit(c))
            break;
          y = y * 10 + (c - '0');
          ++suborder;
        }
        result = true;
      }
      if (result) {
        long long order = 0;
        if (toupper(c) == 'E') {
          if (!read_llong(fp, 10, &order))
            break;
          c = fgetc(fp);
        }

        ld = x;
        if (suborder > 0)
          ld += (long double)y / ipow(10, suborder);
        if (order > 0)
          ld *= ipow(10, order);
        else if (order < 0)
          ld /= ipow(10, -order);
      }
    }
    break;
  }

  if (result)
    *p = negative ? -ld : ld;
  if (c != EOF)
    ungetc(c, fp);
  return result;
}

static bool read_store_float(FILE *fp, int long_count, VaList *va) {
  long double x;
  if (!read_ldouble(fp, &x))
    return false;

  switch (long_count) {
  case 0:  { float *p = va_arg(va->ap, float*); *p = x; } break;
  case 1:  { double *p = va_arg(va->ap, double*); *p = x; } break;
  default:  { long double *p = va_arg(va->ap, long double*); *p = x; } break;
  }
  return true;
}
#endif

static bool read_store_string(FILE *fp, int order, VaList *va) {
  char *p = va_arg(va->ap, char*);
  size_t n = order > 0 ? order : SIZE_MAX;
  for (size_t i = 0; i < n; ++i) {
    int c = fgetc(fp);
    if (c == '\0') {
      if (i == 0)
        return false;
      break;
    }
    if (isspace(c)) {
      ungetc(c, fp);
      break;
    }
    *p++ = c;
  }
  *p = '\0';
  return true;
}

int vfscanf(FILE *restrict fp, const char *restrict format, va_list ap) {
  int count = 0;
  int c;
  VaList va;
  va_copy(va.ap, ap);
  for (const unsigned char *p = (unsigned char*)format; (c = *p) != '\0'; ++p) {
    if ((c == '%' && p[1] != '%') || isspace(c)) {
      int c2;
      while (c2 = fgetc(fp), isspace(c2))
        ;
      ungetc(c2, fp);
      if (c != '%')
        continue;
    }

    if (c == '%') {
      c = *(++p);
      if (c != '%') {
        int order = 0;
        if (isdigit(c)) {
          order = c - '0';
          while (c = *(++p), isdigit(c))
            order = order * 10 + (c - '0');
        }

        int long_count = 0;
        for (;; c = *(++p)) {
          if (c == 'l')
            ++long_count;
          else if (c == 'L')
            long_count += 2;
          else
            break;
        }

        switch (c) {
        case 'u':
          // No special handling for unsigned?
          // Fallthrough.
        case 'd': case 'i':
          if (read_store_llong(fp, 10, long_count, &va)) {
            ++count;
            continue;
          }
          break;
        case 'x':
          if (read_store_llong(fp, 16, long_count, &va)) {
            ++count;
            continue;
          }
          break;
        case 'o':
          if (read_store_llong(fp, 8, long_count, &va)) {
            ++count;
            continue;
          }
          break;
#ifndef __NO_FLONUM
        case 'f':
          if (read_store_float(fp, long_count, &va)) {
            ++count;
            continue;
          }
          break;
#endif
        case 's':
          if (read_store_string(fp, order, &va)) {
            ++count;
            continue;
          }
          break;
        default:  // Unsupported.
          break;
        }
        // Error.
        return count;
      }
    }

    int in = fgetc(fp);
    if (c != in) {
      ungetc(in, fp);
      break;
    }
  }
  return count;
}

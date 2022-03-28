#if !defined(__NO_FLONUM) && !defined(__WASM)
#include <math.h>
#include <stdbool.h>
#include <stdint.h>  // int64_t

static const double LOG2 = 0.693147180559945;
static const double _M_2PI = 2 * M_PI;
static const double TAN225 = 0.414213562373095;  // tan(22.5 degree)

static const int EXP_BIT = 11;
static const int EXP_POS = 52;
static const int EXP_BIOS = 1022;

int isfinite(double x) {
  int64_t *q = (int64_t*)&x;
  int e = ((int)((*q) >> EXP_POS)) & ((1 << EXP_BIT) - 1);
  return e != (1 << EXP_BIT) - 1;
}

int isnan(double x) {
  int64_t *q = (int64_t*)&x;
  int e = ((int)((*q) >> (EXP_POS - 1))) & ((1 << (EXP_BIT + 1)) - 1);
  return e == (1 << (EXP_BIT + 1)) - 1;
}

int isinf(double x) {
  int64_t *q = (int64_t*)&x;
  int e = ((int)((*q) >> (EXP_POS - 1))) & ((1 << (EXP_BIT + 1)) - 1);
  return e == (((1 << EXP_BIT) - 1) << 1);
}

static double normalize_radian(double x) {
  double y = fmod(x, _M_2PI);
  if (y > M_PI)
    return y - _M_2PI;
  if (y < -M_PI)
    return y + _M_2PI;
  return y;
}

double sin(double x) {
  static const double TABLE[] = {
     1 / 1.0,
    -1 / (3.0 * 2 * 1),
     1 / (5.0 * 4 * 3 * 2 * 1),
    -1 / (7.0 * 6 * 5 * 4 * 3 * 2 * 1),
     1 / (9.0 * 8 * 7 * 6 * 5 * 4 * 3 * 2 * 1),
    -1 / (11.0 * 10 * 9 * 8 * 7 * 6 * 5 * 4 * 3 * 2 * 1),
     1 / (13.0 * 12 * 11 * 10 * 9 * 8 * 7 * 6 * 5 * 4 * 3 * 2 * 1),
    -1 / (15.0 * 14 * 13 * 12 * 11 * 10 * 9 * 8 * 7 * 6 * 5 * 4 * 3 * 2 * 1),
     1 / (17.0 * 16 * 15 * 14 * 13 * 12 * 11 * 10 * 9 * 8 * 7 * 6 * 5 * 4 * 3 * 2 * 1),
    -1 / (19.0 * 18 * 17 * 16 * 15 * 14 * 13 * 12 * 11 * 10 * 9 * 8 * 7 * 6 * 5 * 4 * 3 * 2 * 1),
  };

  x = normalize_radian(x);

  double v = 0;
  double xx = x * x;
  for (int i = 0; i < sizeof(TABLE) / sizeof(*TABLE); ++i) {
    v += x * TABLE[i];
    x *= xx;
  }
  return v;
}

double cos(double x) {
  static const double TABLE[] = {
     1 / 1.0,
    -1 / (2.0 * 1),
     1 / (4.0 * 3 * 2 * 1),
    -1 / (6.0 * 5 * 4 * 3 * 2 * 1),
     1 / (8.0 * 7 * 6 * 5 * 4 * 3 * 2 * 1),
    -1 / (10.0 * 9 * 8 * 7 * 6 * 5 * 4 * 3 * 2 * 1),
     1 / (12.0 * 11 * 10 * 9 * 8 * 7 * 6 * 5 * 4 * 3 * 2 * 1),
    -1 / (14.0 * 13 * 12 * 11 * 10 * 9 * 8 * 7 * 6 * 5 * 4 * 3 * 2 * 1),
     1 / (16.0 * 15 * 14 * 13 * 12 * 11 * 10 * 9 * 8 * 7 * 6 * 5 * 4 * 3 * 2 * 1),
    -1 / (18.0 * 17 * 16 * 15 * 14 * 13 * 12 * 11 * 10 * 9 * 8 * 7 * 6 * 5 * 4 * 3 * 2 * 1),
  };

  x = normalize_radian(x);

  double v = 0;
  double xx = x * x;
  x = 1;
  for (int i = 0; i < sizeof(TABLE) / sizeof(*TABLE); ++i) {
     v += x * TABLE[i];
    x *= xx;
  }
  return v;
}

double tan(double x) {
  return sin(x) / cos(x);  // TODO;
}

double atan(double x) {
  bool neg = x < 0;
  x = fabs(x);
  bool inv = false;
  if (x > 1) {
    x = 1.0 / x;
    inv = true;
  }
  bool diag = false;
  if (x > TAN225) {
    x = (1 - x) / (1 + x);
    diag = true;
  }

  double t = 0;
  {
    double _xx = -x * x;
    double k = x;
    for (int i = 0; i < 16; ++i) {
      t += k / (i * 2 + 1);
      k *= _xx;
    }
  }

  if (diag)
    t = (M_PI / 4) - t;
  if (inv)
    t = (M_PI / 2) - t;
  return neg ? -t : t;
}

double sqrt(double _) {
  __asm("sqrtsd %xmm0, %xmm0");
}

double frexp(double x, int *p) {
  if (x == 0 || !isfinite(x)) {
    *p = 0;
    return x;
  }
  int64_t *q = (int64_t*)&x;
  int e = ((int)((*q) >> EXP_POS)) & ((1 << EXP_BIT) - 1);
  *p = e - EXP_BIOS;
  *q = (*q & ~((((int64_t)1 << EXP_BIT) - 1) << EXP_POS)) | ((int64_t)EXP_BIOS << EXP_POS);
  return x;
}

double log(double x) {
  if (x <= 0)
    return x < 0 ? NAN : -HUGE_VAL;
  if (!isfinite(x))
    return x;

  int n;
  x = frexp(x, &n);
  // 0.5 <= x < 1.0

  double y = (x - 1) / (x + 1);
  double yy = y * y;
  double total = 0;
  for (int i = 1; i <= 15; ++i) {
    total += y / (i * 2 - 1);
    y *= yy;
  }
  return 2 * total + n * LOG2;
}

double exp(double x) {
  bool neg = x < 0;
  x = fabs(x);
  unsigned int n = x;  // TODO: Care about overflow
  double r = (x - n) /* * log(base)*/;   // log(e) = 1

  double result = 1;
  double y = 1;
  for (int i = 1; i <= 15; ++i) {
    y *= r / i;
    result += y;
  }

  double poe = M_E;
  for (; n > 0; n >>= 1, poe *= poe) {
    if (n & 1)
      result *= poe;
  }
  return neg ? 1.0 / result : result;
}

double pow(double base, double x) {
  bool neg = x < 0;
  x = fabs(x);
  unsigned int n = x;  // TODO: Care about overflow
  double r = (x - n) * log(base);

  double result = 1;
  double y = 1;
  for (int i = 1; i <= 15; ++i) {
    y *= r / i;
    result += y;
  }

  double poe = base;
  for (; n > 0; n >>= 1, poe *= poe) {
    if (n & 1)
      result *= poe;
  }
  return neg ? 1.0 / result : result;
}

double fabs(double x) {
  return x >= 0 ? x : -x;
}

double floor(double x) {
  // TODO:
  if (x >= 0) {
    unsigned long l = x;
    return l;
  } else {
    unsigned long l = -x;
    return x + l < 0 ? -l - 1 : l;
  }
}

double ceil(double x) {
  // TODO:
  return -floor(-x);
}

double fmod(double x, double m) {
  m = fabs(m);
  if (x >= 0)
    return x - floor(x / m) * m;
  else
    return x - ceil(x / m) * m;
}

double drand48(void) {
  static unsigned int X = 1;
  static const unsigned int A = 214013;
  static const unsigned int C = 2531011;
  X = X * A + C;
  return (X >> 16) / 65535.0;
}

double erand48(unsigned short xsubi[3]) {
  return drand48();
}
#endif

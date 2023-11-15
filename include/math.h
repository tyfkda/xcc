#pragma once

#ifndef __NO_FLONUM
#define M_PI        (3.14159265358979323846)
#define M_PI_2      (1.57079632679489661923)
#define M_PI_4      (0.78539816339744830962)
#define M_1_PI      (1.0 / M_PI)
#define M_2_PI      (2.0 / M_PI)
#define M_2_SQRTPI  (1.128379167095513)
#define M_SQRT2     (1.414213562373095)
#define M_SQRT1_2   (0.707106781186548)
#define M_E         (2.718281828459045)
#define M_LOG2E     (1.442695040888963)
#define M_LOG10E    (0.434294481903252)
#define M_LN2       (0.693147180559945)
#define M_LN10      (2.302585092994046)
#define NAN         (0.0 / 0.0)
#define HUGE_VAL    (1.0 / 0.0)

double sin(double);
double cos(double);
double tan(double);
double asin(double);
double acos(double);
double atan(double);
double atan2(double y, double x);
double sinh(double);
double cosh(double);
double tanh(double);
double asinh(double);
double acosh(double);
double atanh(double);
double sqrt(double);
double log(double x);
double log10(double x);
double exp(double x);
double pow(double base, double x);
double fabs(double);
double floor(double);
double ceil(double x);
double round(double x);
double modf(double x, double *pint);
double fmod(double x, double m);
double frexp(double x, int *p);
double copysign(double x, double f);

int isfinite(double x);
int isnan(double x);
int isinf(double x);

#if defined(__APPLE__) || defined(__GNUC__) || defined(__riscv)
// isfinite, isinf and isnan is defined by macro and not included in lib file,
// so it will be link error.
#include <stdint.h>
#define isfinite(x)  ({ \
  const int64_t __mask = ((((int64_t)1 << 11) - 1) << 52); \
  double __tmp = (x); \
  int64_t __q = *(int64_t*)&__tmp; \
  (__q & __mask) != __mask; \
})

#define isinf(x)  ({ \
  const int64_t __mask = ((((int64_t)1 << 11) - 1) << 52); \
  const int64_t __mask2 = ((((int64_t)1 << 12) - 1) << 51); \
  double __tmp = (x); \
  int64_t __q = *(int64_t*)&__tmp; \
  (__q & __mask2) == __mask; \
})

#define isnan(x)  ({ \
  const int64_t __mask2 = ((((int64_t)1 << 12) - 1) << 51); \
  double __tmp = (x); \
  int64_t __q = *(int64_t*)&__tmp; \
  (__q & __mask2) == __mask2; \
})

#endif

#endif

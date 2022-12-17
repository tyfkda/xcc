#pragma once

#ifndef __NO_FLONUM
#define M_PI      (3.14159265358979323846)
#define M_E       (2.718281828459045)
#define NAN       (0.0 / 0.0)
#define HUGE_VAL  (1.0 / 0.0)

double sin(double);
double cos(double);
double tan(double);
double atan(double);
double sqrt(double);
double log(double x);
double exp(double x);
double pow(double base, double x);
double fabs(double);
double floor(double);
double ceil(double x);
double fmod(double x, double m);
double frexp(double x, int *p);

int isfinite(double x);
int isnan(double x);
int isinf(double x);

#if defined(__APPLE__)
// isfinite, isinf and isnan is defined by macro and not included in lib file,
// so it will be link error.
#include "stdint.h"
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

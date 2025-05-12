#include <math.h>

#ifndef __NO_FLONUM
#include <float.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

#include "../math/_ieee.h"
#include "../../tests/xtest.h"

#define EXPECT(expected, actual)  EXPECT_DEQ(expected, actual)
#define EXPECT_EQ_D64(expected, actual)  EXPECT_TRUE(equal_d64(expected, actual))

static bool equal_d64(double a, double b) {
  union u { double d; int64_t u; };
  union u ua, ub;
  ua.d = a;
  ub.d = b;
  return ua.u == ub.u;
}

TEST(misc) {
  EXPECT_NEAR(M_SQRT2, sqrt(2.0));
  EXPECT_NEAR(0.0, sqrt(0.0));
  EXPECT_NAN(sqrt(-1.0));
  EXPECT_NEAR(HUGE_VAL, sqrt(HUGE_VAL));

  EXPECT_NEAR(0.5, cos(M_PI / 3));
  EXPECT_NEAR(-0.8660254, sin(-M_PI / 3));
  EXPECT_NEAR(-0.267949, tan(2 * 11 * M_PI / 24));
  EXPECT_NEAR(1.107148, atan(2));
  EXPECT_NEAR(2.034443, atan2(2, -1));
  EXPECT_NEAR(0.0, atan2(0, 0));
  EXPECT_NEAR(1.0, log(M_E));
  EXPECT_NEAR(M_E, exp(1.0));
  EXPECT_NEAR(1.858729, pow(1.2, 3.4));
  EXPECT_NEAR( 1.14, fmod( 12.34,  5.6));
  EXPECT_NEAR( 1.14, fmod( 12.34, -5.6));
  EXPECT_NEAR(-1.14, fmod(-12.34,  5.6));
  EXPECT_NEAR(-1.14, fmod(-12.34, -5.6));
}

TEST(fabs) {
  EXPECT(1.23, fabs(1.23));
  EXPECT(4.56, fabs(-4.56));

  EXPECT_EQ_D64(0.0, fabs(0.0));
  EXPECT_EQ_D64(0.0, fabs(-0.0));
  EXPECT_EQ_D64(HUGE_VAL, fabs(-HUGE_VAL));
  EXPECT_EQ_D64(NAN, fabs(-NAN));
}

TEST(floor) {
  EXPECT(1.0, floor(1.999999));
  EXPECT(0.0, floor(0.999999));
  EXPECT(0.0, floor(-0.0));
  EXPECT(1999.0, floor(1999.999999));
  EXPECT(123.0, floor(123.0));
  EXPECT(-2.0, floor(-1.000001));
  EXPECT(-1.0, floor(-1.0));
  EXPECT(-2000.0, floor(-1999.999999));

  EXPECT(0.0, floor(0.5));
  EXPECT(-1.0, floor(-0.5));

  int64_t ONE = (int64_t)1 << FRAC_BIT;
  EXPECT((double)(ONE / 2), floor((double)(ONE / 2) + 0.5));
  EXPECT((double)(ONE), floor((double)(ONE) + 0.5));
  EXPECT((double)-(ONE / 2) - 1, floor((double)-(ONE / 2) - 0.5));
  EXPECT((double)-(ONE), floor((double)-(ONE) - 0.5));  // Fraction is under precision, so floor function doesn't detect fraction.
}

TEST(ceil) {
  EXPECT(2.0, ceil(1.999999));
  EXPECT(1.0, ceil(0.999999));
  EXPECT(0.0, ceil(-0.0));
  EXPECT(2000.0, ceil(1999.999999));
  EXPECT(123.0, ceil(123.0));
  EXPECT(-1.0, ceil(-1.000001));
  EXPECT(-1.0, ceil(-1.0));
  EXPECT(-1999.0, ceil(-1999.999999));

  EXPECT(1.0, ceil(0.5));
  EXPECT(0.0, ceil(-0.5));

  int64_t ONE = (int64_t)1 << FRAC_BIT;
  EXPECT((double)(ONE / 2) + 1, ceil((double)(ONE / 2) + 0.5));
  EXPECT((double)(ONE), ceil((double)(ONE) + 0.5));  // Fraction is under precision, so ceil function doesn't detect fraction.
  EXPECT((double)-(ONE / 2), ceil((double)-(ONE / 2) - 0.5));
  EXPECT((double)-(ONE), ceil((double)-(ONE) - 0.5));
}

TEST(round) {
  EXPECT(2.0, round(1.999999));
  EXPECT(1.0, round(0.999999));
  EXPECT(0.0, round(-0.0));
  EXPECT(2000.0, round(1999.999999));
  EXPECT(123.0, round(123.0));
  EXPECT(-1.0, round(-1.000001));
  EXPECT(-1.0, round(-1.0));
  EXPECT(-2000.0, round(-1999.999999));

  EXPECT(1.0, round(0.5));
  EXPECT(-1.0, round(-0.5));

  int64_t ONE = (int64_t)1 << FRAC_BIT;
  EXPECT((double)(ONE / 2) + 1, round((double)(ONE / 2) + 0.5));
  EXPECT((double)(ONE), round((double)(ONE) + 0.5));  // Fraction is under precision, so round function doesn't detect fraction.
  EXPECT((double)-(ONE / 2) - 1, round((double)-(ONE / 2) - 0.5));
  EXPECT((double)-(ONE), round((double)-(ONE) - 0.5));
}

TEST(modf) {
  double i = 123;
  EXPECT(0.5, modf(1.5, &i));
  EXPECT(1.0, i);
  EXPECT(-0.25, modf(-1234.25, &i));
  EXPECT(-1234.0, i);

  EXPECT_NAN(modf(NAN, &i));
  EXPECT_NAN(i);
  EXPECT(0.0, modf(HUGE_VAL, &i));
  EXPECT(HUGE_VAL, i);
}

TEST(frexp) {
  int e;
  EXPECT(0.5, frexp(1.0, &e));
  EXPECT_EQ(1, e);
  EXPECT(0.0, frexp(0.0, &e));
  EXPECT_EQ(0, e);
  e = 1234;
  EXPECT(HUGE_VAL, frexp(HUGE_VAL, &e));
  EXPECT_EQ(0, e);
  e = 5678;
  EXPECT_NAN(frexp(NAN, &e));
  EXPECT_EQ(0, e);

  EXPECT((uint64_t)0x1fffffffffffff / (double)(1ULL << 53), frexp(DBL_MAX, &e));
  EXPECT_EQ(1024, e);
}

TEST(isfinite) {
  EXPECT_TRUE(isfinite(1.23));
  EXPECT_TRUE(isfinite(0.0));
  EXPECT_FALSE(isfinite(HUGE_VAL));
  EXPECT_FALSE(isfinite(NAN));
  EXPECT_TRUE(isfinite(DBL_MAX));
}

TEST(isinf) {
  EXPECT_TRUE(isinf(HUGE_VAL));
  EXPECT_FALSE(isinf(1.23));
  EXPECT_FALSE(isinf(0.0));
  EXPECT_FALSE(isinf(NAN));
  EXPECT_FALSE(isinf(DBL_MAX));
}

TEST(isnan) {
  EXPECT_TRUE(isnan(NAN));
  EXPECT_TRUE(isnan(copysign(NAN, -1)));
  EXPECT_FALSE(isnan(1.23));
  EXPECT_FALSE(isnan(0.0));
  EXPECT_FALSE(isnan(HUGE_VAL));
  EXPECT_FALSE(isnan(DBL_MAX));
}

TEST(copysign_signbit) {
  double pzero = 0.0;
  double nzero = -0.0;
  EXPECT_DEQ(123, copysign(-123, pzero));
  EXPECT_DEQ(-456, copysign(+456, nzero));
  EXPECT_EQ(1, signbit(copysign(pzero, -1)));
  EXPECT_EQ(0, signbit(copysign(nzero, +1)));

  EXPECT_EQ(0, signbit(NAN));
  double nnan = copysign(NAN, -1);
  EXPECT_EQ(1, signbit(nnan));
  EXPECT_EQ(0, signbit(copysign(nnan, +1)));
}

TEST(negative_zero) {
  double nzero = -0.0;
  EXPECT_TRUE(nzero == 0.0);
  EXPECT_EQ(1, signbit(nzero));
  EXPECT_EQ(-1.0, copysign(1.0, nzero));

  double inv = 1.0 / nzero;
  EXPECT_TRUE(isinf(inv));
  EXPECT_TRUE(inv < 0.0);

  EXPECT_NEAR(M_PI, atan2(0.0, nzero));
  EXPECT_NEAR(-M_PI, atan2(nzero, nzero));
  EXPECT_EQ_D64(nzero, atan2(nzero, 0.0));
}

XTEST_MAIN();
#else

#include <stdio.h>
int main() {
  printf("math_test: Skipped\n");
  return 0;
}
#endif

#include <math.h>

#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>

int error_count;

#define EXPECT(expected, actual)  expect(#actual, expected, actual)
#define EXPECT_ABOUT(expected, actual)  expect_about(#actual, expected, actual)
#define EXPECTI(expected, actual)  expecti(#actual, expected, actual)
#define EXPECT_TRUE(actual)   expecti(#actual, true, !!(actual))
#define EXPECT_FALSE(actual)  expecti(#actual, false, !!(actual))
#define EXPECT_NAN(actual)    expecti(#actual, true, !!isnan(actual))

void expect(const char *title, double expected, double actual) {
  printf("%s => ", title);

  if (expected != actual) {
    printf("ERR, %f expected, but got %f\n", expected, actual);
    ++error_count;
  } else {
    printf("OK\n");
  }
}

void expect_about(char *title, double expected, double actual) {
  printf("%s => ", title);
  double d = expected - actual;
  const double eps = (double)1e-5;
  if (d < -eps || d > eps) {
    printf("ERR, %f expected, but got %f\n", expected, actual);
    ++error_count;
  } else {
    printf("OK\n");
  }
}

void expecti(const char *title, int64_t expected, int64_t actual) {
  printf("%s => ", title);

  if (expected != actual) {
    printf("ERR, %" PRId64 " expected, but got %" PRId64 "\n", expected, actual);
    ++error_count;
  } else {
    printf("OK\n");
  }
}

void test_math(void) {
  EXPECT_ABOUT(1.41421356, sqrt(2.0));
  EXPECT_ABOUT(0.5, cos(M_PI / 3));
  EXPECT_ABOUT(-0.8660254, sin(-M_PI / 3));
  EXPECT_ABOUT(-0.267949, tan(2 * 11 * M_PI / 24));
  EXPECT_ABOUT(1.107148, atan(2));
  EXPECT_ABOUT(1.0, log(M_E));
  EXPECT_ABOUT(M_E, exp(1.0));
  EXPECT_ABOUT(1.858729, pow(1.2, 3.4));
  EXPECT(1.23, fabs(-1.23));
  EXPECT(1.0, floor(1.999999));
  EXPECT(-2.0, floor(-1.000001));
  EXPECT(2.0, ceil(1.000001));
  EXPECT(-1.0, ceil(-1.999999));
  EXPECT_ABOUT( 1.14, fmod( 12.34,  5.6));
  EXPECT_ABOUT( 1.14, fmod( 12.34, -5.6));
  EXPECT_ABOUT(-1.14, fmod(-12.34,  5.6));
  EXPECT_ABOUT(-1.14, fmod(-12.34, -5.6));
}

void test_frexp(void) {
  int e;
  EXPECT(0.5, frexp(1.0, &e));
  EXPECTI(1, e);
  EXPECT(0.0, frexp(0.0, &e));
  EXPECTI(0, e);
  e = 1234;
  EXPECT(HUGE_VAL, frexp(HUGE_VAL, &e));
  EXPECTI(1234, e);
  EXPECT_NAN(frexp(NAN, &e));
  EXPECTI(1234, e);
}

void test_isinf(void) {
  EXPECT_TRUE(isinf(HUGE_VAL));
  EXPECT_FALSE(isinf(1.23));
  EXPECT_FALSE(isinf(0.0));
  EXPECT_FALSE(isinf(NAN));
}

void test_isnan(void) {
  EXPECT_TRUE(isnan(NAN));
  EXPECT_FALSE(isnan(1.23));
  EXPECT_FALSE(isnan(0.0));
  EXPECT_FALSE(isnan(HUGE_VAL));
}

int main() {
  test_math();
  test_frexp();
  test_isinf();
  test_isnan();
  return error_count > 255 ? 255 : error_count;
}

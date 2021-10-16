#ifdef __NO_FLONUM
int main() {return 0;}
#else

#ifdef USE_SINGLE
typedef float Number;
#define NUMBER_TYPE_SIZE  (4)
#else
typedef double Number;
#define NUMBER_TYPE_SIZE  (8)

double mix_params(int i0, double d0, int i1, float f1, int i2, double d2, int i3, float f3) {
  return (i0 * i1 * i2 * i3) / (d0 * f1 * d2 * f3);
}

double mix_many_params(int n, int i1, double d1, int i2, double d2, int i3, double d3, int i4, double d4, int i5, double d5, int i6, double d6) {
  return i1 * d1 + i2 * d2 + i3 * d3 + i4 * d4 + i5 * d5 + i6 * d6;
}
#endif

#include "flotest.inc"

int main(void) {
  number_test();

#ifndef USE_SINGLE
  expect_about("mix_params", 0.2734375, mix_params(1, 2, 3, 4, 5, 6, 7, 8));
  expect_about("mix_many_params", 322.0, mix_many_params(20, 1, 2.0, 3, 4.0f, 5, 6.0, 7, 8.0f, 9, 10.0, 11, 12.0f));

  // math.h

  expect_about("sqrt2", 1.41421356, sqrt(2.0));
  expect_about("cos", 0.5, cos(M_PI / 3));
  expect_about("sin", -0.8660254, sin(-M_PI / 3));
  expect_about("tan", -0.267949, tan(2 * 11 * M_PI / 24));
  expect_about("atan", 1.107148, atan(2));
  expect_about("log", 1.0, log(M_E));
  expect_about("exp", M_E, exp(1.0));
  expect_about("pow", 1.858729, pow(1.2, 3.4));
  expect_about("fabs", 1.23, fabs(-1.23));
  expect("floor+", 1.0, floor(1.999999));
  expect("floor-", -2.0, floor(-1.000001));
  expect("ceil+", 2.0, ceil(1.000001));
  expect("ceil-", -1.0, ceil(-1.999999));
  expect_about("fmod++",  1.14, fmod( 12.34,  5.6));
  expect_about("fmod+-",  1.14, fmod( 12.34, -5.6));
  expect_about("fmod-+", -1.14, fmod(-12.34,  5.6));
  expect_about("fmod--", -1.14, fmod(-12.34, -5.6));

  {
    float a=12.34f;
    a+=56.78;
    expect_about("float+=double", 69.12f, a);
  }
#endif

  return 0;
}
#endif

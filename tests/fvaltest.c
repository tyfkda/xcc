#ifdef __NO_FLONUM
int main() {return 0;}
#else

#include "stdbool.h"
#include "stdlib.h"  // exit
#include "math.h"
#include "../examples/util.h"

void expect(char *title, double expected, double actual) {
  puts(title);
  puts(" => ");
  if (expected == actual) {
    puts("OK\n");
    return;
  }
  puts("NG: ");
  putdeci(expected);
  puts(" expected, but got ");
  putdeci(actual);
  puts("\n");
  exit(1);
}

void expect_about(char *title, double expected, double actual) {
  puts(title);
  puts(" => ");
  double d = expected - actual;
  const double eps = 1e-6;
  if (d > -eps && d < eps) {
    puts("OK\n");
    return;
  }
  puts("NG: ");
  putdeci(expected);
  puts(" expected, but got ");
  putdeci(actual);
  puts("\n");
  exit(1);
}

void expecti(char *title, int expected, int actual) {
  puts(title);
  puts(" => ");
  if (expected == actual) {
    puts("OK\n");
    return;
  }
  puts("NG: ");
  putdeci(expected);
  puts(" expected, but got ");
  putdeci(actual);
  puts("\n");
  exit(1);
}

void fail(char *title) {
  puts(title);
  puts(" => NG\n");
  exit(1);
}

double mix_params(int i0, double d0, int i1, double d1, int i2, double d2, int i3, double d3) {
  return (i0 * i1 * i2 * i3) / (d0 * d1 * d2 * d3);
}

double Empty;

int main(void) {
  double x, y;
  expect("w/o initializer", 0.0, Empty);
  expect("zero", 0.0, 0);
  expect("decimal", 42.0, 42);
  expect("+-", 21, (x=5, x+20-4));
  expect("*/", 7.5, (x=5, x*3/2));
  expect("unary -", -3.69, (x=3.69, -x));
  expect("pre inc", 11, (x=10, ++x));
  {
    expect("post dec", 10, (x=10, x--));
    expect("post dec after", 9, x);
  }
  {
    static double g;
    expect("pre dec g", 9.5, (g=10.5, --g));
  }
  {
    static double g;
    expect("post inc g", 10.25, (g=10.25, g++));
    expect("post inc after", 11.25, g);
  }

  expecti("!", false, (x=5, !x));
  expecti("||", true, (x=0.0, y=5.0, x || y));

  expect("sizeof(double)", 8, sizeof(double));
  expect("sizeof(float)", 4, sizeof(float));

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

  expect_about("mix_params", 0.2734375, mix_params(1, 2, 3, 4, 5, 6, 7, 8));
  return 0;
}
#endif

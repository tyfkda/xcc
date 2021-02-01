#include "stdbool.h"
#include "stdlib.h"  // exit
#include "math.h"
#include "../examples/util.h"

void expect(char *title, Number expected, Number actual) {
  putstr(title);
  putstr(" => ");
  if (expected == actual) {
    putstr("OK\n");
    return;
  }
  putstr("NG: ");
  putdeci(expected);
  putstr(" expected, but got ");
  putdeci(actual);
  putstr("\n");
  exit(1);
}

void expect_about(char *title, Number expected, Number actual) {
  putstr(title);
  putstr(" => ");
  Number d = expected - actual;
  const Number eps = (Number)1e-5;
  if (d > -eps && d < eps) {
    putstr("OK\n");
    return;
  }
  putstr("NG: ");
  putdeci(expected);
  putstr(" expected, but got ");
  putdeci(actual);
  putstr("\n");
  exit(1);
}

void expecti(char *title, int expected, int actual) {
  putstr(title);
  putstr(" => ");
  if (expected == actual) {
    putstr("OK\n");
    return;
  }
  putstr("NG: ");
  putdeci(expected);
  putstr(" expected, but got ");
  putdeci(actual);
  putstr("\n");
  exit(1);
}

void fail(char *title) {
  putstr(title);
  putstr(" => NG\n");
  exit(1);
}

int number_test(void) {
  static Number Empty;
  Number x, y;

  expect("sizeof(double)", NUMBER_TYPE_SIZE, sizeof(Number));

  expect("w/o initializer", 0.0, Empty);
  expect("zero", 0.0, 0);
  expect("decimal", 42.0, 42);
  expect("+-", 21, (x=5, x+20-4));
  expect("*/", 7.5, (x=5, x*3/2));
  expect("unary -", -3.69, (x=3.69, -x));
  expect("pre inc", 11, (x=10, ++x));
  {
    x = 10;
    expect("post dec", 10, x--);
    expect("post dec after", 9, x);
  }
  {
    static Number g = 10.5;
    expect("pre dec g", 9.5, --g);
  }
  {
    static Number g = 10.25;
    expect("post inc g", 10.25, g++);
    expect("post inc after", 11.25, g);
  }

  expecti("!", false, (x=5, !x));
  expecti("&&", false, (x=0.2, y=0.0, x && y));
  expecti("||", true, (x=0.0, y=0.05, x || y));

  return 0;
}

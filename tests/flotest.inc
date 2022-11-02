#include "stdbool.h"
#include "stdio.h"
#include "stdlib.h"  // exit
#include "math.h"

void expect(char *title, Number expected, Number actual) {
  printf("%s => ", title);
  if (expected == actual) {
    printf("OK\n");
    return;
  }
  printf("NG: %f expected, but got %f\n", expected, actual);
  exit(1);
}

void expect_about(char *title, Number expected, Number actual) {
  printf("%s => ", title);
  Number d = expected - actual;
  const Number eps = (Number)1e-5;
  if (d > -eps && d < eps) {
    printf("OK\n");
    return;
  }
  printf("NG: %f expected, but got %f\n", expected, actual);
  exit(1);
}

void expecti(char *title, int expected, int actual) {
  printf("%s => ", title);
  if (expected == actual) {
    printf("OK\n");
    return;
  }
  printf("NG: %d expected, but got %d\n", expected, actual);
  exit(1);
}

void fail(char *title) {
  printf("%s => NG", title);
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

#define TEST_CAST_TO(ITYPE) { \
    x = 45.6; \
    ITYPE i = x; \
    expecti("cast to " #ITYPE, 45, i); \
  }
  TEST_CAST_TO(char);
  TEST_CAST_TO(short);
  TEST_CAST_TO(int);
  TEST_CAST_TO(long);

#define TEST_CAST_FROM(ITYPE) { \
    ITYPE i = -32; \
    expecti("cast from " #ITYPE, -32, i); \
  }
  TEST_CAST_FROM(char);
  TEST_CAST_FROM(short);
  TEST_CAST_FROM(int);
  TEST_CAST_FROM(long);

  {
    static Number g = 10.5;
    expect("pre dec g", 9.5, --g);
  }
  {
    static Number g = 10.25;
    expect("post inc g", 10.25, g++);
    expect("post inc after", 11.25, g);
  }
  {
    static Number f = 123;
    expect("number with int initializer", 123, f);
  }
  {
    static int i = (Number)12.34;
    expect("int with number initializer", 12, i);
  }

  expecti("!", false, (x=5, !x));
  expecti("&&", false, (x=0.2, y=0.0, x && y));
  expecti("||", true, (x=0.0, y=0.05, x || y));

  return 0;
}

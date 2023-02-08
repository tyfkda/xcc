// Longjmp test
//
// Compile:
//   $ ./xcc -olongjmp_test libsrc/tests/longjmp_test.c
//
// Run:
//   $ ./longjmp_test || echo fail

#include <setjmp.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>

int error_count;

void fail(const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  printf("FAILED, ");
  vprintf(fmt, ap);
  puts("");
  va_end(ap);
  ++error_count;
}

void expect(const char *title, int expected, int actual) {
  if (expected != actual) {
    printf("%s: ", title);
    fail("%d expected, but got %d", expected, actual);
  }
}

jmp_buf env;

void simple(int param) {
  if (param)
    longjmp(env, param * 2);
}
void test_simple(void) {
  int result;
  result = setjmp(env);
  if (result == 0) {
    simple(0);
  } else {
    fail("simple-nolongjmp, unreachable");
  }

  if ((result = setjmp(env)) == 0) {
    simple(255);
    fail("simple-longjmp, unreachable");
  } else {
    expect("simple-longjmp", 510, result);
  }
}

void test_zero(void) {
  int result = setjmp(env);
  if (result == 0) {
    longjmp(env, 0);
    fail("zero, unreachable");
  } else {
    expect("zero", true, result != 0);
  }
}

void test_multiple_times(void) {
  int result;
  static int i;  // Local variable might be roll back!
  i = 0;
  if (result = setjmp(env), result == 0) {
    simple(1);
    fail("multiple-times, unreachable");
  } else {
    if (++i < 5)
      simple(result);
    expect("multiple-times", 32, result);
  }
}

void nested(jmp_buf env2) {
  if (!setjmp(env))
    longjmp(env2, 777);
}
void test_nested(void) {
  jmp_buf env2;
  int result;
  if (result = setjmp(env2), result) {
    expect("nested", 777, result);
  } else {
    nested(env2);
    fail("nested, unreachable");
  }
}

int main(void) {
  test_simple();
  test_zero();
  test_multiple_times();
  test_nested();
  return error_count < 255 ? error_count : 255;
}

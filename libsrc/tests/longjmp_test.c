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

#include "./xtest.h"

jmp_buf env;

void simple(int param) {
  if (param)
    longjmp(env, param * 2);
}
TEST(simple) {
  int result;
  result = setjmp(env);
  if (result == 0) {
    simple(0);
  } else {
    fail("unreachable1");
  }

  if ((result = setjmp(env)) == 0) {
    simple(255);
    fail("unreachable2");
  } else {
    EXPECT_EQ(510, result);
  }
} END_TEST()

TEST(zero) {
  int result = setjmp(env);
  if (result == 0) {
    longjmp(env, 0);
    fail("unreachable");
  } else {
    EXPECT_TRUE(result != 0);
  }
} END_TEST()

TEST(multiple_times) {
  int result;
  static int i;  // Local variable might be roll back!
  i = 0;
  if (result = setjmp(env), result == 0) {
    simple(1);
    fail("unreachable");
  } else {
    if (++i < 5)
      simple(result);
    EXPECT_EQ(32, result);
  }
} END_TEST()

void nested(jmp_buf env2) {
  if (!setjmp(env))
    longjmp(env2, 777);
}
TEST(nested) {
  jmp_buf env2;
  int result;
  if (result = setjmp(env2), result) {
    EXPECT_EQ(777, result);
  } else {
    nested(env2);
    fail("nested, unreachable");
  }
} END_TEST()

int main(void) {
  return RUN_ALL_TESTS(
    test_simple,
    test_zero,
    test_multiple_times,
    test_nested,
  );
}

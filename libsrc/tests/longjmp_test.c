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

#include "../../tests/xtest.h"

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
}

TEST(zero) {
  int result = setjmp(env);
  if (result == 0) {
    longjmp(env, 0);
  }
  EXPECT_TRUE(result != 0);
}

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
}

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
}

XTEST_MAIN();

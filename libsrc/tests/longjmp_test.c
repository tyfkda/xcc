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

static void simple(int param) {
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

  result = 0;
  if (setjmp(env) == 0) {
    simple(1);
  } else {
    result = 4321;
  }
  EXPECT_EQ(4321, result);
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

static void nested(jmp_buf env2) {
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

TEST(volatile_local_preserved) {
  volatile int x = 0;
  int result = setjmp(env);
  if (result == 0) {
    x = 99;
    longjmp(env, 1);
  }
  EXPECT_EQ(99, x);
}

TEST(sideeffect) {
  jmp_buf envs[2];
  memset(&envs[1], -1, sizeof(envs[1]));
  int result = -1;
  volatile int i = 1;
  if (setjmp(envs[i--]) == 0) {
    jmp_buf *p = &envs[0];
    volatile int val;
    val = 55;
    if ((result = setjmp(envs[0])) == 0) {
      longjmp(*p++, ++val);
    }
  }
  EXPECT_EQ(56, result);
  EXPECT_EQ(0, i);
}

XTEST_MAIN();

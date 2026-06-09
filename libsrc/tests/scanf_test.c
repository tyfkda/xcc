#include <stdio.h>

#include "../../tests/xtest.h"

TEST(sscanf) {
  {
    int i = -1;
    int result = sscanf("d: 12345", "d: %d", &i);
    EXPECT_TRUE(result == 1 && i == 12345);
  }
  {
    int i = -1;
    int result = sscanf("non-d: ABCDE", "non-d: %d", &i);
    EXPECT_TRUE(result == 0 && i == -1);
  }
  {
    long l = -1;
    int result = sscanf("ld: -9876", "ld: %ld", &l);
    EXPECT_TRUE(result == 1 && l == -9876L);
  }
  {
    long long ll = -1;
    int result = sscanf("lld: +123456789", "lld: %lld", &ll);
    EXPECT_TRUE(result == 1 && ll == 123456789LL);
  }
  {
    int i = -1;
    int result = sscanf("x: aBcDeFgHi", "x: %xgHi", &i);
    EXPECT_TRUE(result == 1 && i == 0xabcdef);
  }

#ifndef __NO_FLONUM
  {
    float f = -1;
    double d = -1;
    long double ld = -1;
    int result = sscanf("floats: 1.23e4  .56e-7  -9.87e100", "floats: %f %lf %Lf", &f, &d, &ld);
    EXPECT_TRUE(result == 3 && fabsf(f - 1.23e4f) < 1e-5f && fabs(d - .56e-7) < 1e-12 && fabsl(ld + 9.87e100) < 1e95);
  }
  {
    double d1 = -1, d2 = -1;
    int result = sscanf("naninfoo", "%lf%lf", &d1, &d2);
    EXPECT_TRUE(result == 2 && isnan(d1) && isinf(d2) && d2 > 0);
  }
#endif

  {
    char s1[8], s2[8];
    int result = sscanf("s:  Hello   world  !", "s: %7s %7s", s1, s2);
    EXPECT_TRUE(result == 2 && strcmp(s1, "Hello") == 0 && strcmp(s2, "world") == 0);
  }
  {
    char s1[8], s2[8];
    int result = sscanf("lengthlimitedstring", "%7s%7s", s1, s2);
    EXPECT_TRUE(result == 2 && strcmp(s1, "lengthl") == 0 && strcmp(s2, "imiteds") == 0);
  }

  {
    int i = -1;
    int result = sscanf("auto-whiltespaces: \t\n 333", "auto-whiltespaces:%d", &i);
    EXPECT_TRUE(result == 1 && i == 333);
  }
}

XTEST_MAIN();

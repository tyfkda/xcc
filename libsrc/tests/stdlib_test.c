#include <stdlib.h>

#include "./xtest.h"

TEST(strtod) {
#ifndef __NO_FLONUM
  char *p;
  char *s;
  EXPECT_DEQ(1.25, strtod(s="1.25", &p));
  EXPECT_PTREQ(s + 4, p);
  EXPECT_DEQ(5e-10, strtod(s="+5.0e-10x", &p));
  EXPECT_PTREQ(s + 8, p);
  EXPECT_DEQ(-1e50, strtod(s="-1e50+", &p));
  EXPECT_PTREQ(s + 5, p);
  EXPECT_DEQ(0.0, strtod(s="NonDecimalString", &p));
  EXPECT_PTREQ(s, p);
#endif
} END_TEST()

int main() {
  return RUN_ALL_TESTS(
    test_strtod,
  );
}

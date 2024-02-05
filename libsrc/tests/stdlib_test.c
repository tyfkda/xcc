#include <stdlib.h>

#include <limits.h>

#include "./xtest.h"

TEST(atoi) {
  EXPECT_EQ(123, atoi("123"));
  EXPECT_EQ(12345, atoi("0012345.678"));
  EXPECT_EQ(-98765, atoi("-98765"));
  EXPECT_EQ(765, atoi(" \t\n+765 XYZ"));
  EXPECT_EQ(-591751050, atoi("1094624909430"));  // 0xfedcba9876: Value not clammped.

  EXPECT_EQ(0, atoi("+ 333"));
} END_TEST()

TEST(atol) {
  EXPECT_EQ(123, atol("123"));
  EXPECT_EQ(12345, atol("0012345.678"));
  EXPECT_EQ(-98765, atol("-98765"));
  EXPECT_EQ(765, atol(" \t\n+765 XYZ"));
#if defined(__LP64__)
  EXPECT_EQ(0x7fffffffffffffffL, atol("98765432109876543210"));  // Value clamped.
#endif

  EXPECT_EQ(0, atol("+ 333"));
} END_TEST()

TEST(atoll) {
  EXPECT_EQ(123, atoll("123"));
  EXPECT_EQ(12345, atoll("0012345.678"));
  EXPECT_EQ(-98765, atoll("-98765"));
  EXPECT_EQ(765, atoll(" \t\n+765 XYZ"));
  // EXPECT_EQ(0x7fffffffffffffffLL, atoll("98765432109876543210"));  // Value clamped.

  EXPECT_EQ(0, atoll("+ 333"));
} END_TEST()

TEST(atof) {
#ifndef __NO_FLONUM
  EXPECT_DEQ(1.25, atof("1.25"));
  EXPECT_DEQ(5e-10, atof("+5.0e-10x"));
  EXPECT_DEQ(-1e50, atof("\t\n -1e50+"));
  EXPECT_DEQ(0.0, atof("NonDecimalString"));

  EXPECT_DEQ(0.0, atof("+ 333.33"));
#endif
} END_TEST()

TEST(strtoll) {
  char *p, *s;
  EXPECT_EQ(123, strtoll(s="123", &p, 10));
  EXPECT_PTREQ(s + 3, p);
  EXPECT_EQ(4567, strtoll(s="\t\n  004567.789", &p, 10));
  EXPECT_PTREQ(s + 10, p);
  EXPECT_EQ(-987, strtoll(s="-987_65", &p, 10));
  EXPECT_PTREQ(s + 4, p);
  EXPECT_EQ(55, strtoll(s="+678-90", &p, 8));  // 067
  EXPECT_PTREQ(s + 3, p);
  EXPECT_EQ(563900, strtoll(s="89abcxyz", &p, 16));  // 0x89abc
  EXPECT_PTREQ(s + 5, p);
  EXPECT_EQ(150, strtoull(s="100101102", &p, 2));
  EXPECT_PTREQ(s + 8, p);
  EXPECT_EQ(9223372036854775807, strtoll(s="fedcba9876543210", &p, 16));  // Clamped to 0x7fffffffffffffffLL.
  EXPECT_PTREQ(s + 16, p);
  EXPECT_EQ(-0x8000000000000000LL, strtoll(s="-92233720368547758070", &p, 10));
  EXPECT_PTREQ(s + 21, p);

  EXPECT_EQ(0, strtoll(s="+ 333", &p, 10));
  EXPECT_PTREQ(s, p);
  EXPECT_EQ(987, strtoll(s="987", NULL, 10));  // Null accepted
} END_TEST()

TEST(strtoull) {
  char *p, *s;
  EXPECT_EQ(123, strtoull(s="123", &p, 10));
  EXPECT_PTREQ(s + 3, p);
  EXPECT_EQ(4567, strtoull(s="\t\n  004567.789", &p, 10));
  EXPECT_PTREQ(s + 10, p);
  EXPECT_EQ(55, strtoull(s="+678-90", &p, 8));  // 067
  EXPECT_PTREQ(s + 3, p);
  EXPECT_EQ(563900, strtoull(s="89abcxyz", &p, 16));  // 0x89abc
  EXPECT_PTREQ(s + 5, p);
  EXPECT_EQ(150, strtoull(s="100101102", &p, 2));
  EXPECT_PTREQ(s + 8, p);
  EXPECT_EQ(18364758544493064720ULL, strtoull(s="fedcba9876543210", &p, 16)); // 0xfedcba9876543210ULL
  EXPECT_PTREQ(s + 16, p);
  EXPECT_EQ(0xffffffffffffffffULL, strtoull(s="18446744073709551616", &p, 10));  // 18446744073709551616 is 0x1_0000_0000_0000_0000ULL, but clamped.
  EXPECT_PTREQ(s + 20, p);

  EXPECT_EQ(0, strtoull(s="+ 333", &p, 10));
  EXPECT_PTREQ(s, p);
  EXPECT_EQ(987, strtoull(s="987", NULL, 10));  // Null accepted
} END_TEST()

TEST(strtod) {
#ifndef __NO_FLONUM
  char *p, *s;
  EXPECT_DEQ(1.25, strtod(s="1.25", &p));
  EXPECT_PTREQ(s + 4, p);
  EXPECT_DEQ(5e-10, strtod(s="+5.0e-10x", &p));
  EXPECT_PTREQ(s + 8, p);
  EXPECT_DEQ(-1e50, strtod(s="\t\n -1e50+", &p));
  EXPECT_PTREQ(s + 8, p);
  EXPECT_DEQ(60.375, strtod(s="0xF1.8p-2", &p));
  EXPECT_DEQ(0.0, strtod(s="NonDecimalString", &p));
  EXPECT_PTREQ(s, p);

  EXPECT_EQ(0, strtod(s="+ 333.33", &p));
  EXPECT_PTREQ(s, p);
  EXPECT_DEQ(987.06125, strtod(s="987.06125", NULL));  // Null accepted

  union u { double d; uint64_t x; } u;
  u.d = strtod("-0.0", NULL);
  EXPECT_EQ(0x8000000000000000LL, u.x);
#endif
} END_TEST()

int main() {
  return RUN_ALL_TESTS(
    test_atoi,
    test_atol,
    test_atoll,
    test_atof,
    test_strtoll,
    test_strtoull,
    test_strtod,
  );
}

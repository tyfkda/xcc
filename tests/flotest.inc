#include "stdbool.h"
#include "stdint.h"
#include "stdio.h"
#include "stdlib.h"  // exit
#include "math.h"  // HUGE_VAL, NAN

#include "./xtest.h"

#ifdef USE_SINGLE
#define EXPECT(title, expected, actual)  expectf32(title, expected, actual)
#else
#define EXPECT(title, expected, actual)  expectf64(title, expected, actual)
#endif

TEST(number) {
  static Number Empty;
  Number x, y;

  expecti64("sizeof(double)", NUMBER_TYPE_SIZE, sizeof(Number));

  EXPECT("w/o initializer", 0.0, Empty);
  EXPECT("zero", 0.0, 0);
  EXPECT("decimal", 42.0, 42);
  EXPECT("hex literal", 12.5, 0x1.9p3);
  EXPECT("hex literal w/o .", 1.25, 0xap-3);

  EXPECT("+-", 21, (x=5, x+20-4));
  EXPECT("*/", 7.5, (x=5, x*3/2));
  EXPECT("unary -", -3.69, (x=3.69, -x));
  EXPECT("pre inc", 11, (x=10, ++x));
  {
    x = 10;
    EXPECT("post dec", 10, x--);
    EXPECT("post dec after", 9, x);
  }

#define TEST_CAST_TO(ITYPE) { \
    x = 45.6; \
    ITYPE i = x; \
    expecti64("cast to " #ITYPE, 45, i); \
  }
  TEST_CAST_TO(char);
  TEST_CAST_TO(short);
  TEST_CAST_TO(int);
  TEST_CAST_TO(long);

#define TEST_CAST_FROM(ITYPE) { \
    ITYPE i = -32; \
    expecti64("cast from " #ITYPE, -32, i); \
  }
  TEST_CAST_FROM(char);
  TEST_CAST_FROM(short);
  TEST_CAST_FROM(int);
  TEST_CAST_FROM(long);

  {
    static Number g = 10.5;
    EXPECT("pre dec g", 9.5, --g);
  }
  {
    static Number g = 10.25;
    EXPECT("post inc g", 10.25, g++);
    EXPECT("post inc after", 11.25, g);
  }
  {
    static Number f = 123;
    EXPECT("number with int initializer", 123, f);
  }
  {
    static int i = (Number)12.34;
    EXPECT("int with number initializer", 12, i);
  }

  expecti64("!", false, (x=5, !x));
  expecti64("&&", false, (x=0.2, y=0.0, x && y));
  expecti64("||", true, (x=0.0, y=0.05, x || y));

  {
    uint64_t ul = (uint64_t)-1L;
    expecti64("from unsigned max", 1, (x = ul, x >= 0));
  }
} END_TEST()

TEST(nan) {
  const int X = 0x4567, Y = 0x9abc;
  Number inf = HUGE_VAL;
  Number nan = NAN;

  EXPECT_TRUE(inf == inf);
  EXPECT_FALSE(nan == inf);
  EXPECT_FALSE(nan == -inf);
  EXPECT_FALSE(nan == nan);
  EXPECT_TRUE(!(nan == nan));
  EXPECT_EQ(X, inf == inf ? X : Y);
  EXPECT_EQ(Y, nan == inf ? X : Y);
  EXPECT_EQ(Y, nan == -inf ? X : Y);
  EXPECT_EQ(Y, nan == nan ? X : Y);
  EXPECT_FALSE(inf != inf);
  EXPECT_TRUE(nan != inf);
  EXPECT_TRUE(nan != -inf);
  EXPECT_TRUE(nan != nan);
  EXPECT_EQ(Y, inf != inf ? X : Y);
  EXPECT_EQ(X, nan != inf ? X : Y);
  EXPECT_EQ(X, nan != -inf ? X : Y);
  EXPECT_EQ(X, nan != nan ? X : Y);

  EXPECT_FALSE(nan > 0);
  EXPECT_FALSE(nan >= 0);
  EXPECT_FALSE(nan < 0);
  EXPECT_FALSE(nan <= 0);
  EXPECT_EQ(Y, nan > 0 ? X : Y);
  EXPECT_EQ(Y, nan >= 0 ? X : Y);
  EXPECT_EQ(Y, nan < 0 ? X : Y);
  EXPECT_EQ(Y, nan <= 0 ? X : Y);
  EXPECT_FALSE(0 < nan);
  EXPECT_FALSE(0 <= nan);
  EXPECT_FALSE(0 > nan);
  EXPECT_FALSE(0 >= nan);
  EXPECT_EQ(Y, 0 < nan ? X : Y);
  EXPECT_EQ(Y, 0 <= nan ? X : Y);
  EXPECT_EQ(Y, 0 > nan ? X : Y);
  EXPECT_EQ(Y, 0 >= nan ? X : Y);

  EXPECT_FALSE(nan > nan);
  EXPECT_FALSE(nan >= nan);
  EXPECT_FALSE(nan < nan);
  EXPECT_FALSE(nan <= nan);
  EXPECT_FALSE(nan > inf);
  EXPECT_FALSE(nan >= inf);
  EXPECT_FALSE(nan < inf);
  EXPECT_FALSE(nan <= inf);
  EXPECT_FALSE(nan > -inf);
  EXPECT_FALSE(nan >= -inf);
  EXPECT_FALSE(nan < -inf);
  EXPECT_FALSE(nan <= -inf);
} END_TEST()

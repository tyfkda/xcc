#include <stdio.h>

#include <inttypes.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>  // intptr_t
#include <string.h>

#ifndef __NO_FLONUM
#include <math.h>
#endif

#define SSIZE  (64)
#define MARKER  (0xbd)

int error_count;

#define EXPECT_EQUALS(expected, actual)  expect(#actual, expected, actual)
#define EXPECT_TRUE(actual)   expect(#actual, true, !!(actual))
#define EXPECT_FALSE(actual)  expect(#actual, false, !!(actual))
#define EXPECT_NOT_NULL(actual)  EXPECT_TRUE(actual != NULL)

void expect(const char *title, int64_t expected, int64_t actual) {
  printf("%s => ", title);

  if (expected != actual) {
    printf("ERR, %" PRId64 " expected, but got %" PRId64 "\n", expected, actual);
    ++error_count;
  } else {
    printf("OK\n");
  }
}

void expect_vsnprintf(const char *expected, int expected_len, const char *fmt, ...) {
  printf("%s => ", expected);
  char out[SSIZE + 1];
  out[SSIZE] = MARKER;

  va_list ap;
  va_start(ap, fmt);
  int len = vsnprintf(out, SSIZE, fmt, ap);
  va_end(ap);
  int written = len < SSIZE - 1 ? len : SSIZE - 1;

  if (len != expected_len || memcmp(expected, out, written) != 0) {
    fflush(stdout);
    fprintf(stderr, "ERR, actual [%s], len=%d/%d\n", out, len, expected_len);
    ++error_count;
  } else if ((unsigned char)out[SSIZE] != MARKER) {
    fflush(stdout);
    fprintf(stderr, "ERR, marker broken\n");
    ++error_count;
  } else if (out[written] != '\0') {
    fflush(stdout);
    fprintf(stderr, "ERR, not nul terminated, %d, [%s]\n", len, out);
    ++error_count;
  } else {
    printf("OK\n");
  }
}

void test_open_memstream(void) {
  char *ptr = NULL;
  size_t size = 0;
  FILE *fp = open_memstream(&ptr, &size);
  EXPECT_NOT_NULL(fp);
  if (fp != NULL) {
    EXPECT_EQUALS(12, fprintf(fp, "Hello world\n"));
    EXPECT_EQUALS(14, fprintf(fp, "Number: %d\n", 12345));
    fclose(fp);
    EXPECT_NOT_NULL(ptr);
    EXPECT_EQUALS(12 + 14, size);
  }
}

void test_sprintf(void) {
  char buf[16];
  memset(buf, 0x7f, sizeof(buf));
  EXPECT_EQUALS(5, sprintf(buf, "%d", 12345));
  EXPECT_EQUALS('\0', buf[5]);
  EXPECT_EQUALS(0x7f, buf[6]);
}

void test_vsnprintf(void) {
#define EXPECT(expected, fmt, ...)  expect_vsnprintf(expected, sizeof(expected)-1, fmt, __VA_ARGS__)
  EXPECT("Number:123", "Number:%d", 123);
  EXPECT("Negative:-456", "Negative:%d", -456);
  EXPECT("Flag:+789", "Flag:%+d", 789);
  EXPECT("FlagNeg:-987", "FlagNeg:%+d", -987);
  EXPECT("Padding:  654", "Padding:%5d", 654);
  EXPECT("ZeroPadding:00321", "ZeroPadding:%05d", 321);
  EXPECT("PaddingOver:12345678", "PaddingOver:%5d", 12345678);
  EXPECT("EndPadding:234  ", "EndPadding:%-5d", 234);
  EXPECT("Hex:89ab", "Hex:%x", 0x89ab);
  if (sizeof(int) == 4) {
    EXPECT("Hex-:ffff7655", "Hex-:%x", -0x89ab);
  }

  EXPECT("String:Foo.", "String:%s.", "Foo");
  EXPECT("BeginPadding:  Bar", "BeginPadding:%5s", "Bar");
  EXPECT("EndPadding:Baz  ", "EndPadding:%-5s", "Baz");
  EXPECT("SubstringRemain:   Fo", "SubstringRemain:%5.5s", "Fo");
  EXPECT("SubstringCut:FooBa", "SubstringCut:%5.5s", "FooBarBaz");
  EXPECT("NullString:(null)", "NullString:%s", NULL);

  EXPECT("Param:  Foo", "Param:%*s", 5, "Foo");
  EXPECT("Param2:FooBa", "Param2:%.*s", 5, "FooBarBaz");

  EXPECT("Character", "Char%ccter", 'a');
  EXPECT("Nul\0Inserted", "Nul%cInserted", '\0');
  EXPECT("%", "%%", 666);

  EXPECT("MoreThanBufferSize:12345678901234567890123456789012345678901234567890", "MoreThanBufferSize:%s", "12345678901234567890123456789012345678901234567890");

  EXPECT("Pointer:0x1234", "Pointer:%p", (void*)(intptr_t)0x1234);
  EXPECT("Pointer:  0x1234", "Pointer:%8p", (void*)(intptr_t)0x1234);
  EXPECT("NullPointer:0x0", "NullPointer:%p", NULL);

#ifndef __NO_FLONUM
  EXPECT("Float:1.234000", "Float:%f", 1.234);
  EXPECT("Float-:-1.234000", "Float-:%f", -1.234);
  EXPECT("Float+:+1.234000", "Float+:%+f", 1.234);
  EXPECT("FloatSub:1.23", "FloatSub:%.2f", 1.234);
  EXPECT("inf:inf", "inf:%f", HUGE_VAL);
  EXPECT("nan:nan", "nan:%f", NAN);
#endif
#undef EXPECT
}

int main() {
  test_open_memstream();
  test_sprintf();
  test_vsnprintf();
  return error_count > 255 ? 255 : error_count;
}

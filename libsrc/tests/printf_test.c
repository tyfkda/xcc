#include <stdio.h>
#include <stdarg.h>
#include <string.h>

#define SSIZE  (64)
#define MARKER  (0xbd)

int error_count;

#define TEST(expected, fmt, ...)  test(expected, sizeof(expected)-1, fmt, __VA_ARGS__)

void test(const char *expected, int expected_len, const char *fmt, ...) {
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
    fprintf(stderr, "ERR, not nul terminated\n");
    ++error_count;
  } else {
    printf("OK\n");
  }
}

int main() {
  TEST("Number:123", "Number:%d", 123);
  TEST("Negative:-456", "Negative:%d", -456);
  TEST("Flag:+789", "Flag:%+d", 789);
  TEST("FlagNeg:-987", "FlagNeg:%+d", -987);
  TEST("Padding:  654", "Padding:%5d", 654);
  TEST("ZeroPadding:00321", "ZeroPadding:%05d", 321);
  TEST("PaddingOver:12345678", "PaddingOver:%5d", 12345678);
  TEST("EndPadding:234  ", "EndPadding:%-5d", 234);
  TEST("Hex:89ab", "Hex:%x", 0x89ab);

  TEST("String:Foo.", "String:%s.", "Foo");
  TEST("BeginPadding:  Bar", "BeginPadding:%5s", "Bar");
  TEST("EndPadding:Baz  ", "EndPadding:%-5s", "Baz");
  TEST("SubstringRemain:   Fo", "SubstringRemain:%5.5s", "Fo");
  TEST("SubstringCut:FooBa", "SubstringCut:%5.5s", "FooBarBaz");

  TEST("Param:  Foo", "Param:%*s", 5, "Foo");
  TEST("Param2:FooBa", "Param2:%.*s", 5, "FooBarBaz");

  TEST("Character", "Char%ccter", 'a');
  TEST("Nul\0Inserted", "Nul%cInserted", '\0');
  TEST("%", "%%", 666);

  TEST("MoreThanBufferSize:12345678901234567890123456789012345678901234567890", "MoreThanBufferSize:%s", "12345678901234567890123456789012345678901234567890");

  return error_count > 0;
}

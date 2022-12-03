#include <stdio.h>
#include <stdarg.h>
#include <string.h>

#define SSIZE  (64)
#define MARKER  (0xbd)

int error_count;

void assert_vsnprintf(const char *expected, int expected_len, const char *fmt, ...) {
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

void test_vsnprintf(void) {
#define ASSERT(expected, fmt, ...)  assert_vsnprintf(expected, sizeof(expected)-1, fmt, __VA_ARGS__)
  ASSERT("Number:123", "Number:%d", 123);
  ASSERT("Negative:-456", "Negative:%d", -456);
  ASSERT("Flag:+789", "Flag:%+d", 789);
  ASSERT("FlagNeg:-987", "FlagNeg:%+d", -987);
  ASSERT("Padding:  654", "Padding:%5d", 654);
  ASSERT("ZeroPadding:00321", "ZeroPadding:%05d", 321);
  ASSERT("PaddingOver:12345678", "PaddingOver:%5d", 12345678);
  ASSERT("EndPadding:234  ", "EndPadding:%-5d", 234);
  ASSERT("Hex:89ab", "Hex:%x", 0x89ab);

  ASSERT("String:Foo.", "String:%s.", "Foo");
  ASSERT("BeginPadding:  Bar", "BeginPadding:%5s", "Bar");
  ASSERT("EndPadding:Baz  ", "EndPadding:%-5s", "Baz");
  ASSERT("SubstringRemain:   Fo", "SubstringRemain:%5.5s", "Fo");
  ASSERT("SubstringCut:FooBa", "SubstringCut:%5.5s", "FooBarBaz");

  ASSERT("Param:  Foo", "Param:%*s", 5, "Foo");
  ASSERT("Param2:FooBa", "Param2:%.*s", 5, "FooBarBaz");

  ASSERT("Character", "Char%ccter", 'a');
  ASSERT("Nul\0Inserted", "Nul%cInserted", '\0');
  ASSERT("%", "%%", 666);

  ASSERT("MoreThanBufferSize:12345678901234567890123456789012345678901234567890", "MoreThanBufferSize:%s", "12345678901234567890123456789012345678901234567890");
#undef ASSERT
}

void test_open_memstream(void) {
  char *ptr = NULL;
  size_t size = 0;
  FILE *fp = open_memstream(&ptr, &size);
  if (fp == NULL) {
    fprintf(stderr, "Failed to open memstream\n");
    ++error_count;
  } else {
    int len1 = fprintf(fp, "Hello world\n");
    int len2 = fprintf(fp, "Number: %d\n", 12345);
    fclose(fp);
    if (ptr == NULL || size != (size_t)(len1 + len2)) {
      fprintf(stderr, "open_memstream, ERR, %p, %d, %d, %d\n", ptr, len1, len2, (int)size);
      ++error_count;
    }
  }
}

int main() {
  test_vsnprintf();
  test_open_memstream();
  return error_count > 255 ? 255 : error_count;
}

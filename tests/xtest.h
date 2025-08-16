#pragma once

#include <inttypes.h>  // PRId64
#include <math.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>  // int64_t
#include <stdio.h>
#include <stdlib.h>  // exit
#include <string.h>

#ifdef __GNUC__
#pragma GCC diagnostic ignored "-Wunused-function"
#endif

#define MAX_TEST_CASES  32

#define XTEST_WEAK  __attribute__((weak))

struct XTestCase {
  const char *title;
  void (*fn)(void);
};
XTEST_WEAK struct XTestCase xtest_cases[MAX_TEST_CASES];
XTEST_WEAK size_t xtest_case_count;

#define TEST(name) \
  static void test_ ## name (void); \
  __attribute__((constructor)) static void register_test_ ## name (void) { \
    if (xtest_case_count >= MAX_TEST_CASES) { \
      fprintf(stderr, "Too many test cases\n"); \
      exit(1); \
    } \
    struct XTestCase *p = &xtest_cases[xtest_case_count++]; \
    p->title = #name; \
    p->fn = test_ ## name; \
  } \
  static void test_ ## name (void)
#define END_TEST()

#define EXPECT_EQ(expected, actual)  expecti64(#actual, expected, actual)
#define EXPECT_TRUE(actual)   expecti64(#actual, true, !!(actual))
#define EXPECT_FALSE(actual)  expecti64(#actual, false, !!(actual))
#define EXPECT_NULL(actual)  EXPECT_TRUE(actual == NULL)
#define EXPECT_NOT_NULL(actual)  EXPECT_TRUE(actual != NULL)
#define EXPECT_NAN(actual)    expecti64(#actual, true, !!isnan(actual))
#define EXPECT_PTREQ(expected, actual)  expectptr(#actual, expected, actual)
#define EXPECT_STREQ(title, expected, actual)  expectstr(title, expected, actual)

#if !defined(__NO_FLONUM)
#define EXPECT_DEQ(expected, actual)  expectf64(#actual, expected, actual)
#endif
#if !defined(XTEST_NO_EXPECT_NEAR) && !defined(__NO_FLONUM)
#define EXPECT_NEAR(expected, actual)  expect_near(#actual, expected, actual)
#endif

XTEST_WEAK const char *suite_name;
XTEST_WEAK int failed_suite_count;
XTEST_WEAK int test_count, error_count;

#define CLEAR_LINE  "\033[1G\033[2K"
#define RED  "\033[31m"
#define GREEN  "\033[32m"
#define BOLD  "\033[1m"
#define RESET_COLOR  "\033[0m"

XTEST_WEAK void begin_test_suite(const char *title) {
  suite_name = title;
  test_count = error_count = 0;
}

XTEST_WEAK void end_test_suite(void) {
  if (error_count == 0) {
    printf(CLEAR_LINE "  %s: " GREEN "OK: %d" RESET_COLOR "\n", suite_name, test_count);
  } else {
    printf(CLEAR_LINE "    " RED BOLD "ERROR: %d/%d" RESET_COLOR "\n", error_count, test_count);
    ++failed_suite_count;
  }
}

XTEST_WEAK void begin_test(const char *title) {
  ++test_count;
  printf(CLEAR_LINE "  %s %d: %s", suite_name, test_count, title);
  fflush(stdout);
}

XTEST_WEAK bool fail(const char *fmt, ...) {
  ++error_count;
  printf("  " RED BOLD "FAILED" RESET_COLOR ": ");
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stdout, fmt, ap);
  va_end(ap);
  fputc('\n', stdout);
  return false;
}

XTEST_WEAK bool expecti64(const char *title, int64_t expected, int64_t actual) {
  begin_test(title);
  if (expected == actual)
    return true;
  return fail("%s, %" PRId64 " expected, but got %" PRId64 "\n", title, expected, actual);
}

XTEST_WEAK bool expectptr(const char *title, void *expected, void *actual) {
  begin_test(title);
  if (expected == actual)
    return true;
  return fail("%s, %p expected, but got %p\n", title, expected, actual);
}

XTEST_WEAK bool expectstr(const char *title, const char *expected, const char *actual) {
  begin_test(title);
  if (strcmp(expected, actual) == 0)
    return true;
  return fail("\"%s\" expected, but got \"%s\"\n", expected, actual);
}

#if !defined(__NO_FLONUM)
XTEST_WEAK bool expectf64(const char *title, double expected, double actual) {
  begin_test(title);
  if (expected == actual)
    return true;
  return fail("%s, %f expected, but got %f\n", title, expected, actual);
}

XTEST_WEAK bool expectf32(const char *title, float expected, float actual) {
  begin_test(title);
  if (expected == actual)
    return true;
  return fail("%s, %f expected, but got %f\n", title, expected, actual);
}
#endif

#if !defined(XTEST_NO_EXPECT_NEAR) && !defined(__NO_FLONUM)
XTEST_WEAK bool expect_near(const char *title, double expected, double actual) {
  begin_test(title);
  if (isfinite(actual)) {
    double d = expected - actual;
    const double eps = (double)1e-5;
    if (d >= -eps && d <= eps)
      return true;
  } else if (!isnan(expected)) {
    if (expected == actual)
      return true;
  }

  return fail("%s, %f expected, but got %f\n", title, expected, actual);
}
#endif

XTEST_WEAK int xtest_main(void) {
  for (size_t i = 0; i < xtest_case_count; ++i) {
    struct XTestCase *p = &xtest_cases[i];
    begin_test_suite(p->title);
    (*p->fn)();
    end_test_suite();
  }
  return failed_suite_count == 0 ? 0 : 1;
}

#define XTEST_MAIN()  int main(void) { return xtest_main(); }

#pragma once

#include <inttypes.h>  // PRId64
#include <math.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>  // int64_t
#include <stdio.h>
#include <string.h>

#ifdef __GNUC__
#pragma GCC diagnostic ignored "-Wunused-function"
#endif

#define TEST(name)  void test_ ## name (void) { begin_test_suite(#name);
#define END_TEST()  end_test_suite(); }

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

#define RUN_ALL_TESTS(...)  ({ \
  static void (*const tests[])(void) = {__VA_ARGS__}; \
  for (size_t i = 0; i < sizeof(tests)/sizeof(*tests); ++i) \
    (*tests[i])(); \
  failed_suite_count < 255 ? failed_suite_count : 255; })

static const char *suite_name;
static int failed_suite_count;
static int test_count, error_count;
static const char *test_title;

#define CLEAR_LINE  "\033[1G\033[2K"
#define RED  "\033[31m"
#define GREEN  "\033[32m"
#define BOLD  "\033[1m"
#define RESET_COLOR  "\033[0m"

static void begin_test_suite(const char *title) {
  suite_name = title;
  test_count = error_count = 0;
}

static void end_test_suite(void) {
  if (error_count == 0) {
    printf(CLEAR_LINE "  %s: " GREEN "OK: %d" RESET_COLOR "\n", suite_name, test_count);
  } else {
    printf(CLEAR_LINE "    " RED BOLD "ERROR: %d/%d" RESET_COLOR "\n", error_count, test_count);
    ++failed_suite_count;
  }
}

static void begin_test(const char *title) {
  test_title = title;
  ++test_count;
  printf(CLEAR_LINE "  %s %d: %s", suite_name, test_count, title);
  fflush(stdout);
}

static void fail(const char *fmt, ...) {
  ++error_count;
  printf("  " RED BOLD "FAILED" RESET_COLOR ": ");
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stdout, fmt, ap);
  va_end(ap);
  fputc('\n', stdout);
}

static void expecti64(const char *title, int64_t expected, int64_t actual) {
  begin_test(title);
  if (expected != actual)
    fail("%s, %" PRId64 " expected, but got %" PRId64 "\n", title, expected, actual);
}

static void expectptr(const char *title, void *expected, void *actual) {
  begin_test(title);
  if (expected != actual)
    fail("%s, %p expected, but got %p\n", title, expected, actual);
}

static void expectstr(const char *title, const char *expected, const char *actual) {
  begin_test(title);
  if (strcmp(expected, actual) != 0)
    fail("\"%s\" expected, but got \"%s\"\n", expected, actual);
}

#if !defined(__NO_FLONUM)
static void expectf64(const char *title, double expected, double actual) {
  begin_test(title);
  if (expected != actual)
    fail("%s, %f expected, but got %f\n", title, expected, actual);
}

static void expectf32(const char *title, float expected, float actual) {
  begin_test(title);
  if (expected != actual)
    fail("%s, %f expected, but got %f\n", title, expected, actual);
}
#endif

#if !defined(XTEST_NO_EXPECT_NEAR) && !defined(__NO_FLONUM)
void expect_near(const char *title, double expected, double actual) {
  begin_test(title);
  int ok = 0;
  if (isfinite(actual)) {
    double d = expected - actual;
    const double eps = (double)1e-5;
    ok = d >= -eps && d <= eps;
  } else if (!isnan(expected)) {
    ok = expected == actual;
  }
  if (!ok)
    fail("%s, %f expected, but got %f\n", title, expected, actual);
}
#endif

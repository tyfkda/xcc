#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "table.h"
#include "util.h"

#define EXPECT(expected, actual)  expect(__LINE__, expected, actual)
#define EXPECT_STREQ(title, expected, actual)  expect_streq(title, expected, actual)
#define EXPECT_NULL(title, actual)  expect_null(title, actual)

void expect(int line, int expected, int actual) {
  if (expected == actual)
    return;
  fprintf(stderr, "%d: %d expected, but got %d\n",
          line, expected, actual);
  exit(1);
}

void expect_streq(const char *title, const char *expected, const char *actual) {
  if (strcmp(expected, actual) == 0)
    return;
  fprintf(stderr, "%s: \"%s\" expected, but got \"%s\"\n",
          title, expected, actual);
  exit(1);
}

void expect_null(const char *title, const void *actual) {
  if (actual == NULL)
    return;
  fprintf(stderr, "%s: null expected, but got %p\n",
          title, actual);
  exit(1);
}

void test_vector(void) {
  Vector *vec = new_vector();
  EXPECT(0, vec->len);

  for (int i = 0; i < 100; i++)
    vec_push(vec, (void *)(intptr_t)i);

  EXPECT(100, vec->len);
  EXPECT(0, (intptr_t)vec->data[0]);
  EXPECT(50, (intptr_t)vec->data[50]);
  EXPECT(99, (intptr_t)vec->data[99]);

  vec_insert(vec, 10, (void*)(intptr_t)123);
  EXPECT(101, vec->len);
  EXPECT(9, (intptr_t)vec->data[9]);
  EXPECT(123, (intptr_t)vec->data[10]);
  EXPECT(10, (intptr_t)vec->data[11]);
  EXPECT(99, (intptr_t)vec->data[100]);

  vec_remove_at(vec, 20);
  EXPECT(100, vec->len);
  EXPECT(18, (intptr_t)vec->data[19]);
  EXPECT(20, (intptr_t)vec->data[20]);

  EXPECT(false, vec_contains(vec, (void*)(intptr_t)19));
  EXPECT(true, vec_contains(vec, (void*)(intptr_t)20));
}

void test_sb(void) {
  StringBuffer sb;
  sb_init(&sb);

  EXPECT_STREQ("Empty", "", sb_to_string(&sb));

  sb_append(&sb, "abc", NULL);
  const char *sub = "12345";
  sb_append(&sb, sub, sub + 2);
  EXPECT_STREQ("append", "abc12", sb_to_string(&sb));
  EXPECT(false, sb_empty(&sb));

  sb_clear(&sb);
  EXPECT(true, sb_empty(&sb));
}

void test_escape(void) {
  StringBuffer sb;
  sb_init(&sb);

  static const char s1[] = "\"a b\tc\rd\ne\\\x1b";
  escape_string(s1, sizeof(s1), &sb);
  EXPECT_STREQ("escape_string", "\\\"a b\\tc\\rd\\ne\\\\\\x1b\\0", sb_to_string(&sb));
}

void test_is_fullpath(void) {
  EXPECT(true, is_fullpath("/foo/bar"));
  EXPECT(false, is_fullpath("./foo/bar"));
  EXPECT(false, is_fullpath("/foo/../bar"));
  EXPECT(true, is_fullpath("/foo/bar..baz/"));
  EXPECT(true, is_fullpath("/foo/..bar"));
}

void test_cat_path(void) {
  EXPECT_STREQ("Relative", "/user/foo/inc/stdio.h", cat_path("/user/foo", "inc/stdio.h"));
  EXPECT_STREQ("Absolute", "/inc/stdio.h", cat_path("/user/foo", "/inc/stdio.h"));
  EXPECT_STREQ("Current", "/user/foo/inc/stdio.h", cat_path("/user/foo", "./inc/stdio.h"));
  EXPECT_STREQ("Parent", "/user/inc/stdio.h", cat_path("/user/foo", "../inc/stdio.h"));
  EXPECT_STREQ("Dir", "/user/foo/bar/baz/", cat_path("/user/foo", "bar/baz/"));
  EXPECT_STREQ("Path is root w/ ..", "/baz", cat_path("/user/foo", "/bar/../baz"));
  EXPECT_STREQ("Redundant slash", "/user/foo/bar/baz", cat_path("/user/foo", "bar//baz"));
  EXPECT_STREQ("Root 1", "/", cat_path("/", "."));
  EXPECT_STREQ("Root 2", "/", cat_path("/user/foo", "../.."));
  EXPECT_STREQ("Root 3", "/", cat_path("/user/foo", "../../"));
  EXPECT_NULL("Illegal", cat_path("/user/foo", "../../.."));
  EXPECT_STREQ("Root end with '/'", "/user/foo/inc/stdio.h", cat_path("/user/foo/", "inc/stdio.h"));
  EXPECT_STREQ("Not root", "user/foo/inc/stdio.h", cat_path("user/foo", "inc/stdio.h"));
  EXPECT_STREQ("Non root ancestor", "user/inc/stdio.h", cat_path("user/foo", "../inc/stdio.h"));
  EXPECT_STREQ("Non root ancestor", "../inc/stdio.h", cat_path("user/foo", "../../../inc/stdio.h"));
}

void test_change_ext(void) {
  EXPECT_STREQ("has ext", "foo.o", change_ext("foo.c", "o"));
  EXPECT_STREQ("no ext", "foo.o", change_ext("foo", "o"));
  EXPECT_STREQ("mult ext", "foo.bar.baz.o", change_ext("foo.bar.baz.c", "o"));
  EXPECT_STREQ("dir", "/foo/bar.baz/qux.s", change_ext("/foo/bar.baz/qux", "s"));
}

void runtest(void) {
  test_vector();
  test_sb();
  test_escape();
  test_is_fullpath();
  test_cat_path();
  test_change_ext();

  printf("OK\n");
}

int main(void) {
  runtest();
  return 0;
}

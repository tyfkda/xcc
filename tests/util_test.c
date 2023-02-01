#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "table.h"
#include "util.h"

#include "./xtest.h"

TEST(vector) {
  Vector *vec = new_vector();
  EXPECT_EQ(0, vec->len);

  for (int i = 0; i < 100; i++)
    vec_push(vec, (void *)(intptr_t)i);

  EXPECT_EQ(100, vec->len);
  EXPECT_EQ(0, (intptr_t)vec->data[0]);
  EXPECT_EQ(50, (intptr_t)vec->data[50]);
  EXPECT_EQ(99, (intptr_t)vec->data[99]);

  vec_insert(vec, 10, (void*)(intptr_t)123);
  EXPECT_EQ(101, vec->len);
  EXPECT_EQ(9, (intptr_t)vec->data[9]);
  EXPECT_EQ(123, (intptr_t)vec->data[10]);
  EXPECT_EQ(10, (intptr_t)vec->data[11]);
  EXPECT_EQ(99, (intptr_t)vec->data[100]);

  vec_remove_at(vec, 20);
  EXPECT_EQ(100, vec->len);
  EXPECT_EQ(18, (intptr_t)vec->data[19]);
  EXPECT_EQ(20, (intptr_t)vec->data[20]);

  EXPECT_EQ(false, vec_contains(vec, (void*)(intptr_t)19));
  EXPECT_EQ(true, vec_contains(vec, (void*)(intptr_t)20));
} END_TEST()

TEST(sb) {
  StringBuffer sb;
  sb_init(&sb);

  EXPECT_STREQ("Empty", "", sb_to_string(&sb));

  sb_append(&sb, "abc", NULL);
  const char *sub = "12345";
  sb_append(&sb, sub, sub + 2);
  EXPECT_STREQ("append", "abc12", sb_to_string(&sb));
  EXPECT_EQ(false, sb_empty(&sb));

  sb_clear(&sb);
  EXPECT_EQ(true, sb_empty(&sb));
} END_TEST()

TEST(escape) {
  StringBuffer sb;
  sb_init(&sb);

  static const char s1[] = "\"a b\tc\rd\ne\\\x1b";
  escape_string(s1, sizeof(s1), &sb);
  EXPECT_STREQ("escape_string", "\\\"a b\\tc\\rd\\ne\\\\\\x1b\\0", sb_to_string(&sb));
} END_TEST()

TEST(is_fullpath) {
  EXPECT_EQ(true, is_fullpath("/foo/bar"));
  EXPECT_EQ(false, is_fullpath("./foo/bar"));
  EXPECT_EQ(false, is_fullpath("/foo/../bar"));
  EXPECT_EQ(true, is_fullpath("/foo/bar..baz/"));
  EXPECT_EQ(true, is_fullpath("/foo/..bar"));
} END_TEST()

TEST(cat_path) {
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
  EXPECT_NULL(cat_path("/user/foo", "../../.."));
  EXPECT_STREQ("Root end with '/'", "/user/foo/inc/stdio.h", cat_path("/user/foo/", "inc/stdio.h"));
  EXPECT_STREQ("Not root", "user/foo/inc/stdio.h", cat_path("user/foo", "inc/stdio.h"));
  EXPECT_STREQ("Non root ancestor", "user/inc/stdio.h", cat_path("user/foo", "../inc/stdio.h"));
  EXPECT_STREQ("Non root ancestor", "../inc/stdio.h", cat_path("user/foo", "../../../inc/stdio.h"));
} END_TEST()

TEST(change_ext) {
  EXPECT_STREQ("has ext", "foo.o", change_ext("foo.c", "o"));
  EXPECT_STREQ("no ext", "foo.o", change_ext("foo", "o"));
  EXPECT_STREQ("mult ext", "foo.bar.baz.o", change_ext("foo.bar.baz.c", "o"));
  EXPECT_STREQ("dir", "/foo/bar.baz/qux.s", change_ext("/foo/bar.baz/qux", "s"));
} END_TEST()

int main(void) {
  return RUN_ALL_TESTS(
    test_vector,
    test_sb,
    test_escape,
    test_is_fullpath,
    test_cat_path,
    test_change_ext,
  );
}

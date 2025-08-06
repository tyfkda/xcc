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
}

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
}

TEST(escape) {
  StringBuffer sb;
  sb_init(&sb);

  static const char s1[] = "\"a b\tc\rd\ne\\\x1b";
  escape_string(s1, sizeof(s1), &sb);
  EXPECT_STREQ("escape_string", "\\\"a b\\tc\\rd\\ne\\\\\\x1b\\0", sb_to_string(&sb));
}

TEST(is_fullpath) {
  EXPECT_EQ(true, is_fullpath("/foo/bar"));
  EXPECT_EQ(false, is_fullpath("./foo/bar"));
  EXPECT_EQ(false, is_fullpath("/foo/../bar"));
  EXPECT_EQ(true, is_fullpath("/foo/bar..baz/"));
  EXPECT_EQ(true, is_fullpath("/foo/..bar"));
}

TEST(join_paths) {
  EXPECT_STREQ("Relative", "/usr/foo/inc/stdio.h", JOIN_PATHS("/usr/foo", "inc/stdio.h"));
  EXPECT_STREQ("Relative end /", "/usr/foo/inc/sys/", JOIN_PATHS("/usr/foo", "inc/sys/"));
  EXPECT_STREQ("Absolute", "/inc/stdio.h", JOIN_PATHS("/usr/foo", "/inc/stdio.h"));
  EXPECT_STREQ("Absolute end /", "/inc/sys/", JOIN_PATHS("/usr/foo", "/inc/sys/"));
  EXPECT_STREQ("Current", "/usr/foo/inc/stdio.h", JOIN_PATHS("/usr/foo", "./inc/stdio.h"));
  EXPECT_STREQ("Parent", "/usr/inc/stdio.h", JOIN_PATHS("/usr/foo", "../inc/stdio.h"));
  EXPECT_STREQ("Path is root w/ ..", "/baz", JOIN_PATHS("/usr/foo", "/bar/../baz"));
  EXPECT_STREQ("Redundant slash", "/usr/foo/bar/baz", JOIN_PATHS("/usr/foo", "bar//baz"));
  EXPECT_STREQ("Root 1", "/", JOIN_PATHS("/", "."));
  EXPECT_STREQ("Root 2", "/", JOIN_PATHS("/usr/foo", "../.."));
  EXPECT_STREQ("Root 3", "/", JOIN_PATHS("/usr/foo", "../../"));
  EXPECT_STREQ("Root 4", "/", JOIN_PATHS(".", "/"));
  EXPECT_NULL(JOIN_PATHS("/usr/foo", "../../../bar"));
  EXPECT_STREQ("Ancestor", "../bar", JOIN_PATHS("usr/foo", "../../../bar"));
  EXPECT_STREQ("Root end with '/'", "/usr/foo/inc/stdio.h",
               JOIN_PATHS("/usr/foo/", "inc/stdio.h"));
  EXPECT_STREQ("Not root", "usr/foo/inc/stdio.h", JOIN_PATHS("usr/foo", "inc/stdio.h"));
  EXPECT_STREQ("Non root ancestor", "usr/inc/stdio.h", JOIN_PATHS("usr/foo", "../inc/stdio.h"));
  EXPECT_STREQ("Non root ancestor2", "../inc/stdio.h",
               JOIN_PATHS("usr/foo", "../../../inc/stdio.h"));
  EXPECT_STREQ("Cwd", "foo.txt", JOIN_PATHS(".", "foo.txt"));
  EXPECT_STREQ("Cwd with abs", "/bar.txt", JOIN_PATHS(".", "/bar.txt"));
  EXPECT_STREQ("Cwd with abs2", "../../baz.txt", JOIN_PATHS(".", "../../baz.txt"));
  EXPECT_STREQ("Parent", "../foo.txt", JOIN_PATHS("..", "foo.txt"));
  EXPECT_STREQ("Parent with abs", "/bar.txt", JOIN_PATHS("..", "/bar.txt"));
  EXPECT_STREQ("Parent with abs2", "../../../baz.txt", JOIN_PATHS("..", "../../baz.txt"));
  EXPECT_STREQ("Relative top", ".", JOIN_PATHS("usr/foo", "../.."));
  EXPECT_STREQ("Relative top with /", "./", JOIN_PATHS("usr/foo", "../../"));
  EXPECT_STREQ("Relative top 3", "bar", JOIN_PATHS("usr/foo", "../../bar"));
}

TEST(change_ext) {
  EXPECT_STREQ("has ext", "foo.o", change_ext("foo.c", "o"));
  EXPECT_STREQ("no ext", "foo.o", change_ext("foo", "o"));
  EXPECT_STREQ("mult ext", "foo.bar.baz.o", change_ext("foo.bar.baz.c", "o"));
  EXPECT_STREQ("dir", "/foo/bar.baz/qux.s", change_ext("/foo/bar.baz/qux", "s"));
}

XTEST_MAIN();

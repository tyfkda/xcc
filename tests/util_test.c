#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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
}

void test_map(void) {
  Map *map = new_map();
  EXPECT(0, map_count(map));
  EXPECT(0, (intptr_t)map_get(map, "foo"));

  map_put(map, "foo", (void *)2);
  EXPECT(2, (intptr_t)map_get(map, "foo"));

  map_put(map, "bar", (void *)4);
  EXPECT(4, (intptr_t)map_get(map, "bar"));

  map_put(map, "foo", (void *)6);
  EXPECT(6, (intptr_t)map_get(map, "foo"));

  EXPECT(2, map_count(map));

  intptr_t value = -1;
  EXPECT(false, map_try_get(map, "qux", (void**)&value));
  map_put(map, "qux", (void *)0);
  EXPECT(true, map_try_get(map, "qux", (void**)&value));
  EXPECT(0, value);
}

void test_abspath(void) {
  EXPECT_STREQ("Relative", "/user/foo/inc/stdio.h", abspath("/user/foo", "inc/stdio.h"));
  EXPECT_STREQ("Absolute", "/inc/stdio.h", abspath("/user/foo", "/inc/stdio.h"));
  EXPECT_STREQ("Current", "/user/foo/inc/stdio.h", abspath("/user/foo", "./inc/stdio.h"));
  EXPECT_STREQ("Parent", "/user/inc/stdio.h", abspath("/user/foo", "../inc/stdio.h"));
  EXPECT_STREQ("Dir", "/user/foo/bar/baz/", abspath("/user/foo", "bar/baz/"));
  EXPECT_STREQ("Redundant slash", "/user/foo/bar/baz", abspath("/user/foo", "bar//baz"));
  EXPECT_STREQ("Root 1", "/", abspath("/", "."));
  EXPECT_STREQ("Root 2", "/", abspath("/user/foo", "../.."));
  EXPECT_STREQ("Root 3", "/", abspath("/user/foo", "../../"));
  EXPECT_NULL("Illegal", abspath("/user/foo", "../../.."));
  EXPECT_STREQ("Root end with '/'", "/user/foo/inc/stdio.h", abspath("/user/foo/", "inc/stdio.h"));
  EXPECT_STREQ("Not root", "user/foo/inc/stdio.h", abspath("user/foo", "inc/stdio.h"));
}

void runtest(void) {
  test_vector();
  test_map();
  test_abspath();

  printf("OK\n");
}

int main(void) {
  runtest();
  return 0;
}

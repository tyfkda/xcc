#include <limits.h>
#include <stdlib.h>
#include <string.h>

#define XTEST_NO_EXPECT_NEAR
#include "../../tests/xtest.h"

#ifndef PATH_MAX
#define PATH_MAX 512
#endif

ssize_t _canonicalize_path(char *buf, ssize_t size, const char *path);

static char *canonicalize(const char *base, const char *path, char *buf) {
  strncpy(buf, base, PATH_MAX);
  return _canonicalize_path(buf, PATH_MAX, path) >= 0 ? buf : NULL;
}

TEST(canonicalize_path) {
#define CANONICALIZE(base, path)  canonicalize(base, path, buf)
  char buf[PATH_MAX];
  EXPECT_STREQ("Relative", "/usr/foo/inc/stdio.h", CANONICALIZE("/usr/foo", "inc/stdio.h"));
  EXPECT_STREQ("Relative end /", "/usr/foo/inc/sys/", CANONICALIZE("/usr/foo", "inc/sys/"));
  EXPECT_STREQ("Absolute", "/inc/stdio.h", CANONICALIZE("/usr/foo", "/inc/stdio.h"));
  EXPECT_STREQ("Absolute end /", "/inc/sys/", CANONICALIZE("/usr/foo", "/inc/sys/"));
  EXPECT_STREQ("Current", "/usr/foo/inc/stdio.h", CANONICALIZE("/usr/foo", "./inc/stdio.h"));
  EXPECT_STREQ("Parent", "/usr/inc/stdio.h", CANONICALIZE("/usr/foo", "../inc/stdio.h"));
  EXPECT_STREQ("Path is root w/ ..", "/baz", CANONICALIZE("/usr/foo", "/bar/../baz"));
  EXPECT_STREQ("Redundant slash", "/usr/foo/bar/baz", CANONICALIZE("/usr/foo", "bar//baz"));
  EXPECT_STREQ("Root 1", "/", CANONICALIZE("/", "."));
  EXPECT_STREQ("Root 2", "/", CANONICALIZE("/usr/foo", "../.."));
  EXPECT_STREQ("Root 3", "/", CANONICALIZE("/usr/foo", "../../"));
  EXPECT_STREQ("Root 4", "/", CANONICALIZE(".", "/"));
  EXPECT_NULL(CANONICALIZE("/usr/foo", "../../../bar"));
  EXPECT_STREQ("Ancestor", "../bar", CANONICALIZE("usr/foo", "../../../bar"));
  EXPECT_STREQ("Root end with '/'", "/usr/foo/inc/stdio.h",
               CANONICALIZE("/usr/foo/", "inc/stdio.h"));
  EXPECT_STREQ("Not root", "usr/foo/inc/stdio.h", CANONICALIZE("usr/foo", "inc/stdio.h"));
  EXPECT_STREQ("Non root ancestor", "usr/inc/stdio.h", CANONICALIZE("usr/foo", "../inc/stdio.h"));
  EXPECT_STREQ("Non root ancestor2", "../inc/stdio.h",
               CANONICALIZE("usr/foo", "../../../inc/stdio.h"));
  EXPECT_STREQ("Cwd", "foo.txt", CANONICALIZE(".", "foo.txt"));
  EXPECT_STREQ("Cwd with abs", "/bar.txt", CANONICALIZE(".", "/bar.txt"));
  EXPECT_STREQ("Cwd with abs2", "../../baz.txt", CANONICALIZE(".", "../../baz.txt"));
  EXPECT_STREQ("Parent", "../foo.txt", CANONICALIZE("..", "foo.txt"));
  EXPECT_STREQ("Parent with abs", "/bar.txt", CANONICALIZE("..", "/bar.txt"));
  EXPECT_STREQ("Parent with abs2", "../../../baz.txt", CANONICALIZE("..", "../../baz.txt"));
  EXPECT_STREQ("Relative top", ".", CANONICALIZE("usr/foo", "../.."));
  EXPECT_STREQ("Relative top with /", "./", CANONICALIZE("usr/foo", "../../"));
  EXPECT_STREQ("Relative top 3", "bar", CANONICALIZE("usr/foo", "../../bar"));
#undef CANONICALIZE
}

XTEST_MAIN();

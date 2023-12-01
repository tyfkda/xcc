#include <stdio.h>
#include <sys/stat.h>

#include <string.h>

#include "./xtest.h"

TEST(basic_access) {
  const char fn[] = "tmp_file_test.txt";
  const char data[] = "The quick brown fox jumps over the lazy dog";
  const size_t datasize = sizeof(data) - 1;  // Exclude \0.
  char buf[256];

  // fwrite
  FILE *fp = fopen(fn, "w");
  EXPECT_NOT_NULL(fp);
  if (fp != NULL) {
    EXPECT_EQ(datasize, fwrite(data, 1, datasize, fp));
    EXPECT_EQ(0, fclose(fp));
  }

  // fread
  fp = fopen(fn, "r");
  EXPECT_NOT_NULL(fp);
  if (fp != NULL) {
    EXPECT_EQ(datasize, fread(buf, 1, sizeof(buf), fp));
    EXPECT_EQ(0, memcmp(buf, data, datasize));

    // feof
    EXPECT_TRUE(feof(fp));

    // fseek: from current position.
    EXPECT_EQ(0, fseek(fp, -22, SEEK_CUR));
    // ftell
    EXPECT_EQ(datasize - 22, ftell(fp));
    EXPECT_FALSE(feof(fp));

    // fseek: to the end.
    EXPECT_EQ(0, fseek(fp, 0, SEEK_END));
    EXPECT_EQ(datasize, ftell(fp));

    EXPECT_EQ(0, fclose(fp));
  }

  // remove
  EXPECT_EQ(0, remove(fn));
} END_TEST()

TEST(stat) {
  // Test regular file.
  const char fn[] = "tmp_stat.txt";
  FILE *fp = fopen(fn, "w");
  EXPECT_NOT_NULL(fp);
  if (fp != NULL) {
    fprintf(fp, "Dummy\n");
    fclose(fp);

    struct stat st;
    int r;
    EXPECT_EQ(0, r = stat(fn, &st));
    if (r == 0) {
      EXPECT_TRUE(S_ISREG(st.st_mode));
      EXPECT_FALSE(S_ISDIR(st.st_mode));
    }

    // remove(fn);
  }

  // Test directory.
  // TODO: Add `mkdir`
  const char dn[] = "tests";
  // EXPECT_EQ(0, mkdir(dn, S_IRWXU | S_IRGRP | S_IXGRP | S_IROTH | S_IXOTH));
  {
    struct stat st;
    int r;
    EXPECT_EQ(0, r = stat(dn, &st));
    if (r == 0) {
      EXPECT_TRUE(S_ISDIR(st.st_mode));
      EXPECT_FALSE(S_ISREG(st.st_mode));
    }
  }
  // EXPECT_EQ(0, rmdir());
} END_TEST()

int main() {
  return RUN_ALL_TESTS(
    test_basic_access,
    test_stat,
  );
}

#include <stdio.h>

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

    // fseek: from current position.
    EXPECT_EQ(0, fseek(fp, -22, SEEK_CUR));
    // ftell
    EXPECT_EQ(datasize - 22, ftell(fp));

    // fseek: to the end.
    EXPECT_EQ(0, fseek(fp, 0, SEEK_END));
    EXPECT_EQ(datasize, ftell(fp));

    EXPECT_EQ(0, fclose(fp));
  }

  // remove
  EXPECT_EQ(0, remove(fn));
} END_TEST()

int main() {
  return RUN_ALL_TESTS(
    test_basic_access,
  );
}

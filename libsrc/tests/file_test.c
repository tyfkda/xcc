#include <stdio.h>
#include <sys/stat.h>
#include <unistd.h>

#include <string.h>

#include "../../tests/xtest.h"

#if 0
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
  // EXPECT_EQ(0, remove(fn));

  // mkdir
  {
    const char dn[] = "tmp_ddd";
    int r;
    EXPECT_EQ(0, r = mkdir(dn, 0755));
    if (r == 0) {
      // rmdir
      EXPECT_EQ(0, rmdir(dn));
    }
  }
}

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

    remove(fn);
  }

  // Test directory.
  const char dn[] = "tests";
  {
    struct stat st;
    int r;
    EXPECT_EQ(0, r = stat(dn, &st));
    if (r == 0) {
      EXPECT_TRUE(S_ISDIR(st.st_mode));
      EXPECT_FALSE(S_ISREG(st.st_mode));
    }
  }
}

TEST(ungetc) {
  static char buf[] = "12345";
  FILE *fp = fmemopen(buf, sizeof(buf) - 1, "r");
  EXPECT_NOT_NULL(fp);
  if (fp != NULL) {
    int c;
    EXPECT_EQ('1', fgetc(fp));
    EXPECT_EQ('2', c = fgetc(fp));
    EXPECT_EQ('2', ungetc(c, fp));
    EXPECT_EQ('2', fgetc(fp));

    for (int c2; (c2 = fgetc(fp)) != EOF; c = c2)
      ;
    EXPECT_EQ('5', c);
    EXPECT_TRUE(feof(fp));
    ungetc(c, fp);
    EXPECT_FALSE(feof(fp));
    EXPECT_EQ('5', fgetc(fp));

    fclose(fp);
  }
}
#endif

XTEST_MAIN();

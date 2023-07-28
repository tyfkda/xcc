// Compiled on XCC

#include "stdio.h"
#include "stdlib.h"  // exit

#define XTEST_NO_EXPECT_NEAR
#include "./xtest.h"

extern int array[];
extern int *ptr;
extern int sq(int x);
extern int ref_export(void);
#ifndef __NO_FLONUM
extern double many_fargs(double a, double b, double c, double d, double e, double f, double g, double h, double i);
#endif

int export_var = 9876;

TEST(all) {
  expecti64("external array", 222, array[2]);
  expecti64("external ptr", 333, ptr[3]);
  expecti64("funcall", 1234321, sq(1111));
  expecti64("export_var", 9876, ref_export());
#ifndef __NO_FLONUM
  expectf64("many_dargs", 17.0, many_fargs(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0));
#endif
} END_TEST()

int main(void) {
  return RUN_ALL_TESTS(test_all);
}

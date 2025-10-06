// Compiled on XCC

#include "stdio.h"
#include "stdlib.h"  // exit

#define XTEST_NO_EXPECT_NEAR
#include "./xtest.h"

extern int array[];
extern int *ptr;
extern int sq(int x);
extern int ref_export(void);
extern void store_common(int x);
#ifndef __NO_FLONUM
extern double many_fargs(double a, double b, double c, double d, double e, double f, double g,
                         double h, double i);
#endif

int export_var = 9876;
int common_var;

TEST(all) {
  expecti64("external array", 222, array[2]);
  expecti64("external ptr", 333, ptr[3]);
  expecti64("funcall", 1234321, sq(1111));
  expecti64("export_var", 9876, ref_export());
  expecti64("common_var", 4567, (store_common(4567), common_var));
#ifndef __NO_FLONUM
  expectf64("many_dargs", 17.0, many_fargs(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0));
#endif
}

//

typedef struct { long x, y, z; } LargeStruct;

extern LargeStruct pass_struct(LargeStruct v);

TEST(struct) {
  LargeStruct v = {111, 222, 333};
  LargeStruct v2 = pass_struct(v);
  EXPECT_TRUE(v2.x == -111 && v2.y == ~222 && v2.z == !333);
}

//

XTEST_MAIN();

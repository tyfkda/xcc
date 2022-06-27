// Compiled on XCC

#include "stdio.h"
#include "stdlib.h"  // exit

extern int array[];
extern int *ptr;
extern int sq(int x);
extern int ref_export(void);
#ifndef __NO_FLONUM
extern double many_fargs(double a, double b, double c, double d, double e, double f, double g, double h, double i);
#endif

int export = 9876;

void expect(char *title, long expected, long actual) {
  printf("%s => ", title);
  if (expected == actual) {
    printf("OK\n");
    return;
  }
  printf("NG: %ld expected, but got %ld\n", expected, actual);
  exit(1);
}

#ifndef __NO_FLONUM
void expectf(char *title, double expected, double actual) {
  printf("%s => ", title);
  if (expected == actual) {
    printf("OK\n");
    return;
  }
  printf("NG: %f expected, but got %f\n", expected, actual);
  exit(1);
}
#endif

int main(void) {
  expect("external array", 222, array[2]);
  expect("external ptr", 333, ptr[3]);
  expect("funcall", 1234321, sq(1111));
  expect("export", 9876, ref_export());
#ifndef __NO_FLONUM
  expectf("many_dargs", 17.0, many_fargs(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0));
#endif

  return 0;
}

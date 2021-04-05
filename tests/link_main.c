// Compiled on XCC

#include "../inc/stdarg.h"
#include "../inc/stdlib.h"  // exit
#include "../examples/util.h"

extern int array[];
extern int *ptr;
extern int sq(int x);
extern int ref_export(void);
extern double many_fargs(double a, double b, double c, double d, double e, double f, double g, double h, double i);

int export = 9876;

void expect(char *title, long expected, long actual) {
  putstr(title);
  putstr(" => ");
  if (expected == actual) {
    putstr("OK\n");
    return;
  }
  putstr("NG: ");
  putdeci(expected);
  putstr(" expected, but got ");
  putdeci(actual);
  putstr("\n");
  exit(1);
}

void expectf(char *title, double expected, double actual) {
  putstr(title);
  putstr(" => ");
  if (expected == actual) {
    putstr("OK\n");
    return;
  }
  putstr("NG: ");
  putdeci(expected);
  putstr(" expected, but got ");
  putdeci(actual);
  putstr("\n");
  exit(1);
}

int main(void) {
  expect("external array", 222, array[2]);
  expect("external ptr", 333, ptr[3]);
  expect("funcall", 1234321, sq(1111));
  expect("export", 9876, ref_export());
  expectf("many_dargs", 17.0, many_fargs(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0));

  return 0;
}

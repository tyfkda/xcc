// Compiled on gcc

#include <stdarg.h>

int array[] = {0, 111, 222, 333, 444, 555, 666, 777};
int *ptr = array;

extern int export_var;
int common_var;

int sq(int x) {
  return x * x;
}

int ref_export(void) {
  return export_var;
}

void store_common(int x) {
  common_var = x;
}

#ifndef __NO_FLONUM
double many_fargs(double a, double b, double c, double d, double e, double f, double g, double h,
                  double i) {
  return h + i;
}
#endif

//

typedef struct { short w; } SmallStruct;
typedef struct { long x, y, z; } LargeStruct;

LargeStruct pass_struct(LargeStruct v) {
  return (LargeStruct){-v.x, ~v.y, !v.z};
}

long pass_struct_small_large(int n, SmallStruct s, LargeStruct l) {
  long acc;
  acc = s.w * 1000 + l.x * 100 + l.y * 10 + l.z;
  return acc;
}

long pass_struct_small_large_vaargs(int n, SmallStruct s, LargeStruct l, ...) {
  va_list ap;
  va_start(ap, l);
  long acc;
  acc = s.w * 1000 + l.x * 100 + l.y * 10 + l.z;
  for (int i = 0; i < n; ++i) {
    if ((i & 1) == 0)
      acc = acc * 10 + va_arg(ap, SmallStruct).w;
    else
      acc = acc * 10 + va_arg(ap, LargeStruct).y;
  }
  return acc;
}

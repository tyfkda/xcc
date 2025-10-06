// Compiled on gcc

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

typedef struct { long x, y, z; } LargeStruct;

LargeStruct pass_struct(LargeStruct v) {
  return (LargeStruct){-v.x, ~v.y, !v.z};
}

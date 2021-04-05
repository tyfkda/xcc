// Compiled on gcc

int array[] = {0, 111, 222, 333, 444, 555, 666, 777};
int *ptr = array;

extern int export;

int sq(int x) {
  return x * x;
}

int ref_export(void) {
  return export;
}

double many_fargs(double a, double b, double c, double d, double e, double f, double g, double h, double i) {
  return h + i;
}

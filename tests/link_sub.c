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

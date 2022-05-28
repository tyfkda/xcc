#include <stdio.h>
#include <stdlib.h>

int compare(const void *va, const void *vb) {
  const int *pa = va;
  const int *pb = vb;
  return *pa - *pb;
}

int main(int argc, char *argv[]) {
  int array[] = {5, 9, 3, 8, 4, 0, 7, 1, 6, 2};

  qsort(array, sizeof(array) / sizeof(*array), sizeof(*array), compare);

  for (int i = 0; i < sizeof(array) / sizeof(*array); ++i)
    printf("%d ", array[i]);
  printf("\n");
  return 0;
}
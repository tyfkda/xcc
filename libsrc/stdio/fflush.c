#include "stdio.h"

int fflush(FILE *fp) {
  return fseek(fp, 0, SEEK_CUR);
}

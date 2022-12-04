#include "stdio.h"

#include "./_file.h"

int fputc(int c, FILE* fp) {
  return FPUTC(c, fp);
}

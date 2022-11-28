#include "stdio.h"

#include "./_file.h"

int fgetc(FILE *fp) {
  unsigned char c;
  size_t count = fread(&c, 1, 1, fp);
  return count == 1 ? c : EOF;
}

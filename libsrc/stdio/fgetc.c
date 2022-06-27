#include "stdio.h"
#include "unistd.h"  // read

#include "./_file.h"

int fgetc(FILE *fp) {
  unsigned char c;
  int len = read(fp->fd, &c, 1);
  return len == 1 ? c : EOF;
}

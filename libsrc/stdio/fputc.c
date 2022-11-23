#include "stdio.h"
#include "unistd.h"  // write

#include "./_file.h"

int fputc(int c, FILE *fp) {
  unsigned char cc = c;
  int len = write(fp->fd, &cc, 1);
  return len == 1 ? c : EOF;
}

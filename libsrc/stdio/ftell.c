#include "stdio.h"
#include "unistd.h"  // read

#include "./_file.h"

long ftell(FILE *fp) {
  off_t result = lseek(fp->fd, 0, SEEK_CUR);
  return result >= 0 ? result + fp->wp - (fp->rs - fp->rp) : result;
}

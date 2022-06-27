#include "stdio.h"
#include "unistd.h"  // read

#include "./_file.h"

long ftell(FILE *fp) {
  return lseek(fp->fd, 0, SEEK_CUR);
}

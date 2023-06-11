#include "stdio.h"
#include "unistd.h"  // read

#include "./_file.h"

int fseek(FILE *fp, long offset, int origin) {
  fflush(fp);
  off_t result = lseek(fp->fd, offset, origin);
  if (result == -1)
    return 1;  // TODO:
  fp->flag &= ~FF_EOF;
  return 0;
}

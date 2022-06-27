#include "stdio.h"
#include "unistd.h"  // read

#include "./_file.h"

int fseek(FILE *fp, long offset, int origin) {
  off_t result = lseek(fp->fd, offset, origin);
  if (result == -1)
    return 1;  // TODO:
  return 0;
}

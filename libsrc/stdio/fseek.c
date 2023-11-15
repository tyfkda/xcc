#include "stdio.h"

#include "./_file.h"

int fseek(FILE *fp, long offset, int origin) {
  fflush(fp);
  off_t o = offset;
  if ((fp->iof->seek)(fp, &o, origin) == -1)
    return -1;
  return 0;
}

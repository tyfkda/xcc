#include "stdio.h"

#include "./_file.h"

int fseek(FILE *fp, long offset, int origin) {
  off_t o = offset;
  return (fp->iof->seek)(fp, &o, origin);
}

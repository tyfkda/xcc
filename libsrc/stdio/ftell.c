#include "stdio.h"

#include "./_file.h"

long ftell(FILE *fp) {
  off_t o = 0;
  ssize_t result = (fp->iof->seek)(fp, &o, SEEK_CUR);
  if (result >= 0) {
    result = o + fp->wp - (fp->rs - fp->rp);
  }
  return result;
}

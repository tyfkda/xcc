#include "stdio.h"

#include "./_file.h"

int fputc(int c, FILE* fp) {
  if (!(fp->flag & FF_WRITE))
    return EOF;

  return FPUTC(c, fp);
}

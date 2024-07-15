#include "stdio.h"

#include "./_file.h"

// Make sure inline function is emitted.
extern inline int FPUTC(int c, FILE *fp);

int fputc(int c, FILE* fp) {
  if (!(fp->flag & FF_WRITE))
    return EOF;

  return FPUTC(c, fp);
}

#include "stdio.h"

#include "./_file.h"

int ungetc(int c, FILE *fp) {
  if (!(fp->flag & FF_READ))
    return EOF;
  fp->unget_char = c;
  fp->flag &= ~FF_EOF;
  return c;
}

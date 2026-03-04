#include "stdio.h"

#include "_file.h"

void setbuf(FILE *fp, char *buf) {
  int mode = fp->flag & FF_BINARY ? _IOFBF : _IOLBF;
  setvbuf(fp, buf, mode, BUFSIZ);
}

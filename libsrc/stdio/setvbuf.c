#include "stdio.h"

#include "_file.h"
#include "errno.h"

int setvbuf(FILE *fp, char *buf, int mode, size_t size) {
  int bufflag = 0;
  switch (mode) {
  case _IOFBF:  bufflag = FF_BUF_FULL; break;
  case _IOLBF:  bufflag = FF_BUF_LINE; break;
  case _IONBF:  break;
  default:
    errno = EINVAL;
    return -1;
  }
  int flag = fp->flag;
  fp->wbuf = fp->rbuf = (unsigned char*)buf;  // !!!
  fp->rcapa = fp->wcapa = size;
  fp->rp = fp->rs = fp->wp = 0;
  fp->unget_char = EOF;

  fp->flag = (flag & ~FF_BUF_MASK) | bufflag;

  return 0;
}

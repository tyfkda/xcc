#include "stdio.h"

#include "unistd.h"
#include "./_file.h"

static int _fflush(FILE *fp) {
  unsigned int wp = fp->wp;
  if (wp > 0) {
    size_t write_size = write(fp->fd, fp->wbuf, wp);
    if (write_size != (size_t)wp)
      return EOF;
    fp->wp = 0;
  }
  return 0;
}

int _fseek(void *cookie, off_t *offset, int origin) {
  FILE *fp = cookie;
  _fflush(fp);
  off_t result = lseek(fp->fd, *offset, origin);
  if (result == -1)
    return -1;
  fp->flag &= ~FF_EOF;
  *offset = result;
  return 0;
}

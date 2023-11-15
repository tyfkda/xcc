#include "stdio.h"

#include "unistd.h"
#include "./_file.h"

int _fseek(void *cookie, off_t *offset, int origin) {
  FILE *fp = cookie;
  off_t result = lseek(fp->fd, *offset, origin);
  if (result == -1)
    return -1;
  fp->flag &= ~FF_EOF;
  fp->rp = fp->rs = 0;
  *offset = result;
  return 0;
}

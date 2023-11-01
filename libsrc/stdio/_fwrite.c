#include "stdio.h"

#include "./_file.h"

#define FLUSH(fp)  (fflush(fp) == 0)

ssize_t _fwrite(void *cookie, const char *buf, size_t size) {
  FILE *fp = cookie;
  _finit(fp);
  if (fp->wp >= fp->ws && !FLUSH(fp))
    return EOF;

  for (size_t i = 0; i < size; ++i) {
    char c = *buf++;
    fp->wbuf[fp->wp++] = c;
    if (fp->wp >= fp->ws ||
        (c == '\n' && !(fp->flag & FF_BINARY))) {
      if (!FLUSH(fp))
        return i;
    }
  }
  return size;
}

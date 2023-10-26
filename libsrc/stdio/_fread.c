#include "stdio.h"
#include "string.h"  // memcpy
#include "unistd.h"  // read

#include "./_file.h"

// Normal: if actual read size is less than required size, it means EOF.
// Pipe:   if subscriber is faster than publisher, it happens. len == 0 means EOF.
#define FILE_IS_END(fp, readsize)  (!((fp)->flag & FF_FIFO) || (readsize) == 0)

extern ssize_t _fread(void *cookie, char *buf, size_t total) {
  FILE *fp = cookie;
  _finit(fp);
  unsigned char *p = (unsigned char*)buf;
  if (fp->rs > 0) {
    size_t d = fp->rs - fp->rp;
    if (total <= d) {
      memcpy(p, &fp->rbuf[fp->rp], total);
      fp->rp += total;
      if (fp->rp >= fp->rs)
        fp->rp = fp->rs = 0;
      return total;
    }
    memcpy(p, &fp->rbuf[fp->rp], d);
    total -= d;
    p += d;
  }

  if (total < sizeof(fp->rbuf) * 3 / 4) {
    // Read more than requested size and store them to the buffer.
    ssize_t len = read(fp->fd, fp->rbuf, sizeof(fp->rbuf));
    fp->rs = len;
    size_t n = len < 0 || (size_t)len < total ? (size_t)len : total;
    fp->rp = n;
    if (n > 0) {
      memcpy(p, fp->rbuf, n);
      p += n;
    }
    if (len < (ssize_t)sizeof(fp->rbuf) && (ssize_t)n == len) {
      if (FILE_IS_END(fp, len))
        fp->flag |= FF_EOF;
    }
  } else {
    // Read to the given buffer directly.
    ssize_t len = read(fp->fd, p, total);
    if (len >= 0) {
      p += len;
      fp->rp = fp->rs = 0;
      if ((size_t)len < total) {
        if (FILE_IS_END(fp, len))
          fp->flag |= FF_EOF;
      }
    }
  }

  // TODO: Align by size.
  return (uintptr_t)(p - (unsigned char*)buf);
}

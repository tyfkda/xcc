#include "stdio.h"
#include "string.h"  // memcpy
#include "unistd.h"  // read

#include "./_file.h"

size_t fread(void *buffer, size_t size, size_t count, FILE *fp) {
  unsigned char *p = buffer;
  size_t total = size * count;
  if (fp->rs > 0) {
    int d = fp->rs - fp->rp;
    if (total <= d) {
      memcpy(p, &fp->rbuf[fp->rp], total);
      fp->rp += total;
      if (fp->rp >= fp->rs)
        fp->rp = fp->rs = 0;
      return total / size;
    }
    memcpy(p, &fp->rbuf[fp->rp], d);
    total -= d;
    p += d;
  }

  if (total < sizeof(fp->rbuf) * 3 / 4) {
    // Read more than requested size and store them to the buffer.
    ssize_t len = read(fp->fd, fp->rbuf, sizeof(fp->rbuf));
    fp->rs = len;
    size_t n = len < total ? len : total;
    fp->rp = n;
    if (n > 0) {
      memcpy(p, fp->rbuf, n);
      p += n;
    }
  } else {
    // Read to the given buffer directly.
    ssize_t len = read(fp->fd, p, total);
    p += len;
    fp->rp = fp->rs = 0;
  }

  // TODO: Align by size.
  return (uintptr_t)(p - (unsigned char*)buffer) / size;
}

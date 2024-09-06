#include "stdio.h"

#include "stdlib.h"  // free
#include "string.h"  // memcpy
#include "sys/stat.h"  // fstat
#include "unistd.h"

#include "./_file.h"

// Normal: if actual read size is less than required size, it means EOF.
// Pipe:   if subscriber is faster than publisher, it happens. len == 0 means EOF.
#define FILE_IS_END(fp, readsize)  (!((fp)->flag & FF_FIFO) || (readsize) == 0)

static void _finit(FILE *fp) {
  if ((fp->flag & FF_INITIALIZED))
    return;

  struct stat st;
  int r = fstat(fp->fd, &st);
  if (r == 0) {
    switch (st.st_mode & S_IFMT) {
    case S_IFIFO:
    case S_IFSOCK:
      fp->flag |= FF_FIFO;
      break;
    default:
      break;
    }
  } else {
    // TODO: Retry.
  }
  fp->flag |= FF_INITIALIZED;
}

ssize_t _fread(void *cookie, char *buf, size_t total) {
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

  if (total < fp->rcapa * 3 / 4) {
    // Read more than requested size and store them to the buffer.
    ssize_t len = read(fp->fd, fp->rbuf, fp->rcapa);
    fp->rs = len;
    size_t n = len < 0 || (size_t)len < total ? (size_t)len : total;
    fp->rp = n;
    if (n > 0) {
      memcpy(p, fp->rbuf, n);
      p += n;
    }
    if (len < (ssize_t)fp->rcapa && (ssize_t)n == len) {
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

ssize_t _fwrite(void *cookie, const char *buf, size_t size) {
  FILE *fp = cookie;
  _finit(fp);
  if (fp->wp >= fp->wcapa && (*fp->flush)(fp) != 0)
    return EOF;

  for (size_t i = 0; i < size; ++i) {
    char c = *buf++;
    fp->wbuf[fp->wp++] = c;
    if (fp->wp >= fp->wcapa ||
        (c == '\n' && !(fp->flag & FF_BINARY))) {
      if ((*fp->flush)(fp) != 0)
        return i;
    }
  }
  return size;
}

int _fflush(FILE *fp) {
  if (fp->flag & FF_WRITE) {
    unsigned int wp = fp->wp;
    if (wp > 0) {
      size_t write_size = write(fp->fd, fp->wbuf, wp);
      if (write_size != (size_t)wp)
        return EOF;
      fp->wp = 0;
    }
  }
  return 0;
}

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

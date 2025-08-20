#include "stdio.h"

#include "errno.h"
#include "fcntl.h"  // O_ACCMODE, etc.
#include "stdbool.h"
#include "stdlib.h"
#include "string.h"  // strchr

#include "_file.h"

#define MINCAPA  (16)

static bool _growmem(FILE *fp) {
  int newsiz = fp->wcapa * 2;
  if (newsiz < MINCAPA)
    newsiz = MINCAPA;
  void *newbuf = realloc(fp->wbuf, newsiz);
  if (newbuf == NULL)
    return false;
  fp->wbuf = newbuf;
  fp->wcapa = newsiz;
  return true;
}

static ssize_t _memread(void *cookie, char *buf, size_t size) {
  FILE *fp = cookie;
  unsigned char *p = (unsigned char*)buf;
  unsigned int sz = fp->wcapa - fp->wp;
  if (sz <= 0) {
    fp->flag |= FF_EOF;
    return 0;
  }
  if (sz > size)
    sz = size;

  memcpy(p, &fp->wbuf[fp->wp], sz);
  fp->wp += sz;
  return sz;
}

static ssize_t _memwrite(void *cookie, const char *buf, size_t size) {
  FILE *fp = cookie;
  for (size_t i = 0; i < size; ++i) {
    if (fp->wp + 1 >= fp->wcapa &&
        !((fp->flag & FF_GROWMEM) && _growmem(fp))) {
      break;
    }
    fp->wbuf[fp->wp++] = *buf++;
  }
  return size;  // Returns size even if buffer filled.
}

static int _memflush(FILE *fp) {
  unsigned int wp = fp->wp;
  if (wp < fp->wcapa) {
    fp->wbuf[wp] = '\0';
  }
  return 0;
}

static int _memseek(void *cookie, off_t *offset, int origin) {
  FILE *fp = cookie;
  (*fp->flush)(fp);
  switch (origin) {
  case SEEK_SET:
    fp->wp = *offset;
    break;
  case SEEK_CUR:
    // TODO: Check range.
    fp->wp += *offset;
    break;
  case SEEK_END:
    // TODO: Check range.
    fp->wp = fp->wcapa + *offset;
    break;
  }
  fp->unget_char = EOF;
  return 0;
}

static int _memclose(void *cookie) {
  FILE *fp = cookie;
  _remove_opened_file(fp);
  (*fp->flush)(fp);
  free(fp);
  return 0;
}

void _fmemopen2(void *buf, size_t size, const char *mode, FILE *fp) {
  static const cookie_io_functions_t kMemVTable = {
    .read = _memread,
    .write = _memwrite,
    .seek = _memseek,
    .close = _memclose,
  };

  fp->iof = &kMemVTable;
  fp->flush = _memflush;
  fp->fd = -1;
  fp->rp = fp->rs = 0;
  fp->wp = 0;
  fp->wbuf = buf;
  fp->wcapa = size;
  fp->unget_char = EOF;

  int flag = 0;
  int oflag = _detect_open_flag(mode);
  switch (oflag & O_ACCMODE) {
  case O_RDONLY:  flag |= FF_READ; break;
  case O_WRONLY:  flag |= FF_WRITE; break;
  case O_RDWR:    flag |= FF_READ | FF_WRITE; break;
  default: break;
  }
  if (strchr(mode, 'b') != NULL)
    flag |= FF_BINARY;
  fp->flag = flag;

  _add_opened_file(fp);
}

FILE *fmemopen(void *buf, size_t size, const char *mode) {
  if (size <= 0) {
    errno = EINVAL;
    return NULL;
  }

  FILE *fp = calloc(1, sizeof(*fp));
  if (fp == NULL) {
    errno = ENOMEM;
  } else {
    _fmemopen2(buf, size, mode, fp);
  }
  return fp;
}

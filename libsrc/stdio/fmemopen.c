#include "stdio.h"

#include "stdbool.h"
#include "stdlib.h"
#include "string.h"

#include "_file.h"

#define MINCAPA  (16)

static bool _growmem(FILE *fp) {
  int newsiz = fp->ws * 2;
  if (newsiz < MINCAPA)
    newsiz = MINCAPA;
  void *newbuf = realloc(fp->wbuf, newsiz);
  if (newbuf == NULL)
    return false;
  fp->wbuf = newbuf;
  fp->ws = newsiz;
  return true;
}

static ssize_t _memread(void *cookie, char *buf, size_t size) {
  FILE *fp = cookie;
  unsigned char *p = (unsigned char*)buf;
  unsigned int sz = fp->ws - fp->wp;
  if (sz > size)
    sz = size;

  memcpy(p, &fp->wbuf[fp->wp], sz);
  fp->wp += sz;
  return sz;
}

static ssize_t _memwrite(void *cookie, const char *buf, size_t size) {
  FILE *fp = cookie;
  for (size_t i = 0; i < size; ++i) {
    if (fp->wp + 1 >= fp->ws &&
        !((fp->flag & FF_GROWMEM) && _growmem(fp))) {
      break;
    }
    fp->wbuf[fp->wp++] = *buf++;
  }
  return size;  // Returns size even if buffer filled.
}

static int _memflush(FILE *fp) {
  unsigned int wp = fp->wp;
  if (wp < fp->ws) {
    fp->wbuf[wp] = '\0';
  }
  if (fp->pmem != NULL)
    *fp->pmem = (char*)fp->wbuf;
  if (fp->psize != NULL)
    *fp->psize = wp;
  return 0;
}

static int _memseek(void *cookie, off_t *offset, int origin) {
  FILE *fp = cookie;
  _memflush(fp);
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
    fp->wp = fp->ws + *offset;
    break;
  }
  return 0;
}

static int _memclose(void *cookie) {
  FILE *fp = cookie;
  _remove_opened_file(fp);
  _memflush(fp);
  free(fp);
  return 0;
}

FILE *fmemopen(void *buf, size_t size, const char *mode) {
  FILE *fp = fdopen(-1, mode);
  if (fp != NULL) {
    static const cookie_io_functions_t kMemVTable = {
      .read = _memread,
      .write = _memwrite,
      .seek = _memseek,
      .close = _memclose,
    };

    fp->iof = &kMemVTable;
    fp->flush = _memflush;
    fp->wbuf = buf;
    fp->ws = size;
    fp->pmem = NULL;
    fp->psize = NULL;
  }
  return fp;
}

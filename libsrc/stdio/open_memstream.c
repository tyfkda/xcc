#include "stdio.h"

#include "stdlib.h"
#include "_file.h"

extern void _fmemopen2(void *buf, size_t size, const char *mode, FILE *fp);

typedef struct MEMFILE {
  FILE file;
  char **pmem;
  size_t *psize;
} MEMFILE;

static int _memflush2(FILE *fp) {
  unsigned int wp = fp->wp;
  if (wp < fp->wcapa) {
    fp->wbuf[wp] = '\0';
  }
  MEMFILE *memfp = (MEMFILE*)fp;
  if (memfp->pmem != NULL)
    *memfp->pmem = (char*)fp->wbuf;
  if (memfp->psize != NULL)
    *memfp->psize = wp;
  return 0;
}

FILE *open_memstream(char **ptr, size_t *sizeloc) {
  MEMFILE *fp = calloc(1, sizeof(*fp));
  if (fp != NULL) {
    _fmemopen2(NULL, 0, "w", &fp->file);
    fp->file.flush = _memflush2;
    fp->file.flag |= FF_GROWMEM;
    fp->pmem = ptr;
    fp->psize = sizeloc;
  }
  return &fp->file;
}

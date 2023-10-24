#pragma once

#include "stddef.h"  // size_t

#define FF_BINARY   (1 << 0)
#define FF_READ     (1 << 1)
#define FF_WRITE    (1 << 2)
#define FF_EOF      (1 << 3)
#define FF_MEMORY   (1 << 4)
#define FF_GROWMEM  (1 << 5)

struct FILE {
  int (*fputc)(int c, FILE *fp);
  unsigned char *wbuf;
  // TODO: fflush.

  int fd;
  unsigned int rp, rs;
  unsigned int wp, ws;
  unsigned int flag;

  // TODO: allocate buffers only if required.
  union {
    struct {
      unsigned char rbuf[256];
      unsigned char wwork[32];
    };  // For file.
    struct {
      char **pmem;
      size_t *psize;
    };  // For memory.
  };
};

extern int _fputc(int c, FILE* fp);

#define FPUTC(c, fp)  ((fp->fputc)(c, fp))

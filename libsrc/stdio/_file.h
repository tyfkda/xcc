#pragma once

#define FF_BINARY  (1 << 0)
#define FF_MEMORY  (1 << 1)

struct FILE {
  int (*fputc)(int c, FILE *fp);
  unsigned char *wbuf;
  // TODO: fflush.

  int fd;
  int rp, rs;
  int wp, ws;
  int flag;

  // TODO: allocate buffers only if required.
  struct {
    unsigned char rbuf[256];
    unsigned char wwork[32];
  };
};

extern int _fputc(int c, FILE* fp);

#define FPUTC(c, fp)  ((fp->fputc)(c, fp))

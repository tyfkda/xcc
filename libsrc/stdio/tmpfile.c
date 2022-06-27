#include "stdio.h"

#if defined(__WASM)
extern int _tmpfile(void);

FILE *tmpfile(void) {
  int fd = _tmpfile();
  if (fd < 0)
    return NULL;

  FILE *fp = malloc(sizeof(*fp));
  if (fp == NULL) {
    close(fd);
    return NULL;
  }

  fp->fd = fd;
  return fp;
}
#endif

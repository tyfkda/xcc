#include "stdio.h"
#include "stdlib.h"  // free
#include "unistd.h"  // close

#include "_file.h"
#include "_fileman.h"

static void remove_opened(FILE *fp) {
  extern FILEMAN __fileman;

  FILE **files = __fileman.opened;
  for (int i = 0, length = __fileman.length; i < length; ++i) {
    if (files[i] == fp) {
      --length;
      if (i < length) {
        // Swap to the last.
        files[i] = files[length];
      }
      __fileman.length = length;
      break;
    }
  }
}

int fclose(FILE *fp) {
  if (fp == NULL)
    return EOF;
  remove_opened(fp);
  fflush(fp);
  if (!(fp->flag & FF_MEMORY))
    close(fp->fd);
  free(fp);
  return 0;
}

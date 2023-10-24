#include "stdio.h"

#include "stdlib.h"  // free
#include "unistd.h"  // close
#include "./_file.h"
#include "_fileman.h"

void _remove_opened_file(FILE *fp) {
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

int _fclose(void *cookie) {
  FILE *fp = cookie;
  _remove_opened_file(fp);
  fflush(fp);
  close(fp->fd);
  free(fp);
  return 0;
}

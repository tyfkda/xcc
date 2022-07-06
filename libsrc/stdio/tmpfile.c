#include "stdio.h"

#if defined(__WASM)
extern int _tmpfile(void);
#else
#include "stdlib.h"  // mkstemp
#include "unistd.h"  // close
#endif

FILE *tmpfile(void) {
#if defined(__WASM)
  int fd = _tmpfile();
#else
  char template[] = "/tmp/tmpXXXXXX";
  int fd = mkstemp(template);
#endif
  FILE *fp = NULL;
  if (fd >= 0) {
    fp = fdopen(fd, "w+");
    if (fp == NULL) {
      close(fd);
    }
  }
  return fp;
}

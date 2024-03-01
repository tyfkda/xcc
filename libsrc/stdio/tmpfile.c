#include "stdio.h"

#include "stdlib.h"  // mkstemp
#include "unistd.h"  // close

FILE *tmpfile(void) {
  char template[] = "/tmp/tmpXXXXXX";
  int fd = mkstemp(template);
  FILE *fp = NULL;
  if (fd >= 0) {
    unlink(template);
    fp = fdopen(fd, "w+");
    if (fp == NULL) {
      close(fd);
    }
  }
  return fp;
}

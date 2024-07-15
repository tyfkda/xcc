#include "stdio.h"
#include "fcntl.h"  // open
#include "unistd.h"  // close

#include "_file.h"

FILE *fopen(const char *fileName, const char *mode) {
  FILE *fp = NULL;
  int flag = _detect_open_flag(mode);
  if (flag != -1) {
    const int mod = S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH;
    int fd = open(fileName, flag, mod);
    if (fd >= 0) {
      fp = fdopen(fd, mode);
      if (fp == NULL) {
        close(fd);
      }
    }
  }
  return fp;
}

#include "stdio.h"
#include "fcntl.h"  // open
#include "stdlib.h"  // malloc
#include "string.h"  // strcmp
#include "unistd.h"  // close

#include "_file.h"

FILE *fopen(const char *fileName, const char *mode) {
  static const struct {
    const char *str;
    int flag;
  } kTable[] = {
    {"r", O_RDONLY},
    {"w", O_WRONLY | O_CREAT | O_TRUNC},
    {"a", O_WRONLY | O_CREAT | O_APPEND},
    {"rb", O_RDONLY},
    {"wb", O_WRONLY | O_CREAT | O_TRUNC},
    {"ab", O_WRONLY | O_CREAT | O_APPEND},
    {"r+", O_RDONLY},
    {"w+", O_WRONLY | O_CREAT},
    {"a+", O_WRONLY | O_CREAT | O_APPEND},
    {"r+b", O_RDONLY},
    {"w+b", O_WRONLY | O_CREAT},
    {"a+b", O_WRONLY | O_CREAT | O_APPEND},
    {"rb+", O_RDONLY},
    {"wb+", O_WRONLY | O_CREAT},
    {"ab+", O_WRONLY | O_CREAT | O_APPEND},
  };

  int flag = -1;
  for (int i = 0; i < sizeof(kTable) / sizeof(*kTable); ++i) {
    if (strcmp(kTable[i].str, mode) == 0) {
      flag = kTable[i].flag;
      break;
    }
  }
  if (flag == -1)
    return NULL;

  int mod = S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH;
  int fd = open(fileName, flag, mod);
  if (fd < 0) {
    return NULL;
  }

  FILE *fp = malloc(sizeof(*fp));
  if (fp == NULL) {
    close(fd);
    return NULL;
  }

  fp->fd = fd;
  return fp;
}

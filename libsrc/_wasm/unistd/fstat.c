#include "sys/stat.h"
#include "errno.h"
#include "../wasi.h"

extern void _set_stat(Filestat *fs, struct stat *st);

int fstat(int fd, struct stat *st) {
  Filestat fs;
  uint32_t result = fd_filestat_get(fd, &fs);
  if (result == 0) {
    _set_stat(&fs, st);
    return 0;
  }
  errno = ENOENT;
  return -1;
}

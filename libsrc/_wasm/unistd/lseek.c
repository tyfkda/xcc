#include "unistd.h"

off_t lseek(int fd, off_t offset, int whence) {
  size_t size;
  int result = fd_seek(fd, offset, whence, &size);
  return result == 0 ? size : -1;
}

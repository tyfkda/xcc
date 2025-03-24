#include "unistd.h"
#include "errno.h"
#include "../wasi.h"

off_t lseek(int fd, off_t offset, int whence) {
  size_t size;
  int result = fd_seek(fd, offset, whence, &size);
  if (result == 0)
    return size;
  errno = EIO;  // TODO
  return -1;
}

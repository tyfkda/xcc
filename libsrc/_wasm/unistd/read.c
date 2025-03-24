#include "unistd.h"
#include "errno.h"
#include "../wasi.h"

ssize_t read(int fd, void *buf, size_t size) {
  Iov iov;
  iov.str = buf;
  iov.n = size;
  size_t readed;
  int result = fd_read(fd, &iov, 1, &readed);
  if (result == 0)
    return readed;
  errno = EIO;  // TODO
  return -1;
}

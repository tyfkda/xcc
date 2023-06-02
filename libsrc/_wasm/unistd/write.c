#include "unistd.h"
#include "../wasi.h"

ssize_t write(int fd, const void *str, size_t len) {
  Iov iov;
  iov.str = str;
  iov.n = len;
  size_t written;
  int result = fd_write(fd, &iov, 1, &written);
  return result == 0 ? written : -1;
}

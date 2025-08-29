#include "unistd.h"
#include "_syscall.h"

ssize_t read(int fd, void *buf, size_t size) {
  ssize_t ret;
  SYSCALL_RET(__NR_read, ret, "r"(fd), "r"(buf), "r"(size));
  SET_ERRNO(ret);
  return ret;
}

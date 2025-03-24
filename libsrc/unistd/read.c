#include "unistd.h"
#include "_syscall.h"

ssize_t read(int fd, void *buf, size_t size) {
  ssize_t ret;
  SYSCALL_RET(__NR_read, ret);
  SET_ERRNO(ret);
  return ret;
}

#include "unistd.h"
#include "../wasi.h"

int close(int fd) {
  return fd_close(fd);
}

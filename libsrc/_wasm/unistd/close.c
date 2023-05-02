#include "unistd.h"
#include "../wasi.h"
#include "./wasi_impl.h"

int close(int fd) {
  return fd_close(fd);
}

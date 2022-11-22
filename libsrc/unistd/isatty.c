#if !defined(__WASM)
#include "unistd.h"
#include "sys/ioctl.h"  // termio

int isatty(int fd) {
#if defined(__NR_ioctl)
  struct termio tm;
  return ioctl(fd, TCGETA, &tm) == 0 ? 1 : 0;
#else
  // TODO:
  return fd < 3;
#endif
}
#endif

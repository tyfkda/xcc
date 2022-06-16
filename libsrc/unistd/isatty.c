#include "unistd.h"
#include "sys/ioctl.h"  // termio

int isatty(int fd) {
  struct termio tm;
  return ioctl(fd, TCGETA, &tm) == 0 ? 1 : 0;
}

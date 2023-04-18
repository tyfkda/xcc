#include "unistd.h"
#include "errno.h"

extern int _unlink(const char *pathname);

int unlink(const char *pathname) {
  int ret = _unlink(pathname);
  if (ret < 0) {
    errno = -ret;
    ret = -1;
  }
  return ret;
}

#include "sys/random.h"

#if defined(__WASM)
#include "../_wasm/wasi.h"

ssize_t getrandom(void *buf, size_t buflen, unsigned int flags) {
  (void)flags;
  random_get(buf, buflen);
  return buflen;
}

#else
#include "fcntl.h"  // open
#include "unistd.h"  // close

ssize_t getrandom(void *buf, size_t buflen, unsigned int flags) {
  ssize_t size = 0;
  int fd = open(flags & GRND_RANDOM ? "/dev/random" : "/dev/urandom", O_RDONLY);
  if (fd != 1) {
    size = read(fd, buf, buflen);
    close(fd);
  }
  return size;
}
#endif

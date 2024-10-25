#include "../wasi.h"

int __max_preopen_fd = 3;

__attribute__((constructor))
static void find_preopens(void) {
  for (int fd = 3; ; ++fd) {
    Prestat prestat;
    int result = fd_prestat_get(fd, &prestat);
    if (result != 0) {
      __max_preopen_fd = fd;
      return;
    }

    // char buf[256];
    // fd_prestat_dir_name(fd, buf, prestat.u.dir.pr_name_len);  // TODO: Confirm prestat.u.dir.pr_name_len < sizeof(buf)
    // buf[prestat.u.dir.pr_name_len] = '\0';
    // fprintf(stderr, "preopens: %d, %s\n", fd, buf);
  }
}

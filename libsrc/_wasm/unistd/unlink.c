#include "unistd.h"
#include "errno.h"
#include "string.h"
#include "../wasi.h"

extern int __max_preopen_fd;

int unlink(const char *fn) {
  // Search from preopens
  size_t fnlen = strlen(fn);
  for (int base_fd = 3; base_fd < __max_preopen_fd; ++base_fd) {
    Prestat prestat;
    if (fd_prestat_get(base_fd, &prestat) != 0)
      break;

    size_t l = prestat.u.dir.pr_name_len;  // Includes '\0' or not, depending on the environment,
    char buf[256];
    fd_prestat_dir_name(base_fd, buf, l);
    buf[l] = '\0';

    if ((*fn == '/' && *buf != '/') ||
        (*fn != '/' && strcmp(buf, ".") != 0))
      continue;

    const char *fn2 = fn;
    size_t fnlen2 = fnlen;

    if (strncmp(fn, buf, l) == 0 && fn[l] == '/') {
      fn2 = fn + (l + 1);
      fnlen2 = fnlen - (l + 1);
    } else if (l == 1 && *buf == '/' && *fn == '/') {
      fn2 = fn + 1;
      fnlen2 = fnlen - 1;
    }
    int result = path_unlink_file(base_fd, fn2, fnlen2);
    if (result == 0)
      return 0;
  }
  return -1;
}

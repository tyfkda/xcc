#include "sys/stat.h"
#include "errno.h"
#include "string.h"  // memset, strncmp
#include "../wasi.h"

extern int __max_preopen_fd;

extern void _set_stat(Filestat *fs, struct stat *st);

int stat(const char *fn, struct stat *st) {
  memset(st, 0, sizeof(st));
  size_t fnlen = strlen(fn);
  for (int base_fd = 3; base_fd < __max_preopen_fd; ++base_fd) {
    Prestat prestat;
    fd_prestat_get(base_fd, &prestat);
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

    Filestat fs;
    uint32_t result = path_filestat_get(base_fd, 0, fn2, fnlen2, &fs);
    if (result == 0) {
      _set_stat(&fs, st);
      return 0;
    }
  }
  errno = ENOENT;
  return -1;
}

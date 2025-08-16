#include "_search_preopen.h"

#include "limits.h"
#include "string.h"
#include "sys/types.h"  // ssize_t
#include "../wasi.h"

extern ssize_t _canonicalize_path_cwd(char *buf, ssize_t size, const char *path);

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

bool _search_preopen(const char *fn_org, void *data, SearchFileCallback callback) {
  char fn[PATH_MAX];
  _canonicalize_path_cwd(fn, sizeof(fn), fn_org);

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

    if (callback(base_fd, fn2, fnlen2, data))
      return true;
  }
  return false;
}

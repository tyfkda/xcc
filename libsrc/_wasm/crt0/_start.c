#include "alloca.h"  // alloca
#include "stdlib.h"  // exit

#include "../wasi.h"

extern void __wasm_call_ctors(void);

int __max_preopen_fd = 3;

static int find_preopens(void) {
  for (int fd = 3; ; ++fd) {
    Prestat prestat;
    int result = fd_prestat_get(fd, &prestat);
    if (result != 0)
      return fd;

    // char buf[256];
    // fd_prestat_dir_name(fd, buf, prestat.u.dir.pr_name_len);  // TODO: Confirm prestat.u.dir.pr_name_len < sizeof(buf)
    // buf[prestat.u.dir.pr_name_len] = '\0';
    // fprintf(stderr, "preopens: %d, %s\n", fd, buf);
  }
}

void _start(void) {
#define main  __main_argc_argv
  extern int main(int, char**);
  char **argv;
  int argc, len;
  int r = args_sizes_get(&argc, &len);
  if (r == 0) {
    argv = alloca(sizeof(char*) * (argc + 1) + len);
    char *str = ((char*)argv) + sizeof(char*) * (argc + 1);
    args_get(argv, str);
  } else {  // Ignore error.
    argc = 1;
    argv = alloca(sizeof(char*) * (argc + 1));
    argv[0] = "*";
  }
  argv[argc] = NULL;

  __max_preopen_fd = find_preopens();

  __wasm_call_ctors();

  int ec = main(argc, argv);
  exit(ec);
#undef main
}

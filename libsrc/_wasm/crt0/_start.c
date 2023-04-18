#include "alloca.h"  // alloca
#include "stdio.h"   // fflush
#include "stdlib.h"  // atexit, exit

#include "../../stdio/_fileman.h"
#include "../wasi.h"

extern FILEMAN __fileman;

// char **environ;

static void __flush_all_files(void) {
  fflush(stdout);
  fflush(stderr);

  struct FILE **files = __fileman.opened;
  for (int i = 0, length = __fileman.length; i < length; ++i)
    fflush(files[i]);
}

static void _atexit_proc(void) {
  __flush_all_files();
}

void _start(void) {
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

  atexit(_atexit_proc);
  int ec = main(argc, argv);
  exit(ec);
}

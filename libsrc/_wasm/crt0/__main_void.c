#include "alloca.h"

#include "../wasi.h"

extern int __main_argc_argv(int, char**);

__attribute__((weak))
int __main_void(void) {
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

  return __main_argc_argv(argc, argv);
}

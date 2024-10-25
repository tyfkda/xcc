#include "alloca.h"  // alloca
#include "stdlib.h"  // exit

#include "../wasi.h"

extern void __wasm_call_ctors(void);

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

  __wasm_call_ctors();

  int ec = main(argc, argv);
  exit(ec);
#undef main
}

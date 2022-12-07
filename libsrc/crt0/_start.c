#include "stdlib.h"  // atexit, exit

#if defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wunused-function"
#endif

char **environ;

#if defined(__XV6)
void _start(void) {
  __asm("call main\n"
        "mov %eax, %edi\n"
        "jmp exit");
}

#elif defined(__linux__) || defined(__WASM)

#include "stdio.h"  // fflush
#include "../stdio/_fileman.h"

#if defined(__WASM)
#include <stdlib.h>  // malloc, exit
extern int args_sizes_get(int *pargc, int *plen);
extern int args_get(char **pargv, char *pstr);
#endif

extern FILEMAN __fileman;

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

#if defined(__WASM)
int _start(void) {
  extern int main(int, char**);
  char **argv;
  int argc, len;
  int r = args_sizes_get(&argc, &len);
  if (r == 0) {
    argv = malloc(sizeof(char*) * (argc + 1) + len);
    char *str = ((char*)argv) + sizeof(char*) * (argc + 1);
    args_get(argv, str);
  } else {  // Ignore error.
    argc = 1;
    argv = malloc(sizeof(char*) * (argc + 1));
    argv[0] = "*";
  }
  argv[argc] = NULL;

  atexit(_atexit_proc);
  int ec = main(argc, argv);
  exit(ec);
  return ec;  // Dummy.
}
#else
static void start2(int argc, char *argv[], char *env[]) {
  extern int main(int, char**, char **);
  environ = env;
  atexit(_atexit_proc);
  int ec = main(argc, argv, env);
  exit(ec);
}

void _start(void) {
#if defined(__x86_64__)
  __asm("mov (%rsp), %rdi\n"
        "lea 8(%rsp), %rsi\n"
        "lea 8(%rsi, %rdi, 8), %rdx\n"
        "jmp start2");
#elif defined(__aarch64__)
  __asm("mov x0, x1\n"
        "mov x1, x2\n"
        "mov x2, x3\n"
        "b start2");
#else
#error unknown target
#endif
}
#endif

#elif defined(__APPLE__)

// Use libc.

extern void exit(int code);

#else
#error Target not supported
#endif

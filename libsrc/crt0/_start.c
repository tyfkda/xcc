char **environ;

#if defined(__XV6)
void _start(void) {
  __asm("call main\n"
        "mov %eax, %edi\n"
        "jmp exit");
}

#elif defined(__linux__) || defined(__WASM)

#if defined(__WASM)
#include <stdlib.h>  // malloc, exit
extern int args_sizes_get(int *pargc, int *plen);
extern int args_get(char **pargv, char *pstr);
#endif

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
    char **argv = malloc(sizeof(char*) * (argc + 1));
    argv[0] = "*";
  }
  argv[argc] = NULL;

  int ec = main(argc, argv);
  exit(ec);
  return ec;  // Dummy.
}
#else
void _start(void) {
#if defined(__x86_64__)
  __asm("mov (%rsp), %rdi\n"
        "lea 8(%rsp), %rsi\n"
        "lea 8(%rsi, %rdi, 8), %rdx\n"
#if 0
        "mov %rdx, environ(%rip)\n"
#else
        "lea environ(%rip), %rax\n"
        "mov %rdx, (%rax)\n"
#endif
        "call main\n"
        "mov %eax, %edi\n"
        "jmp exit");
#elif defined(__aarch64__)
  __asm("mov x0, x1\n"
        "mov x1, x2\n"
        "mov x2, x3\n"
        "adrp x3, environ\n"
        "add x3, x3, :lo12:environ\n"
        "str x2, [x3]\n"
        "bl main\n"
        "b exit");
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

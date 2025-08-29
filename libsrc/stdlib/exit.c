#include "stdlib.h"
#include "_exit.h"

OnExitChain *__on_exit_chain;

#if defined(__linux__) || defined(__wasm)
#include "stdbool.h"

#if defined(__wasm)
#include "../_wasm/wasi.h"
#else
#include "../unistd/_syscall.h"

_Noreturn static void proc_exit(int code);
static void proc_exit(int code) {
#ifdef __NR_exit_group
  SYSCALL(__NR_exit_group);
#endif
  SYSCALL(__NR_exit, "r"(code));
  for (;;)
    ;
}
#endif

void exit(int code) {
  // TODO: Guard multiple calls

  OnExitChain *chain = __on_exit_chain;
  __on_exit_chain = NULL;
  for (; chain != NULL; chain = chain->next) {
    chain->func();
  }

  proc_exit(code);
}

#elif defined(__APPLE__)

// Use libc.

extern void exit(int code);

#else
#error Target not supported
#endif

#include "stdlib.h"
#include "_exit.h"

OnExitChain *__on_exit_chain;

#if defined(__linux__) || defined(__WASM)
#include "stdbool.h"

#if defined(__WASM)
#include "../_wasm/wasi.h"
#else
#include "../unistd/_syscall.h"

#if defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wunused-parameter"
#endif
static void proc_exit(int code) __attribute__((noreturn));
static void proc_exit(int code) {
#ifdef __NR_exit_group
  SYSCALL(__NR_exit_group);
#endif
  SYSCALL(__NR_exit);
  for (;;)
    ;
}
#endif

void exit(int code) {
  // TODO: Guard multiple calls

#if defined(__WASM)
  // On wcc, if there is a indirect function call but no actual function reference exists,
  // table/elem section are not emitted and cause a load error.
  // To avoid this, make sure a function reference exists and table/elem section are emitted.
  (void)exit;
#endif
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

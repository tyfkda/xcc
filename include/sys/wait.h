#pragma once

#if !defined(__wasm)
#include <sys/types.h>  // pid_t

#define _WSTOPPED       0x7f
#define WEXITSTATUS(x)  (((x) >> 8) & 0xff)
#define WTERMSIG(x)     ((x) & 0x7f)
#define WSTOPSIG(x)     ((x) >> 8)
#define WIFEXITED(x)    (WTERMSIG(x) == 0)
#define WIFSIGNALED(x)  (WTERMSIG(x) != _WSTOPPED && WTERMSIG(x) != 0)  // (((signed char) (((status) & 0x7f) + 1) >> 1) > 0)

#if defined(__APPLE__)
#define WIFSTOPPED(x)   (WTERMSIG(x) == _WSTOPPED && WSTOPSIG(x) != 0x13)
#define WIFCONTINUED(x) (_WSTATUS(x) == _WSTOPPED && WSTOPSIG(x) == 0x13)

#else
#define __W_CONTINUED   0xffff
#define WIFSTOPPED(x)   (WTERMSIG(x) == _WSTOPPED)
#define WIFCONTINUED(x) ((x) == __W_CONTINUED)
#endif

struct rusage;

pid_t wait(int *status);
pid_t waitpid(pid_t, int*, int);
pid_t wait4(pid_t pid, int *status, int options, struct rusage *usage);
#endif

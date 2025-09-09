#pragma once

#include <sys/types.h>  // pid_t

#define _WSTOPPED       0x7f
#define WTERMSIG(x)     ((x) & 0x7f)
#define WEXITSTATUS(x)  ((x) >> 8)
#define WIFEXITED(x)    (WTERMSIG(x) == 0)
#define WIFSIGNALED(x)  (WTERMSIG(x) != _WSTOPPED && WTERMSIG(x) != 0)

struct rusage;

pid_t wait(int *status);
pid_t waitpid(pid_t, int*, int);
pid_t wait4(pid_t pid, int *status, int options, struct rusage *usage);

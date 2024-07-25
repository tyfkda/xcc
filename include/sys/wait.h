#pragma once

#include <sys/types.h>  // pid_t

#define _WSTOPPED       0177            /* _WSTATUS if process is stopped */

#define _W_INT(i)       (i)
#define _WSTATUS(x)     (_W_INT(x) & 0177)
#define WIFEXITED(x)    (_WSTATUS(x) == 0)
#define WEXITSTATUS(x)  (_W_INT(x) >> 8)
#define WTERMSIG(x)     (_WSTATUS(x))
#define WIFSIGNALED(x)  (_WSTATUS(x) != _WSTOPPED && _WSTATUS(x) != 0)

struct rusage;

pid_t waitpid(pid_t, int*, int);
pid_t wait4(pid_t pid, int* status, int options, struct rusage *usage);

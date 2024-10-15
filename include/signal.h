#pragma once

#include <sys/types.h>  // pid_t

#define	SIGKILL	9
#define	SIGCHLD	17

#define SIG_DFL  ((void (*)(int))0)

int kill(pid_t pid, int sig);

void (*signal(int sig, void (*func)(int)))(int);

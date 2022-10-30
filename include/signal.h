#pragma once

#include "sys/types.h"  // pid_t

#define	SIGKILL	9
#define	SIGCHLD	17

int kill(pid_t pid, int sig);

void (*signal(int sig, void (*func)(int)))(int);

#pragma once

#include "sys/types.h"  // pid_t

#define	SIGKILL	9	// kill

int kill(pid_t pid, int sig);

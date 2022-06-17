#pragma once

#include "sys/types.h"  // pid_t

pid_t waitpid(pid_t, int*, int);
pid_t wait4(pid_t pid, int* status, int options, struct rusage *usage);

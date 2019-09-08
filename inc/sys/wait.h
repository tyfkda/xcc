#pragma once

#include "sys/types.h"  // pid_t

pid_t waitpid(pid_t, int*, int);

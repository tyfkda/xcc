#pragma once

#include <stddef.h>  // size_t
#include <sys/types.h>  // ssize_t

#define GRND_NONBLOCK (0x0001)
#define GRND_RANDOM   (0x0002)

ssize_t getrandom(void *buf, size_t buflen, unsigned int flags);

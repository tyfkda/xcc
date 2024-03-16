#pragma once

#if defined(__GNUC__) && !defined(__XCC)

#include_next <alloca.h>

#else

#include <stddef.h>  // size_t

void *alloca(size_t size);

#endif

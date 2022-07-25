#pragma once

#if (defined(__linux__) || defined(__APPLE__)) && !defined(__XV6)
#define USE_ALLOCA
#endif

#if defined(USE_ALLOCA)
#include <alloca.h>
#define ALLOCA(size)  alloca(size)
#else
#define ALLOCA(size)  malloc(size)
#endif

#if defined(__aarch64__)
#define REGARG_SIZE  (16)
#else
#define REGARG_SIZE  (8)
#endif

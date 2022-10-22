#pragma once

#if defined(__aarch64__)
# define AS_USE_CC
# if !defined(__APPLE__)
#  define NO_STD_LIB
# endif

#elif !defined(__XV6) && !defined(__linux__)
# define AS_USE_CC
#endif

#if !defined(__XV6)
#define USE_ALLOCA
#endif

#if defined(USE_ALLOCA)
#include <alloca.h>
#define ALLOCA(size)  alloca(size)
#else
#define ALLOCA(size)  malloc(size)
#endif

#if defined(__x86_64__)
#define PHYSICAL_REG_MAX   (7 - 1)  // TODO: Remove `-1`
#define PHYSICAL_FREG_MAX  (7)

#define MAX_REG_ARGS   (6)
#define MAX_FREG_ARGS  (8)

#elif defined(__aarch64__)
#define PHYSICAL_REG_MAX   (17)
#define PHYSICAL_FREG_MAX  (24)

#define MAX_REG_ARGS   (8)
#define MAX_FREG_ARGS  (8)
#endif

#if defined(__aarch64__)
#define REGARG_SIZE  (16)
#else
#define REGARG_SIZE  (8)
#endif

#if defined(__APPLE__) && defined(__aarch64__)
// variadic arguments are passed through stack, not registers.
#define VAARG_ON_STACK
#endif

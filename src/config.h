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
#define MAX_REG_ARGS   (6)
#define MAX_FREG_ARGS  (8)

#elif defined(__aarch64__)
#define MAX_REG_ARGS   (8)
#define MAX_FREG_ARGS  (8)
#endif

#if defined(__APPLE__) && defined(__aarch64__)
// variadic arguments are passed through stack, not registers.
#define VAARG_ON_STACK
#endif

#define TARGET_CHAR_BIT  8

#if defined(__ILP32__)
#define POINTER_SIZE  (4)  /*sizeof(void*)*/
#elif defined(__LP64__)
#define POINTER_SIZE  (8)  /*sizeof(void*)*/
#else
// ?
#endif

// Elf
#if defined(__x86_64__)
#define MACHINE_TYPE  EM_X86_64
#elif defined(__aarch64__)
#define MACHINE_TYPE  EM_AARCH64
#endif

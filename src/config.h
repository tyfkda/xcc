#pragma once

#ifdef _WIN32

// 32-bit or 64-bit
#if _WIN64
#define __LP64__
#else
#define __ILP32__
#endif

#else // POSIX

#define USE_ALLOCA

#endif


// Architecture
#define XCC_ARCH_X64      1
#define XCC_ARCH_AARCH64  2
#define XCC_ARCH_RISCV64  3
#define XCC_ARCH_WASM     4

#if !defined(XCC_TARGET_ARCH)
# if defined(__WASM)
#  define XCC_TARGET_ARCH  XCC_ARCH_WASM
# elif defined(__x86_64__)
#  define XCC_TARGET_ARCH  XCC_ARCH_X64
# elif defined(__aarch64__)
#  define XCC_TARGET_ARCH  XCC_ARCH_AARCH64
# elif defined(__riscv) && defined(__LP64__)
#  define XCC_TARGET_ARCH  XCC_ARCH_RISCV64
# else
#  error "Target architecture unspecified"
# endif
#endif

// Apple, or not
#define XCC_PLATFORM_POSIX  1
#define XCC_PLATFORM_APPLE  2
#define XCC_PLATFORM_WASI   3

#if !XCC_TARGET_PLATFORM
# if defined(__WASM) || XCC_TARGET_ARCH == XCC_ARCH_WASM
#  define XCC_TARGET_PLATFORM  XCC_PLATFORM_WASI
# elif defined(__APPLE__)
#  define XCC_TARGET_PLATFORM  XCC_PLATFORM_APPLE
# else
#  define XCC_TARGET_PLATFORM  XCC_PLATFORM_POSIX
# endif
#endif

//

#if XCC_TARGET_ARCH == XCC_ARCH_AARCH64
# define USE_SYS_AS
# define USE_SYS_LD
# if XCC_TARGET_PLATFORM != XCC_PLATFORM_APPLE
#  define NO_STD_LIB
# endif
#endif

#if defined(USE_ALLOCA)
#include <alloca.h>
#define ALLOCA(size)  alloca(size)
#else
#define ALLOCA(size)  malloc(size)
#endif

#if !defined(VAARG_ON_STACK) && XCC_TARGET_PLATFORM == XCC_PLATFORM_APPLE && XCC_TARGET_ARCH == XCC_ARCH_AARCH64
// variadic arguments are passed through stack, not registers.
#define VAARG_ON_STACK  1
#endif

#if !defined(VAARG_FP_AS_GP) && XCC_TARGET_ARCH == XCC_ARCH_RISCV64
// Pass floating-point arguments in general-purpose registers for variadic arguments.
#define VAARG_FP_AS_GP  1
#endif

#if !defined(MANGLE_PREFIX) && XCC_TARGET_PLATFORM == XCC_PLATFORM_APPLE
#define MANGLE_PREFIX  "_"
#endif

#define TARGET_CHAR_BIT  8

#if defined(__ILP32__)
#define POINTER_SIZE  4  /*sizeof(void*)*/
#elif defined(__LP64__)
#define POINTER_SIZE  8  /*sizeof(void*)*/
#else
// ?
#endif

// Elf
#if XCC_TARGET_ARCH == XCC_ARCH_X64
#define MACHINE_TYPE  EM_X86_64
#elif XCC_TARGET_ARCH == XCC_ARCH_AARCH64
#define MACHINE_TYPE  EM_AARCH64
#elif XCC_TARGET_ARCH == XCC_ARCH_RISCV64
#define MACHINE_TYPE  EM_RISCV
#endif

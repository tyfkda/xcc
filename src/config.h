#pragma once

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

#if XCC_TARGET_PLATFORM == XCC_PLATFORM_APPLE
// Apple
#  define USE_SYS_LD
#endif

#define USE_ALLOCA

#if defined(USE_ALLOCA)
#include <alloca.h>
#define ALLOCA(size)  alloca(size)
#else
#define ALLOCA(size)  malloc(size)
#endif

#if !defined(VAARG_ON_STACK) && XCC_TARGET_PLATFORM == XCC_PLATFORM_APPLE && \
    XCC_TARGET_ARCH == XCC_ARCH_AARCH64
// variadic arguments are passed through stack, not registers.
#define VAARG_ON_STACK  1
#endif

#if !defined(VAARG_FP_AS_GP) && XCC_TARGET_ARCH == XCC_ARCH_RISCV64
// Pass floating-point arguments in general-purpose registers for variadic arguments.
#define VAARG_FP_AS_GP  1
#endif

#if !defined(STRUCT_ARG_AS_POINTER) && XCC_TARGET_ARCH == XCC_ARCH_RISCV64
// Put struct pointer instead of instance
#define STRUCT_ARG_AS_POINTER  1
#endif

#if !defined(VAARG_STRUCT_AS_POINTER) && \
    ((XCC_TARGET_PLATFORM == XCC_PLATFORM_APPLE && XCC_TARGET_ARCH == XCC_ARCH_AARCH64) || \
     (XCC_TARGET_ARCH == XCC_ARCH_RISCV64))
// Put struct pointer instead of instance
#define VAARG_STRUCT_AS_POINTER  1
#endif

#if !defined(MANGLE_PREFIX) && XCC_TARGET_PLATFORM == XCC_PLATFORM_APPLE
#define MANGLE_PREFIX  "_"
#endif

#define TARGET_CHAR_BIT  8

// Programming model.
#define XCC_PROGRAMMING_MODEL_ILP32  1
#define XCC_PROGRAMMING_MODEL_LP64   2

#if XCC_TARGET_ARCH == XCC_ARCH_WASM
#define XCC_TARGET_PROGRAMMING_MODEL  XCC_PROGRAMMING_MODEL_ILP32
#else
#define XCC_TARGET_PROGRAMMING_MODEL  XCC_PROGRAMMING_MODEL_LP64
#endif

#if XCC_TARGET_PROGRAMMING_MODEL == XCC_PROGRAMMING_MODEL_ILP32
#define TARGET_POINTER_SIZE  4  /*sizeof(void*)*/
#elif XCC_TARGET_PROGRAMMING_MODEL == XCC_PROGRAMMING_MODEL_LP64
#define TARGET_POINTER_SIZE  8  /*sizeof(void*)*/
#else
#error "Unsupported programming model"
#endif

//

#if XCC_TARGET_ARCH == XCC_ARCH_X64
#define MACHINE_TYPE  EM_X86_64
#define MINREGSIZE  (1)

#elif XCC_TARGET_ARCH == XCC_ARCH_AARCH64
#define MACHINE_TYPE  EM_AARCH64
#define MINREGSIZE  (4)

#elif XCC_TARGET_ARCH == XCC_ARCH_RISCV64
#define MACHINE_TYPE  EM_RISCV
#define MINREGSIZE  (4)  // 8

#elif XCC_TARGET_ARCH == XCC_ARCH_WASM
#define MINREGSIZE  (4)

#endif

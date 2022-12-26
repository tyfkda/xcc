#pragma once

#include <stddef.h>

#if defined(__ILP32__)
typedef char       int8_t;
typedef short      int16_t;
typedef int        int32_t;
typedef long long  int64_t;

typedef unsigned char       uint8_t;
typedef unsigned short      uint16_t;
typedef unsigned int        uint32_t;
typedef unsigned long long  uint64_t;

typedef int32_t  intptr_t;
typedef uint32_t uintptr_t;
#else
typedef char  int8_t;
typedef short int16_t;
typedef int   int32_t;
typedef long  int64_t;

typedef unsigned char  uint8_t;
typedef unsigned short uint16_t;
typedef unsigned int   uint32_t;
typedef unsigned long  uint64_t;

typedef int64_t  intptr_t;
typedef uint64_t uintptr_t;
#endif

typedef long long  intmax_t;
typedef unsigned long long  uintmax_t;

#define INTPTR_MAX  ((((intptr_t)1) << (sizeof(intptr_t) * 8 - 1)) - 1)
#define INTPTR_MIN  (((intptr_t)-1) << (sizeof(intptr_t) * 8 - 1))

#define INTMAX_MAX   ((intmax_t)((((uintmax_t)1) << (sizeof(intmax_t) * 8 - 1)) - 1))
#define UINTMAX_MAX  ((uintmax_t)-1)

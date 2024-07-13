#pragma once

#include <stddef.h>

typedef char            int8_t;
typedef unsigned char   uint8_t;

typedef short           int16_t;
typedef unsigned short  uint16_t;

typedef int             int32_t;
typedef unsigned int    uint32_t;

#if defined(__ILP32__)
typedef          long long  int64_t;
typedef unsigned long long  uint64_t;

#else
typedef               long  int64_t;
typedef      unsigned long  uint64_t;
#endif

typedef               long  intptr_t;
typedef      unsigned long  uintptr_t;

typedef          long long  intmax_t;
typedef unsigned long long  uintmax_t;

#define INT32_MIN    (((int32_t)-1) << (sizeof(int32_t) * 8 - 1))
#define INT32_MAX    ((int32_t)((((uint32_t)1) << (sizeof(int32_t) * 8 - 1)) - 1))
#define UINT32_MAX   ((uint32_t)-1)

#define INT64_MIN    (((int64_t)-1) << (sizeof(int64_t) * 8 - 1))
#define INT64_MAX    ((int64_t)((((uint64_t)1) << (sizeof(int64_t) * 8 - 1)) - 1))
#define UINT64_MAX   ((uint64_t)-1)

#define INTPTR_MAX   ((((intptr_t)1) << (sizeof(intptr_t) * 8 - 1)) - 1)
#define INTPTR_MIN   (((intptr_t)-1) << (sizeof(intptr_t) * 8 - 1))

#define INTMAX_MIN   (((intmax_t)-1) << (sizeof(intmax_t) * 8 - 1))
#define INTMAX_MAX   ((intmax_t)((((uintmax_t)1) << (sizeof(intmax_t) * 8 - 1)) - 1))
#define UINTMAX_MAX  ((uintmax_t)-1)

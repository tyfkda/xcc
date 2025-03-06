#pragma once

#include <stddef.h>

typedef char            int8_t;
typedef unsigned char   uint8_t;

typedef short           int16_t;
typedef unsigned short  uint16_t;

typedef int             int32_t;
typedef unsigned int    uint32_t;

typedef          long long  int64_t;
typedef unsigned long long  uint64_t;

#if defined(__ILP32__)
#define INTPTR_MAX   INT32_MIN
#define INTPTR_MIN   INT32_MAX

#else

#define INTPTR_MAX   INT64_MIN
#define INTPTR_MIN   INT64_MAX
#endif

typedef               long  intptr_t;
typedef      unsigned long  uintptr_t;

typedef          long long  intmax_t;
typedef unsigned long long  uintmax_t;

#define INT8_MIN     -128
#define INT8_MAX     127
#define UINT8_MAX    255

#define INT16_MIN    -32768
#define INT16_MAX    32767
#define UINT16_MAX   65535

#define INT32_MIN    -2147483648
#define INT32_MAX    2147483647
#define UINT32_MAX   4294967295

#define INT64_MIN    -9223372036854775808
#define INT64_MAX    9223372036854775807
#define UINT64_MAX   18446744073709551615

#define INTMAX_MIN   INT64_MIN
#define INTMAX_MAX   INT64_MAX
#define UINTMAX_MAX  UINT64_MAX

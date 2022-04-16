#pragma once

#include "stddef.h"

typedef char  int8_t;
typedef short int16_t;
typedef int   int32_t;
typedef long  int64_t;

typedef unsigned char  uint8_t;
typedef unsigned short uint16_t;
typedef unsigned int   uint32_t;
typedef unsigned long  uint64_t;

typedef long intptr_t;
typedef size_t uintptr_t;

#define INTPTR_MAX  ((((intptr_t)1) << (sizeof(intptr_t) * 8 - 1)) - 1)
#define INTPTR_MIN  (((intptr_t)-1) << (sizeof(intptr_t) * 8 - 1))

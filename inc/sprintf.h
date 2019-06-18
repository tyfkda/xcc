#pragma once

#include "stdarg.h"  // va_list
#include "stddef.h"  // size_t

size_t sprintf(char *out, const char *fmt, ...);
size_t snprintf(char*, size_t n, const char*, ...);
size_t vsnprintf(char *out, size_t n, const char *fmt_, va_list ap);

#pragma once

#if defined(__XCC)
typedef void **va_list;

#define va_start(ap, param)  /*(void)*/(ap = (void**)&(param))
#define va_end(ap)           /*(void)*/(ap = 0)
#define va_arg(ap, type)     (*(type*)(ap += 1))  // Assume little endian
#else

typedef __builtin_va_list __gnuc_va_list;
typedef __gnuc_va_list va_list;

#define va_start(v,l)   __builtin_va_start(v,l)
#define va_end(v)       __builtin_va_end(v)
#define va_arg(v,l)     __builtin_va_arg(v,l)
#define va_copy(d,s)    __builtin_va_copy(d,s)

#endif

#pragma once

typedef void **va_list;

#define va_start(ap, param)  /*(void)*/(ap = (void**)&(param))
#define va_end(ap)           /*(void)*/(ap = 0)
#define va_arg(ap, type)     (*(type*)(ap += 1))  // Assume little endian

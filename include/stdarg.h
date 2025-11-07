#pragma once

#if defined(__GNUC__) && !defined(__XCC)

#include_next <stdarg.h>

#elif defined(__wasm)

typedef __builtin_va_list __gnuc_va_list;
typedef __gnuc_va_list va_list;

#define va_start(ap,p)    __builtin_va_start(ap,p)
#define va_end(ap)        __builtin_va_end(ap)
#define va_arg(ap,ty)     __builtin_va_arg(ap,ty)
#define va_copy(dst,src)  __builtin_va_copy(dst,src)

#elif (defined(__APPLE__) && defined(__aarch64__)) || defined(__riscv)
typedef void **va_list;

#define va_start(ap,p)    __builtin_va_start(ap,p)
#define va_end(ap)        /*(void)*/(ap = 0)
#define va_arg(ap, ty)    __builtin_va_arg(ap,ty)
#define va_copy(dst,src)  (dst = src)

#define __builtin_va_arg(ap, ty) ( \
  *(ty *) \
    ((__builtin_classify_type(ty) == __builtin_classify_type(struct{}) \
      ? (*((ap)++)) /* struct: pointer */ \
      : ((ap)++)  /* Assume little endian */ )))

#else  // not (__APPLE__ and __aarch64__) nor __riscv

#include <stdint.h>

struct __va_elem {
  unsigned int gp_offset;
  unsigned int fp_offset;
  void *overflow_arg_area;
  void *reg_save_area;
};

typedef struct __va_elem __builtin_va_list[1];

typedef __builtin_va_list __gnuc_va_list;
typedef __gnuc_va_list va_list;

#define va_start(ap,p)    __builtin_va_start(ap,p)
#define va_end(ap)        __builtin_va_end(ap)
#define va_arg(ap,ty)     __builtin_va_arg(ap,ty)
#define va_copy(dst,src)  __builtin_va_copy(dst,src)

#if defined(__aarch64__) || defined(__riscv)
#define __GP_REG_ARGS  (8)
#else
#define __GP_REG_ARGS  (6)
#endif
#define __FP_REG_ARGS  (8)
#define __WORD_SIZE    (8)
#define __DBL_SIZE     (8)

#define __GP_OFFSET_MAX  (__GP_REG_ARGS * __WORD_SIZE)
#define __FP_OFFSET_MAX  (__GP_OFFSET_MAX + __FP_REG_ARGS * __DBL_SIZE)

//

#define __va_arg_mem(ap, sz, align) ({ \
  uintptr_t p = (uintptr_t)((ap)->overflow_arg_area); \
  if ((align) > 8)  p = (p + 15) / 16 * 16; \
  (ap)->overflow_arg_area = (void*)((p + (sz) + 7) & -8); \
  (char*)p; })

#define __va_arg_gp(ap, sz, align) \
  ((ap)->gp_offset < __GP_OFFSET_MAX \
    ? (char*)(ap)->reg_save_area + ((ap)->gp_offset += __WORD_SIZE) - __WORD_SIZE \
    : __va_arg_mem(ap, sz, align))

#define __va_arg_fp(ap, sz, align) \
  ((ap)->fp_offset < __FP_OFFSET_MAX \
    ? (char*)(ap)->reg_save_area + ((ap)->fp_offset += __DBL_SIZE) - __DBL_SIZE \
    : __va_arg_mem(ap, sz, align))

#ifdef __aarch64__
# define __va_arg_struct(ap, ty)  (sizeof(ty) <= 16 \
    ? __va_arg_gp(ap, sizeof(ty *), _Alignof(ty *)) \
    : ((char*)*(ty **)__va_arg_gp(ap, sizeof(ty *), _Alignof(ty *))))
#else
# define __va_arg_struct(ap, ty)  __va_arg_mem(ap, sizeof(ty), _Alignof(ty))
#endif

#ifndef __NO_FLONUM
#define __va_arg_fp_case(ap, ty) \
  (__builtin_classify_type(ty) == __builtin_classify_type(double))
#else
#define __va_arg_fp_case(ap, ty)  0
#endif

#define __builtin_va_arg(ap, ty) ( \
  *(ty *) \
    (__va_arg_fp_case(ap, ty) \
      ? __va_arg_fp(ap, sizeof(ty), _Alignof(ty)) \
    : (__builtin_classify_type(ty) == __builtin_classify_type(int) || \
       __builtin_classify_type(ty) == __builtin_classify_type(void*)) \
      ? __va_arg_gp(ap, sizeof(ty), _Alignof(ty)) \
    : __va_arg_struct(ap, ty)))

#define __builtin_va_copy(dest, src) ((dest)[0] = (src)[0])

#define __builtin_va_end(ap)  /* none */

#endif

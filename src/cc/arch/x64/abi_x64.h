// SysV x86-64 ABI helpers.
// Aggregates <= 16 bytes are split into 8-byte chunks and classified as INTEGER or SSE;
// otherwise they fall back to MEMORY (stack).
#pragma once

#include <stdbool.h>
#include <stddef.h>

#include "ir.h"
#include "type.h"

typedef enum {
  X64_ABI_NO_CLASS = 0,
  X64_ABI_INTEGER,
  X64_ABI_SSE,
  X64_ABI_MEMORY,
} X64AbiClass;

#define X64_ABI_MAX_EIGHTBYTES  (2)

typedef struct {
  int count;
  size_t size;
  X64AbiClass classes[X64_ABI_MAX_EIGHTBYTES];
} X64AbiClassInfo;

bool x64_classify_aggregate(const Type *type, X64AbiClassInfo *info);

static inline int x64_count_class(const X64AbiClassInfo *info, X64AbiClass cls) {
  int n = 0;
  for (int i = 0; i < info->count; ++i) {
    if (info->classes[i] == cls)
      ++n;
  }
  return n;
}

static inline size_t x64_eightbyte_size(const X64AbiClassInfo *info, int index) {
  size_t start = (size_t)index * 8;
  size_t end = start + 8;
  if (end > info->size)
    end = info->size;
  return end - start;
}

static inline enum VRegSize x64_chunk_vsize(size_t size) {
  if (size <= 1)
    return VRegSize1;
  if (size <= 2)
    return VRegSize2;
  if (size <= 4)
    return VRegSize4;
  return VRegSize8;
}

static inline enum VRegSize x64_sse_vsize(size_t size) {
  return size > 4 ? VRegSize8 : VRegSize4;
}

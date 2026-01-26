#include "../../../config.h"

#include <stddef.h>

#include "abi_x64.h"
#include "type.h"

static X64AbiClass merge_class(X64AbiClass a, X64AbiClass b) {
  // SysV merge: MEMORY dominates, then INTEGER, then SSE.
  if (a == X64_ABI_NO_CLASS)
    return b;
  if (b == X64_ABI_NO_CLASS)
    return a;
  if (a == X64_ABI_MEMORY || b == X64_ABI_MEMORY)
    return X64_ABI_MEMORY;
  if (a == X64_ABI_INTEGER || b == X64_ABI_INTEGER)
    return X64_ABI_INTEGER;
  return X64_ABI_SSE;
}

static bool add_class(X64AbiClassInfo *info, size_t offset, size_t size, X64AbiClass cls) {
  if (cls == X64_ABI_MEMORY)
    return false;
  if (size == 0 || offset + size > X64_ABI_MAX_EIGHTBYTES * 8)
    return false;

  int start = (int)(offset / 8);
  int end = (int)((offset + size - 1) / 8);
  for (int i = start; i <= end; ++i) {
    info->classes[i] = merge_class(info->classes[i], cls);
    if (info->classes[i] == X64_ABI_MEMORY)
      return false;
  }
  return true;
}

static bool classify_type(const Type *type, size_t base, X64AbiClassInfo *info) {
  if (type == NULL)
    return false;

  switch (type->kind) {
  case TY_FIXNUM:
  case TY_PTR:
    return add_class(info, base, type_size(type), X64_ABI_INTEGER);

  case TY_FLONUM:
    if (type->flonum.kind == FL_LDOUBLE)
      return false;
    return add_class(info, base, type_size(type), X64_ABI_SSE);

  case TY_ARRAY:
    {
      ssize_t len = type->pa.length;
      if (len <= 0)
        return false;
      const Type *elem = type->pa.ptrof;
      size_t elem_size = type_size(elem);
      if (elem_size == 0)
        return false;
      for (ssize_t i = 0; i < len; ++i) {
        if (!classify_type(elem, base + (size_t)i * elem_size, info))
          return false;
      }
      return true;
    }

  case TY_STRUCT:
    {
      const StructInfo *sinfo = type->struct_.info;
      if (sinfo == NULL)
        return false;
      if (sinfo->flag & SIF_FLEXIBLE)
        return false;
      if (sinfo->flag & SIF_UNION) {
        for (int i = 0; i < sinfo->member_count; ++i) {
          const MemberInfo *minfo = &sinfo->members[i];
#ifndef __NO_BITFIELD
          if (minfo->bitfield.active) {
            if (minfo->bitfield.width <= 0)
              continue;
            const Type *btype = get_fixnum_type(minfo->bitfield.base_kind, false, 0);
            size_t align = align_size(btype);
            size_t start_bit = (base + minfo->offset) * TARGET_CHAR_BIT + minfo->bitfield.position;
            size_t end_bit = start_bit + (size_t)minfo->bitfield.width;
            size_t start_byte = start_bit / TARGET_CHAR_BIT;
            size_t end_byte = (end_bit + (TARGET_CHAR_BIT - 1)) / TARGET_CHAR_BIT;
            if (align > 1 && (base + minfo->offset) % align != 0)
              return false;
            if (!add_class(info, start_byte, end_byte - start_byte, X64_ABI_INTEGER))
              return false;
            continue;
          }
#endif
          size_t align = align_size(minfo->type);
          if (align > 1 && (base + minfo->offset) % align != 0)
            return false;
          if (!classify_type(minfo->type, base, info))
            return false;
        }
        return true;
      }
      for (int i = 0; i < sinfo->member_count; ++i) {
        const MemberInfo *minfo = &sinfo->members[i];
#ifndef __NO_BITFIELD
        if (minfo->bitfield.active) {
          if (minfo->bitfield.width <= 0)
            continue;
          const Type *btype = get_fixnum_type(minfo->bitfield.base_kind, false, 0);
          size_t align = align_size(btype);
          size_t start_bit = (base + minfo->offset) * TARGET_CHAR_BIT + minfo->bitfield.position;
          size_t end_bit = start_bit + (size_t)minfo->bitfield.width;
          size_t start_byte = start_bit / TARGET_CHAR_BIT;
          size_t end_byte = (end_bit + (TARGET_CHAR_BIT - 1)) / TARGET_CHAR_BIT;
          if (align > 1 && (base + minfo->offset) % align != 0)
            return false;
          if (!add_class(info, start_byte, end_byte - start_byte, X64_ABI_INTEGER))
            return false;
          continue;
        }
#endif
        size_t align = align_size(minfo->type);
        if (align > 1 && (base + minfo->offset) % align != 0)
          return false;
        if (!classify_type(minfo->type, base + minfo->offset, info))
          return false;
      }
      return true;
    }

  default:
    return false;
  }
}

bool x64_classify_aggregate(const Type *type, X64AbiClassInfo *info) {
  if (info == NULL)
    return false;
  if (type == NULL || type->kind != TY_STRUCT)
    return false;

  size_t size = type_size(type);
  if (size == 0 || size > X64_ABI_MAX_EIGHTBYTES * 8)
    return false;
  if (type->struct_.info->align > 8)
    return false;

  info->size = size;
  info->count = (int)((size + 7) / 8);
  for (int i = 0; i < X64_ABI_MAX_EIGHTBYTES; ++i)
    info->classes[i] = X64_ABI_NO_CLASS;

  if (!classify_type(type, 0, info))
    return false;

  for (int i = 0; i < info->count; ++i) {
    if (info->classes[i] == X64_ABI_NO_CLASS)
      info->classes[i] = X64_ABI_INTEGER;
    if (info->classes[i] == X64_ABI_MEMORY)
      return false;
  }
  return true;
}

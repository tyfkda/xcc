#include "../config.h"
#include "as_util.h"

#ifndef ELF_NOT_SUPPORTED

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>  // realloc
#include <string.h>  // memcpy

#include "util.h"

void strtab_init(Strtab *strtab) {
  table_init(&strtab->offsets);
  strtab->size = 0;
}

size_t strtab_add(Strtab *strtab, const Name *name) {
  void *result;
  if (!table_try_get(&strtab->offsets, name, &result)) {
    size_t offset = strtab->size;
    table_put(&strtab->offsets, name, (void*)offset);
    strtab->size += name->bytes + 1;
    return offset;
  } else {
    return (size_t)result;
  }
}

void *strtab_dump(const Strtab *strtab) {
  void *buf = malloc_or_die(strtab->size);
  unsigned char *p = buf;
  const Name *name;
  void *value;
  for (int it = 0; (it = table_iterate(&strtab->offsets, it, &name, &value)) != -1; ) {
    uintptr_t offset = VOIDP2UINT(value);
    memcpy(p + offset, name->chars, name->bytes);
    p[offset + name->bytes] = '\0';
  }
  return buf;
}
#endif  // !ELF_NOT_SUPPORTED

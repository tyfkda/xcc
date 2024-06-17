#pragma once

#include <stddef.h>  // size_t

#include "table.h"

// String table.
typedef struct {
  Table offsets;
  size_t size;
} Strtab;

void strtab_init(Strtab *strtab);
size_t strtab_add(Strtab *strtab, const Name *name);
void *strtab_dump(Strtab *strtab);

#pragma once

#include <stdbool.h>

// Name

typedef struct Name {
  const char *chars;
  int bytes;
} Name;

const Name *alloc_name(const char *begin, const char *end, bool make_copy);
bool equal_name(const Name *name1, const Name *name2);

const Name *alloc_label(void);

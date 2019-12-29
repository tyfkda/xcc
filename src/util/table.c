#include "table.h"

#include <stdlib.h>  // malloc
#include <string.h>

const Name *alloc_name(const char *begin, const char *end, bool make_copy) {
  int bytes = end != NULL ? (int)(end - begin) : (int)strlen(begin);
  if (make_copy) {
    char *new_str = malloc(bytes);
    memcpy(new_str, begin, bytes);
    begin = new_str;
  }
  Name *name = malloc(sizeof(*name));
  name->chars = begin;
  name->bytes = bytes;
  return name;
}

bool equal_name(const Name *name1, const Name *name2) {
  return name1->bytes == name2->bytes &&
      memcmp(name1->chars, name2->chars, name1->bytes) == 0;
}

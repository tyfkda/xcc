#include "stdio.h"

#include "./_file.h"
#undef getchar

int getchar(void) {
  return fgetc(stdin);
}

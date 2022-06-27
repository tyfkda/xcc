#include "stdio.h"
#include "string.h"

int fputs(const char *s, FILE *fp) {
  return fwrite(s, strlen(s), 1, fp) == 1 ? 1 : EOF;
}

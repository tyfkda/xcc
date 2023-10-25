#include "stdio.h"
#include "string.h"

// Result: Success => positive value, Failure => EOF
int fputs(const char *s, FILE *fp) {
  size_t result = fwrite(s, 1, strlen(s), fp);
  return result != 0 ? result : EOF;
}

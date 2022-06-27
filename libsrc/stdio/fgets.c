#include "stdio.h"

char *fgets(char *s, int n, FILE *fp) {
  --n;
  char *p = s;
  for (int i = 0; i < n; ++i) {
    int c = fgetc(fp);
    if (c == EOF)
      break;
    *p++ = c;
    if (c == '\n')
      break;
  }
  if (p == s)
    return NULL;
  *p = '\0';
  return s;
}

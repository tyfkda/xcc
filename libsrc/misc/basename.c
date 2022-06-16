#include "libgen.h"
#include "string.h"  // strrchr

char *basename(char *path) {
  char *p = strrchr(path, '/');
  if (p != NULL)
    return p + 1;
  else
    return path;
}

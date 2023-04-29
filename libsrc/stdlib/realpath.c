#include "stdlib.h"
#include "unistd.h"

#include "stdio.h"

char *realpath(const char *path, char *resolved_path) {
#if !defined(__WASM)
  char *orig = getcwd(NULL, 0);
  char *result = NULL;
  if (chdir(path) == 0) {
    result = getcwd(resolved_path, resolved_path != 0 ? -1 : 0);
    chdir(orig);
  }
  free(orig);
  return result;
#else
  // TODO: Implement
  return NULL;
#endif
}

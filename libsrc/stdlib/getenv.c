#include "stdlib.h"
#include "string.h"

extern char **environ;

char *getenv(const char *varname) {
  if (environ != NULL) {
    size_t len = strlen(varname);
    for (char **pp = environ, *p; (p = *pp++) != NULL;)
      if (strncmp(p, varname, len) == 0 && p[len] == '=')
        return &p[len + 1];
  }
  return NULL;
}

#include "stdlib.h"
#include "string.h"

char *getenv(const char *varname) {
#ifdef __APPLE__
  extern char ***_NSGetEnviron(void);
  char **environ = *_NSGetEnviron();
#else
  extern char **environ;
#endif

  if (environ != NULL) {
    size_t len = strlen(varname);
    for (char **pp = environ, *p; (p = *pp++) != NULL;)
      if (strncmp(p, varname, len) == 0 && p[len] == '=')
        return &p[len + 1];
  }
  return NULL;
}

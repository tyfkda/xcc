#include "unistd.h"

int execvp(const char *path, char *const args[]) {
#ifdef __APPLE__
  extern char ***_NSGetEnviron(void);
  char **environ = *_NSGetEnviron();
#else
  extern char **environ;
#endif

  return execve(path, args, environ);
}

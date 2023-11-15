#include "unistd.h"

extern char **environ;

int execvp(const char *path, char *const args[]) {
  return execve(path, args, environ);
}

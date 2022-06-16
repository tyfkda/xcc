#include "unistd.h"

//extern char **environ;
//char *environ[] = {"PATH=/bin:/usr/bin", NULL};
char **environ = NULL;

int execvp(const char *path, char *const args[]) {
  return execve(path, args, environ);
}

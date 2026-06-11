#include "stdlib.h"

#if !defined(__wasm)
#include "ctype.h"
#include "errno.h"
#include "stdbool.h"
#include "string.h"
#include "unistd.h"
#include "sys/wait.h"

static bool extract_argument(char **pp, char quote) {
  char *p = *pp, *q = p, c;
  bool result = true;
  for (;;) {
    c = *p;
    if (c == '\0') {
      result = quote == '\0';
      break;
    }

    if (quote == '\0') {
      if (isspace(c))
        break;
    } else if (c == quote) {
      if (p[1] != quote)
        break;
      p += 2;
      continue;
    }
    *q++ = c;
    ++p;
  }
  *q = '\0';
  if (c != '\0')
    ++p;
  *pp = p;
  return result;
}

static int run_command(const char *command) {
  char *dup = strdup(command);
  char **args = NULL;
  int result = 0;
  if (dup != NULL) {
    int n = 0;
    for (char *p = dup;; ) {
      while (isspace(*p))
        *p++ = '\0';
      if (*p == '\0')
        break;

      char quote = '\0';
      char c = *p;
      if (c == '"' || c == '\'') {
        quote = c;
        ++p;
      }

      void *newargs = realloc(args, sizeof(*args) * (n + 1));
      if (newargs == NULL)
        break;
      args = newargs;
      args[n++] = p;

      if (!extract_argument(&p, quote))
        break;
    }
    if (args != NULL) {
      args[n] = NULL;
      result = execvp(args[0], args);
      if (result >= 0)
        return result;
    }
  }
  free(args);
  free(dup);
  errno = ENOMEM;
  return -1;
}

int system(const char *command) {
  pid_t pid = fork();
  if (pid < 0)
    return -1;

  if (pid == 0)
    return run_command(command);

  int status = -1;
  waitpid(pid, &status, 0);
  return status;
}
#endif

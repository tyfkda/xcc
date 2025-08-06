#include <stdio.h>
#include "unistd.h"
#include "errno.h"
#include "limits.h"
#include "stdlib.h"  // getenv, malloc
#include "string.h"  // strlen

#include "../wasi.h"

char __cwd[PATH_MAX] = ".";

static inline void GETCWD(char *dst, size_t siz) {
  strncpy(dst, __cwd, siz);
}

char *getcwd(char *buffer, size_t size) {
  if (buffer == NULL) {
    if (size == 0)
      size = PATH_MAX;
    buffer = malloc(size);
    if (buffer == NULL)
      return NULL;
  }
  GETCWD(buffer, size);
  return buffer;
}

__attribute__((constructor))
static void extract_pwd_from_env(void) {
  // TODO: Use `getenv` to extract PWD (currently init priority is not guaranteed).

  size_t environ_count = 0;
  size_t content_size = 0;
  int r = environ_sizes_get(&environ_count, &content_size);
  if (r == 0) {
    void *buf = calloc(1, (environ_count + 1) * sizeof(char*) + content_size);
    if (buf != NULL) {
      char **env = buf;
      char *contents = (char*)buf + (environ_count + 1) * sizeof(char*);
      r = environ_get(env, contents);
      if (r == 0) {
        for (size_t i = 0; i < environ_count; ++i) {
          const char *p = env[i];
          if (strncmp(p, "PWD=", 4) == 0) {
            strncpy(__cwd, p + 4, sizeof(__cwd));
            __cwd[sizeof(__cwd) - 1] = '\0';  // Ensure null-termination.
            break;
          }
        }
      }
    }
    free(buf);
  }
}

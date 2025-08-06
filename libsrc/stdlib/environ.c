#ifndef __APPLE__
char **environ;
#endif

#ifdef __wasm
#include <stdlib.h>
#include "../_wasm/wasi.h"

__attribute__((constructor))
static void init_environ(void) {
  size_t environ_count = 0;
  size_t content_size = 0;
  int r = environ_sizes_get(&environ_count, &content_size);
  if (r == 0) {
    char **env = calloc(sizeof(char*), environ_count + 1);
    char *contents = calloc(1, content_size);
    r = environ_get(env, contents);
    if (r == 0) {
      env[environ_count] = NULL;
      environ = env;
    }
  }
}
#endif

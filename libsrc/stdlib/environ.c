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
    void *buf = calloc(1, (environ_count + 1) * sizeof(char*) + content_size);
    if (buf != NULL) {
      char **env = buf;
      char *contents = (char*)buf + (environ_count + 1) * sizeof(char*);
      r = environ_get(env, contents);
      if (r == 0) {
        env[environ_count] = NULL;
        environ = env;
      }
    }
  }
}
#endif

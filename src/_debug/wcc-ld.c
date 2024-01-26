#include "../config.h"

#include <assert.h>
#include <stdbool.h>
#include <stdio.h>

#include "../wcc/wasm.h"
#include "../wcc/wasm_linker.h"
#include "../wcc/wcc.h"
#include "util.h"

static void init(void) {
  out_type = OutExecutable;
  functypes = new_vector();
}

int main(int argc, char *argv[]) {
  const char *entry_point = "_start";

  init();

  enum {
    OPT_VERBOSE = 256,
    OPT_ENTRY_POINT,
  };
  static const struct option options[] = {
    {"-verbose", no_argument, OPT_VERBOSE},
    {"-entry-point", required_argument, OPT_ENTRY_POINT},

    {NULL},
  };

  int opt;
  while ((opt = optparse(argc, argv, options)) != -1) {
    switch (opt) {
    default: assert(false); break;
    case OPT_VERBOSE:
      verbose = true;
      break;
    case OPT_ENTRY_POINT:
      entry_point = optarg;
      break;
    case '?':
      fprintf(stderr, "Warning: unknown option: %s\n", argv[optind - 1]);
      break;
    }
  }
  int iarg = optind;

  WasmLinker linker_body;
  WasmLinker *linker = &linker_body;
  linker_init(linker);

  bool result = true;
  for (int i = iarg; i < argc; ++i) {
    const char *src = argv[i];
    if (!read_wasm_obj(linker, src)) {
      fprintf(stderr, "error: failed to read wasm object file: %s\n", src);
      result = false;
      break;
    }
  }
  if (!result)
    return 1;

  Vector *exports = new_vector();
  if (*entry_point != '\0')
    vec_push(exports, alloc_name(entry_point, NULL, false));

  if (!link_wasm_objs(linker, exports))
    return 1;

  return 0;
}

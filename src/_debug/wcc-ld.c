#include "../config.h"

#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>  // atoi

#include "../wcc/wasm.h"
#include "../wcc/wasm_linker.h"
#include "../wcc/wcc.h"
#include "table.h"
#include "util.h"

static void init(void) {
  functypes = new_vector();
  tags = new_vector();
  table_init(&indirect_function_table);
}

int main(int argc, char *argv[]) {
  const char *ofn = "a.wasm";
  const char *entry_point = "_start";
  uint32_t stack_size = DEFAULT_STACK_SIZE;

  init();

  enum {
    OPT_VERBOSE = 256,
    OPT_ENTRY_POINT,
    OPT_STACK_SIZE,
  };
  static const struct option options[] = {
    {"o", required_argument},  // Specify output filename
    {"-verbose", no_argument, OPT_VERBOSE},
    {"-entry-point", required_argument, OPT_ENTRY_POINT},
    {"-stack-size", required_argument, OPT_STACK_SIZE},

    {NULL},
  };

  int opt;
  while ((opt = optparse(argc, argv, options)) != -1) {
    switch (opt) {
    default: assert(false); break;
    case 'o':
      ofn = optarg;
      break;
    case OPT_VERBOSE:
      verbose = true;
      break;
    case OPT_ENTRY_POINT:
      entry_point = optarg;
      break;
    case OPT_STACK_SIZE:
      {
        int size = atoi(optarg);
        if (size <= 0) {
          error("stack-size must be positive");
        }
        stack_size = size;
      }
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

  if (!link_wasm_objs(linker, exports, stack_size) ||
      !linker_emit_wasm(linker, ofn, exports))
    return 1;

  return 0;
}

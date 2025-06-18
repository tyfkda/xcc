#include "../config.h"

#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>  // strtoul

#include "../wcc/wasm_linker.h"
#include "../wcc/wcc.h"
#include "table.h"
#include "util.h"

static void init(void) {
  functypes = new_vector();
  tags = new_vector();
  tables = new_vector();
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
    OPT_ALLOW_UNDEFINED,
    OPT_EXPORT_ALL,
  };
  static const struct option kOptions[] = {
    {"o", required_argument},  // Specify output filename
    {"-verbose", no_argument, OPT_VERBOSE},
    {"-entry-point", required_argument, OPT_ENTRY_POINT},
    {"-stack-size", required_argument, OPT_STACK_SIZE},
    {"-allow-undefined", no_argument, OPT_ALLOW_UNDEFINED},
    {"-export-all", no_argument, OPT_EXPORT_ALL},

    {NULL},
  };

  WasmLinker linker_body;
  WasmLinker *linker = &linker_body;
  linker_init(linker);

  int opt;
  while ((opt = optparse(argc, argv, kOptions)) != -1) {
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
        int base = 10;
        char *p = optarg;
        if (*p == '0' && (p[1] == 'x' || p[1] == 'X')) {
          base = 16;
          p += 2;
        }
        unsigned long size = strtoul(p, &p, base);
        if (size <= 0) {
          error("stack-size must be positive");
        }
        if (*p != '\0') {
          error("positive integer expected: %s", optarg);
        }
        stack_size = size;
      }
      break;
    case OPT_ALLOW_UNDEFINED:
      linker->options.allow_undefined = true;
      break;
    case OPT_EXPORT_ALL:
      linker->options.export_all = true;
      break;
    case '?':
      fprintf(stderr, "Warning: unknown option: %s\n", argv[optind - 1]);
      break;
    }
  }
  int iarg = optind;

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

  Table *exports = alloc_table();
  if (*entry_point != '\0') {
    const Name *entry = alloc_name(entry_point, NULL, false);
    table_put(exports, entry, (void*)entry);
  }

  if (!link_wasm_objs(linker, exports, stack_size) ||
      !linker_emit_wasm(linker, ofn, exports))
    return 1;

  return 0;
}

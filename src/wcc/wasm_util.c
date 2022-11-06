#include "wasm_util.h"

#define WASM_BINARY_MAGIC  {'\0', 'a', 's', 'm'}
#define WASM_BINARY_VERSION  (1)

void emit_wasm_header(FILE *ofp) {
  WASM_HEADER header = {
    WASM_BINARY_MAGIC,
    WASM_BINARY_VERSION,
  };
  fwrite(&header, sizeof(header), 1, ofp);
}

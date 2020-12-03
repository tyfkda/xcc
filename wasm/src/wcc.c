#include "wcc.h"

#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "lexer.h"
#include "parser.h"
#include "table.h"
#include "type.h"
#include "util.h"
#include "var.h"
#include "wasm_util.h"

////////////////////////////////////////////////

typedef struct {
  char magic[4];
  uint32_t binary_version;
} WASM_HEADER;

#define WASM_BINARY_MAGIC  {'\0', 'a', 's', 'm'}
#define WASM_BINARY_VERSION  (1)

const unsigned char SEC_TYPE   = 1;
const unsigned char SEC_FUNC   = 3;
const unsigned char SEC_EXPORT = 7;
const unsigned char SEC_CODE   = 10;

static void emit_wasm(FILE *ofp) {
  WASM_HEADER header = {
    WASM_BINARY_MAGIC,
    WASM_BINARY_VERSION,
  };
  fwrite(&header, sizeof(header), 1, ofp);

  unsigned char exports[] = {
    4,  // string length
    'm', 'a', 'i', 'n',  // export name
    0x00,  // export kind
    0x00,  // export func index
  };

  unsigned char sections[] = {
    // Types
    SEC_TYPE,  // Section "Type" (1)
    0x05,  // Size
    0x01,  // num types
    // type 0
    WT_FUNC,  // func
    0x00,  // num params
    0x01,  // num results
    WT_I32,  // i32

    // Functions
    SEC_FUNC,  // Section "Function" (3)
    0x02,  // Size
    0x01,  // num functions
    0x00,  // function 0 signature index

    // Exports
    SEC_EXPORT,  // Section "Export" (7)
    sizeof(exports) + 1,  // Size
    1,  // num exports
  };
  fwrite(sections, sizeof(sections), 1, ofp);
  fwrite(exports, sizeof(exports), 1, ofp);

  unsigned char code_section[] = {
    // Code
    SEC_CODE,  // Section "Code" (10)
    codesize + 1 + 1,  // Size
    0x01,  // num functions

    // function body 0
  };
  fwrite(code_section, sizeof(code_section), 1, ofp);

  unsigned char buf[5], *p = emit_uleb128(buf, codesize);
  fwrite(buf, p - buf, 1, ofp);

  fwrite(code, codesize, 1, ofp);
}

////////////////////////////////////////////////

static void init_compiler(void) {
  init_lexer();
  init_global();

  set_fixnum_size(FX_CHAR,  1, 1);
  set_fixnum_size(FX_SHORT, 2, 2);
  set_fixnum_size(FX_INT,   4, 4);
  set_fixnum_size(FX_LONG,  4, 4);
  set_fixnum_size(FX_LLONG, 8, 8);
  set_fixnum_size(FX_ENUM,  4, 4);
}

static void compile1(FILE *ifp, const char *filename, Vector *toplevel) {
  set_source_file(ifp, filename);
  parse(toplevel);
}

int main(int argc, char *argv[]) {
  const char *ofn = "a.wasm";
  int iarg;

  init_compiler();

  for (iarg = 1; iarg < argc; ++iarg) {
    char *arg = argv[iarg];
    if (*arg != '-')
      break;

    if (starts_with(arg, "-o")) {
      ofn = arg + 2;
    } else {
      fprintf(stderr, "Unknown option: %s\n", arg);
      return 1;
    }
  }

  // Compile.
  toplevel = new_vector();
  if (iarg < argc) {
    for (int i = iarg; i < argc; ++i) {
      const char *filename = argv[i];
      FILE *ifp = fopen(filename, "r");
      if (ifp == NULL)
        error("Cannot open file: %s\n", filename);
      compile1(ifp, filename, toplevel);
      fclose(ifp);
    }
  } else {
    compile1(stdin, "*stdin*", toplevel);
  }

  gen(toplevel);

  FILE *fp = fopen(ofn, "wb");
  if (fp == NULL) {
    fprintf(stderr, "Cannot open output file\n");
    exit(1);
  }
  emit_wasm(fp);
  fclose(fp);

  return 0;
}

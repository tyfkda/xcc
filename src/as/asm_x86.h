#pragma once

#include <stdbool.h>

typedef struct Inst Inst;

typedef struct Code {
  Inst *inst;
  char len;
  unsigned char buf[15];
} Code;

bool assemble_inst(Inst *inst, Code *code);

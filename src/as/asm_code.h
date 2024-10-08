// Generate code for each architecture.

#pragma once

#include <stdbool.h>

typedef struct Inst Inst;
typedef struct ParseInfo ParseInfo;

#define INST_LONG_OFFSET  (1 << 0)  // 32bit offset on jump, etc.

typedef struct Code {
  Inst *inst;
  char flag;
  char len;
#if XCC_TARGET_ARCH == XCC_ARCH_RISCV64
  unsigned char buf[26];
#else
  unsigned char buf[14];
#endif
} Code;

void assemble_inst(Inst *inst, ParseInfo *info, Code *code);

#ifndef MAKE_CODE
#define MAKE_CODE(inst, code, ...)  do { unsigned char buf[] = {__VA_ARGS__}; make_code(inst, code, buf, sizeof(buf)); } while (0)
#endif

#ifndef MAKE_CODE16
#define MAKE_CODE16(inst, code, ...)  do { unsigned short buf[] = {__VA_ARGS__}; make_code16(inst, code, buf, sizeof(buf)); } while (0)
#endif

#ifndef MAKE_CODE32
#define MAKE_CODE32(inst, code, ...)  do { unsigned int buf[] = {__VA_ARGS__}; make_code32(inst, code, buf, sizeof(buf)); } while (0)
#endif

#define IM8(x)   (x)
#define IM16(x)  (x), ((x) >> 8)
#define IM32(x)  (x), ((x) >> 8), ((x) >> 16), ((x) >> 24)
#define IM64(x)  (x), ((x) >> 8), ((x) >> 16), ((x) >> 24), ((x) >> 32), ((x) >> 40), ((x) >> 48), ((x) >> 56)

void make_code(Inst *inst, Code *code, unsigned char *buf, int len);
void make_code16(Inst *inst, Code *code, unsigned short *buf, int len);
void make_code32(Inst *inst, Code *code, unsigned int *buf, int len);

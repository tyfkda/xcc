#include "../../../config.h"
#include "asm_code.h"

#include <assert.h>
#include <string.h>  // memcpy

#include "inst.h"
#include "parse_asm.h"
#include "util.h"

#ifndef MAKE_CODE16
#define MAKE_CODE16(inst, code, ...)  do { unsigned short buf[] = {__VA_ARGS__}; make_code16(inst, code, buf, sizeof(buf)); } while (0)
#endif

#ifndef MAKE_CODE32
#define MAKE_CODE32(inst, code, ...)  do { unsigned int buf[] = {__VA_ARGS__}; make_code32(inst, code, buf, sizeof(buf)); } while (0)
#endif

void make_code16(Inst *inst, Code *code, unsigned short *buf, int len) {
  assert(len <= (int)sizeof(code->buf));
  code->inst = inst;
  code->len = len;
  memcpy(code->buf, buf, len);
}

void make_code32(Inst *inst, Code *code, unsigned int *buf, int len) {
  assert(len <= (int)sizeof(code->buf));
  code->inst = inst;
  code->len = len;
  memcpy(code->buf, buf, len);
}

inline bool is_im6(int64_t x) {
  return x <= ((1L << 5) - 1) && x >= -(1L << 5);
}

inline bool is_im12(int64_t x) {
  return x <= ((1L << 11) - 1) && x >= -(1L << 11);
}

inline bool is_im18(int64_t x) {
  return x <= ((1L << 17) - 1) && x >= -(1L << 17);
}

inline bool assemble_error(const ParseInfo *info, const char *message) {
  parse_error(info, message);
  return false;
}

//

#define ZERO  0

#define C_LI(rd, imm)       MAKE_CODE16(inst, code, 0x4001 | ((imm & 0x20) << 12) | (rd << 7) | ((imm & 0x1f) << 2))
#define C_LUI(rd, imm)      MAKE_CODE16(inst, code, 0x6001 | ((imm & 0x20) << 12) | (rd << 7) | ((imm & 0x1f) << 2))
#define C_ADDIW(rd, imm)    MAKE_CODE16(inst, code, 0x2001 | ((imm & 0x20) << 12) | (rd << 7) | ((imm & 0x1f) << 2))

#define ADDIW(rd, rs, imm)  MAKE_CODE32(inst, code, 0x00000019 | ((int32_t)imm << 20) | (rs << 15) | (rd << 7))
#define ADDI(rd, rs, imm)   MAKE_CODE32(inst, code, 0x00000013 | ((int32_t)imm << 20) | (rd << 7))

#define LI(rd, imm)         ADDI(rd, ZERO, imm)
#define MV(rd, rs)          ADDI(rd, rs, 0)

static unsigned char *asm_noop(Inst *inst, Code *code) {
  UNUSED(inst);
  unsigned char *p = code->buf;
  return p;
}

static unsigned char *asm_li(Inst *inst, Code *code) {
  int rd = inst->opr1.reg.no;
  int64_t imm =inst->opr2.immediate;
  if (is_im6(imm)) {
    C_LI(rd, imm);
  } else if (is_im12(imm)) {
    LI(rd, imm);
  } else if (is_im18(imm)) {
    int h = imm >> 12, l = imm & 0xfff;
    C_LUI(rd, h);
    if (is_im6(imm))
      C_ADDIW(rd, l);
    else
      ADDIW(rd, rd, l);
  } else {
    // TODO:
    return NULL;
  }
  return code->buf;
}

static unsigned char *asm_ret(Inst *inst, Code *code) {
  MAKE_CODE16(inst, code, 0x8082);
  return code->buf;
}

////////////////////////////////////////////////

typedef unsigned char *(*AsmInstFunc)(Inst *inst, Code *code);
typedef struct {
  AsmInstFunc func;
  enum OperandType opr1_type;
  enum OperandType opr2_type;
  enum OperandType opr3_type;
  int flag;
} AsmInstTable;

static const AsmInstTable *table[] = {
  [NOOP] = (const AsmInstTable[]){ {asm_noop, NOOPERAND, NOOPERAND, NOOPERAND}, {NULL} },
  [LI] = (const AsmInstTable[]){ {asm_li, REG, IMMEDIATE, NOOPERAND}, {NULL} },
  [RET] = (const AsmInstTable[]){ {asm_ret, NOOPERAND, NOOPERAND, NOOPERAND}, {NULL} },
};

void assemble_inst(Inst *inst, const ParseInfo *info, Code *code) {
  code->flag = 0;
  code->len = 0;

  const AsmInstTable *pt = NULL;
  if (inst->op < (enum Opcode)ARRAY_SIZE(table) && table[inst->op] != NULL) {
    for (const AsmInstTable *p = table[inst->op]; p->func != NULL; ++p) {
      if (inst->opr1.type == p->opr1_type && inst->opr2.type == p->opr2_type && inst->opr3.type == p->opr3_type) {
        pt = p;
        break;
      }
    }
  }

  if (pt != NULL) {
    unsigned char *p = (*pt->func)(inst, code);
    if (p != NULL) {
      if (p > code->buf) {
        code->inst = inst;
        code->len = p - code->buf;
        assert((size_t)code->len <= sizeof(code->buf));
      }
      return;
    }
  }

  assemble_error(info, "Illegal opeand");
}

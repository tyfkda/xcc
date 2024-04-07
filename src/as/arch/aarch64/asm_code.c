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

inline bool assemble_error(const ParseInfo *info, const char *message) {
  parse_error(info, message);
  return false;
}

static unsigned char *asm_noop(Inst *inst, Code *code) {
  UNUSED(inst);
  unsigned char *p = code->buf;
  return p;
}

static unsigned char *asm_mov(Inst *inst, Code *code) {
  Operand *opr1 = &inst->opr1;
  Operand *opr2 = &inst->opr2;
  uint32_t x = 0x52800000U | (opr1->reg.size == REG64 ? (1U << 31) : 0U) | (opr2->immediate << 5) | opr1->reg.no;
  MAKE_CODE32(inst, code, x);
  return code->buf;
}

static unsigned char *asm_ret(Inst *inst, Code *code) {
  MAKE_CODE32(inst, code, 0xd65f03c0);
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
  [MOV] = (const AsmInstTable[]){ {asm_mov, REG, IMMEDIATE, NOOPERAND}, {NULL} },
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

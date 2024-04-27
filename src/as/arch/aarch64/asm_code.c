#include "../../../config.h"
#include "asm_code.h"

#include <assert.h>
#include <string.h>  // memcpy

#include "aarch64_code.h"
#include "inst.h"
#include "parse_asm.h"
#include "util.h"

#define ZERO  31
#define SP    31
#define LR    30

void make_code16(Inst *inst, Code *code, unsigned short *buf, int len) {
  assert(code->len + len <= (int)sizeof(code->buf));
  code->inst = inst;
  memcpy(code->buf + code->len, buf, len);
  code->len += len;
}

void make_code32(Inst *inst, Code *code, unsigned int *buf, int len) {
  assert(code->len + len <= (int)sizeof(code->buf));
  code->inst = inst;
  memcpy(code->buf + code->len, buf, len);
  code->len += len;
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
  Operand *opr1 = &inst->opr[0];
  Operand *opr2 = &inst->opr[1];
  if (opr2->type == IMMEDIATE) {
    uint32_t sz = opr1->reg.size == REG64 ? 1 : 0;
    W_MOVZ(sz, opr1->reg.no, opr2->immediate, 0);
    return code->buf;
  }
  return NULL;
}

static unsigned char *asm_ldpstp(Inst *inst, Code *code) {
  static const uint32_t kPrePost[] = { 2 , 3, 1 };
  Operand *opr1 = &inst->opr[0];
  Operand *opr2 = &inst->opr[1];
  assert(opr1->reg.size == opr2->reg.size);
  uint32_t sz = opr1->reg.size == REG64 ? 1 : 0;
  Operand *opr3 = &inst->opr[2];
  assert(opr3->indirect.reg.size == REG64);
  assert(opr3->indirect.offset == NULL || opr3->indirect.offset->kind == EX_FIXNUM);
  int64_t offset = opr3->indirect.offset != NULL ? opr3->indirect.offset->fixnum : 0;
  assert(offset < (1 << (6 + 3)) && offset >= -(1 << (6 + 3)));
  uint32_t base = opr3->indirect.reg.no;
  uint32_t prepost = kPrePost[opr3->indirect.prepost];
  switch (inst->op) {
  case LDP:  W_LDP(sz, opr1->reg.no, opr2->reg.no, offset, base, prepost); break;
  case STP:  W_STP(sz, opr1->reg.no, opr2->reg.no, offset, base, prepost); break;
  default: assert(false); break;
  }
  return code->buf;
}

static unsigned char *asm_bl(Inst *inst, Code *code) {
  W_BL(0);
  return code->buf;
}

static unsigned char *asm_blr(Inst *inst, Code *code) {
  Operand *opr1 = &inst->opr[0];
  if (opr1->reg.size == REG64) {
    W_BLR(opr1->reg.no);
    return code->buf;
  }
  return NULL;
}

static unsigned char *asm_ret(Inst *inst, Code *code) {
  W_RET(LR);
  return code->buf;
}

////////////////////////////////////////////////

typedef unsigned char *(*AsmInstFunc)(Inst *inst, Code *code);

static const AsmInstFunc table[] = {
  [NOOP] = asm_noop,
  [MOV] = asm_mov,
  [LDP] = asm_ldpstp,
  [STP] = asm_ldpstp,
  [BL] = asm_bl,
  [BLR] = asm_blr,
  [RET] = asm_ret,
};

void assemble_inst(Inst *inst, const ParseInfo *info, Code *code) {
  code->flag = 0;
  code->len = 0;

  const AsmInstFunc *func = NULL;
  if (inst->op < (enum Opcode)ARRAY_SIZE(table) && table[inst->op] != NULL) {
    func = &table[inst->op];

    if (func != NULL) {
      unsigned char *p = (*func)(inst, code);
      if (p != NULL) {
        if (p > code->buf) {
          code->inst = inst;
          code->len = p - code->buf;
          assert((size_t)code->len <= sizeof(code->buf));
        }
        return;
      }
    }
  }

  assemble_error(info, "Illegal operand");
}

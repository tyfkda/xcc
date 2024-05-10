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

static const uint32_t kPrePost[] = { 2, 3, 1 };

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

static unsigned char *asm_2ri(Inst *inst, Code *code) {
  Operand *opr1 = &inst->opr[0];
  Operand *opr2 = &inst->opr[1];
  Operand *opr3 = &inst->opr[2];
  assert(opr1->reg.size == opr2->reg.size);
  uint32_t sz = opr1->reg.size == REG64 ? 1 : 0;
  int64_t imm = 0;
  if (opr3->type == IMMEDIATE) {
    if (opr3->immediate < -(1 << 12) || opr3->immediate >= (1 << 12))
      return NULL;
    imm = opr3->immediate;
  }

  switch (inst->op) {
  case ADD_I:
    if (imm >= 0)  W_ADD_I(sz, opr1->reg.no, opr2->reg.no, imm);
    else           W_SUB_I(sz, opr1->reg.no, opr2->reg.no, -imm);
    break;
  default: assert(false); return NULL;
  }
  return code->buf;
}

static unsigned char *asm_ldrstr(Inst *inst, Code *code) {
  Operand *opr1 = &inst->opr[0];
  uint32_t sz = opr1->reg.size == REG64 ? 1 : 0;
  Operand *opr2 = &inst->opr[1];
  assert(opr2->indirect.reg.size == REG64);
  assert(opr2->indirect.offset == NULL || opr2->indirect.offset->kind == EX_FIXNUM);
  int64_t offset = opr2->indirect.offset != NULL ? opr2->indirect.offset->fixnum : 0;
  assert(offset < (1 << (6 + 3)) && offset >= -(1 << (6 + 3)));
  uint32_t base = opr2->indirect.reg.no;
  uint32_t prepost = kPrePost[opr2->indirect.prepost];
  switch (inst->op) {
  case LDRB: case LDRH: case LDR:
  case LDRSB: case LDRSH:
    {
      uint32_t b = inst->op - LDRB, s = 0;
      if (b >= 3) {
        b -= 3;
        s = 1;
      }
      b |= sz;
      if (opr2->indirect.prepost == 0) {
        if (offset >= 0)
          W_LDR_UIMM(b, s, opr1->reg.no, offset >> (2 + sz), base);
        else
          W_LDUR(b, s, opr1->reg.no, offset, base);
      } else {
        W_LDR(b, s, opr1->reg.no, offset, base, prepost);
      }
    }
    break;
  case STRB: case STRH: case STR:
    if (opr2->indirect.prepost == 0) {
      if (offset >= 0)
        W_STR_UIMM((inst->op - STRB) | sz, opr1->reg.no, 0, base);
      else
        W_STUR((inst->op - STRB) | sz, opr1->reg.no, offset, base);
    } else {
      W_STR((inst->op - STRB) | sz, opr1->reg.no, offset, base, prepost);
    }
    break;
  default: assert(false); break;
  }
  return code->buf;
}

static unsigned char *asm_ldpstp(Inst *inst, Code *code) {
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

static unsigned char *asm_adrp(Inst *inst, Code *code) {
  Operand *opr1 = &inst->opr[0];
  if (opr1->reg.size == REG64) {
    W_ADRP(opr1->reg.no, 0);
    return code->buf;
  }
  return NULL;
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
  [ADD_I] = asm_2ri,
  [LDRB] = asm_ldrstr, [LDRSB] = asm_ldrstr, [LDR] = asm_ldrstr,
  [LDRH] = asm_ldrstr, [LDRSH] = asm_ldrstr, [LDRSW] = asm_ldrstr,
  [STRB] = asm_ldrstr, [STRH] = asm_ldrstr,  [STR] = asm_ldrstr,
  [LDP] = asm_ldpstp,
  [STP] = asm_ldpstp,
  [ADRP] = asm_adrp,
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

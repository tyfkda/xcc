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
  switch (opr2->type) {
  case REG:
    {
      assert(opr1->reg.size == opr2->reg.size);
      uint32_t sz = opr1->reg.size == REG64 ? 1 : 0;
      if (opr1->reg.sp || opr2->reg.sp)
        P_MOV_SP(sz, opr1->reg.no, opr2->reg.no);
      else
        P_MOV(sz, opr1->reg.no, opr2->reg.no);
    }
    return code->buf;
  case IMMEDIATE:
    {
      uint32_t sz = opr1->reg.size == REG64 ? 1 : 0;
      int64_t imm = opr2->immediate;
      if (imm >= 0)
        W_MOVZ(sz, opr1->reg.no, imm, 0);
      else
        W_MOVN(sz, opr1->reg.no, ~imm, 0);
      return code->buf;
    }
  default: assert(false); break;
  }
  return NULL;
}

static unsigned char *asm_movk(Inst *inst, Code *code) {
  Operand *opr1 = &inst->opr[0];
  Operand *opr2 = &inst->opr[1];
  Operand *opr3 = &inst->opr[2];
  assert(opr3->type == SHIFT && (opr3->immediate & 15) == 0);
  uint32_t sz = opr1->reg.size == REG64 ? 1 : 0;
  W_MOVK(sz, opr1->reg.no, opr2->immediate, opr3->immediate >> 4);
  return code->buf;
}

static unsigned char *asm_3r(Inst *inst, Code *code) {
  Operand *opr1 = &inst->opr[0];
  Operand *opr2 = &inst->opr[1];
  Operand *opr3 = &inst->opr[2];
  assert(opr1->reg.size == opr2->reg.size);
  assert(opr1->reg.size == opr3->reg.size);
  uint32_t sz = opr1->reg.size == REG64 ? 1 : 0;

  switch (inst->op) {
  case ADD_R:
    if (inst->opr[3].type == NOOPERAND) {
      if (!opr1->reg.sp && !opr2->reg.sp) {
        W_ADD_S(sz, opr1->reg.no, opr2->reg.no, opr3->reg.no, 0, 0);
      } else {
        const uint32_t option = 3;  // LSL 0
        W_ADD_E(sz, opr1->reg.no, opr2->reg.no, opr3->reg.no, option, 0);
      }
    } else {
      Operand *ext = &inst->opr[3];
      assert(ext->type == EXTEND);
      // TODO: Range check for imm.
      int option = ext->extend.option;
      if (option < 8)  // extend
        W_ADD_E(sz, opr1->reg.no, opr2->reg.no, opr3->reg.no, option, ext->extend.imm);
      else
        W_ADD_S(sz, opr1->reg.no, opr2->reg.no, opr3->reg.no, option & 3, ext->extend.imm);
    }
    break;
  case SUB_R:
    if (inst->opr[3].type == NOOPERAND) {
      if (!opr1->reg.sp && !opr2->reg.sp) {
        W_SUB_S(sz, opr1->reg.no, opr2->reg.no, opr3->reg.no, 0, 0);
      } else {
        const uint32_t option = 3;  // LSL 0
        W_SUB_E(sz, opr1->reg.no, opr2->reg.no, opr3->reg.no, option, 0);
      }
    } else {
      Operand *ext = &inst->opr[3];
      assert(ext->type == EXTEND);
      // TODO: Range check for imm.
      int option = ext->extend.option;
      if (option < 8)  // extend
        W_SUB_E(sz, opr1->reg.no, opr2->reg.no, opr3->reg.no, option, ext->extend.imm);
      else
        W_SUB_S(sz, opr1->reg.no, opr2->reg.no, opr3->reg.no, option & 3, ext->extend.imm);
    }
    break;
  case MUL:  P_MUL(sz, opr1->reg.no, opr2->reg.no, opr3->reg.no); break;
  case SDIV: W_SDIV(sz, opr1->reg.no, opr2->reg.no, opr3->reg.no); break;
  case UDIV: W_UDIV(sz, opr1->reg.no, opr2->reg.no, opr3->reg.no); break;
  case AND:  W_AND_S(sz, opr1->reg.no, opr2->reg.no, opr3->reg.no, 0); break;
  case ORR:  W_ORR_S(sz, opr1->reg.no, opr2->reg.no, opr3->reg.no, 0); break;
  case EOR:  W_EOR_S(sz, opr1->reg.no, opr2->reg.no, opr3->reg.no, 0); break;
  case EON:  W_EON_S(sz, opr1->reg.no, opr2->reg.no, opr3->reg.no, 0); break;
  case LSL_R:  W_LSLV(sz, opr1->reg.no, opr2->reg.no, opr3->reg.no); break;
  case LSR_R:  W_LSRV(sz, opr1->reg.no, opr2->reg.no, opr3->reg.no); break;
  case ASR_R:  W_ASRV(sz, opr1->reg.no, opr2->reg.no, opr3->reg.no); break;
  default: assert(false); return NULL;
  }
  return code->buf;
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
  case SUB_I:
    if (imm >= 0)  W_SUB_I(sz, opr1->reg.no, opr2->reg.no, imm);
    else           W_ADD_I(sz, opr1->reg.no, opr2->reg.no, -imm);
    break;
  case LSL_I:  P_LSL_I(sz, opr1->reg.no, opr2->reg.no, imm); break;
  case LSR_I:  P_LSR_I(sz, opr1->reg.no, opr2->reg.no, imm); break;
  case ASR_I:  P_ASR_I(sz, opr1->reg.no, opr2->reg.no, imm); break;
  default: assert(false); return NULL;
  }
  return code->buf;
}

static unsigned char *asm_2r(Inst *inst, Code *code) {
  Operand *opr1 = &inst->opr[0];
  Operand *opr2 = &inst->opr[1];
  uint32_t sz = opr1->reg.size == REG64 ? 1 : 0;

  switch (inst->op) {
  case CMP_R:  P_CMP(sz, opr1->reg.no, opr2->reg.no); break;
  case CMN_R:  P_CMN(sz, opr1->reg.no, opr2->reg.no); break;
  case SXTB:
  case SXTH:
  case SXTW:
    {
      uint32_t imms = (8U << (inst->op - SXTB)) - 1U;
      assert(opr2->reg.size == REG32);
      W_SBFM(sz, opr1->reg.no, sz, opr2->reg.no, 0, imms);
    }
    break;
  case UXTB:
  case UXTH:
    {
      uint32_t imms = (8U << (inst->op - UXTB)) - 1U;
      assert(opr2->reg.size == REG32);
      W_UBFM(0, opr1->reg.no, 0, opr2->reg.no, 0, imms);
    }
    break;
  case UXTW:
    P_MOV(0, opr1->reg.no, opr2->reg.no);
    break;
  default: assert(false); return NULL;
  }
  return code->buf;
}

static unsigned char *asm_ri(Inst *inst, Code *code) {
  Operand *opr1 = &inst->opr[0];
  Operand *opr2 = &inst->opr[1];
  uint32_t sz = opr1->reg.size == REG64 ? 1 : 0;
  uint32_t imm = 0;
  if (opr2->type == IMMEDIATE) {
    if (opr2->immediate < 0 || opr2->immediate >= (1 << 12))
      return NULL;
    imm = opr2->immediate;
  }

  switch (inst->op) {
  case CMP_I:  P_CMP_I(sz, opr1->reg.no, imm); break;
  case CMN_I:  P_CMN_I(sz, opr1->reg.no, imm); break;
  default: assert(false); return NULL;
  }
  return code->buf;
}

static unsigned char *asm_4r(Inst *inst, Code *code) {
  Operand *opr1 = &inst->opr[0];
  Operand *opr2 = &inst->opr[1];
  Operand *opr3 = &inst->opr[2];
  Operand *opr4 = &inst->opr[3];
  assert(opr1->reg.size == opr2->reg.size);
  assert(opr1->reg.size == opr3->reg.size);
  assert(opr1->reg.size == opr4->reg.size);
  uint32_t sz = opr1->reg.size == REG64 ? 1 : 0;

  switch (inst->op) {
  case MADD:  W_MADD(sz, opr1->reg.no, opr2->reg.no, opr3->reg.no, opr4->reg.no); break;
  case MSUB:  W_MSUB(sz, opr1->reg.no, opr2->reg.no, opr3->reg.no, opr4->reg.no); break;
  default: assert(false); return NULL;
  }
  return code->buf;
}

static unsigned char *asm_ldrstr(Inst *inst, Code *code) {
  Operand *opr1 = &inst->opr[0];
  uint32_t sz = opr1->reg.size == REG64 ? 1 : 0;
  Operand *opr2 = &inst->opr[1];
  if (opr2->type == INDIRECT) {
    assert(opr2->indirect.reg.size == REG64);
    // assert(opr2->indirect.offset == NULL || opr2->indirect.offset->kind == EX_FIXNUM);
    ExprWithFlag *offset_expr = &opr2->indirect.offset;
    int64_t offset = offset_expr->expr != NULL && offset_expr->expr->kind == EX_FIXNUM ? offset_expr->expr->fixnum : 0;
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
          W_STR_UIMM((inst->op - STRB) | sz, opr1->reg.no, offset >> (2 + sz), base);
        else
          W_STUR((inst->op - STRB) | sz, opr1->reg.no, offset, base);
      } else {
        W_STR((inst->op - STRB) | sz, opr1->reg.no, offset, base, prepost);
      }
      break;
    default: assert(false); break;
    }
    return code->buf;
  } else {
    assert(opr2->type == REGISTER_OFFSET);
    assert(opr2->register_offset.scale == NULL || opr2->register_offset.scale->kind == EX_FIXNUM);

    static const uint32_t opts[] = {3, 6, 2, 3, 3};
    uint32_t opt = opts[opr2->register_offset.extend];

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

        uint32_t s2 = 0;
        Expr *scale = opr2->register_offset.scale;
        if (scale != NULL) {
          assert(scale->kind == EX_FIXNUM);
          int64_t n = scale->fixnum;
          if (!(n == 0 || n == b)) {
            // TODO: Error message.
            return NULL;
          }
          s2 = n == b;
        }
        W_LDR_R(b, opr1->reg.no, opr2->register_offset.base_reg.no, opr2->register_offset.index_reg.no, s, s2, opt);
      }
      break;
    case STRB: case STRH: case STR:
      {
        uint32_t b = (inst->op - STRB) | sz;

        uint32_t s2 = 0;
        Expr *scale = opr2->register_offset.scale;
        if (scale != NULL) {
          assert(scale->kind == EX_FIXNUM);
          int64_t n = scale->fixnum;
          if (!(n == 0 || n == b)) {
            // TODO: Error message.
            return NULL;
          }
          s2 = n == b;
        }
        W_STR_R(b, opr1->reg.no, opr2->register_offset.base_reg.no, opr2->register_offset.index_reg.no, s2, opt);
      }
      break;
    default: assert(false); break;
    }
    return code->buf;
  }
}

static unsigned char *asm_ldpstp(Inst *inst, Code *code) {
  Operand *opr1 = &inst->opr[0];
  Operand *opr2 = &inst->opr[1];
  assert(opr1->reg.size == opr2->reg.size);
  uint32_t sz = opr1->reg.size == REG64 ? 1 : 0;
  Operand *opr3 = &inst->opr[2];
  assert(opr3->indirect.reg.size == REG64);
  assert(opr3->indirect.offset.expr == NULL || opr3->indirect.offset.expr->kind == EX_FIXNUM);
  int64_t offset = opr3->indirect.offset.expr != NULL ? opr3->indirect.offset.expr->fixnum : 0;
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

static unsigned char *asm_cset(Inst *inst, Code *code) {
  Operand *opr1 = &inst->opr[0];
  Operand *opr2 = &inst->opr[1];
  uint32_t sz = opr1->reg.size == REG64 ? 1 : 0;
  P_CSET(sz, opr1->reg.no, opr2->cond);
  return code->buf;
}

static unsigned char *asm_b(Inst *inst, Code *code) {
  W_B();
  return code->buf;
}

static unsigned char *asm_br(Inst *inst, Code *code) {
  Operand *opr1 = &inst->opr[0];
  if (opr1->reg.size == REG64) {
    W_BR(opr1->reg.no);
    return code->buf;
  }
  return NULL;
}

static unsigned char *asm_bcc(Inst *inst, Code *code) {
  uint32_t cond = inst->op == B ? BAL - BEQ : inst->op - BEQ;
  W_BCC(cond);
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

static unsigned char *asm_svc(Inst *inst, Code *code) {
  Operand *opr1 = &inst->opr[0];
  W_SVC(opr1->immediate);
  return code->buf;
}

// FP instructions.

static unsigned char *asm_f_ldrstr(Inst *inst, Code *code) {
  Operand *opr1 = &inst->opr[0];
  Operand *opr2 = &inst->opr[1];
  uint32_t sz = opr1->reg.size == REG64 ? 1 : 0;
  if (opr2->type == INDIRECT) {
    assert(opr2->indirect.reg.size == REG64);
    // assert(opr2->indirect.offset == NULL || opr2->indirect.offset->kind == EX_FIXNUM);
    ExprWithFlag *offset_expr = &opr2->indirect.offset;
    int64_t offset = offset_expr->expr != NULL && offset_expr->expr->kind == EX_FIXNUM ? offset_expr->expr->fixnum : 0;
    assert(offset < (1 << (6 + 3)) && offset >= -(1 << (6 + 3)));
    uint32_t base = opr2->indirect.reg.no;
    uint32_t prepost = kPrePost[opr2->indirect.prepost];

    switch (inst->op) {
    case F_LDR:
      {
        uint32_t s = 0;
        if (opr2->indirect.prepost == 0) {
          if (offset >= 0)
            F_LDR_UIMM(sz, s, opr1->reg.no, offset >> (2 + sz), base);
          else
            F_LDUR(sz, s, opr1->reg.no, offset, base);
        } else {
          F_LDR(sz, s, opr1->reg.no, offset, base, prepost);
        }
      }
      break;
    case F_STR:
      if (opr2->indirect.prepost == 0) {
        if (offset >= 0)
          F_STR_UIMM((inst->op - STRB) | sz, opr1->reg.no, 0, base);
        else
          F_STUR((inst->op - STRB) | sz, opr1->reg.no, offset, base);
      } else {
        F_STR((inst->op - STRB) | sz, opr1->reg.no, offset, base, prepost);
      }
      break;
    default: assert(false); break;
    }
    return code->buf;
  } else {
    assert(opr2->type == REGISTER_OFFSET);
    assert(opr2->register_offset.scale == NULL || opr2->register_offset.scale->kind == EX_FIXNUM);

    static const uint32_t opts[] = {3, 6, 2, 3, 3};
    uint32_t opt = opts[opr2->register_offset.extend];
    uint32_t s = opr2->register_offset.extend > 0 ? 1 : 0;

    switch (inst->op) {
    case LDR:    W_LDR_R(sz, opr1->reg.no, opr2->register_offset.base_reg.no, opr2->register_offset.index_reg.no, 0, s, opt); break;
    case STR:    W_STR_R(sz, opr1->reg.no, opr2->register_offset.base_reg.no, opr2->register_offset.index_reg.no, s, opt); break;
    default: assert(false); break;
    }
    return code->buf;
  }
}

static unsigned char *asm_f_ldpstp(Inst *inst, Code *code) {
  Operand *opr1 = &inst->opr[0];
  Operand *opr2 = &inst->opr[1];
  uint32_t sz = opr1->reg.size == REG64 ? 1 : 0;
  Operand *opr3 = &inst->opr[2];
  assert(opr3->indirect.reg.size == REG64);
  assert(opr3->indirect.offset.expr->kind == EX_FIXNUM);
  int64_t offset = opr3->indirect.offset.expr->fixnum;
  assert(offset < (1 << (6 + 3)) && offset >= -(1 << (6 + 3)));
  uint32_t base = opr3->indirect.reg.no;
  uint32_t prepost = kPrePost[opr3->indirect.prepost];
  switch (inst->op) {
  case F_LDP:  F_LDP(sz, opr1->reg.no, opr2->reg.no, base, offset, prepost); break;
  case F_STP:  F_STP(sz, opr1->reg.no, opr2->reg.no, base, offset, prepost); break;
  default: assert(false); break;
  }
  return code->buf;
}

static unsigned char *asm_f_3r(Inst *inst, Code *code) {
  Operand *opr1 = &inst->opr[0];
  Operand *opr2 = &inst->opr[1];
  Operand *opr3 = &inst->opr[2];
  uint32_t sz = opr1->reg.size == REG64 ? 1 : 0;

  switch (inst->op) {
  case FADD:  FADD(sz, opr1->reg.no, opr2->reg.no, opr3->reg.no); break;
  case FSUB:  FSUB(sz, opr1->reg.no, opr2->reg.no, opr3->reg.no); break;
  case FMUL:  FMUL(sz, opr1->reg.no, opr2->reg.no, opr3->reg.no); break;
  case FDIV:  FDIV(sz, opr1->reg.no, opr2->reg.no, opr3->reg.no); break;
  default: assert(false); break;
  }
  return code->buf;
}

static unsigned char *asm_f_2r(Inst *inst, Code *code) {
  Operand *opr1 = &inst->opr[0];
  Operand *opr2 = &inst->opr[1];
  uint32_t dsz = opr1->reg.size == REG64 ? 1 : 0;
  uint32_t ssz = opr2->reg.size == REG64 ? 1 : 0;

  switch (inst->op) {
  case FMOV:    FMOV(dsz, opr1->reg.no, opr2->reg.no); break;
  case FCMP:    FCMP(dsz, opr1->reg.no, opr2->reg.no); break;
  case FNEG:    FNEG(dsz, opr1->reg.no, opr2->reg.no); break;
  case FSQRT:   FSQRT(dsz, opr1->reg.no, opr2->reg.no); break;
  case SCVTF:   SCVTF(ssz, opr1->reg.no, dsz, opr2->reg.no); break;
  case UCVTF:   UCVTF(ssz, opr1->reg.no, dsz, opr2->reg.no); break;
  case FCVT:    FCVT(dsz, opr1->reg.no, opr2->reg.no); break;
  case FCVTZS:  FCVTZS(dsz, opr1->reg.no, ssz, opr2->reg.no); break;
  case FCVTZU:  FCVTZU(dsz, opr1->reg.no, ssz, opr2->reg.no); break;
  default: assert(false); break;
  }
  return code->buf;
}

////////////////////////////////////////////////

typedef unsigned char *(*AsmInstFunc)(Inst *inst, Code *code);

static const AsmInstFunc table[] = {
  [NOOP] = asm_noop,
  [MOV] = asm_mov, [MOVK] = asm_movk,
  [ADD_R] = asm_3r, [ADD_I] = asm_2ri,
  [SUB_R] = asm_3r, [SUB_I] = asm_2ri,
  [MUL] = asm_3r, [SDIV] = asm_3r, [UDIV] = asm_3r,
  [MADD] = asm_4r, [MSUB] = asm_4r,
  [AND] = asm_3r, [ORR] = asm_3r, [EOR] = asm_3r, [EON] = asm_3r,
  [CMP_R] = asm_2r, [CMP_I] = asm_ri,
  [CMN_R] = asm_2r, [CMN_I] = asm_ri,
  [LSL_R] = asm_3r, [LSL_I] = asm_2ri,
  [LSR_R] = asm_3r, [LSR_I] = asm_2ri,
  [ASR_R] = asm_3r, [ASR_I] = asm_2ri,
  [SXTB] = asm_2r, [SXTH] = asm_2r,  [SXTW] = asm_2r,
  [UXTB] = asm_2r, [UXTH] = asm_2r,  [UXTW] = asm_2r,
  [LDRB] = asm_ldrstr, [LDRSB] = asm_ldrstr, [LDR] = asm_ldrstr,
  [LDRH] = asm_ldrstr, [LDRSH] = asm_ldrstr, [LDRSW] = asm_ldrstr,
  [STRB] = asm_ldrstr, [STRH] = asm_ldrstr,  [STR] = asm_ldrstr,
  [LDP] = asm_ldpstp,
  [STP] = asm_ldpstp,
  [ADRP] = asm_adrp,
  [CSET] = asm_cset,
  [B] = asm_b,
  [BR] = asm_br,
  [BEQ] = asm_bcc,  [BNE] = asm_bcc,  [BHS] = asm_bcc,  [BLO] = asm_bcc,
  [BMI] = asm_bcc,  [BPL] = asm_bcc,  [BVS] = asm_bcc,  [BVC] = asm_bcc,
  [BHI] = asm_bcc,  [BLS] = asm_bcc,  [BGE] = asm_bcc,  [BLT] = asm_bcc,
  [BGT] = asm_bcc,  [BLE] = asm_bcc,  [BAL] = asm_bcc,  [BNV] = asm_bcc,
  [BL] = asm_bl,
  [BLR] = asm_blr,
  [RET] = asm_ret,
  [SVC] = asm_svc,

  [F_LDR] = asm_f_ldrstr,
  [F_STR] = asm_f_ldrstr,
  [F_LDP] = asm_f_ldpstp,
  [F_STP] = asm_f_ldpstp,
  [FMOV] = asm_f_2r,
  [FADD] = asm_f_3r, [FSUB] = asm_f_3r, [FMUL] = asm_f_3r, [FDIV] = asm_f_3r,
  [FCMP] = asm_f_2r, [FNEG] = asm_f_2r,
  [FSQRT] = asm_f_2r,
  [SCVTF] = asm_f_2r, [UCVTF] = asm_f_2r,
  [FCVT] = asm_f_2r, [FCVTZS] = asm_f_2r, [FCVTZU] = asm_f_2r,
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

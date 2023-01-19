#include "../config.h"
#include "asm_x86.h"

#include <assert.h>
#include <stdlib.h>  // exit
#include <string.h>  // memcpy

#include "inst.h"
#include "parse_asm.h"
#include "util.h"

#define ARRAY_SIZE(array)  (sizeof(array) / sizeof(*(array)))

#ifndef PUT_CODE
#define PUT_CODE(p, ...)  do { unsigned char buf[] = {__VA_ARGS__}; memcpy(p, buf, sizeof(buf)); } while (0)
#endif

static const int kPow2Table[] = {-1, 0, 1, -1, 2, -1, -1, -1, 3};

static unsigned char *put_code_filtered(unsigned char *p, const short *buf, size_t count) {
  for (size_t i = 0; i < count; ++i) {
    short c = *buf++;
    if (c >= 0)
      *p++ = c;
  }
  return p;
}

void make_code(Inst *inst, Code *code, unsigned char *buf, int len) {
  assert(len <= (int)sizeof(code->buf));
  code->inst = inst;
  code->len = len;
  memcpy(code->buf, buf, len);
}

static char opr_regno(const Reg *reg) {
  return reg->no | (reg->x << 3);
}

static bool opr_reg8(const Reg *reg) {
  assert(reg->size == REG8);
  return opr_regno(reg) < 4;
}

static bool assemble_error(const ParseInfo *info, const char *message) {
  parse_error(info, message);
  return false;
}

static unsigned char *put_rex0(unsigned char *p, enum RegSize size, int sno, int dno,
                               unsigned char opcode) {
  if (size == REG16)
    *p++ = 0x66;
  if (sno >= 8 || dno >= 8 || size == REG64)
    *p++ = 0x40 | ((dno & 8) >> 3) | ((sno & 8) >> 1) | (size != REG64 ? 0 : 8);
  *p++ = opcode;
  return p;
}

static unsigned char *put_rex1(unsigned char *p, enum RegSize size, int rex_prefix, int dno,
                               unsigned char opcode) {
  p = put_rex0(p, size, 0, dno, opcode);
  *p++ = rex_prefix | (dno & 7);
  return p;
}

static unsigned char *put_rex2(unsigned char *p, enum RegSize size, int sno, int dno,
                               unsigned char opcode) {
  p = put_rex0(p, size, sno, dno, opcode);
  *p++ = 0xc0 | ((sno & 7) << 3) | (dno & 7);
  return p;
}

static unsigned char *put_rex_indirect(
    unsigned char *p, enum RegSize size, int reg, int indirect_reg,
    unsigned char opcode, unsigned char op2, long offset)
{
  p = put_rex0(p, size, reg, indirect_reg,
               size == REG8 ? opcode : (unsigned char)(opcode + 1));
  int d = reg & 7;
  int s = indirect_reg & 7;
  unsigned char code = (offset == 0 && s != RBP - RAX) ? op2 : is_im8(offset) ? (unsigned char)0x40 : (unsigned char)0x80;
  *p++ = code | s | (d << 3);
  if (s == RSP - RAX)
    *p++ = 0x24;

  if (offset == 0 && s != RBP - RAX) {
    ;
  } else if (is_im8(offset)) {
    *p++ = IM8(offset);
  } else if (is_im32(offset)) {
    PUT_CODE(p, IM32(offset));
    p += 4;
  }
  return p;
}

static unsigned char *asm_noop(Inst *inst, Code *code) {
  UNUSED(inst);
  unsigned char *p = code->buf;
  return p;
}

static unsigned char *asm_mov_rr(Inst *inst, Code *code) {
  unsigned char *p = code->buf;
  enum RegSize size = inst->src.reg.size;
  p = put_rex2(p, size, opr_regno(&inst->src.reg), opr_regno(&inst->dst.reg),
               size == REG8 ? 0x88 : 0x89);
  return p;
}

static unsigned char *asm_mov_imr(Inst *inst, Code *code) {
  unsigned char *p = code->buf;
  enum RegSize size = inst->dst.reg.size;
  if (size == REG64 && is_im32(inst->src.immediate)) {
    int d = inst->dst.reg.no;
    int pre = !inst->dst.reg.x ? 0x48 : 0x49;
    MAKE_CODE(inst, code, pre, 0xc7, 0xc0 | d, IM32(inst->src.immediate));
    return code->buf;
  }

  p = put_rex0(p, size, 0, opr_regno(&inst->dst.reg),
               0xb0 | (size == REG8 ? 0 : 8) | inst->dst.reg.no);
  switch (size) {
  case REG8:  PUT_CODE(p, IM8(inst->src.immediate)); break;
  case REG16: PUT_CODE(p, IM16(inst->src.immediate)); break;
  case REG32: PUT_CODE(p, IM32(inst->src.immediate)); break;
  case REG64: PUT_CODE(p, IM64(inst->src.immediate)); break;
  default: assert(false); break;
  }
  p += 1 << size;
  return p;
}

static unsigned char *asm_mov_ir(Inst *inst, Code *code) {
  if (inst->src.indirect.offset->kind == EX_FIXNUM) {
    long offset = inst->src.indirect.offset->fixnum;
    enum RegSize size = inst->dst.reg.size;
    if (inst->src.indirect.reg.no != RIP) {
      int sno = opr_regno(&inst->src.indirect.reg);
      int dno = opr_regno(&inst->dst.reg);
      bool ofszero = offset == 0 && ((sno & 7) != RBP - RAX);
      short buf[] = {
        size == REG16 ? 0x66 : -1,
        0x40 | (size == REG64 ? 0x08 : 0) | ((dno & 8) >> 1) | ((sno & 8) >> 3),
        0x8a | (size == REG8 ? 0 : 0x01),
        ((dno & 7) << 3) | (sno & 7) | (ofszero ? 0 : is_im8(offset) ? 0x40: 0x80),
        (sno & 7) == RSP - RAX ? 0x24 : -1,
      };
      unsigned char *p = code->buf;
      p = put_code_filtered(p, buf, ARRAY_SIZE(buf));

      if (ofszero) {
        // Nothing.
      } else if (is_im8(offset)) {
        *p++ = offset;
      } else {
        PUT_CODE(p, IM32(offset));
        p += 4;
      }
      return p;
    }
  }
  return NULL;
}

static unsigned char *asm_mov_ri(Inst *inst, Code *code) {
  if (inst->dst.indirect.offset->kind == EX_FIXNUM) {
    long offset = inst->dst.indirect.offset->fixnum;
    enum RegSize size = inst->src.reg.size;
    if (inst->dst.indirect.reg.no != RIP) {
      int sno = opr_regno(&inst->src.reg);
      int dno = opr_regno(&inst->dst.indirect.reg);
      bool ofszero = offset == 0 && ((dno & 7) != RBP - RAX);
      short buf[] = {
        size == REG16 ? 0x66 : -1,
        0x40 | (size == REG64 ? 0x08 : 0) | ((sno & 8) >> 1) | ((dno & 8) >> 3),
        0x88 | (size == REG8 ? 0 : 0x01),
        ((sno & 7) << 3) | (dno & 7) | (ofszero ? 0 : is_im8(offset) ? 0x40: 0x80),
        (dno & 7) == RSP - RAX ? 0x24 : -1,
      };
      unsigned char *p = code->buf;
      p = put_code_filtered(p, buf, ARRAY_SIZE(buf));

      if (ofszero) {
        // Nothing.
      } else if (is_im8(offset)) {
        *p++ = offset;
      } else {
        PUT_CODE(p, IM32(offset));
        p += 4;
      }
      return p;
    }
  }
  return NULL;
}

static unsigned char *asm_mov_iir(Inst *inst, Code *code) {
  if (inst->src.indirect_with_index.offset->kind == EX_FIXNUM) {
    long offset = inst->src.indirect_with_index.offset->fixnum;
    assert(is_im32(offset));
    short offset_bit = offset == 0 ? 0x04 : is_im8(offset) ? 0x44 : 0x84;
    enum RegSize size = inst->dst.reg.size;
    assert(inst->src.indirect_with_index.base_reg.no != RIP);
    int bno = opr_regno(&inst->src.indirect_with_index.base_reg);
    int ino = opr_regno(&inst->src.indirect_with_index.index_reg);
    int dno = opr_regno(&inst->dst.reg);
    Expr *scale_expr = inst->src.indirect_with_index.scale;
    char scale_bit = scale_expr != NULL ? kPow2Table[scale_expr->fixnum] : 0;
    short x = ((bno & 8) >> 3) | ((ino & 8) >> 2) | ((dno & 8) >> 1);
    short prefix = size == REG16 ? 0x66 : size == REG64 || x != 0 ? 0x48 | x : -1;
    short buf[] = {
      prefix,
      size == REG8 ? 0x8a : 0x8b,
      ((dno & 7) << 3) | offset_bit,
      (scale_bit << 6) | ((ino & 7) << 3) | (bno & 7),
    };
    unsigned char *p = code->buf;
    p = put_code_filtered(p, buf, ARRAY_SIZE(buf));

    if (offset != 0) {
      if (is_im8(offset)) {
        *p++ = IM8(offset);
      } else {
        PUT_CODE(p, IM32(offset));
        p += 4;
      }
    }
    return p;
  }
  return NULL;
}

static unsigned char *asm_mov_dr(Inst *inst, Code *code) {
  enum RegSize size = inst->dst.reg.size;
  int dno = opr_regno(&inst->dst.reg);
  short prefix = size == REG16 ? 0x66 : size == REG64 ? 0x48 : -1;
  short buf[] = {
    prefix,
    size == REG8 ? 0x8a : 0x8b,
    ((dno & 7) << 3) | 0x04,
    0x25,
  };
  unsigned char *p = code->buf;
  p = put_code_filtered(p, buf, ARRAY_SIZE(buf));

  PUT_CODE(p, IM32(0));
  p += 4;
  return p;
}

static unsigned char *asm_mov_rd(Inst *inst, Code *code) {
  enum RegSize size = inst->src.reg.size;
  int sno = opr_regno(&inst->src.reg);
  short prefix = size == REG16 ? 0x66 : size == REG64 ? 0x48 : -1;
  short buf[] = {
    prefix,
    size == REG8 ? 0x88 : 0x89,
    ((sno & 7) << 3) | 0x04,
    0x25,
  };
  unsigned char *p = code->buf;
  p = put_code_filtered(p, buf, ARRAY_SIZE(buf));

  PUT_CODE(p, IM32(0));
  p += 4;
  return p;
}

static unsigned char *asm_movbwlq_imi(Inst *inst, Code *code) {
  long offset = inst->dst.indirect.offset->fixnum;
  unsigned char sno = 0;
  unsigned char dno = opr_regno(&inst->dst.indirect.reg);
  unsigned char op = (offset == 0 && (dno & 7) != RBP - RAX) ? 0x00 : is_im8(offset) ? (unsigned char)0x40 : (unsigned char)0x80;
  short buf[] = {
    inst->op == MOVW ? 0x66 : -1,
    (inst->op == MOVQ || dno >= 8) ? 0x40 | (inst->op == MOVQ ? 0x08 : 0) | ((dno & 8) >> 3) | ((sno & 8) >> 1) : -1,
    0xc6 | (inst->op == MOVB ? 0 : 1),
    op | (dno & 7) | ((sno & 7) << 3),
    (dno & 7) == RSP - RAX ? 0x24 : -1,
  };

  unsigned char *p = code->buf;
  p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
  if (offset == 0 && (dno & 7) != RBP - RAX) {
    ;
  } else if (is_im8(offset)) {
    *p++ = IM8(offset);
  } else if (is_im32(offset)) {
    PUT_CODE(p, IM32(offset));
    p += 4;
  }

  long value = inst->src.immediate;
  switch (inst->op) {
  case MOVB: *p++ = IM8(value); break;
  case MOVW: PUT_CODE(p, IM16(value)); p += 2; break;
  case MOVL: case MOVQ:
    PUT_CODE(p, IM32(value));
    p += 4;
    break;
  default: assert(false); break;
  }
  return p;
}

static unsigned char *asm_movszx_rr(Inst *inst, Code *code) {
  int s = inst->src.reg.no;
  int d = inst->dst.reg.no;
  unsigned char op = inst->op == MOVZX ? 0xb6 : 0xbe;
  unsigned char *p = code->buf;
  switch (inst->src.reg.size) {
  case REG8:
    switch (inst->dst.reg.size) {
    case REG16:
      if (opr_reg8(&inst->src.reg) && !inst->dst.reg.x) {
        MAKE_CODE(inst, code, 0x66, 0x0f, op, 0xc0 + s + d * 8);
      } else {
        int pre = (!inst->src.reg.x ? 0x40 : 0x41) + (!inst->dst.reg.x ? 0 : 4);
        MAKE_CODE(inst, code, 0x66, pre, 0x0f, op, 0xc0 + s + d * 8);
      }
      return p;
    case REG32:
      if (opr_reg8(&inst->src.reg) && !inst->dst.reg.x) {
        MAKE_CODE(inst, code, 0x0f, op, 0xc0 + s + d * 8);
      } else {
        int pre = (!inst->src.reg.x ? 0x40 : 0x41) + (!inst->dst.reg.x ? 0 : 4);
        MAKE_CODE(inst, code, pre, 0x0f, op, 0xc0 + s + d * 8);
      }
      return p;
    case REG64:
      {
        int pre = (!inst->src.reg.x ? 0x48 : 0x49) + (!inst->dst.reg.x ? 0 : 4);
        MAKE_CODE(inst, code, pre, 0x0f, op, 0xc0 + s + d * 8);
        return p;
      }
    default:
      break;
    }
    break;
  case REG16:
    switch (inst->dst.reg.size) {
    case REG32:
      if (!inst->src.reg.x && !inst->dst.reg.x) {
        MAKE_CODE(inst, code, 0x0f, op | 1, 0xc0 + s + d * 8);
      } else {
        int pre = (!inst->src.reg.x ? 0x40 : 0x41) + (!inst->dst.reg.x ? 0 : 4);
        MAKE_CODE(inst, code, pre, 0x0f, op | 1, 0xc0 + s + d * 8);
      }
      return p;
    case REG64:
      {
        int pre = (!inst->src.reg.x ? 0x48 : 0x49) + (!inst->dst.reg.x ? 0 : 4);
        MAKE_CODE(inst, code, pre, 0x0f, op | 1, 0xc0 + s + d * 8);
        return p;
      }
    default:
      break;
    }
    break;
  case REG32:
    // "MOVZX %32bit, %64bit" doesn't exist!
    if (inst->dst.reg.size == REG64 && inst->op == MOVSX) {
      int pre = (!inst->src.reg.x ? 0x48 : 0x49) + (!inst->dst.reg.x ? 0 : 4);
      MAKE_CODE(inst, code, pre, 0x63, 0xc0 + s + d * 8);
      return p;
    }
    break;
  default:
    break;
  }
  return NULL;
}

#ifndef __NO_FLONUM
static unsigned char *asm_movsds_xx(Inst *inst, Code *code, bool single) {
  unsigned char prefix = single ? 0xf3 : 0xf2;
  unsigned char sno = inst->src.regxmm - XMM0;
  unsigned char dno = inst->dst.regxmm - XMM0;
  short buf[] = {
    prefix,
    sno >= 8 || dno >= 8 ? (unsigned char)0x40 | ((sno & 8) >> 3) | ((dno & 8) >> 1) : -1,
    0x0f,
    0x10,
    (unsigned char)0xc0 | ((dno & 7) << 3) | (sno & 7),
  };
  unsigned char *p = code->buf;
  p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
  return p;
}
static unsigned char *asm_movsd_xx(Inst *inst, Code *code) { return asm_movsds_xx(inst, code, false); }
static unsigned char *asm_movss_xx(Inst *inst, Code *code) { return asm_movsds_xx(inst, code, true); }

static unsigned char *asm_movsds_ix(Inst *inst, Code *code, bool single) {
  long offset;
  if (inst->src.indirect.offset->kind == EX_FIXNUM &&
      (offset = inst->src.indirect.offset->fixnum, is_im32(offset))) {
    if (inst->src.indirect.reg.no != RIP) {
      unsigned char prefix = single ? 0xf3 : 0xf2;
      unsigned char sno = opr_regno(&inst->src.indirect.reg);
      unsigned char dno = inst->dst.regxmm - XMM0;
      int d = dno & 7;
      int s = sno & 7;
      unsigned char op = (offset == 0 && s != RBP - RAX) ? (unsigned char)0x00 : is_im8(offset) ? (unsigned char)0x40 : (unsigned char)0x80;

      short buf[] = {
        prefix,
        sno >= 8 || dno >= 8 ? (unsigned char)0x40 | ((sno & 8) >> 3) | ((dno & 8) >> 1) : -1,
        0x0f,
        0x10,
        op | s | (d << 3),
        s == RSP - RAX ? 0x24 : -1,
      };
      unsigned char *p = code->buf;
      p = put_code_filtered(p, buf, ARRAY_SIZE(buf));

      if (offset == 0 && s != RBP - RAX) {
        ;
      } else if (is_im8(offset)) {
        *p++ = IM8(offset);
      } else {  // is_im32(offset)
        PUT_CODE(p, IM32(offset));
        p += 4;
      }
      return p;
    }
  }
  return NULL;
}
static unsigned char *asm_movsd_ix(Inst *inst, Code *code) { return asm_movsds_ix(inst, code, false); }
static unsigned char *asm_movss_ix(Inst *inst, Code *code) { return asm_movsds_ix(inst, code, true); }

static unsigned char *asm_movsds_xi(Inst *inst, Code *code, bool single) {
  long offset;
  if (inst->dst.indirect.offset->kind == EX_FIXNUM &&
      (offset = inst->dst.indirect.offset->fixnum, is_im32(offset))) {
    if (inst->dst.indirect.reg.no != RIP) {
      unsigned char prefix = single ? 0xf3 : 0xf2;
      unsigned char sno = inst->src.regxmm - XMM0;
      unsigned char dno = opr_regno(&inst->dst.indirect.reg);
      int d = dno & 7;
      int s = sno & 7;
      unsigned char op = (offset == 0 && d != RBP - RAX) ? (unsigned char)0x00 : is_im8(offset) ? (unsigned char)0x40 : (unsigned char)0x80;

      short buf[] = {
        prefix,
        sno >= 8 || dno >= 8 ? (unsigned char)0x40 | ((dno & 8) >> 3) | ((sno & 8) >> 1) : -1,
        0x0f,
        0x11,
        op | d | (s << 3),
        d == RSP - RAX ? 0x24 : -1,
      };
      unsigned char *p = code->buf;
      p = put_code_filtered(p, buf, ARRAY_SIZE(buf));

      if (offset == 0 && d != RBP - RAX) {
        ;
      } else if (is_im8(offset)) {
        *p++ = IM8(offset);
      } else {  // is_im32(offset)
        PUT_CODE(p, IM32(offset));
        p += 4;
      }
      return p;
    }
  }
  return NULL;
}
static unsigned char *asm_movsd_xi(Inst *inst, Code *code) { return asm_movsds_xi(inst, code, false); }
static unsigned char *asm_movss_xi(Inst *inst, Code *code) { return asm_movsds_xi(inst, code, true); }

static unsigned char *assemble_bop_sd(Inst *inst, Code *code, bool single, unsigned char op) {
  unsigned char *p = code->buf;
  unsigned char prefix = single ? 0xf3 : 0xf2;
  if (inst->src.type == REG_XMM && inst->dst.type == REG_XMM) {
    unsigned char sno = inst->src.regxmm - XMM0;
    unsigned char dno = inst->dst.regxmm - XMM0;
    short buf[] = {
      prefix,
      sno >= 8 || dno >= 8 ? (unsigned char)0x40 | ((sno & 8) >> 3) | ((dno & 8) >> 1) : -1,
      0x0f,
      op,
      (unsigned char)0xc0 | ((dno & 7) << 3) | (sno & 7),
    };
    p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
  }

  return p;
}

static unsigned char *asm_addsd_xx(Inst *inst, Code *code) { return assemble_bop_sd(inst, code, false, 0x58); }
static unsigned char *asm_addss_xx(Inst *inst, Code *code) { return assemble_bop_sd(inst, code, true, 0x58); }

static unsigned char *asm_subsd_xx(Inst *inst, Code *code) { return assemble_bop_sd(inst, code, false, 0x5c); }
static unsigned char *asm_subss_xx(Inst *inst, Code *code) { return assemble_bop_sd(inst, code, true, 0x5c); }

static unsigned char *asm_mulsd_xx(Inst *inst, Code *code) { return assemble_bop_sd(inst, code, false, 0x59); }
static unsigned char *asm_mulss_xx(Inst *inst, Code *code) { return assemble_bop_sd(inst, code, true, 0x59); }

static unsigned char *asm_divsd_xx(Inst *inst, Code *code) { return assemble_bop_sd(inst, code, false, 0x5e); }
static unsigned char *asm_divss_xx(Inst *inst, Code *code) { return assemble_bop_sd(inst, code, true, 0x5e); }

static unsigned char *assemble_ucomisd(Inst *inst, Code *code, bool single) {
  unsigned char *p = code->buf;
  if (inst->src.type == REG_XMM && inst->dst.type == REG_XMM) {
    unsigned char sno = inst->src.regxmm - XMM0;
    unsigned char dno = inst->dst.regxmm - XMM0;
    short buf[] = {
      single ? -1 : 0x66,
      sno >= 8 || dno >= 8 ? (unsigned char)0x40 | ((sno & 8) >> 3) | ((dno & 8) >> 1) : -1,
      0x0f,
      0x2e,
      (unsigned char)0xc0 | ((dno & 7) << 3) | (sno & 7),
    };
    p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
  }
  return p;
}
static unsigned char *asm_ucomisd_xx(Inst *inst, Code *code) { return assemble_ucomisd(inst, code, false); }
static unsigned char *asm_ucomiss_xx(Inst *inst, Code *code) { return assemble_ucomisd(inst, code, true); }

static unsigned char *assemble_cvtsi2sd(Inst *inst, Code *code, bool single) {
  unsigned char *p = code->buf;
  unsigned char prefix = single ? 0xf3 : 0xf2;
  unsigned char sno = opr_regno(&inst->src.reg);
  unsigned char dno = inst->dst.regxmm - XMM0;
  short buf[] = {
    prefix,
    sno >= 8 || dno >= 8 || inst->src.reg.size == REG64 ? (unsigned char)0x40 | ((sno & 8) >> 3) | ((dno & 8) >> 1) | (inst->src.reg.size == REG64 ? 8 : 0) : -1,
    0x0f,
    0x2a,
    (unsigned char)0xc0 | ((dno & 7) << 3) | (sno & 7),
  };
  p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
  return p;
}
static unsigned char *asm_cvtsi2sd_rx(Inst *inst, Code *code) { return assemble_cvtsi2sd(inst, code, false); }
static unsigned char *asm_cvtsi2ss_rx(Inst *inst, Code *code) { return assemble_cvtsi2sd(inst, code, true); }

static unsigned char *assemble_cvttsd2si(Inst *inst, Code *code, bool single) {
  unsigned char *p = code->buf;
  unsigned char prefix = single ? 0xf3 : 0xf2;
  unsigned char sno = inst->src.regxmm - XMM0;
  unsigned char dno = opr_regno(&inst->dst.reg);
  short buf[] = {
    prefix,
    sno >= 8 || dno >= 8 || inst->dst.reg.size == REG64 ? (unsigned char)0x40 | ((sno & 8) >> 3) | ((dno & 8) >> 1) | (inst->dst.reg.size == REG64 ? 8 : 0) : -1,
    0x0f,
    0x2c,
    (unsigned char)0xc0 | ((dno & 7) << 3) | (sno & 7),
  };
  p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
  return p;
}
static unsigned char *asm_cvttsd2si_xr(Inst *inst, Code *code) { return assemble_cvttsd2si(inst, code, false); }
static unsigned char *asm_cvttss2si_xr(Inst *inst, Code *code) { return assemble_cvttsd2si(inst, code, true); }

static unsigned char *assemble_cvtsd2ss(Inst *inst, Code *code, bool single) {
  unsigned char *p = code->buf;
  unsigned char prefix = single ? 0xf3 : 0xf2;
  unsigned char sno = inst->src.regxmm - XMM0;
  unsigned char dno = inst->dst.regxmm - XMM0;
  short buf[] = {
    prefix,
    sno >= 8 || dno >= 8 ? (unsigned char)0x40 | ((sno & 8) >> 3) | ((dno & 8) >> 1) : -1,
    0x0f,
    0x5a,
    (unsigned char)0xc0 | ((dno & 7) << 3) | (sno & 7),
  };
  p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
  return p;
}
static unsigned char *asm_cvtsd2ss_xx(Inst *inst, Code *code) { return assemble_cvtsd2ss(inst, code, false); }
static unsigned char *asm_cvtss2sd_xx(Inst *inst, Code *code) { return assemble_cvtsd2ss(inst, code, true); }

static unsigned char *asm_sqrtsd_xx(Inst *inst, Code *code) {
  unsigned char sno = inst->src.regxmm - XMM0;
  unsigned char dno = inst->dst.regxmm - XMM0;
  short buf[] = {
    0xf2,
    sno >= 8 || dno >= 8 ? (unsigned char)0x40 | ((sno & 8) >> 3) | ((dno & 8) >> 1) : -1,
    0x0f,
    0x51,
    (unsigned char)0xc0 | ((dno & 7) << 3) | (sno & 7),
  };
  unsigned char *p = code->buf;
  p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
  return p;
}

#endif

static long signed_immediate(long value, enum RegSize size) {
  switch (size) {
  case REG8:   return (int8_t)value;
  case REG16:  return (int16_t)value;
  case REG32:  return (int32_t)value;
  default: assert(false);  // Fallthrough
  case REG64:  return (int64_t)value;
  }
}

static unsigned char *asm_lea_ir(Inst *inst, Code *code) {
  assert(inst->src.indirect.offset != NULL);
  unsigned char *p = code->buf;
  if (inst->src.indirect.reg.no != RIP) {
    if (inst->src.indirect.offset->kind == EX_FIXNUM) {
      long offset = inst->src.indirect.offset->fixnum;
      enum RegSize size = inst->dst.reg.size;
      p = put_rex_indirect(
          p, size,
          opr_regno(&inst->dst.reg),
          opr_regno(&inst->src.indirect.reg),
          0x8c, 0x00, offset);
      return p;
    }
  } else {
    if (inst->src.indirect.offset->kind != EX_FIXNUM) {
      int pre = !inst->dst.reg.x ? 0x48 : 0x4c;
      int d = inst->dst.reg.no;
      MAKE_CODE(inst, code, pre, 0x8d, 0x05 | (d << 3), IM32(0));
      return p;
    }
  }
  return NULL;
}

static unsigned char *asm_lea_iir(Inst *inst, Code *code) {
  Expr *offset_expr = inst->src.indirect_with_index.offset;
  Expr *scale_expr = inst->src.indirect_with_index.scale;
  if ((offset_expr == NULL || offset_expr->kind == EX_FIXNUM) &&
      (scale_expr == NULL || scale_expr->kind == EX_FIXNUM)) {
    long offset = offset_expr != NULL ? offset_expr->fixnum : 0;
    long scale = scale_expr != NULL ? scale_expr->fixnum : 1;
    if (is_im32(offset) && 1 <= scale && scale <= 8 && IS_POWER_OF_2(scale)) {
      int breg = opr_regno(&inst->src.indirect_with_index.base_reg);
      int ireg = opr_regno(&inst->src.indirect_with_index.index_reg);
      int dreg = opr_regno(&inst->dst.reg);
      bool noofs = offset == 0 && breg != RBP - RAX;
      short buf[] = {
        0x48 | ((breg & 8) >> 3) | ((ireg & 8) >> 2) | ((dreg & 8) >> 1),
        0x8d,
        (noofs ? 0x00 : is_im8(offset) ? 0x40 : 0x80) | ((dreg & 7) << 3) | 0x04,
        (breg & 7) | ((ireg & 7) << 3) | (kPow2Table[scale] << 6),
        breg == RSP - RAX ? 0x24 : -1,
      };

      unsigned char *p = code->buf;
      p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
      if (noofs) {
        ;
      } else if (is_im8(offset)) {
        *p++ = IM8(offset);
      } else if (is_im32(offset)) {
        PUT_CODE(p, IM32(offset));
        p += 4;
      }
      return p;
    }
  }
  return NULL;
}

static unsigned char *asm_add_rr(Inst *inst, Code *code) {
  enum RegSize size = inst->src.reg.size;
  unsigned char *p = code->buf;
  p = put_rex2(p, size, opr_regno(&inst->src.reg), opr_regno(&inst->dst.reg),
               size == REG8 ? 0x00 : 0x01);
  return p;
}

static unsigned char *asm_add_imr(Inst *inst, Code *code) {
  long value = inst->src.immediate;
  if (is_im32(value)) {
    bool im8 = is_im8(value);
    enum RegSize size = inst->dst.reg.size;
    int d = opr_regno(&inst->dst.reg);
    unsigned char *p = code->buf;
    if (d == RAX - RAX && (size == REG8 || !is_im8(value)))
      p = put_rex0(p, size, 0, d, im8 ? 0x04 : 0x05);
    else
      p = put_rex1(p, size, 0xc0, d, im8 ? 0x83 : 0x81);

    if (im8) {
      *p++ = IM8(value);
    } else {
      switch (size) {
      case REG8:   *p++ = IM8(value); break;
      case REG16:  PUT_CODE(p, IM16(value)); p += 2; break;
      case REG32:
      case REG64:
        PUT_CODE(p, IM32(value));
        p += 4;
        break;
      default:  assert(false); break;
      }
    }
    return p;
  }
  return NULL;
}

static unsigned char *asm_add_ir(Inst *inst, Code *code) {
  if (inst->src.indirect.offset->kind == EX_FIXNUM && inst->src.indirect.reg.no != RIP) {
    long offset = inst->src.indirect.offset->fixnum;
    enum RegSize size = inst->dst.reg.size;
    unsigned char *p = code->buf;
    p = put_rex_indirect(
        p, size,
        opr_regno(&inst->dst.reg),
        opr_regno(&inst->src.indirect.reg),
        0x02, 0x00, offset);
    return p;
  }
  return NULL;
}

static unsigned char *asm_add_iir(Inst *inst, Code *code) {
  assert(inst->src.indirect_with_index.offset->kind == EX_FIXNUM && inst->src.indirect_with_index.offset->fixnum == 0);  // TODO
  unsigned char scale = 0;
  if (inst->src.indirect_with_index.scale != NULL) {
    assert(inst->src.indirect_with_index.scale->kind == EX_FIXNUM);
    int s = inst->src.indirect_with_index.scale->fixnum;
    assert(s == 1 || s == 2 || s == 4 || s == 8);
    scale = kPow2Table[s];
  }

  unsigned char bno = inst->src.indirect_with_index.base_reg.no;
  unsigned char ino = inst->src.indirect_with_index.index_reg.no;
  unsigned char dno = inst->dst.reg.no;
  short buf[] = {
    (unsigned char)0x48 | (inst->src.indirect_with_index.base_reg.x) | (inst->src.indirect_with_index.index_reg.x << 1) | (inst->dst.reg.x << 2),
    0x03,
    0x04 | (dno << 3),
    bno | (ino << 3) | (scale << 6),
  };
  unsigned char *p = code->buf;
  p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
  return p;
}

static unsigned char *asm_addq_imi(Inst *inst, Code *code) {
  if (inst->dst.indirect.offset->kind == EX_FIXNUM) {
    long value = inst->src.immediate;
    if (is_im32(value)) {
      long offset = inst->dst.indirect.offset->fixnum;
      unsigned char sno = 0;
      unsigned char dno = opr_regno(&inst->dst.indirect.reg);
      unsigned char op = (offset == 0 && (dno & 7) != RBP - RAX) ? 0x00 : is_im8(offset) ? (unsigned char)0x40 : (unsigned char)0x80;
      short buf[] = {
        (unsigned char)0x48 | ((dno & 8) >> 3) | ((sno & 8) >> 1),
        (is_im8(value) ? 0x83 : 0x81),
        op | (dno & 7) | ((sno & 7) << 3),
        (dno & 7) == RSP - RAX ? 0x24 : -1,
      };
      unsigned char *p = code->buf;
      p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
      if (offset == 0 && (dno & 7) != RBP - RAX) {
        ;
      } else if (is_im8(offset)) {
        *p++ = IM8(offset);
      } else if (is_im32(offset)) {
        PUT_CODE(p, IM32(offset));
        p += 4;
      }

      if (is_im8(value)) {
        *p++ = IM8(value);
      } else {
        PUT_CODE(p, IM32(value));
        p += 4;
      }
      return p;
    }
  }
  return NULL;
}

static unsigned char *asm_sub_rr(Inst *inst, Code *code) {
  enum RegSize size = inst->src.reg.size;
  unsigned char *p = code->buf;
  p = put_rex2(p, size, opr_regno(&inst->src.reg), opr_regno(&inst->dst.reg),
               size == REG8 ? 0x28 : 0x29);
  return p;
}

static unsigned char *asm_sub_imr(Inst *inst, Code *code) {
  long value = inst->src.immediate;
  if (is_im32(value)) {
    bool im8 = is_im8(value);
    enum RegSize size = inst->dst.reg.size;
    int d = opr_regno(&inst->dst.reg);
    unsigned char *p = code->buf;
    if (d == RAX - RAX && (size == REG8 || !is_im8(value)))
      p = put_rex0(p, size, 0, d, im8 ? 0x2c : 0x2d);
    else
      p = put_rex1(p, size, 0xe8, d, im8 ? 0x83 : 0x81);

    if (im8) {
      *p++ = IM8(value);
    } else {
      switch (size) {
      case REG8:   *p++ = IM8(value); break;
      case REG16:  PUT_CODE(p, IM16(value)); p += 2; break;
      case REG32:
      case REG64:
        PUT_CODE(p, IM32(value));
        p += 4;
        break;
      default:  assert(false); break;
      }
    }
    return p;
  }
  return NULL;
}

static unsigned char *asm_sub_ir(Inst *inst, Code *code) {
  if (inst->src.indirect.offset->kind == EX_FIXNUM && inst->src.indirect.reg.no != RIP) {
    long offset = inst->src.indirect.offset->fixnum;
    enum RegSize size = inst->dst.reg.size;
    unsigned char *p = code->buf;
    p = put_rex_indirect(
        p, size,
        opr_regno(&inst->dst.reg),
        opr_regno(&inst->src.indirect.reg),
        0x2a, 0x00, offset);
    return p;
  }
  return NULL;
}

static unsigned char *asm_sub_iir(Inst *inst, Code *code) {
  assert(inst->src.indirect_with_index.offset->kind == EX_FIXNUM && inst->src.indirect_with_index.offset->fixnum == 0);  // TODO
  unsigned char scale = 0;
  if (inst->src.indirect_with_index.scale != NULL) {
    assert(inst->src.indirect_with_index.scale->kind == EX_FIXNUM);
    int s = inst->src.indirect_with_index.scale->fixnum;
    assert(s == 1 || s == 2 || s == 4 || s == 8);
    scale = kPow2Table[s];
  }

  unsigned char bno = inst->src.indirect_with_index.base_reg.no;
  unsigned char ino = inst->src.indirect_with_index.index_reg.no;
  unsigned char dno = inst->dst.reg.no;
  short buf[] = {
    (unsigned char)0x48 | (inst->src.indirect_with_index.base_reg.x) | (inst->src.indirect_with_index.index_reg.x << 1) | (inst->dst.reg.x << 2),
    0x2b,
    0x04 | (dno << 3),
    bno | (ino << 3) | (scale << 6),
  };
  unsigned char *p = code->buf;
  p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
  return p;
}

static unsigned char *asm_subq_imi(Inst *inst, Code *code) {
  if (inst->dst.indirect.offset->kind == EX_FIXNUM) {
    long value = inst->src.immediate;
    if (is_im32(value)) {
      long offset = inst->dst.indirect.offset->fixnum;
      unsigned char sno = 0;
      unsigned char dno = opr_regno(&inst->dst.indirect.reg);
      unsigned char op = (offset == 0 && (dno & 7) != RBP - RAX) ? 0x28 : is_im8(offset) ? (unsigned char)0x40 : (unsigned char)0x80;
      short buf[] = {
        (unsigned char)0x48 | ((dno & 8) >> 3) | ((sno & 8) >> 1),
        (is_im8(value) ? 0x83 : 0x81),
        op | (dno & 7) | ((sno & 7) << 3),
        (dno & 7) == RSP - RAX ? 0x24 : -1,
      };
      unsigned char *p = code->buf;
      p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
      if (offset == 0 && (dno & 7) != RBP - RAX) {
        ;
      } else if (is_im8(offset)) {
        *p++ = IM8(offset);
      } else if (is_im32(offset)) {
        PUT_CODE(p, IM32(offset));
        p += 4;
      }

      if (is_im8(value)) {
        *p++ = IM8(value);
      } else {
        PUT_CODE(p, IM32(value));
        p += 4;
      }
      return p;
    }
  }
  return NULL;
}

static unsigned char *asm_mul_r(Inst *inst, Code *code) {
  enum RegSize size = inst->src.reg.size;
  unsigned char *p = code->buf;
  p = put_rex0(p, size, 0, opr_regno(&inst->src.reg),
               0xf6 | (size == REG8 ? 0 : 1));
  *p++ = 0xe0 | inst->src.reg.no;
  return p;
}

static unsigned char *asm_div_r(Inst *inst, Code *code) {
  enum RegSize size = inst->src.reg.size;
  unsigned char *p = code->buf;
  p = put_rex0(p, size, 0, opr_regno(&inst->src.reg),
               0xf6 | (size == REG8 ? 0 : 1));
  *p++ = 0xf0 | inst->src.reg.no;
  return p;
}

static unsigned char *asm_idiv_r(Inst *inst, Code *code) {
  enum RegSize size = inst->src.reg.size;
  unsigned char *p = code->buf;
  p = put_rex0(p, size, 0, opr_regno(&inst->src.reg),
               0xf6 | (size == REG8 ? 0 : 1));
  *p++ = 0xf8 | inst->src.reg.no;
  return p;
}

static unsigned char *asm_neg_r(Inst *inst, Code *code) {
  unsigned char *p = code->buf;
  p = put_rex1(p, inst->src.reg.size,
               0xd8, opr_regno(&inst->src.reg), 0xf7);
  return p;
}

static unsigned char *asm_not_r(Inst *inst, Code *code) {
  unsigned char *p = code->buf;
  p = put_rex1(p, inst->src.reg.size,
               0xd0, opr_regno(&inst->src.reg), 0xf7);
  return p;
}

static unsigned char *asm_inc_r(Inst *inst, Code *code) {
  unsigned char *p = code->buf;
  p = put_rex1(p, inst->src.reg.size,
               0xc0, opr_regno(&inst->src.reg), 0xff);
  return p;
}

static unsigned char *asm_incbwlq_i(Inst *inst, Code *code) {
  if (inst->src.indirect.reg.no != RIP) {
    enum RegSize size = inst->op + (REG8 - INCB);
    long offset = inst->src.indirect.offset->fixnum;
    unsigned char *p = code->buf;
    p = put_rex_indirect(
        p, size,
        0, opr_regno(&inst->src.indirect.reg),
        0xfe, 0x00, offset);
    return p;
  }
  return NULL;
}

static unsigned char *asm_dec_r(Inst *inst, Code *code) {
  unsigned char *p = code->buf;
  p = put_rex1(p, inst->src.reg.size,
               0xc8, opr_regno(&inst->src.reg), 0xff);
  return p;
}

static unsigned char *asm_decbwlq_i(Inst *inst, Code *code) {
  if (inst->src.indirect.reg.no != RIP) {
    enum RegSize size = inst->op + (REG8 - DECB);
    long offset = inst->src.indirect.offset->fixnum;
    unsigned char *p = code->buf;
    p = put_rex_indirect(
        p, size,
        1, opr_regno(&inst->src.indirect.reg),
        0xfe, 0x08, offset);
    return p;
  }
  return NULL;
}

static unsigned char *asm_and_rr(Inst *inst, Code *code) {
  enum RegSize size = inst->dst.reg.size;
  unsigned char *p = code->buf;
  p = put_rex2(p, size, opr_regno(&inst->src.reg), opr_regno(&inst->dst.reg),
               size == REG8 ? 0x20 : 0x21);
  return p;
}

static unsigned char *asm_and_imr(Inst *inst, Code *code) {
  long value = inst->src.immediate;
  enum RegSize size = inst->dst.reg.size;
  unsigned char *p = code->buf;
  if (is_im8(value) && (size != REG8 || opr_regno(&inst->dst.reg) != AL - AL)) {
    p = put_rex1(p, size,
                 0xe0, opr_regno(&inst->dst.reg),
                 0x83);
    *p++ = IM8(value);
    return p;
  } else if (size <= REG32 || is_im32(value)) {
    if (opr_regno(&inst->dst.reg) == RAX - RAX) {
      p = put_rex0(p, size,
                   0, opr_regno(&inst->dst.reg),
                   size == REG8 ? 0x24 : 0x25);
    } else {
      p = put_rex1(p, size,
                   0xe0, opr_regno(&inst->dst.reg),
                   size == REG8 ? 0x80 : 0x81);
    }

    switch (size) {
    case REG8:  PUT_CODE(p, IM8(value)); p += 1; break;
    case REG16: PUT_CODE(p, IM16(value)); p += 2; break;
    case REG32: case REG64:
      PUT_CODE(p, IM32(value));
      p += 4;
      break;
    default: assert(false); break;
    }
    return p;
  }
  return NULL;
}

static unsigned char *asm_or_rr(Inst *inst, Code *code) {
  enum RegSize size = inst->dst.reg.size;
  unsigned char *p = code->buf;
  p = put_rex2(p, size, opr_regno(&inst->src.reg), opr_regno(&inst->dst.reg),
               size == REG8 ? 0x08 : 0x09);
  return p;
}

static unsigned char *asm_or_imr(Inst *inst, Code *code) {
  long value = inst->src.immediate;
  enum RegSize size = inst->dst.reg.size;
  unsigned char *p = code->buf;
  if (is_im8(value) && (size != REG8 || opr_regno(&inst->dst.reg) != AL - AL)) {
    p = put_rex1(p, size,
                 0xc8, opr_regno(&inst->dst.reg),
                 0x83);
    *p++ = IM8(value);
    return p;
  } else if (size <= REG32 || is_im32(value)) {
    if (opr_regno(&inst->dst.reg) == RAX - RAX) {
      p = put_rex0(p, size,
                   0, opr_regno(&inst->dst.reg),
                   size == REG8 ? 0x0c : 0x0d);
    } else {
      p = put_rex1(p, size,
                   0xc8, opr_regno(&inst->dst.reg),
                   size == REG8 ? 0x80 : 0x81);
    }
    switch (size) {
    case REG8:  PUT_CODE(p, IM8(value)); p += 1; break;
    case REG16: PUT_CODE(p, IM16(value)); p += 2; break;
    case REG32: case REG64:
      PUT_CODE(p, IM32(value));
      p += 4;
      break;
    default: assert(false); break;
    }
    return p;
  }
  return NULL;
}

static unsigned char *asm_xor_rr(Inst *inst, Code *code) {
  enum RegSize size = inst->dst.reg.size;
  unsigned char *p = code->buf;
  p = put_rex2(p, size, opr_regno(&inst->src.reg), opr_regno(&inst->dst.reg),
               size == REG8 ? 0x30 : 0x31);
  return p;
}

static unsigned char *asm_xor_imr(Inst *inst, Code *code) {
  long value = inst->src.immediate;
  enum RegSize size = inst->dst.reg.size;
  unsigned char *p = code->buf;
  if (is_im8(value) && (size != REG8 || opr_regno(&inst->dst.reg) != AL - AL)) {
    p = put_rex1(p, size,
                 0xf0, opr_regno(&inst->dst.reg),
                 0x83);
    *p++ = IM8(value);
    return p;
  } else if (size <= REG32 || is_im32(value)) {
    if (opr_regno(&inst->dst.reg) == RAX - RAX) {
      p = put_rex0(p, size,
                   0, opr_regno(&inst->dst.reg),
                   size == REG8 ? 0x34 : 0x35);
    } else {
      p = put_rex1(p, size,
                   0xf0, opr_regno(&inst->dst.reg),
                   size == REG8 ? 0x80 : 0x81);
    }
    switch (size) {
    case REG8:  PUT_CODE(p, IM8(value)); p += 1; break;
    case REG16: PUT_CODE(p, IM16(value)); p += 2; break;
    case REG32: case REG64:
      PUT_CODE(p, IM32(value));
      p += 4;
      break;
    default: assert(false); break;
    }
    return p;
  }
  return NULL;
}

static unsigned char *asm_shl_rr(Inst *inst, Code *code) {
  enum RegSize size = inst->dst.reg.size;
  unsigned char *p = code->buf;
  p = put_rex1(p, size, 0xe0, opr_regno(&inst->dst.reg),
               size == REG8 ? 0xd2 : 0xd3);
  return p;
}

static unsigned char *asm_shl_imr(Inst *inst, Code *code) {
  enum RegSize size = inst->dst.reg.size;
  unsigned char *p = code->buf;
  if (inst->src.immediate == 1) {
    p = put_rex1(p, size, 0xe0, opr_regno(&inst->dst.reg),
                 size == REG8 ? 0xd0 : 0xd1);
  } else {
    p = put_rex1(p, size, 0xe0, opr_regno(&inst->dst.reg),
                 size == REG8 ? 0xc0 : 0xc1);
    *p++ = IM8(inst->src.immediate);
  }
  return p;
}

static unsigned char *asm_shr_rr(Inst *inst, Code *code) {
  enum RegSize size = inst->dst.reg.size;
  unsigned char *p = code->buf;
  p = put_rex1(p, size, 0xe8, opr_regno(&inst->dst.reg),
               size == REG8 ? 0xd2 : 0xd3);
  return p;
}

static unsigned char *asm_shr_imr(Inst *inst, Code *code) {
  enum RegSize size = inst->dst.reg.size;
  unsigned char *p = code->buf;
  if (inst->src.immediate == 1) {
    p = put_rex1(p, size, 0xe8, opr_regno(&inst->dst.reg),
                 size == REG8 ? 0xd0 : 0xd1);
  } else {
    p = put_rex1(p, size, 0xe8, opr_regno(&inst->dst.reg),
                 size == REG8 ? 0xc0 : 0xc1);
    *p++ = IM8(inst->src.immediate);
  }
  return p;
}

static unsigned char *asm_sar_rr(Inst *inst, Code *code) {
  enum RegSize size = inst->dst.reg.size;
  unsigned char *p = code->buf;
  p = put_rex1(p, size, 0xf8, opr_regno(&inst->dst.reg),
               size == REG8 ? 0xd2 : 0xd3);
  return p;
}

static unsigned char *asm_sar_imr(Inst *inst, Code *code) {
  enum RegSize size = inst->dst.reg.size;
  unsigned char *p = code->buf;
  if (inst->src.immediate == 1) {
    p = put_rex1(p, size, 0xf8, opr_regno(&inst->dst.reg),
                 size == REG8 ? 0xd0 : 0xd1);
  } else {
    p = put_rex1(p, size, 0xf8, opr_regno(&inst->dst.reg),
                 size == REG8 ? 0xc0 : 0xc1);
    *p++ = IM8(inst->src.immediate);
  }
  return p;
}

static unsigned char *asm_cmp_rr(Inst *inst, Code *code) {
  enum RegSize size = inst->src.reg.size;
  unsigned char *p = code->buf;
  p = put_rex2(p, size, opr_regno(&inst->src.reg), opr_regno(&inst->dst.reg),
               size == REG8 ? 0x38 : 0x39);
  return p;
}

static unsigned char *asm_cmp_imr(Inst *inst, Code *code) {
  enum RegSize size = inst->dst.reg.size;
  long value = signed_immediate(inst->src.immediate, size);
  if (is_im32(value) || size <= REG32) {
    bool im8 = is_im8(value);
    int d = opr_regno(&inst->dst.reg);
    unsigned char *p = code->buf;
    if (d == RAX - RAX && (size == REG8 || !im8))
      p = put_rex0(p, size, 0, d, size == REG8 ? 0x3c : 0x3d);
    else
      p = put_rex1(p, size, 0xf8, d, 0x80 | (size == REG8 ? 0 : im8 ? 3 : 1));

    if (im8) {
      *p++ = IM8(value);
    } else {
      switch (size) {
      case REG8:   *p++ = IM8(value); break;
      case REG16:  PUT_CODE(p, IM16(value)); p += 2; break;
      case REG32:
      case REG64:
        PUT_CODE(p, IM32(value));
        p += 4;
        break;
      }
    }
    return p;
  }
  return NULL;
}

static unsigned char *asm_test_rr(Inst *inst, Code *code) {
  enum RegSize size = inst->src.reg.size;
  unsigned char *p = code->buf;
  p = put_rex2(p, size, opr_regno(&inst->src.reg), opr_regno(&inst->dst.reg),
               size == REG8 ? 0x84 : 0x85);
  return p;
}

static unsigned char *asm_cwtl(Inst *inst, Code *code) {
  MAKE_CODE(inst, code, 0x98);
  return code->buf;
}

static unsigned char *asm_cltd(Inst *inst, Code *code) {
  MAKE_CODE(inst, code, 0x99);
  return code->buf;
}

static unsigned char *asm_cqto(Inst *inst, Code *code) {
  MAKE_CODE(inst, code, 0x48, 0x99);
  return code->buf;
}

static unsigned char *asm_set_r(Inst *inst, Code *code) {
  unsigned char *p = code->buf;
  p = put_rex0(p, REG8, 0, opr_regno(&inst->src.reg), 0x0f);
  *p++ = 0x90 | (inst->op - SETO);
  *p++ = 0xc0 | inst->src.reg.no;
  return p;
}

static unsigned char *asm_push_r(Inst *inst, Code *code) {
  unsigned char *p = code->buf;
  p = put_rex0(p, REG32, 0, opr_regno(&inst->src.reg),
               0x50 | inst->src.reg.no);
  return p;
}

static unsigned char *asm_push_im(Inst *inst, Code *code) {
  unsigned char *p = code->buf;
  long value = inst->src.immediate;
  if (is_im8(value)) {
    *p++ = 0x6a;
    *p++ = IM8(value);
    return p;
  } else if (is_im32(value)) {
    *p++ = 0x68;
    PUT_CODE(p, IM32(value));
    p += 4;
    return p;
  }
  return NULL;
}

static unsigned char *asm_pop_r(Inst *inst, Code *code) {
  unsigned char *p = code->buf;
  p = put_rex0(p, REG32, 0, opr_regno(&inst->src.reg),
               0x58 | inst->src.reg.no);
  return p;
}

static unsigned char *asm_jmp_d(Inst *inst, Code *code) {
  MAKE_CODE(inst, code, 0xeb, IM8(0));  // Short jmp in default.
  return code->buf;
}

static unsigned char *asm_jmp_der(Inst *inst, Code *code) {
  int s = inst->src.reg.no;
  short buf[] = {
    inst->src.reg.x ? 0x41 : -1,
    0xff, 0xe0 | s,
  };
  unsigned char *p = code->buf;
  p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
  return p;
}

static unsigned char *asm_jmp_dei(Inst *inst, Code *code) {
  Expr *offset_expr = inst->src.indirect.offset;
  if ((offset_expr == NULL || offset_expr->kind == EX_FIXNUM)) {
    long offset = offset_expr != NULL ? offset_expr->fixnum : 0;
    if (is_im32(offset)) {
      short offset_bit = offset == 0 ? 0x20 : is_im8(offset) ? 0x60 : 0xa0;
      short b = inst->src.indirect.reg.no;
      short buf[] = {
        inst->src.indirect.reg.x ? 0x41 : -1,
        0xff, offset_bit | b,
      };
      unsigned char *p = code->buf;
      p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
      if (offset != 0) {
        if (is_im8(offset)) {
          *p++ = IM8(offset);
        } else {
          PUT_CODE(p, IM32(offset));
          p += 4;
        }
      }
      return p;
    }
  }
  return NULL;
}

static unsigned char *asm_jmp_deii(Inst *inst, Code *code) {
  Expr *offset_expr = inst->src.indirect_with_index.offset;
  Expr *scale_expr = inst->src.indirect_with_index.scale;
  if ((offset_expr == NULL || offset_expr->kind == EX_FIXNUM) &&
      (scale_expr == NULL || scale_expr->kind == EX_FIXNUM)) {
    long offset = offset_expr != NULL ? offset_expr->fixnum : 0;
    long scale = scale_expr != NULL ? scale_expr->fixnum : 1;
    if (is_im32(offset) && 1 <= scale && scale <= 8 && IS_POWER_OF_2(scale)) {
      short offset_bit = offset == 0 ? 0x20 : is_im8(offset) ? 0x60 : 0xa0;
      short b = inst->src.indirect_with_index.base_reg.no;
      short scale_bit = kPow2Table[scale];
      short i = inst->src.indirect_with_index.index_reg.no;
      short prefix = inst->src.indirect_with_index.base_reg.x | (inst->src.indirect_with_index.index_reg.x << 1);
      short buf[] = {
        prefix != 0 ? 0x40 | prefix : -1,
        0xff,
        offset_bit | 0x04,
        (scale_bit << 6) | (i << 3) | b,
      };
      unsigned char *p = code->buf;
      p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
      if (offset != 0) {
        if (is_im8(offset)) {
          *p++ = IM8(offset);
        } else {
          PUT_CODE(p, IM32(offset));
          p += 4;
        }
      }
      return p;
    }
  }
  return NULL;
}

static unsigned char *asm_jxx_d(Inst *inst, Code *code) {
  MAKE_CODE(inst, code, 0x70 + (inst->op - JO), IM8(0));  // Short jmp in default.
  return code->buf;
}

static unsigned char *asm_call_d(Inst *inst, Code *code) {
  MAKE_CODE(inst, code, 0xe8, IM32(0));
  return code->buf;
}

static unsigned char *asm_call_der(Inst *inst, Code *code) {
  int s = inst->src.reg.no;
  if (!inst->src.reg.x) {
    MAKE_CODE(inst, code, 0xff, 0xd0 + s);
  } else {
    MAKE_CODE(inst, code, 0x41, 0xff, 0xd0 + s);
  }
  return code->buf;
}

static unsigned char *asm_ret(Inst *inst, Code *code) {
  MAKE_CODE(inst, code, 0xc3);
  return code->buf;
}

static unsigned char *asm_int_im(Inst *inst, Code *code) {
  long value = inst->src.immediate;
  MAKE_CODE(inst, code, 0xcd, IM8(value));
  return code->buf;
}

static unsigned char *asm_syscall(Inst *inst, Code *code) {
  MAKE_CODE(inst, code, 0x0f, 0x05);
  return code->buf;
}

////////////////////////////////////////////////

enum {
  NO_SAME_REG_SIZE = 1 << 0,
  SRC_CL_ONLY = 1 << 1,
  SRC_REG8_ONLY = 1 << 2,
  SRC_REG64_ONLY = 1 << 3,
  DST_REG64_ONLY = 1 << 4,
};

typedef unsigned char * (*AsmInstFunc)(Inst *inst, Code *code);
typedef struct {
  AsmInstFunc func;
  enum OperandType src_type;
  enum OperandType dst_type;
  int flag;
} AsmInstTable;

static const AsmInstTable table_noop[] ={
    {asm_noop, NOOPERAND, NOOPERAND},
    {NULL} };

static const AsmInstTable table_mov[] ={
    {asm_mov_rr, REG, REG},
    {asm_mov_imr, IMMEDIATE, REG},
    {asm_mov_ir, INDIRECT, REG},
    {asm_mov_ri, REG, INDIRECT},
    {asm_mov_iir, INDIRECT_WITH_INDEX, REG},
    {asm_mov_dr, DIRECT, REG},
    {asm_mov_rd, REG, DIRECT},
    {NULL} };

static const AsmInstTable table_movbwlq[] ={
    {asm_movbwlq_imi, IMMEDIATE, INDIRECT},
    {NULL} };

static const AsmInstTable table_movszx[] ={
    {asm_movszx_rr, REG, REG, NO_SAME_REG_SIZE},
    {NULL} };

static const AsmInstTable table_lea[] ={
    {asm_lea_ir, INDIRECT, REG, DST_REG64_ONLY},
    {asm_lea_iir, INDIRECT_WITH_INDEX, REG, DST_REG64_ONLY},
    {NULL} };

static const AsmInstTable table_add[] ={
    {asm_add_rr, REG, REG},
    {asm_add_imr, IMMEDIATE, REG},
    {asm_add_ir, INDIRECT, REG},
    {asm_add_iir, INDIRECT_WITH_INDEX, REG},
    {NULL} };

static const AsmInstTable table_addq[] ={
    {asm_addq_imi, IMMEDIATE, INDIRECT},
    {NULL} };

static const AsmInstTable table_sub[] ={
    {asm_sub_rr, REG, REG},
    {asm_sub_imr, IMMEDIATE, REG},
    {asm_sub_ir, INDIRECT, REG},
    {asm_sub_iir, INDIRECT_WITH_INDEX, REG},
    {NULL} };

static const AsmInstTable table_subq[] ={
    {asm_subq_imi, IMMEDIATE, INDIRECT},
    {NULL} };

static const AsmInstTable table_mul[] ={
    {asm_mul_r, REG, NOOPERAND},
    {NULL} };

static const AsmInstTable table_div[] ={
    {asm_div_r, REG, NOOPERAND},
    {NULL} };

static const AsmInstTable table_idiv[] ={
    {asm_idiv_r, REG, NOOPERAND},
    {NULL} };

static const AsmInstTable table_neg[] ={
    {asm_neg_r, REG, NOOPERAND},
    {NULL} };

static const AsmInstTable table_not[] ={
    {asm_not_r, REG, NOOPERAND},
    {NULL} };

static const AsmInstTable table_inc[] ={
    {asm_inc_r, REG, NOOPERAND},
    {NULL} };

static const AsmInstTable table_incbwlq[] ={
    {asm_incbwlq_i, INDIRECT, NOOPERAND},
    {NULL} };

static const AsmInstTable table_dec[] ={
    {asm_dec_r, REG, NOOPERAND},
    {NULL} };

static const AsmInstTable table_decbwlq[] ={
    {asm_decbwlq_i, INDIRECT, NOOPERAND},
    {NULL} };

static const AsmInstTable table_and[] ={
    {asm_and_rr, REG, REG},
    {asm_and_imr, IMMEDIATE, REG},
    {NULL} };

static const AsmInstTable table_or[] ={
    {asm_or_rr, REG, REG},
    {asm_or_imr, IMMEDIATE, REG},
    {NULL} };

static const AsmInstTable table_xor[] ={
    {asm_xor_rr, REG, REG},
    {asm_xor_imr, IMMEDIATE, REG},
    {NULL} };

static const AsmInstTable table_shl[] ={
    {asm_shl_rr, REG, REG, NO_SAME_REG_SIZE | SRC_CL_ONLY},
    {asm_shl_imr, IMMEDIATE, REG},
    {NULL} };

static const AsmInstTable table_shr[] ={
    {asm_shr_rr, REG, REG, NO_SAME_REG_SIZE | SRC_CL_ONLY},
    {asm_shr_imr, IMMEDIATE, REG},
    {NULL} };

static const AsmInstTable table_sar[] ={
    {asm_sar_rr, REG, REG, NO_SAME_REG_SIZE | SRC_CL_ONLY},
    {asm_sar_imr, IMMEDIATE, REG},
    {NULL} };

static const AsmInstTable table_cmp[] ={
    {asm_cmp_rr, REG, REG},
    {asm_cmp_imr, IMMEDIATE, REG},
    {NULL} };

static const AsmInstTable table_test[] ={
    {asm_test_rr, REG, REG},
    {NULL} };

static const AsmInstTable table_cwtl[] ={
    {asm_cwtl, NOOPERAND, NOOPERAND},
    {NULL} };

static const AsmInstTable table_cltd[] ={
    {asm_cltd, NOOPERAND, NOOPERAND},
    {NULL} };

static const AsmInstTable table_cqto[] ={
    {asm_cqto, NOOPERAND, NOOPERAND},
    {NULL} };

static const AsmInstTable table_set[] ={
    {asm_set_r, REG, NOOPERAND, SRC_REG8_ONLY},
    {NULL} };

static const AsmInstTable table_push[] ={
    {asm_push_r, REG, NOOPERAND, SRC_REG64_ONLY},
    {asm_push_im, IMMEDIATE, NOOPERAND},
    {NULL} };

static const AsmInstTable table_pop[] ={
    {asm_pop_r, REG, NOOPERAND, SRC_REG64_ONLY},
    {NULL} };

static const AsmInstTable table_jmp[] ={
    {asm_jmp_d, DIRECT, NOOPERAND},
    {asm_jmp_der, DEREF_REG, NOOPERAND},
    {asm_jmp_dei, DEREF_INDIRECT, NOOPERAND},
    {asm_jmp_deii, DEREF_INDIRECT_WITH_INDEX, NOOPERAND},
    {NULL} };

static const AsmInstTable table_jxx[] ={
    {asm_jxx_d, DIRECT, NOOPERAND},
    {NULL} };

static const AsmInstTable table_call[] ={
    {asm_call_d, DIRECT, NOOPERAND},
    {asm_call_der, DEREF_REG, NOOPERAND},
    {NULL} };

static const AsmInstTable table_ret[] ={
    {asm_ret, NOOPERAND, NOOPERAND},
    {NULL} };

static const AsmInstTable table_int[] ={
    {asm_int_im, IMMEDIATE, NOOPERAND},
    {NULL} };

static const AsmInstTable table_syscall[] ={
    {asm_syscall, NOOPERAND, NOOPERAND},
    {NULL} };

#ifndef __NO_FLONUM
static const AsmInstTable table_movsd[] ={
    {asm_movsd_xx, REG_XMM, REG_XMM},
    {asm_movsd_ix, INDIRECT, REG_XMM},
    {asm_movsd_xi, REG_XMM, INDIRECT},
    {NULL} };
static const AsmInstTable table_movss[] ={
    {asm_movss_xx, REG_XMM, REG_XMM},
    {asm_movss_ix, INDIRECT, REG_XMM},
    {asm_movss_xi, REG_XMM, INDIRECT},
    {NULL} };

static const AsmInstTable table_addsd[] ={
    {asm_addsd_xx, REG_XMM, REG_XMM},
    {NULL} };
static const AsmInstTable table_addss[] ={
    {asm_addss_xx, REG_XMM, REG_XMM},
    {NULL} };

static const AsmInstTable table_subsd[] ={
    {asm_subsd_xx, REG_XMM, REG_XMM},
    {NULL} };
static const AsmInstTable table_subss[] ={
    {asm_subss_xx, REG_XMM, REG_XMM},
    {NULL} };

static const AsmInstTable table_mulsd[] ={
    {asm_mulsd_xx, REG_XMM, REG_XMM},
    {NULL} };
static const AsmInstTable table_mulss[] ={
    {asm_mulss_xx, REG_XMM, REG_XMM},
    {NULL} };

static const AsmInstTable table_divsd[] ={
    {asm_divsd_xx, REG_XMM, REG_XMM},
    {NULL} };
static const AsmInstTable table_divss[] ={
    {asm_divss_xx, REG_XMM, REG_XMM},
    {NULL} };

static const AsmInstTable table_ucomisd[] ={
    {asm_ucomisd_xx, REG_XMM, REG_XMM},
    {NULL} };
static const AsmInstTable table_ucomiss[] ={
    {asm_ucomiss_xx, REG_XMM, REG_XMM},
    {NULL} };

static const AsmInstTable table_cvtsi2sd[] ={
    {asm_cvtsi2sd_rx, REG, REG_XMM},
    {NULL} };
static const AsmInstTable table_cvtsi2ss[] ={
    {asm_cvtsi2ss_rx, REG, REG_XMM},
    {NULL} };

static const AsmInstTable table_cvttsd2si[] ={
    {asm_cvttsd2si_xr, REG_XMM, REG},
    {NULL} };
static const AsmInstTable table_cvttss2si[] ={
    {asm_cvttss2si_xr, REG_XMM, REG},
    {NULL} };

static const AsmInstTable table_cvtsd2ss[] ={
    {asm_cvtsd2ss_xx, REG_XMM, REG_XMM},
    {NULL} };
static const AsmInstTable table_cvtss2sd[] ={
    {asm_cvtss2sd_xx, REG_XMM, REG_XMM},
    {NULL} };

static const AsmInstTable table_sqrtsd[] ={
    {asm_sqrtsd_xx, REG_XMM, REG_XMM},
    {NULL} };
#endif

static const AsmInstTable *table[] = {
  [NOOP] = table_noop,
  [MOV] = table_mov,
  [MOVB] = table_movbwlq,  [MOVW] = table_movbwlq,  [MOVL] = table_movbwlq,  [MOVQ] = table_movbwlq,
  [MOVSX] = table_movszx,  [MOVZX] = table_movszx,
  [LEA] = table_lea,
  [ADD] = table_add,
  [ADDQ] = table_addq,
  [SUB] = table_sub,
  [SUBQ] = table_subq,
  [MUL] = table_mul,
  [DIV] = table_div,
  [IDIV] = table_idiv,
  [NEG] = table_neg,
  [NOT] = table_not,
  [INC] = table_inc,
  [INCB] = table_incbwlq,  [INCW] = table_incbwlq,  [INCL] = table_incbwlq,  [INCQ] = table_incbwlq,
  [DEC] = table_dec,
  [DECB] = table_decbwlq,  [DECW] = table_decbwlq,  [DECL] = table_decbwlq,  [DECQ] = table_decbwlq,
  [AND] = table_and,
  [OR] = table_or,
  [XOR] = table_xor,
  [SHL] = table_shl,
  [SHR] = table_shr,
  [SAR] = table_sar,
  [CMP] = table_cmp,
  [TEST] = table_test,
  [CWTL] = table_cwtl,  [CLTD] = table_cltd,  [CQTO] = table_cqto,
  [SETO] = table_set,  [SETNO] = table_set,  [SETB] = table_set,  [SETAE] = table_set,
  [SETE] = table_set,  [SETNE] = table_set,  [SETBE] = table_set,  [SETA] = table_set,
  [SETS] = table_set,  [SETNS] = table_set,  [SETP] = table_set,  [SETNP] = table_set,
  [SETL] = table_set,  [SETGE] = table_set,  [SETLE] = table_set,  [SETG] = table_set,
  [JMP] = table_jmp,
  [JO] = table_jxx,  [JNO] = table_jxx,  [JB] = table_jxx,  [JAE] = table_jxx,
  [JE] = table_jxx,  [JNE] = table_jxx,  [JBE] = table_jxx,  [JA] = table_jxx,
  [JS] = table_jxx,  [JNS] = table_jxx,  [JP] = table_jxx,  [JNP] = table_jxx,
  [JL] = table_jxx,  [JGE] = table_jxx,  [JLE] = table_jxx,  [JG] = table_jxx,
  [CALL] = table_call,
  [RET] = table_ret,
  [PUSH] = table_push,
  [POP] = table_pop,
  [INT] = table_int,
  [SYSCALL] = table_syscall,
#ifndef __NO_FLONUM
  [MOVSD] = table_movsd,  [MOVSS] = table_movss,
  [ADDSD] = table_addsd,  [ADDSS] = table_addss,
  [SUBSD] = table_subsd,  [SUBSS] = table_subss,
  [MULSD] = table_mulsd,  [MULSS] = table_mulss,
  [DIVSD] = table_divsd,  [DIVSS] = table_divss,
  [UCOMISD] = table_ucomisd,  [UCOMISS] = table_ucomiss,
  [CVTSI2SD] = table_cvtsi2sd,  [CVTSI2SS] = table_cvtsi2ss,
  [CVTTSD2SI] = table_cvttsd2si,  [CVTTSS2SI] = table_cvttss2si,
  [CVTSD2SS] = table_cvtsd2ss,  [CVTSS2SD] = table_cvtss2sd,
  [SQRTSD] = table_sqrtsd,
#endif
};

void assemble_inst(Inst *inst, const ParseInfo *info, Code *code) {
  code->flag = 0;
  code->len = 0;

  const AsmInstTable *pt = NULL;
  if (inst->op < (enum Opcode)(sizeof(table) / sizeof(*table)) && table[inst->op] != NULL) {
    for (const AsmInstTable *p = table[inst->op]; p->func != NULL; ++p) {
      if (inst->src.type == p->src_type && inst->dst.type == p->dst_type) {
        pt = p;
        break;
      }
    }
  }

  if (pt != NULL) {
    if (pt->src_type == REG && pt->dst_type == REG &&
        inst->src.reg.size != inst->dst.reg.size &&
        !(pt->flag & NO_SAME_REG_SIZE)) {
      assemble_error(info, "Different source and destination register size");
      return;
    }
    if ((pt->flag & SRC_CL_ONLY) && opr_regno(&inst->src.reg) != CL - AL) {
      assemble_error(info, "`%cl` expected");
      return;
    }
    if (((pt->flag & SRC_REG8_ONLY) && inst->src.reg.size != REG8) ||
        ((pt->flag & SRC_REG64_ONLY) && inst->src.reg.size != REG64) ||
        ((pt->flag & DST_REG64_ONLY) && inst->dst.reg.size != REG64)) {
      assemble_error(info, "Illegal opeand");
      return;
    }

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

#include "../../../config.h"
#include "asm_code.h"

#include <assert.h>
#include <stdlib.h>  // exit
#include <string.h>  // memcpy

#include "inst.h"
#include "parse_asm.h"
#include "util.h"

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
  assert(code->len + len <= (int)sizeof(code->buf));
  code->inst = inst;
  memcpy(code->buf + code->len, buf, len);
  code->len += len;
}

inline char opr_regno(const Reg *reg) {
  return reg->no | (reg->x << 3);
}

inline bool opr_reg8(const Reg *reg) {
  assert(reg->size == REG8);
  return opr_regno(reg) < 4;
}

inline bool assemble_error(const ParseInfo *info, const char *message) {
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

#define MAKE_REX0(size, sno, dno, opcode) \
  (size) == REG16 ? 0x66 : -1, \
  ((sno) >= 8 || (dno) >= 8 || \
      ((size) == REG8 && ((sno) >= 4 || (dno) >= 4)) || \
      (size) == REG64) \
    ? (0x40 | (((dno) & 8) >> 3) | (((sno) & 8) >> 1) | ((size) != REG64 ? 0 : 8)) : -1, \
  opcode

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

#define MAKE_REX_INDIRECT(size, reg, indirect_reg, opcode, op2, offset) \
  MAKE_REX0(size, reg, indirect_reg, \
            (size) == REG8 ? opcode : (unsigned char)((opcode) + 1)), \
  (((offset) == 0 && indirect_reg != RBP - RAX) ? op2 : is_im8(offset) ? 0x40 : 0x80) | (indirect_reg & 7) | ((reg & 7) << 3), \
  indirect_reg == RSP - RAX ? 0x24 : -1

#define MAKE_OFFSET(offset, no) \
  ((offset) == 0 && no) ? -1 : (offset) & 0xff, \
  ((offset) == 0 && no) || is_im8(offset) ? -1 : ((offset) >>  8) & 0xff, \
  ((offset) == 0 && no) || is_im8(offset) ? -1 : ((offset) >> 16) & 0xff, \
  ((offset) == 0 && no) || is_im8(offset) ? -1 : ((offset) >> 24) & 0xff

#define MAKE_IM(size, value) \
  (value) & 0xff, \
  size < REG16 ? -1 : ((value) >>  8) & 0xff, \
  size < REG32 ? -1 : ((value) >>  16) & 0xff, \
  size < REG32 ? -1 : ((value) >>  24) & 0xff, \
  size < REG64 ? -1 : ((value) >>  32) & 0xff, \
  size < REG64 ? -1 : ((value) >>  40) & 0xff, \
  size < REG64 ? -1 : ((value) >>  48) & 0xff, \
  size < REG64 ? -1 : ((value) >>  56) & 0xff

static unsigned char *asm_noop(Inst *inst, Code *code) {
  UNUSED(inst);
  unsigned char *p = code->buf;
  return p;
}

static unsigned char *asm_mov_rr(Inst *inst, Code *code) {
  unsigned char *p = code->buf;
  enum RegSize size = inst->opr[0].reg.size;
  p = put_rex2(p, size, opr_regno(&inst->opr[0].reg), opr_regno(&inst->opr[1].reg),
               size == REG8 ? 0x88 : 0x89);
  return p;
}

static unsigned char *asm_mov_imr(Inst *inst, Code *code) {
  unsigned char *p = code->buf;
  enum RegSize size = inst->opr[1].reg.size;
  if (size == REG64 && is_im32(inst->opr[0].immediate)) {
    int d = inst->opr[1].reg.no;
    int pre = !inst->opr[1].reg.x ? 0x48 : 0x49;
    MAKE_CODE(inst, code, pre, 0xc7, 0xc0 | d, IM32(inst->opr[0].immediate));
    return code->buf;
  }

  short buf[] = {
    MAKE_REX0(
      size, 0, opr_regno(&inst->opr[1].reg),
      0xb0 | (size == REG8 ? 0 : 8) | inst->opr[1].reg.no),
      MAKE_IM(size, inst->opr[0].immediate),
  };
  p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
  return p;
}

static unsigned char *asm_mov_ir(Inst *inst, Code *code) {
  if (inst->opr[0].indirect.reg.no != RIP) {
    if (inst->opr[0].indirect.offset.expr->kind == EX_FIXNUM) {
      long offset = inst->opr[0].indirect.offset.expr->fixnum;
      enum RegSize size = inst->opr[1].reg.size;
      int sno = opr_regno(&inst->opr[0].indirect.reg);
      int dno = opr_regno(&inst->opr[1].reg);
      bool ofszero = offset == 0 && ((sno & 7) != RBP - RAX);
      short buf[] = {
        size == REG16 ? 0x66 : -1,
        (size == REG64 || sno >= 8 || dno >= 8) ?  0x40 | (size == REG64 ? 0x08 : 0) | ((dno & 8) >> 1) | ((sno & 8) >> 3) : -1,
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
  } else {
    if (inst->opr[0].indirect.offset.expr->kind != EX_FIXNUM) {
      int pre = !inst->opr[1].reg.x ? 0x48 : 0x4c;
      int d = inst->opr[1].reg.no;
      MAKE_CODE(inst, code, pre, 0x8b, 0x05 | (d << 3), IM32(0));
      return code->buf;
    }
  }
  return NULL;
}

static unsigned char *asm_mov_ri(Inst *inst, Code *code) {
  if (inst->opr[1].indirect.offset.expr->kind == EX_FIXNUM) {
    long offset = inst->opr[1].indirect.offset.expr->fixnum;
    enum RegSize size = inst->opr[0].reg.size;
    if (inst->opr[1].indirect.reg.no != RIP) {
      int sno = opr_regno(&inst->opr[0].reg);
      int dno = opr_regno(&inst->opr[1].indirect.reg);
      bool ofszero = offset == 0 && ((dno & 7) != RBP - RAX);
      short buf[] = {
        size == REG16 ? 0x66 : -1,
        (size == REG64 || sno >= 8 || dno >= 8) ? 0x40 | (size == REG64 ? 0x08 : 0) | ((sno & 8) >> 1) | ((dno & 8) >> 3) : -1,
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
  if (inst->opr[0].indirect_with_index.offset->kind == EX_FIXNUM) {
    long offset = inst->opr[0].indirect_with_index.offset->fixnum;
    assert(is_im32(offset));
    short offset_bit = offset == 0 ? 0x04 : is_im8(offset) ? 0x44 : 0x84;
    enum RegSize size = inst->opr[1].reg.size;
    assert(inst->opr[0].indirect_with_index.base_reg.no != RIP);
    int bno = opr_regno(&inst->opr[0].indirect_with_index.base_reg);
    int ino = opr_regno(&inst->opr[0].indirect_with_index.index_reg);
    int dno = opr_regno(&inst->opr[1].reg);
    Expr *scale_expr = inst->opr[0].indirect_with_index.scale;
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
  enum RegSize size = inst->opr[1].reg.size;
  int dno = opr_regno(&inst->opr[1].reg);
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
  enum RegSize size = inst->opr[0].reg.size;
  int sno = opr_regno(&inst->opr[0].reg);
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

static unsigned char *asm_mov_sr(Inst *inst, Code *code) {
  if (inst->opr[0].segment.reg == FS && inst->opr[1].reg.size == REG64) {
    Expr *offset = inst->opr[0].segment.offset;
    if (offset == NULL || offset->kind == EX_FIXNUM) {
      uint64_t value = offset == NULL ? 0 : offset->fixnum;
      short buf[] = {
        0x64, 0x48, 0x8b, 0x04, 0x25,
      };
      unsigned char *p = code->buf;
      p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
      PUT_CODE(p, IM32(value));
      p += 4;
      return p;
    }
  }
  return NULL;
}

static unsigned char *asm_movbwlq_imi(Inst *inst, Code *code) {
  long offset = inst->opr[1].indirect.offset.expr->fixnum;
  unsigned char sno = 0;
  unsigned char dno = opr_regno(&inst->opr[1].indirect.reg);
  unsigned char op = (offset == 0 && (dno & 7) != RBP - RAX) ? 0x00 : is_im8(offset) ? (unsigned char)0x40 : (unsigned char)0x80;
  short buf[] = {
    inst->op == MOVW_IMI ? 0x66 : -1,
    (inst->op == MOVQ_IMI || dno >= 8) ? 0x40 | (inst->op == MOVQ_IMI ? 0x08 : 0) | ((dno & 8) >> 3) | ((sno & 8) >> 1) : -1,
    0xc6 | (inst->op == MOVB_IMI ? 0 : 1),
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

  long value = inst->opr[0].immediate;
  switch (inst->op) {
  case MOVB_IMI: *p++ = IM8(value); break;
  case MOVW_IMI: PUT_CODE(p, IM16(value)); p += 2; break;
  case MOVL_IMI: case MOVQ_IMI:
    PUT_CODE(p, IM32(value));
    p += 4;
    break;
  default: assert(false); break;
  }
  return p;
}

static unsigned char *asm_movbwlq_imd(Inst *inst, Code *code) {
  assert(inst->opr[1].direct.expr->kind == EX_FIXNUM);
  int64_t dst = inst->opr[1].direct.expr->fixnum;
  assert(is_im32(dst));

  short buf[] = {
    inst->op == MOVW_IMD ? 0x66 : -1,
    inst->op == MOVQ_IMD ? 0x48 : -1,
    0xc6 | (inst->op == MOVB_IMD ? 0 : 1),
    0x04, 0x25,
  };

  unsigned char *p = code->buf;
  p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
  PUT_CODE(p, IM32(dst));
  p += 4;

  long value = inst->opr[0].immediate;
  switch (inst->op) {
  case MOVB_IMD: *p++ = IM8(value); break;
  case MOVW_IMD: PUT_CODE(p, IM16(value)); p += 2; break;
  case MOVL_IMD: case MOVQ_IMD:
    PUT_CODE(p, IM32(value));
    p += 4;
    break;
  default: assert(false); break;
  }
  return p;
}

static unsigned char *asm_movszx_rr(Inst *inst, Code *code) {
  int s = inst->opr[0].reg.no;
  int d = inst->opr[1].reg.no;
  unsigned char op = inst->op == MOVZX ? 0xb6 : 0xbe;
  unsigned char *p = code->buf;
  switch (inst->opr[0].reg.size) {
  case REG8:
    switch (inst->opr[1].reg.size) {
    case REG16:
      if (opr_reg8(&inst->opr[0].reg) && !inst->opr[1].reg.x) {
        MAKE_CODE(inst, code, 0x66, 0x0f, op, 0xc0 + s + d * 8);
      } else {
        int pre = ((inst->opr[0].reg.x & 1) == 0 ? 0x40 : 0x41) + (!inst->opr[1].reg.x ? 0 : 4);
        MAKE_CODE(inst, code, 0x66, pre, 0x0f, op, 0xc0 + s + d * 8);
      }
      return p;
    case REG32:
      if (opr_reg8(&inst->opr[0].reg) && !inst->opr[1].reg.x) {
        MAKE_CODE(inst, code, 0x0f, op, 0xc0 + s + d * 8);
      } else {
        int pre = ((inst->opr[0].reg.x & 1) == 0 ? 0x40 : 0x41) + (!inst->opr[1].reg.x ? 0 : 4);
        MAKE_CODE(inst, code, pre, 0x0f, op, 0xc0 + s + d * 8);
      }
      return p;
    case REG64:
      {
        int pre = ((inst->opr[0].reg.x & 1) == 0 ? 0x48 : 0x49) + (!inst->opr[1].reg.x ? 0 : 4);
        MAKE_CODE(inst, code, pre, 0x0f, op, 0xc0 + s + d * 8);
        return p;
      }
    default:
      break;
    }
    break;
  case REG16:
    switch (inst->opr[1].reg.size) {
    case REG32:
      if (!inst->opr[0].reg.x && !inst->opr[1].reg.x) {
        MAKE_CODE(inst, code, 0x0f, op | 1, 0xc0 + s + d * 8);
      } else {
        int pre = (!inst->opr[0].reg.x ? 0x40 : 0x41) + (!inst->opr[1].reg.x ? 0 : 4);
        MAKE_CODE(inst, code, pre, 0x0f, op | 1, 0xc0 + s + d * 8);
      }
      return p;
    case REG64:
      {
        int pre = (!inst->opr[0].reg.x ? 0x48 : 0x49) + (!inst->opr[1].reg.x ? 0 : 4);
        MAKE_CODE(inst, code, pre, 0x0f, op | 1, 0xc0 + s + d * 8);
        return p;
      }
    default:
      break;
    }
    break;
  case REG32:
    // "MOVZX %32bit, %64bit" doesn't exist!
    if (inst->opr[1].reg.size == REG64 && inst->op == MOVSX) {
      int pre = (!inst->opr[0].reg.x ? 0x48 : 0x49) + (!inst->opr[1].reg.x ? 0 : 4);
      MAKE_CODE(inst, code, pre, 0x63, 0xc0 + s + d * 8);
      return p;
    }
    break;
  default:
    break;
  }
  return NULL;
}

static unsigned char *asm_movsds_xx(Inst *inst, Code *code, bool single) {
  unsigned char prefix = single ? 0xf3 : 0xf2;
  unsigned char sno = inst->opr[0].regxmm - XMM0;
  unsigned char dno = inst->opr[1].regxmm - XMM0;
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
  if (inst->opr[0].indirect.offset.expr->kind == EX_FIXNUM &&
      (offset = inst->opr[0].indirect.offset.expr->fixnum, is_im32(offset))) {
    if (inst->opr[0].indirect.reg.no != RIP) {
      unsigned char prefix = single ? 0xf3 : 0xf2;
      unsigned char sno = opr_regno(&inst->opr[0].indirect.reg);
      unsigned char dno = inst->opr[1].regxmm - XMM0;
      int d = dno & 7;
      int s = sno & 7;
      unsigned char op = (offset == 0 && s != RBP - RAX) ? (unsigned char)0x00
                         : is_im8(offset)                ? (unsigned char)0x40
                                                         : (unsigned char)0x80;

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
  if (inst->opr[1].indirect.offset.expr->kind == EX_FIXNUM &&
      (offset = inst->opr[1].indirect.offset.expr->fixnum, is_im32(offset))) {
    if (inst->opr[1].indirect.reg.no != RIP) {
      unsigned char prefix = single ? 0xf3 : 0xf2;
      unsigned char sno = inst->opr[0].regxmm - XMM0;
      unsigned char dno = opr_regno(&inst->opr[1].indirect.reg);
      int d = dno & 7;
      int s = sno & 7;
      unsigned char op = (offset == 0 && d != RBP - RAX) ? (unsigned char)0x00
                         : is_im8(offset)                ? (unsigned char)0x40
                                                         : (unsigned char)0x80;

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
  if (inst->opr[0].type == REG_XMM && inst->opr[1].type == REG_XMM) {
    unsigned char sno = inst->opr[0].regxmm - XMM0;
    unsigned char dno = inst->opr[1].regxmm - XMM0;
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

static unsigned char *asm_xorpd_xx(Inst *inst, Code *code) {
  bool single = inst->op == XORPS;
  unsigned char *p = code->buf;
  if (inst->opr[0].type == REG_XMM && inst->opr[1].type == REG_XMM) {
    unsigned char sno = inst->opr[0].regxmm - XMM0;
    unsigned char dno = inst->opr[1].regxmm - XMM0;
    short buf[] = {
      single ? 0x66 : -1,
      sno >= 8 || dno >= 8 ? (unsigned char)0x40 | ((sno & 8) >> 3) | ((dno & 8) >> 1) : -1,
      0x0f,
      0x57,
      (unsigned char)0xc0 | ((dno & 7) << 3) | (sno & 7),
    };
    p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
  }

  return p;
}
#define asm_xorps_xx  asm_xorpd_xx

static unsigned char *assemble_ucomisd(Inst *inst, Code *code, bool single) {
  unsigned char *p = code->buf;
  if (inst->opr[0].type == REG_XMM && inst->opr[1].type == REG_XMM) {
    unsigned char sno = inst->opr[0].regxmm - XMM0;
    unsigned char dno = inst->opr[1].regxmm - XMM0;
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
  unsigned char sno = opr_regno(&inst->opr[0].reg);
  unsigned char dno = inst->opr[1].regxmm - XMM0;
  short buf[] = {
    prefix,
    sno >= 8 || dno >= 8 || inst->opr[0].reg.size == REG64 ? (unsigned char)0x40 | ((sno & 8) >> 3) | ((dno & 8) >> 1) | (inst->opr[0].reg.size == REG64 ? 8 : 0) : -1,
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
  unsigned char sno = inst->opr[0].regxmm - XMM0;
  unsigned char dno = opr_regno(&inst->opr[1].reg);
  short buf[] = {
    prefix,
    sno >= 8 || dno >= 8 || inst->opr[1].reg.size == REG64 ? (unsigned char)0x40 | ((sno & 8) >> 3) | ((dno & 8) >> 1) | (inst->opr[1].reg.size == REG64 ? 8 : 0) : -1,
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
  unsigned char sno = inst->opr[0].regxmm - XMM0;
  unsigned char dno = inst->opr[1].regxmm - XMM0;
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
  unsigned char sno = inst->opr[0].regxmm - XMM0;
  unsigned char dno = inst->opr[1].regxmm - XMM0;
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

static long signed_immediate(long value, enum RegSize size) {
  switch (size) {
  case REG8:   return (int8_t)value;
  case REG16:  return (int16_t)value;
  case REG32:  return (int32_t)value;
  case REG64:  return (int64_t)value;
  }
  assert(!"Must not reached");
  return value;
}

static unsigned char *asm_lea_ir(Inst *inst, Code *code) {
  assert(inst->opr[0].indirect.offset.expr != NULL);
  unsigned char *p = code->buf;
  if (inst->opr[0].indirect.reg.no != RIP) {
    if (inst->opr[0].indirect.offset.expr->kind == EX_FIXNUM) {
      long offset = inst->opr[0].indirect.offset.expr->fixnum;
      enum RegSize size = inst->opr[1].reg.size;
      short buf[] = {
        MAKE_REX_INDIRECT(
          size,
          opr_regno(&inst->opr[1].reg),
          opr_regno(&inst->opr[0].indirect.reg),
          0x8c, 0x00, offset),
        MAKE_OFFSET(offset, inst->opr[0].indirect.reg.no != RBP - RAX),
      };
      p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
      return p;
    }
  } else {
    if (inst->opr[0].indirect.offset.expr->kind != EX_FIXNUM) {
      int pre = !inst->opr[1].reg.x ? 0x48 : 0x4c;
      int d = inst->opr[1].reg.no;
      MAKE_CODE(inst, code, pre, 0x8d, 0x05 | (d << 3), IM32(0));
      return p;
    }
  }
  return NULL;
}

static unsigned char *asm_lea_iir(Inst *inst, Code *code) {
  Expr *offset_expr = inst->opr[0].indirect_with_index.offset;
  Expr *scale_expr = inst->opr[0].indirect_with_index.scale;
  if ((offset_expr == NULL || offset_expr->kind == EX_FIXNUM) &&
      (scale_expr == NULL || scale_expr->kind == EX_FIXNUM)) {
    long offset = offset_expr != NULL ? offset_expr->fixnum : 0;
    long scale = scale_expr != NULL ? scale_expr->fixnum : 1;
    if (is_im32(offset) && 1 <= scale && scale <= 8 && IS_POWER_OF_2(scale)) {
      int breg = opr_regno(&inst->opr[0].indirect_with_index.base_reg);
      int ireg = opr_regno(&inst->opr[0].indirect_with_index.index_reg);
      int dreg = opr_regno(&inst->opr[1].reg);
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
  enum RegSize size = inst->opr[0].reg.size;
  unsigned char *p = code->buf;
  p = put_rex2(p, size, opr_regno(&inst->opr[0].reg), opr_regno(&inst->opr[1].reg),
               size == REG8 ? 0x00 : 0x01);
  return p;
}

static unsigned char *asm_add_imr(Inst *inst, Code *code) {
  long value = inst->opr[0].immediate;
  if (is_im32(value)) {
    bool im8 = is_im8(value);
    enum RegSize size = inst->opr[1].reg.size;
    int d = opr_regno(&inst->opr[1].reg);
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
      }
    }
    return p;
  }
  return NULL;
}

static unsigned char *asm_add_ir(Inst *inst, Code *code) {
  if (inst->opr[0].indirect.offset.expr->kind == EX_FIXNUM && inst->opr[0].indirect.reg.no != RIP) {
    long offset = inst->opr[0].indirect.offset.expr->fixnum;
    enum RegSize size = inst->opr[1].reg.size;
    unsigned char *p = code->buf;
    short buf[] = {
      MAKE_REX_INDIRECT(
        size,
        opr_regno(&inst->opr[1].reg),
        opr_regno(&inst->opr[0].indirect.reg),
        0x02, 0x00, offset),
      MAKE_OFFSET(offset, inst->opr[0].indirect.reg.no != RBP - RAX),
    };
    p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
    return p;
  }
  return NULL;
}

static unsigned char *asm_add_iir(Inst *inst, Code *code) {
  assert(inst->opr[0].indirect_with_index.offset->kind == EX_FIXNUM && inst->opr[0].indirect_with_index.offset->fixnum == 0);  // TODO
  unsigned char scale = 0;
  if (inst->opr[0].indirect_with_index.scale != NULL) {
    assert(inst->opr[0].indirect_with_index.scale->kind == EX_FIXNUM);
    int s = inst->opr[0].indirect_with_index.scale->fixnum;
    assert(s == 1 || s == 2 || s == 4 || s == 8);
    scale = kPow2Table[s];
  }

  unsigned char bno = inst->opr[0].indirect_with_index.base_reg.no;
  unsigned char ino = inst->opr[0].indirect_with_index.index_reg.no;
  unsigned char dno = inst->opr[1].reg.no;
  short buf[] = {
    (unsigned char)0x48 | (inst->opr[0].indirect_with_index.base_reg.x) | (inst->opr[0].indirect_with_index.index_reg.x << 1) | (inst->opr[1].reg.x << 2),
    0x03,
    0x04 | (dno << 3),
    bno | (ino << 3) | (scale << 6),
  };
  unsigned char *p = code->buf;
  p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
  return p;
}

static unsigned char *asm_addq_imi(Inst *inst, Code *code) {
  if (inst->opr[1].indirect.offset.expr->kind == EX_FIXNUM) {
    long value = inst->opr[0].immediate;
    if (is_im32(value)) {
      long offset = inst->opr[1].indirect.offset.expr->fixnum;
      unsigned char sno = 0;
      unsigned char dno = opr_regno(&inst->opr[1].indirect.reg);
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
  enum RegSize size = inst->opr[0].reg.size;
  unsigned char *p = code->buf;
  p = put_rex2(p, size, opr_regno(&inst->opr[0].reg), opr_regno(&inst->opr[1].reg),
               size == REG8 ? 0x28 : 0x29);
  return p;
}

static unsigned char *asm_sub_imr(Inst *inst, Code *code) {
  long value = inst->opr[0].immediate;
  if (is_im32(value)) {
    bool im8 = is_im8(value);
    enum RegSize size = inst->opr[1].reg.size;
    int d = opr_regno(&inst->opr[1].reg);
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
      }
    }
    return p;
  }
  return NULL;
}

static unsigned char *asm_sub_ir(Inst *inst, Code *code) {
  if (inst->opr[0].indirect.offset.expr->kind == EX_FIXNUM && inst->opr[0].indirect.reg.no != RIP) {
    long offset = inst->opr[0].indirect.offset.expr->fixnum;
    enum RegSize size = inst->opr[1].reg.size;
    unsigned char *p = code->buf;
    short buf[] = {
      MAKE_REX_INDIRECT(
        size,
        opr_regno(&inst->opr[1].reg),
        opr_regno(&inst->opr[0].indirect.reg),
        0x2a, 0x00, offset),
      MAKE_OFFSET(offset, inst->opr[0].indirect.reg.no != RBP - RAX),
    };
    p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
    return p;
  }
  return NULL;
}

static unsigned char *asm_sub_iir(Inst *inst, Code *code) {
  assert(inst->opr[0].indirect_with_index.offset->kind == EX_FIXNUM && inst->opr[0].indirect_with_index.offset->fixnum == 0);  // TODO
  unsigned char scale = 0;
  if (inst->opr[0].indirect_with_index.scale != NULL) {
    assert(inst->opr[0].indirect_with_index.scale->kind == EX_FIXNUM);
    int s = inst->opr[0].indirect_with_index.scale->fixnum;
    assert(s == 1 || s == 2 || s == 4 || s == 8);
    scale = kPow2Table[s];
  }

  unsigned char bno = inst->opr[0].indirect_with_index.base_reg.no;
  unsigned char ino = inst->opr[0].indirect_with_index.index_reg.no;
  unsigned char dno = inst->opr[1].reg.no;
  short buf[] = {
    (unsigned char)0x48 | (inst->opr[0].indirect_with_index.base_reg.x) | (inst->opr[0].indirect_with_index.index_reg.x << 1) | (inst->opr[1].reg.x << 2),
    0x2b,
    0x04 | (dno << 3),
    bno | (ino << 3) | (scale << 6),
  };
  unsigned char *p = code->buf;
  p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
  return p;
}

static unsigned char *asm_subq_imi(Inst *inst, Code *code) {
  if (inst->opr[1].indirect.offset.expr->kind == EX_FIXNUM) {
    long value = inst->opr[0].immediate;
    if (is_im32(value)) {
      long offset = inst->opr[1].indirect.offset.expr->fixnum;
      unsigned char sno = 0;
      unsigned char dno = opr_regno(&inst->opr[1].indirect.reg);
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
  enum RegSize size = inst->opr[0].reg.size;
  unsigned char *p = code->buf;
  short buf[] = {
    MAKE_REX0(
        size, 0, opr_regno(&inst->opr[0].reg),
        0xf6 | (size == REG8 ? 0 : 1)),
    0xe0 | inst->opr[0].reg.no,
  };
  p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
  return p;
}

static unsigned char *asm_div_r(Inst *inst, Code *code) {
  enum RegSize size = inst->opr[0].reg.size;
  unsigned char *p = code->buf;
  short buf[] = {
    MAKE_REX0(
        size, 0, opr_regno(&inst->opr[0].reg),
        0xf6 | (size == REG8 ? 0 : 1)),
    0xf0 | inst->opr[0].reg.no,
  };
  p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
  return p;
}

static unsigned char *asm_idiv_r(Inst *inst, Code *code) {
  enum RegSize size = inst->opr[0].reg.size;
  unsigned char *p = code->buf;
  short buf[] = {
    MAKE_REX0(
        size, 0, opr_regno(&inst->opr[0].reg),
        0xf6 | (size == REG8 ? 0 : 1)),
    0xf8 | inst->opr[0].reg.no,
  };
  p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
  return p;
}

static unsigned char *asm_neg_r(Inst *inst, Code *code) {
  unsigned char *p = code->buf;
  p = put_rex1(p, inst->opr[0].reg.size,
               0xd8, opr_regno(&inst->opr[0].reg), 0xf7);
  return p;
}

static unsigned char *asm_not_r(Inst *inst, Code *code) {
  unsigned char *p = code->buf;
  p = put_rex1(p, inst->opr[0].reg.size,
               0xd0, opr_regno(&inst->opr[0].reg), 0xf7);
  return p;
}

static unsigned char *asm_inc_r(Inst *inst, Code *code) {
  unsigned char *p = code->buf;
  p = put_rex1(p, inst->opr[0].reg.size,
               0xc0, opr_regno(&inst->opr[0].reg), 0xff);
  return p;
}

static unsigned char *asm_incbwlq_i(Inst *inst, Code *code) {
  if (inst->opr[0].indirect.reg.no != RIP) {
    enum RegSize size = inst->op + (REG8 - INCB);
    long offset = inst->opr[0].indirect.offset.expr->fixnum;
    unsigned char *p = code->buf;
    short buf[] = {
      MAKE_REX_INDIRECT(
        size,
        0, opr_regno(&inst->opr[0].indirect.reg),
        0xfe, 0x00, offset),
      MAKE_OFFSET(offset, inst->opr[0].indirect.reg.no != RBP - RAX),
    };
    p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
    return p;
  }
  return NULL;
}

static unsigned char *asm_dec_r(Inst *inst, Code *code) {
  unsigned char *p = code->buf;
  p = put_rex1(p, inst->opr[0].reg.size,
               0xc8, opr_regno(&inst->opr[0].reg), 0xff);
  return p;
}

static unsigned char *asm_decbwlq_i(Inst *inst, Code *code) {
  if (inst->opr[0].indirect.reg.no != RIP) {
    enum RegSize size = inst->op + (REG8 - DECB);
    long offset = inst->opr[0].indirect.offset.expr->fixnum;
    unsigned char *p = code->buf;
    short buf[] = {
      MAKE_REX_INDIRECT(
        size,
        1, opr_regno(&inst->opr[0].indirect.reg),
        0xfe, 0x08, offset),
      MAKE_OFFSET(offset, inst->opr[0].indirect.reg.no != RBP - RAX),
    };
    p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
    return p;
  }
  return NULL;
}

static unsigned char *asm_and_rr(Inst *inst, Code *code) {
  enum RegSize size = inst->opr[1].reg.size;
  unsigned char *p = code->buf;
  p = put_rex2(p, size, opr_regno(&inst->opr[0].reg), opr_regno(&inst->opr[1].reg),
               size == REG8 ? 0x20 : 0x21);
  return p;
}

static unsigned char *asm_and_imr(Inst *inst, Code *code) {
  long value = inst->opr[0].immediate;
  enum RegSize size = inst->opr[1].reg.size;
  unsigned char *p = code->buf;
  if (is_im8(value) && (size != REG8 || opr_regno(&inst->opr[1].reg) != AL - AL)) {
    p = put_rex1(p, size,
                 0xe0, opr_regno(&inst->opr[1].reg),
                 0x83);
    *p++ = IM8(value);
    return p;
  } else if (size <= REG32 || is_im32(value)) {
    if (opr_regno(&inst->opr[1].reg) == RAX - RAX) {
      p = put_rex0(p, size,
                   0, opr_regno(&inst->opr[1].reg),
                   size == REG8 ? 0x24 : 0x25);
    } else {
      p = put_rex1(p, size,
                   0xe0, opr_regno(&inst->opr[1].reg),
                   size == REG8 ? 0x80 : 0x81);
    }

    switch (size) {
    case REG8:  PUT_CODE(p, IM8(value)); p += 1; break;
    case REG16: PUT_CODE(p, IM16(value)); p += 2; break;
    case REG32: case REG64:
      PUT_CODE(p, IM32(value));
      p += 4;
      break;
    }
    return p;
  }
  return NULL;
}

static unsigned char *asm_or_rr(Inst *inst, Code *code) {
  enum RegSize size = inst->opr[1].reg.size;
  unsigned char *p = code->buf;
  p = put_rex2(p, size, opr_regno(&inst->opr[0].reg), opr_regno(&inst->opr[1].reg),
               size == REG8 ? 0x08 : 0x09);
  return p;
}

static unsigned char *asm_or_imr(Inst *inst, Code *code) {
  long value = inst->opr[0].immediate;
  enum RegSize size = inst->opr[1].reg.size;
  unsigned char *p = code->buf;
  if (is_im8(value) && (size != REG8 || opr_regno(&inst->opr[1].reg) != AL - AL)) {
    p = put_rex1(p, size,
                 0xc8, opr_regno(&inst->opr[1].reg),
                 0x83);
    *p++ = IM8(value);
    return p;
  } else if (size <= REG32 || is_im32(value)) {
    if (opr_regno(&inst->opr[1].reg) == RAX - RAX) {
      p = put_rex0(p, size,
                   0, opr_regno(&inst->opr[1].reg),
                   size == REG8 ? 0x0c : 0x0d);
    } else {
      p = put_rex1(p, size,
                   0xc8, opr_regno(&inst->opr[1].reg),
                   size == REG8 ? 0x80 : 0x81);
    }
    switch (size) {
    case REG8:  PUT_CODE(p, IM8(value)); p += 1; break;
    case REG16: PUT_CODE(p, IM16(value)); p += 2; break;
    case REG32: case REG64:
      PUT_CODE(p, IM32(value));
      p += 4;
      break;
    }
    return p;
  }
  return NULL;
}

static unsigned char *asm_xor_rr(Inst *inst, Code *code) {
  enum RegSize size = inst->opr[1].reg.size;
  unsigned char *p = code->buf;
  p = put_rex2(p, size, opr_regno(&inst->opr[0].reg), opr_regno(&inst->opr[1].reg),
               size == REG8 ? 0x30 : 0x31);
  return p;
}

static unsigned char *asm_xor_imr(Inst *inst, Code *code) {
  long value = inst->opr[0].immediate;
  enum RegSize size = inst->opr[1].reg.size;
  unsigned char *p = code->buf;
  if (is_im8(value) && (size != REG8 || opr_regno(&inst->opr[1].reg) != AL - AL)) {
    p = put_rex1(p, size,
                 0xf0, opr_regno(&inst->opr[1].reg),
                 0x83);
    *p++ = IM8(value);
    return p;
  } else if (size <= REG32 || is_im32(value)) {
    if (opr_regno(&inst->opr[1].reg) == RAX - RAX) {
      p = put_rex0(p, size,
                   0, opr_regno(&inst->opr[1].reg),
                   size == REG8 ? 0x34 : 0x35);
    } else {
      p = put_rex1(p, size,
                   0xf0, opr_regno(&inst->opr[1].reg),
                   size == REG8 ? 0x80 : 0x81);
    }
    switch (size) {
    case REG8:  PUT_CODE(p, IM8(value)); p += 1; break;
    case REG16: PUT_CODE(p, IM16(value)); p += 2; break;
    case REG32: case REG64:
      PUT_CODE(p, IM32(value));
      p += 4;
      break;
    }
    return p;
  }
  return NULL;
}

static unsigned char *asm_shl_rr(Inst *inst, Code *code) {
  enum RegSize size = inst->opr[1].reg.size;
  unsigned char *p = code->buf;
  p = put_rex1(p, size, 0xe0, opr_regno(&inst->opr[1].reg),
               size == REG8 ? 0xd2 : 0xd3);
  return p;
}

static unsigned char *asm_shl_imr(Inst *inst, Code *code) {
  enum RegSize size = inst->opr[1].reg.size;
  unsigned char *p = code->buf;
  if (inst->opr[0].immediate == 1) {
    p = put_rex1(p, size, 0xe0, opr_regno(&inst->opr[1].reg),
                 size == REG8 ? 0xd0 : 0xd1);
  } else {
    p = put_rex1(p, size, 0xe0, opr_regno(&inst->opr[1].reg),
                 size == REG8 ? 0xc0 : 0xc1);
    *p++ = IM8(inst->opr[0].immediate);
  }
  return p;
}

static unsigned char *asm_shr_rr(Inst *inst, Code *code) {
  enum RegSize size = inst->opr[1].reg.size;
  unsigned char *p = code->buf;
  p = put_rex1(p, size, 0xe8, opr_regno(&inst->opr[1].reg),
               size == REG8 ? 0xd2 : 0xd3);
  return p;
}

static unsigned char *asm_shr_imr(Inst *inst, Code *code) {
  enum RegSize size = inst->opr[1].reg.size;
  unsigned char *p = code->buf;
  if (inst->opr[0].immediate == 1) {
    p = put_rex1(p, size, 0xe8, opr_regno(&inst->opr[1].reg),
                 size == REG8 ? 0xd0 : 0xd1);
  } else {
    p = put_rex1(p, size, 0xe8, opr_regno(&inst->opr[1].reg),
                 size == REG8 ? 0xc0 : 0xc1);
    *p++ = IM8(inst->opr[0].immediate);
  }
  return p;
}

static unsigned char *asm_sar_rr(Inst *inst, Code *code) {
  enum RegSize size = inst->opr[1].reg.size;
  unsigned char *p = code->buf;
  p = put_rex1(p, size, 0xf8, opr_regno(&inst->opr[1].reg),
               size == REG8 ? 0xd2 : 0xd3);
  return p;
}

static unsigned char *asm_sar_imr(Inst *inst, Code *code) {
  enum RegSize size = inst->opr[1].reg.size;
  unsigned char *p = code->buf;
  if (inst->opr[0].immediate == 1) {
    p = put_rex1(p, size, 0xf8, opr_regno(&inst->opr[1].reg),
                 size == REG8 ? 0xd0 : 0xd1);
  } else {
    p = put_rex1(p, size, 0xf8, opr_regno(&inst->opr[1].reg),
                 size == REG8 ? 0xc0 : 0xc1);
    *p++ = IM8(inst->opr[0].immediate);
  }
  return p;
}

static unsigned char *asm_cmp_rr(Inst *inst, Code *code) {
  enum RegSize size = inst->opr[0].reg.size;
  unsigned char *p = code->buf;
  p = put_rex2(p, size, opr_regno(&inst->opr[0].reg), opr_regno(&inst->opr[1].reg),
               size == REG8 ? 0x38 : 0x39);
  return p;
}

static unsigned char *asm_cmp_imr(Inst *inst, Code *code) {
  enum RegSize size = inst->opr[1].reg.size;
  long value = signed_immediate(inst->opr[0].immediate, size);
  if (is_im32(value) || size <= REG32) {
    bool im8 = is_im8(value);
    int d = opr_regno(&inst->opr[1].reg);
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
  enum RegSize size = inst->opr[0].reg.size;
  unsigned char *p = code->buf;
  p = put_rex2(p, size, opr_regno(&inst->opr[0].reg), opr_regno(&inst->opr[1].reg),
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
  short buf[] = {
    MAKE_REX0(
        REG8, 0, opr_regno(&inst->opr[0].reg), 0x0f),
    0x90 | (inst->op - SETO),
    0xc0 | inst->opr[0].reg.no,
  };
  p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
  return p;
}

static unsigned char *asm_push_r(Inst *inst, Code *code) {
  unsigned char *p = code->buf;
  short buf[] = {
    MAKE_REX0(
        REG32, 0, opr_regno(&inst->opr[0].reg),
        0x50 | inst->opr[0].reg.no),
  };
  p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
  return p;
}

static unsigned char *asm_push_im(Inst *inst, Code *code) {
  unsigned char *p = code->buf;
  long value = inst->opr[0].immediate;
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
  short buf[] = {
    MAKE_REX0(
        REG32, 0, opr_regno(&inst->opr[0].reg),
        0x58 | inst->opr[0].reg.no),
  };
  p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
  return p;
}

static unsigned char *asm_jmp_d(Inst *inst, Code *code) {
  MAKE_CODE(inst, code, 0xeb, IM8(0));  // Short jmp in default.
  return code->buf;
}

static unsigned char *asm_jmp_der(Inst *inst, Code *code) {
  int s = inst->opr[0].reg.no;
  short buf[] = {
    inst->opr[0].reg.x ? 0x41 : -1,
    0xff, 0xe0 | s,
  };
  unsigned char *p = code->buf;
  p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
  return p;
}

static unsigned char *asm_jmp_dei(Inst *inst, Code *code) {
  Expr *offset_expr = inst->opr[0].indirect.offset.expr;
  if ((offset_expr == NULL || offset_expr->kind == EX_FIXNUM)) {
    long offset = offset_expr != NULL ? offset_expr->fixnum : 0;
    if (is_im32(offset)) {
      short offset_bit = offset == 0 ? 0x20 : is_im8(offset) ? 0x60 : 0xa0;
      short b = inst->opr[0].indirect.reg.no;
      short buf[] = {
        inst->opr[0].indirect.reg.x ? 0x41 : -1,
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
  Expr *offset_expr = inst->opr[0].indirect_with_index.offset;
  Expr *scale_expr = inst->opr[0].indirect_with_index.scale;
  if ((offset_expr == NULL || offset_expr->kind == EX_FIXNUM) &&
      (scale_expr == NULL || scale_expr->kind == EX_FIXNUM)) {
    long offset = offset_expr != NULL ? offset_expr->fixnum : 0;
    long scale = scale_expr != NULL ? scale_expr->fixnum : 1;
    if (is_im32(offset) && 1 <= scale && scale <= 8 && IS_POWER_OF_2(scale)) {
      short b = inst->opr[0].indirect_with_index.base_reg.no;
      short scale_bit = kPow2Table[scale];
      short i = inst->opr[0].indirect_with_index.index_reg.no;
      short prefix = inst->opr[0].indirect_with_index.base_reg.x | (inst->opr[0].indirect_with_index.index_reg.x << 1);
      short offset_bit = offset == 0 && b != RBP - RAX ? 0x20 : is_im8(offset) ? 0x60 : 0xa0;
      short buf[] = {
        prefix != 0 ? 0x40 | prefix : -1,
        0xff,
        offset_bit | 0x04,
        (scale_bit << 6) | (i << 3) | b,
      };
      unsigned char *p = code->buf;
      p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
      if (offset != 0 || b == RBP - RAX) {
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
  int s = inst->opr[0].reg.no;
  if (!inst->opr[0].reg.x) {
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
  long value = inst->opr[0].immediate;
  MAKE_CODE(inst, code, 0xcd, IM8(value));
  return code->buf;
}

static unsigned char *asm_syscall(Inst *inst, Code *code) {
  MAKE_CODE(inst, code, 0x0f, 0x05);
  return code->buf;
}

////////////////////////////////////////////////

typedef unsigned char *(*AsmInstFunc)(Inst *inst, Code *code);

static const AsmInstFunc table[] = {
  [NOOP] = asm_noop,
  [MOV_RR] = asm_mov_rr,
  [MOV_IMR] = asm_mov_imr,
  [MOV_IR] = asm_mov_ir,
  [MOV_RI] = asm_mov_ri,
  [MOV_IIR] = asm_mov_iir,
  [MOV_DR] = asm_mov_dr,
  [MOV_RD] = asm_mov_rd,
  [MOV_SR] = asm_mov_sr,
  [MOVB_IMI] = asm_movbwlq_imi,
  [MOVW_IMI] = asm_movbwlq_imi,
  [MOVL_IMI] = asm_movbwlq_imi,
  [MOVQ_IMI] = asm_movbwlq_imi,
  [MOVB_IMD] = asm_movbwlq_imd,
  [MOVW_IMD] = asm_movbwlq_imd,
  [MOVL_IMD] = asm_movbwlq_imd,
  [MOVQ_IMD] = asm_movbwlq_imd,
  [MOVSX] = asm_movszx_rr,  [MOVZX] = asm_movszx_rr,
  [LEA_IR] = asm_lea_ir,
  [LEA_IIR] = asm_lea_iir,
  [ADD_RR] = asm_add_rr,
  [ADD_IMR] = asm_add_imr,
  [ADD_IR] = asm_add_ir,
  [ADD_IIR] = asm_add_iir,
  [ADDQ] = asm_addq_imi,
  [SUB_RR] = asm_sub_rr,
  [SUB_IMR] = asm_sub_imr,
  [SUB_IR] = asm_sub_ir,
  [SUB_IIR] = asm_sub_iir,
  [SUBQ] = asm_subq_imi,
  [MUL] = asm_mul_r,
  [DIV] = asm_div_r,
  [IDIV] = asm_idiv_r,
  [NEG] = asm_neg_r,
  [NOT] = asm_not_r,
  [INC] = asm_inc_r,
  [INCB] = asm_incbwlq_i,  [INCW] = asm_incbwlq_i,  [INCL] = asm_incbwlq_i,  [INCQ] = asm_incbwlq_i,
  [DEC] = asm_dec_r,
  [DECB] = asm_decbwlq_i,  [DECW] = asm_decbwlq_i,  [DECL] = asm_decbwlq_i,  [DECQ] = asm_decbwlq_i,
  [AND_RR] = asm_and_rr,
  [AND_IMR] = asm_and_imr,
  [OR_RR] = asm_or_rr,
  [OR_IMR] = asm_or_imr,
  [XOR_RR] = asm_xor_rr,
  [XOR_IMR] = asm_xor_imr,
  [SHL_RR] = asm_shl_rr,
  [SHL_IMR] = asm_shl_imr,
  [SHR_RR] = asm_shr_rr,
  [SHR_IMR] = asm_shr_imr,
  [SAR_RR] = asm_sar_rr,
  [SAR_IMR] = asm_sar_imr,
  [CMP_RR] = asm_cmp_rr,
  [CMP_IMR] = asm_cmp_imr,
  [TEST] = asm_test_rr,
  [CWTL] = asm_cwtl,
  [CLTD] = asm_cltd,
  [CQTO] = asm_cqto,
  [SETO] = asm_set_r,  [SETNO] = asm_set_r,  [SETB] = asm_set_r,  [SETAE] = asm_set_r,
  [SETE] = asm_set_r,  [SETNE] = asm_set_r,  [SETBE] = asm_set_r,  [SETA] = asm_set_r,
  [SETS] = asm_set_r,  [SETNS] = asm_set_r,  [SETP] = asm_set_r,  [SETNP] = asm_set_r,
  [SETL] = asm_set_r,  [SETGE] = asm_set_r,  [SETLE] = asm_set_r,  [SETG] = asm_set_r,
  [JMP_D] = asm_jmp_d,
  [JMP_DER] = asm_jmp_der,
  [JMP_DEI] = asm_jmp_dei,
  [JMP_DEII] = asm_jmp_deii,
  [JO] = asm_jxx_d,  [JNO] = asm_jxx_d,  [JB] = asm_jxx_d,  [JAE] = asm_jxx_d,
  [JE] = asm_jxx_d,  [JNE] = asm_jxx_d,  [JBE] = asm_jxx_d,  [JA] = asm_jxx_d,
  [JS] = asm_jxx_d,  [JNS] = asm_jxx_d,  [JP] = asm_jxx_d,  [JNP] = asm_jxx_d,
  [JL] = asm_jxx_d,  [JGE] = asm_jxx_d,  [JLE] = asm_jxx_d,  [JG] = asm_jxx_d,
  [CALL_D] = asm_call_d,
  [CALL_DER] = asm_call_der,
  [RET] = asm_ret,
  [PUSH_R] = asm_push_r,
  [PUSH_IM] = asm_push_im,
  [POP] = asm_pop_r,
  [INT] = asm_int_im,
  [SYSCALL] = asm_syscall,

  [MOVSD_XX] = asm_movsd_xx,
  [MOVSD_IX] = asm_movsd_ix,
  [MOVSD_XI] = asm_movsd_xi,
  [MOVSS_XX] = asm_movss_xx,
  [MOVSS_IX] = asm_movss_ix,
  [MOVSS_XI] = asm_movss_xi,
  [ADDSD] = asm_addsd_xx,
  [ADDSS] = asm_addss_xx,
  [SUBSD] = asm_subsd_xx,
  [SUBSS] = asm_subss_xx,
  [MULSD] = asm_mulsd_xx,
  [MULSS] = asm_mulss_xx,
  [DIVSD] = asm_divsd_xx,
  [DIVSS] = asm_divss_xx,
  [XORPD] = asm_xorpd_xx,
  [XORPS] = asm_xorps_xx,
  [UCOMISD] = asm_ucomisd_xx,
  [UCOMISS] = asm_ucomiss_xx,
  [CVTSI2SD] = asm_cvtsi2sd_rx,
  [CVTSI2SS] = asm_cvtsi2ss_rx,
  [CVTTSD2SI] = asm_cvttsd2si_xr,
  [CVTTSS2SI] = asm_cvttss2si_xr,
  [CVTSD2SS] = asm_cvtsd2ss_xx,
  [CVTSS2SD] = asm_cvtss2sd_xx,
  [SQRTSD] = asm_sqrtsd_xx,
};

void assemble_inst(Inst *inst, const ParseInfo *info, Code *code) {
  code->flag = 0;
  code->len = 0;

  const AsmInstFunc *func = NULL;
  if (inst->op < (enum Opcode)ARRAY_SIZE(table)) {
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

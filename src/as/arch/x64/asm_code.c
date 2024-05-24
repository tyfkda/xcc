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

  short buf[] = {
    MAKE_REX0(
      size, 0, opr_regno(&inst->dst.reg),
      0xb0 | (size == REG8 ? 0 : 8) | inst->dst.reg.no),
      MAKE_IM(size, inst->src.immediate),
  };
  p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
  return p;
}

static unsigned char *asm_mov_ir(Inst *inst, Code *code) {
  if (inst->src.indirect.offset->kind == EX_FIXNUM) {
    long long offset = inst->src.indirect.offset->fixnum;
    enum RegSize size = inst->dst.reg.size;
    if (inst->src.indirect.reg.no != RIP) {
      int sno = opr_regno(&inst->src.indirect.reg);
      int dno = opr_regno(&inst->dst.reg);
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
  }
  return NULL;
}

static unsigned char *asm_mov_ri(Inst *inst, Code *code) {
  if (inst->dst.indirect.offset->kind == EX_FIXNUM) {
    long long offset = inst->dst.indirect.offset->fixnum;
    enum RegSize size = inst->src.reg.size;
    if (inst->dst.indirect.reg.no != RIP) {
      int sno = opr_regno(&inst->src.reg);
      int dno = opr_regno(&inst->dst.indirect.reg);
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
  if (inst->src.indirect_with_index.offset->kind == EX_FIXNUM) {
    long long offset = inst->src.indirect_with_index.offset->fixnum;
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

static unsigned char *asm_mov_sr(Inst *inst, Code *code) {
  if (inst->src.segment.reg == FS && inst->dst.reg.size == REG64) {
    Expr *offset = inst->src.segment.offset;
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
  long long offset = inst->dst.indirect.offset->fixnum;
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

  long long value = inst->src.immediate;
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

static unsigned char *asm_movbwlq_imd(Inst *inst, Code *code) {
  assert(inst->dst.direct.expr->kind == EX_FIXNUM);
  int64_t dst = inst->dst.direct.expr->fixnum;
  assert(is_im32(dst));

  short buf[] = {
    inst->op == MOVW ? 0x66 : -1,
    inst->op == MOVQ ? 0x48 : -1,
    0xc6 | (inst->op == MOVB ? 0 : 1),
    0x04, 0x25,
  };

  unsigned char *p = code->buf;
  p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
  PUT_CODE(p, IM32(dst));
  p += 4;

  long long value = inst->src.immediate;
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
        int pre = ((inst->src.reg.x & 1) == 0 ? 0x40 : 0x41) + (!inst->dst.reg.x ? 0 : 4);
        MAKE_CODE(inst, code, 0x66, pre, 0x0f, op, 0xc0 + s + d * 8);
      }
      return p;
    case REG32:
      if (opr_reg8(&inst->src.reg) && !inst->dst.reg.x) {
        MAKE_CODE(inst, code, 0x0f, op, 0xc0 + s + d * 8);
      } else {
        int pre = ((inst->src.reg.x & 1) == 0 ? 0x40 : 0x41) + (!inst->dst.reg.x ? 0 : 4);
        MAKE_CODE(inst, code, pre, 0x0f, op, 0xc0 + s + d * 8);
      }
      return p;
    case REG64:
      {
        int pre = ((inst->src.reg.x & 1) == 0 ? 0x48 : 0x49) + (!inst->dst.reg.x ? 0 : 4);
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
  long long offset;
  if (inst->src.indirect.offset->kind == EX_FIXNUM &&
      (offset = inst->src.indirect.offset->fixnum, is_im32(offset))) {
    if (inst->src.indirect.reg.no != RIP) {
      unsigned char prefix = single ? 0xf3 : 0xf2;
      unsigned char sno = opr_regno(&inst->src.indirect.reg);
      unsigned char dno = inst->dst.regxmm - XMM0;
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
  long long offset;
  if (inst->dst.indirect.offset->kind == EX_FIXNUM &&
      (offset = inst->dst.indirect.offset->fixnum, is_im32(offset))) {
    if (inst->dst.indirect.reg.no != RIP) {
      unsigned char prefix = single ? 0xf3 : 0xf2;
      unsigned char sno = inst->src.regxmm - XMM0;
      unsigned char dno = opr_regno(&inst->dst.indirect.reg);
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

static unsigned char *asm_xorpd_xx(Inst *inst, Code *code) {
  bool single = inst->op == XORPS;
  unsigned char *p = code->buf;
  if (inst->src.type == REG_XMM && inst->dst.type == REG_XMM) {
    unsigned char sno = inst->src.regxmm - XMM0;
    unsigned char dno = inst->dst.regxmm - XMM0;
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

static long long signed_immediate(long long value, enum RegSize size) {
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
  assert(inst->src.indirect.offset != NULL);
  unsigned char *p = code->buf;
  if (inst->src.indirect.reg.no != RIP) {
    if (inst->src.indirect.offset->kind == EX_FIXNUM) {
      long long offset = inst->src.indirect.offset->fixnum;
      enum RegSize size = inst->dst.reg.size;
      short buf[] = {
        MAKE_REX_INDIRECT(
          size,
          opr_regno(&inst->dst.reg),
          opr_regno(&inst->src.indirect.reg),
          0x8c, 0x00, offset),
        MAKE_OFFSET(offset, inst->src.indirect.reg.no != RBP - RAX),
      };
      p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
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
    long long offset = offset_expr != NULL ? offset_expr->fixnum : 0;
    long long scale = scale_expr != NULL ? scale_expr->fixnum : 1;
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
  long long value = inst->src.immediate;
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
      }
    }
    return p;
  }
  return NULL;
}

static unsigned char *asm_add_ir(Inst *inst, Code *code) {
  if (inst->src.indirect.offset->kind == EX_FIXNUM && inst->src.indirect.reg.no != RIP) {
    long long offset = inst->src.indirect.offset->fixnum;
    enum RegSize size = inst->dst.reg.size;
    unsigned char *p = code->buf;
    short buf[] = {
      MAKE_REX_INDIRECT(
        size,
        opr_regno(&inst->dst.reg),
        opr_regno(&inst->src.indirect.reg),
        0x02, 0x00, offset),
      MAKE_OFFSET(offset, inst->src.indirect.reg.no != RBP - RAX),
    };
    p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
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
    long long value = inst->src.immediate;
    if (is_im32(value)) {
      long long offset = inst->dst.indirect.offset->fixnum;
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
  long long value = inst->src.immediate;
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
      }
    }
    return p;
  }
  return NULL;
}

static unsigned char *asm_sub_ir(Inst *inst, Code *code) {
  if (inst->src.indirect.offset->kind == EX_FIXNUM && inst->src.indirect.reg.no != RIP) {
    long long offset = inst->src.indirect.offset->fixnum;
    enum RegSize size = inst->dst.reg.size;
    unsigned char *p = code->buf;
    short buf[] = {
      MAKE_REX_INDIRECT(
        size,
        opr_regno(&inst->dst.reg),
        opr_regno(&inst->src.indirect.reg),
        0x2a, 0x00, offset),
      MAKE_OFFSET(offset, inst->src.indirect.reg.no != RBP - RAX),
    };
    p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
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
    long long value = inst->src.immediate;
    if (is_im32(value)) {
      long long offset = inst->dst.indirect.offset->fixnum;
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
  short buf[] = {
    MAKE_REX0(
        size, 0, opr_regno(&inst->src.reg),
        0xf6 | (size == REG8 ? 0 : 1)),
    0xe0 | inst->src.reg.no,
  };
  p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
  return p;
}

static unsigned char *asm_div_r(Inst *inst, Code *code) {
  enum RegSize size = inst->src.reg.size;
  unsigned char *p = code->buf;
  short buf[] = {
    MAKE_REX0(
        size, 0, opr_regno(&inst->src.reg),
        0xf6 | (size == REG8 ? 0 : 1)),
    0xf0 | inst->src.reg.no,
  };
  p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
  return p;
}

static unsigned char *asm_idiv_r(Inst *inst, Code *code) {
  enum RegSize size = inst->src.reg.size;
  unsigned char *p = code->buf;
  short buf[] = {
    MAKE_REX0(
        size, 0, opr_regno(&inst->src.reg),
        0xf6 | (size == REG8 ? 0 : 1)),
    0xf8 | inst->src.reg.no,
  };
  p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
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
    long long offset = inst->src.indirect.offset->fixnum;
    unsigned char *p = code->buf;
    short buf[] = {
      MAKE_REX_INDIRECT(
        size,
        0, opr_regno(&inst->src.indirect.reg),
        0xfe, 0x00, offset),
      MAKE_OFFSET(offset, inst->src.indirect.reg.no != RBP - RAX),
    };
    p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
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
    long long offset = inst->src.indirect.offset->fixnum;
    unsigned char *p = code->buf;
    short buf[] = {
      MAKE_REX_INDIRECT(
        size,
        1, opr_regno(&inst->src.indirect.reg),
        0xfe, 0x08, offset),
      MAKE_OFFSET(offset, inst->src.indirect.reg.no != RBP - RAX),
    };
    p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
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
  long long value = inst->src.immediate;
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
  long long value = inst->src.immediate;
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
  long long value = inst->src.immediate;
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
  long long value = signed_immediate(inst->src.immediate, size);
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
  short buf[] = {
    MAKE_REX0(
        REG8, 0, opr_regno(&inst->src.reg), 0x0f),
    0x90 | (inst->op - SETO),
    0xc0 | inst->src.reg.no,
  };
  p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
  return p;
}

static unsigned char *asm_push_r(Inst *inst, Code *code) {
  unsigned char *p = code->buf;
  short buf[] = {
    MAKE_REX0(
        REG32, 0, opr_regno(&inst->src.reg),
        0x50 | inst->src.reg.no),
  };
  p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
  return p;
}

static unsigned char *asm_push_im(Inst *inst, Code *code) {
  unsigned char *p = code->buf;
  long long value = inst->src.immediate;
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
        REG32, 0, opr_regno(&inst->src.reg),
        0x58 | inst->src.reg.no),
  };
  p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
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
    long long offset = offset_expr != NULL ? offset_expr->fixnum : 0;
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
    long long offset = offset_expr != NULL ? offset_expr->fixnum : 0;
    long long scale = scale_expr != NULL ? scale_expr->fixnum : 1;
    if (is_im32(offset) && 1 <= scale && scale <= 8 && IS_POWER_OF_2(scale)) {
      short b = inst->src.indirect_with_index.base_reg.no;
      short scale_bit = kPow2Table[scale];
      short i = inst->src.indirect_with_index.index_reg.no;
      short prefix = inst->src.indirect_with_index.base_reg.x | (inst->src.indirect_with_index.index_reg.x << 1);
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
  long long value = inst->src.immediate;
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

typedef unsigned char *(*AsmInstFunc)(Inst *inst, Code *code);
typedef struct {
  AsmInstFunc func;
  enum OperandType src_type;
  enum OperandType dst_type;
  int flag;
} AsmInstTable;

static const AsmInstTable table_movbwlq[] ={
    {asm_movbwlq_imi, IMMEDIATE, INDIRECT},
    {asm_movbwlq_imd, IMMEDIATE, DIRECT},
    {NULL} };

static const AsmInstTable table_movszx[] ={
    {asm_movszx_rr, REG, REG, NO_SAME_REG_SIZE},
    {NULL} };

static const AsmInstTable table_incbwlq[] ={
    {asm_incbwlq_i, INDIRECT, NOOPERAND},
    {NULL} };

static const AsmInstTable table_decbwlq[] ={
    {asm_decbwlq_i, INDIRECT, NOOPERAND},
    {NULL} };

static const AsmInstTable table_set[] ={
    {asm_set_r, REG, NOOPERAND, SRC_REG8_ONLY},
    {NULL} };

static const AsmInstTable table_jxx[] ={
    {asm_jxx_d, DIRECT, NOOPERAND},
    {NULL} };

static const AsmInstTable *table[] = {
  [NOOP] = (const AsmInstTable[]){ {asm_noop, NOOPERAND, NOOPERAND}, {NULL} },
  [MOV] = (const AsmInstTable[]){
    {asm_mov_rr, REG, REG},
    {asm_mov_imr, IMMEDIATE, REG},
    {asm_mov_ir, INDIRECT, REG},
    {asm_mov_ri, REG, INDIRECT},
    {asm_mov_iir, INDIRECT_WITH_INDEX, REG},
    {asm_mov_dr, DIRECT, REG},
    {asm_mov_rd, REG, DIRECT},
    {asm_mov_sr, SEGMENT_OFFSET, REG},
    {NULL} },
  [MOVB] = table_movbwlq,  [MOVW] = table_movbwlq,  [MOVL] = table_movbwlq,  [MOVQ] = table_movbwlq,
  [MOVSX] = table_movszx,  [MOVZX] = table_movszx,
  [LEA] = (const AsmInstTable[]){
    {asm_lea_ir, INDIRECT, REG, DST_REG64_ONLY},
    {asm_lea_iir, INDIRECT_WITH_INDEX, REG, DST_REG64_ONLY},
    {NULL} },
  [ADD] = (const AsmInstTable[]){
    {asm_add_rr, REG, REG},
    {asm_add_imr, IMMEDIATE, REG},
    {asm_add_ir, INDIRECT, REG},
    {asm_add_iir, INDIRECT_WITH_INDEX, REG},
    {NULL} },
  [ADDQ] = (const AsmInstTable[]){ {asm_addq_imi, IMMEDIATE, INDIRECT}, {NULL} },
  [SUB] = (const AsmInstTable[]){
    {asm_sub_rr, REG, REG},
    {asm_sub_imr, IMMEDIATE, REG},
    {asm_sub_ir, INDIRECT, REG},
    {asm_sub_iir, INDIRECT_WITH_INDEX, REG},
    {NULL} },
  [SUBQ] = (const AsmInstTable[]){ {asm_subq_imi, IMMEDIATE, INDIRECT}, {NULL} },
  [MUL] = (const AsmInstTable[]){ {asm_mul_r, REG, NOOPERAND}, {NULL} },
  [DIV] = (const AsmInstTable[]){ {asm_div_r, REG, NOOPERAND}, {NULL} },
  [IDIV] = (const AsmInstTable[]){ {asm_idiv_r, REG, NOOPERAND}, {NULL} },
  [NEG] = (const AsmInstTable[]){ {asm_neg_r, REG, NOOPERAND}, {NULL} },
  [NOT] = (const AsmInstTable[]){ {asm_not_r, REG, NOOPERAND}, {NULL} },
  [INC] = (const AsmInstTable[]){ {asm_inc_r, REG, NOOPERAND}, {NULL} },
  [INCB] = table_incbwlq,  [INCW] = table_incbwlq,  [INCL] = table_incbwlq,  [INCQ] = table_incbwlq,
  [DEC] = (const AsmInstTable[]){ {asm_dec_r, REG, NOOPERAND}, {NULL} },
  [DECB] = table_decbwlq,  [DECW] = table_decbwlq,  [DECL] = table_decbwlq,  [DECQ] = table_decbwlq,
  [AND] = (const AsmInstTable[]){
    {asm_and_rr, REG, REG},
    {asm_and_imr, IMMEDIATE, REG},
    {NULL} },
  [OR] = (const AsmInstTable[]){
    {asm_or_rr, REG, REG},
    {asm_or_imr, IMMEDIATE, REG},
    {NULL} },
  [XOR] = (const AsmInstTable[]){
    {asm_xor_rr, REG, REG},
    {asm_xor_imr, IMMEDIATE, REG},
    {NULL} },
  [SHL] = (const AsmInstTable[]){
    {asm_shl_rr, REG, REG, NO_SAME_REG_SIZE | SRC_CL_ONLY},
    {asm_shl_imr, IMMEDIATE, REG},
    {NULL} },
  [SHR] = (const AsmInstTable[]){
    {asm_shr_rr, REG, REG, NO_SAME_REG_SIZE | SRC_CL_ONLY},
    {asm_shr_imr, IMMEDIATE, REG},
    {NULL} },
  [SAR] = (const AsmInstTable[]){
    {asm_sar_rr, REG, REG, NO_SAME_REG_SIZE | SRC_CL_ONLY},
    {asm_sar_imr, IMMEDIATE, REG},
    {NULL} },
  [CMP] = (const AsmInstTable[]){
    {asm_cmp_rr, REG, REG},
    {asm_cmp_imr, IMMEDIATE, REG},
    {NULL} },
  [TEST] = (const AsmInstTable[]){
    {asm_test_rr, REG, REG},
    {NULL} },
  [CWTL] = (const AsmInstTable[]){ {asm_cwtl, NOOPERAND, NOOPERAND}, {NULL} },
  [CLTD] = (const AsmInstTable[]){ {asm_cltd, NOOPERAND, NOOPERAND}, {NULL} },
  [CQTO] = (const AsmInstTable[]){ {asm_cqto, NOOPERAND, NOOPERAND}, {NULL} },
  [SETO] = table_set,  [SETNO] = table_set,  [SETB] = table_set,  [SETAE] = table_set,
  [SETE] = table_set,  [SETNE] = table_set,  [SETBE] = table_set,  [SETA] = table_set,
  [SETS] = table_set,  [SETNS] = table_set,  [SETP] = table_set,  [SETNP] = table_set,
  [SETL] = table_set,  [SETGE] = table_set,  [SETLE] = table_set,  [SETG] = table_set,
  [JMP] = (const AsmInstTable[]){
    {asm_jmp_d, DIRECT, NOOPERAND},
    {asm_jmp_der, DEREF_REG, NOOPERAND},
    {asm_jmp_dei, DEREF_INDIRECT, NOOPERAND},
    {asm_jmp_deii, DEREF_INDIRECT_WITH_INDEX, NOOPERAND},
    {NULL} },
  [JO] = table_jxx,  [JNO] = table_jxx,  [JB] = table_jxx,  [JAE] = table_jxx,
  [JE] = table_jxx,  [JNE] = table_jxx,  [JBE] = table_jxx,  [JA] = table_jxx,
  [JS] = table_jxx,  [JNS] = table_jxx,  [JP] = table_jxx,  [JNP] = table_jxx,
  [JL] = table_jxx,  [JGE] = table_jxx,  [JLE] = table_jxx,  [JG] = table_jxx,
  [CALL] = (const AsmInstTable[]){
    {asm_call_d, DIRECT, NOOPERAND},
    {asm_call_der, DEREF_REG, NOOPERAND},
    {NULL} },
  [RET] = (const AsmInstTable[]){ {asm_ret, NOOPERAND, NOOPERAND}, {NULL} },
  [PUSH] = (const AsmInstTable[]){
    {asm_push_r, REG, NOOPERAND, SRC_REG64_ONLY},
    {asm_push_im, IMMEDIATE, NOOPERAND},
    {NULL} },
  [POP] = (const AsmInstTable[]){ {asm_pop_r, REG, NOOPERAND, SRC_REG64_ONLY}, {NULL} },
  [INT] = (const AsmInstTable[]){ {asm_int_im, IMMEDIATE, NOOPERAND}, {NULL} },
  [SYSCALL] = (const AsmInstTable[]){ {asm_syscall, NOOPERAND, NOOPERAND}, {NULL} },
  [MOVSD] = (const AsmInstTable[]){
    {asm_movsd_xx, REG_XMM, REG_XMM},
    {asm_movsd_ix, INDIRECT, REG_XMM},
    {asm_movsd_xi, REG_XMM, INDIRECT},
    {NULL} },
  [MOVSS] = (const AsmInstTable[]){
    {asm_movss_xx, REG_XMM, REG_XMM},
    {asm_movss_ix, INDIRECT, REG_XMM},
    {asm_movss_xi, REG_XMM, INDIRECT},
    {NULL} },
  [ADDSD] = (const AsmInstTable[]){ {asm_addsd_xx, REG_XMM, REG_XMM}, {NULL} },
  [ADDSS] = (const AsmInstTable[]){ {asm_addss_xx, REG_XMM, REG_XMM}, {NULL} },
  [SUBSD] = (const AsmInstTable[]){ {asm_subsd_xx, REG_XMM, REG_XMM}, {NULL} },
  [SUBSS] = (const AsmInstTable[]){ {asm_subss_xx, REG_XMM, REG_XMM}, {NULL} },
  [MULSD] = (const AsmInstTable[]){ {asm_mulsd_xx, REG_XMM, REG_XMM}, {NULL} },
  [MULSS] = (const AsmInstTable[]){ {asm_mulss_xx, REG_XMM, REG_XMM}, {NULL} },
  [DIVSD] = (const AsmInstTable[]){ {asm_divsd_xx, REG_XMM, REG_XMM}, {NULL} },
  [DIVSS] = (const AsmInstTable[]){ {asm_divss_xx, REG_XMM, REG_XMM}, {NULL} },
  [XORPD] = (const AsmInstTable[]){ {asm_xorpd_xx, REG_XMM, REG_XMM}, {NULL} },
  [XORPS] = (const AsmInstTable[]){ {asm_xorps_xx, REG_XMM, REG_XMM}, {NULL} },
  [UCOMISD] = (const AsmInstTable[]){ {asm_ucomisd_xx, REG_XMM, REG_XMM}, {NULL} },
  [UCOMISS] = (const AsmInstTable[]){ {asm_ucomiss_xx, REG_XMM, REG_XMM}, {NULL} },
  [CVTSI2SD] = (const AsmInstTable[]){ {asm_cvtsi2sd_rx, REG, REG_XMM}, {NULL} },
  [CVTSI2SS] = (const AsmInstTable[]){ {asm_cvtsi2ss_rx, REG, REG_XMM}, {NULL} },
  [CVTTSD2SI] = (const AsmInstTable[]){ {asm_cvttsd2si_xr, REG_XMM, REG}, {NULL} },
  [CVTTSS2SI] = (const AsmInstTable[]){ {asm_cvttss2si_xr, REG_XMM, REG}, {NULL} },
  [CVTSD2SS] = (const AsmInstTable[]){ {asm_cvtsd2ss_xx, REG_XMM, REG_XMM}, {NULL} },
  [CVTSS2SD] = (const AsmInstTable[]){ {asm_cvtss2sd_xx, REG_XMM, REG_XMM}, {NULL} },
  [SQRTSD] = (const AsmInstTable[]){ {asm_sqrtsd_xx, REG_XMM, REG_XMM}, {NULL} },
};

void assemble_inst(Inst *inst, const ParseInfo *info, Code *code) {
  code->flag = 0;
  code->len = 0;

  const AsmInstTable *pt = NULL;
  if (inst->op < (enum Opcode)ARRAY_SIZE(table) && table[inst->op] != NULL) {
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

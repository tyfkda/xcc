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
  if (sno >= 8 || dno >= 8 ||
      (size == REG8 && (sno >= 4 || dno >= 4)) ||
      size == REG64)
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

static unsigned char *put_rex_indirect_with_index(
    unsigned char *p, int base_reg, int index_reg, int dst_reg,
    unsigned char opcode, unsigned char op2, long offset, int scale)
{
  *p++ = 0x48 | ((base_reg & 8) >> 3) | ((index_reg & 8) >> 2) | ((dst_reg & 8) >> 1);
  *p++ = opcode | 1;

  int b = base_reg & 7;
  int i = index_reg & 7;
  int d = dst_reg & 7;

  char scale_bit = kPow2Table[scale];

  unsigned char code = (offset == 0 && b != RBP - RAX) ? op2 : is_im8(offset) ? (unsigned char)0x40 : (unsigned char)0x80;
  *p++ = code | (d << 3) | 0x04;
  *p++ = b | (i << 3) | (scale_bit << 6);
  //if (b == RSP - RAX)
  //  *p++ = 0x24;

  if (offset == 0 && b != RBP - RAX) {
    ;
  } else if (is_im8(offset)) {
    *p++ = IM8(offset);
  } else if (is_im32(offset)) {
    PUT_CODE(p, IM32(offset));
    p += 4;
  }
  return p;
}

static bool assemble_mov(Inst *inst, const ParseInfo *info, Code *code) {
  unsigned char *p = code->buf;

  if (inst->src.type == REG && inst->dst.type == REG) {
    if (inst->dst.reg.size != inst->src.reg.size)
      return assemble_error(info, "Different source and destination register size");

    enum RegSize size = inst->src.reg.size;
    p = put_rex2(p, size, opr_regno(&inst->src.reg), opr_regno(&inst->dst.reg),
                 size == REG8 ? 0x88 : 0x89);
  } else if (inst->src.type == IMMEDIATE && inst->dst.type == REG) {
    enum RegSize size = inst->dst.reg.size;
    if (size == REG64 && is_im32(inst->src.immediate)) {
      int d = inst->dst.reg.no;
      int pre = !inst->dst.reg.x ? 0x48 : 0x49;
      MAKE_CODE(inst, code, pre, 0xc7, 0xc0 | d, IM32(inst->src.immediate));
      return true;
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
  } else if (inst->src.type == INDIRECT && inst->dst.type == REG) {
    if (inst->src.indirect.offset->kind == EX_FIXNUM) {
      long offset = inst->src.indirect.offset->fixnum;
      enum RegSize size = inst->dst.reg.size;
      if (inst->src.indirect.reg.no == NOREG) {
        int dno = opr_regno(&inst->dst.reg);
        short prefix = size == REG16 ? 0x66 : size == REG64 ? 0x48 : -1;
        short buf[] = {
          prefix,
          size == REG8 ? 0x8a : 0x8b,
          ((dno & 7) << 3) | 0x04,
          0x25,
        };
        p = put_code_filtered(p, buf, ARRAY_SIZE(buf));

        if (is_im32(offset)) {
          PUT_CODE(p, IM32(offset));
          p += 4;
        }
      } else if (inst->src.indirect.reg.no != RIP) {
        enum RegSize size = inst->dst.reg.size;
        p = put_rex_indirect(
            p, size,
            opr_regno(&inst->dst.reg),
            opr_regno(&inst->src.indirect.reg),
            0x8a, 0x00, offset);
      }
    }
  } else if (inst->src.type == REG && inst->dst.type == INDIRECT) {
    if (inst->dst.indirect.offset->kind == EX_FIXNUM) {
      long offset = inst->dst.indirect.offset->fixnum;
      enum RegSize size = inst->src.reg.size;
      if (inst->dst.indirect.reg.no == NOREG) {
        int sno = opr_regno(&inst->src.reg);
        short prefix = size == REG16 ? 0x66 : size == REG64 ? 0x48 : -1;
        short buf[] = {
          prefix,
          size == REG8 ? 0x88 : 0x89,
          ((sno & 7) << 3) | 0x04,
          0x25,
        };
        p = put_code_filtered(p, buf, ARRAY_SIZE(buf));

        if (is_im32(offset)) {
          PUT_CODE(p, IM32(offset));
          p += 4;
        }
      } else if (inst->dst.indirect.reg.no != RIP) {
        p = put_rex_indirect(
            p, size,
            opr_regno(&inst->src.reg),
            opr_regno(&inst->dst.indirect.reg),
            0x88, 0x00, offset);
      }
    }
  } else if (inst->src.type == INDIRECT_WITH_INDEX && inst->dst.type == REG) {
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
      p = put_code_filtered(p, buf, ARRAY_SIZE(buf));

      if (offset != 0) {
        if (is_im8(offset)) {
          *p++ = IM8(offset);
        } else {
          PUT_CODE(p, IM32(offset));
          p += 4;
        }
      }
    }
  }

  if (p > code->buf) {
    code->inst = inst;
    code->len = p - code->buf;
    assert((size_t)code->len <= sizeof(code->buf));
    return true;
  }

  return assemble_error(info, "Illegal operand");
}

#ifndef __NO_FLONUM
static unsigned char *assemble_movsd(Inst *inst, Code *code, bool single) {
  unsigned char *p = code->buf;
  unsigned char prefix = single ? 0xf3 : 0xf2;
  if (inst->src.type == REG_XMM && inst->dst.type == REG_XMM) {
    unsigned char sno = inst->src.regxmm - XMM0;
    unsigned char dno = inst->dst.regxmm - XMM0;
    short buf[] = {
      prefix,
      sno >= 8 || dno >= 8 ? (unsigned char)0x40 | ((sno & 8) >> 3) | ((dno & 8) >> 1) : -1,
      0x0f,
      0x10,
      (unsigned char)0xc0 | ((dno & 7) << 3) | (sno & 7),
    };
    p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
  } else if (inst->src.type == INDIRECT && inst->dst.type == REG_XMM) {
    if (inst->src.indirect.offset->kind == EX_FIXNUM) {
      if (inst->src.indirect.reg.no != NOREG && inst->src.indirect.reg.no != RIP) {
        long offset = inst->src.indirect.offset->fixnum;
        unsigned char sno = opr_regno(&inst->src.indirect.reg);
        unsigned char dno = inst->dst.regxmm - XMM0;
        int d = dno & 7;
        int s = sno & 7;
        unsigned char code = (offset == 0 && s != RBP - RAX) ? (unsigned char)0x00 : is_im8(offset) ? (unsigned char)0x40 : (unsigned char)0x80;

        short buf[] = {
          prefix,
          sno >= 8 || dno >= 8 ? (unsigned char)0x40 | ((sno & 8) >> 3) | ((dno & 8) >> 1) : -1,
          0x0f,
          0x10,
          code | s | (d << 3),
          s == RSP - RAX ? 0x24 : -1,
        };
        p = put_code_filtered(p, buf, ARRAY_SIZE(buf));

        if (offset == 0 && s != RBP - RAX) {
          ;
        } else if (is_im8(offset)) {
          *p++ = IM8(offset);
        } else if (is_im32(offset)) {
          PUT_CODE(p, IM32(offset));
          p += 4;
        }
      }
    }
  } else if (inst->src.type == REG_XMM && inst->dst.type == INDIRECT) {
    if (inst->dst.indirect.offset->kind == EX_FIXNUM) {
      if (inst->dst.indirect.reg.no != NOREG && inst->dst.indirect.reg.no != RIP) {
        long offset = inst->dst.indirect.offset->fixnum;
        unsigned char sno = inst->src.regxmm - XMM0;
        unsigned char dno = opr_regno(&inst->dst.indirect.reg);
        int d = dno & 7;
        int s = sno & 7;
        unsigned char code = (offset == 0 && d != RBP - RAX) ? (unsigned char)0x00 : is_im8(offset) ? (unsigned char)0x40 : (unsigned char)0x80;

        short buf[] = {
          prefix,
          sno >= 8 || dno >= 8 ? (unsigned char)0x40 | ((dno & 8) >> 3) | ((sno & 8) >> 1) : -1,
          0x0f,
          0x11,
          code | d | (s << 3),
          d == RSP - RAX ? 0x24 : -1,
        };
        p = put_code_filtered(p, buf, ARRAY_SIZE(buf));

        if (offset == 0 && d != RBP - RAX) {
          ;
        } else if (is_im8(offset)) {
          *p++ = IM8(offset);
        } else if (is_im32(offset)) {
          PUT_CODE(p, IM32(offset));
          p += 4;
        }
      }
    }
  }

  return p;
}

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

static unsigned char *assemble_cvtsi2sd(Inst *inst, Code *code, bool single) {
  unsigned char *p = code->buf;
  unsigned char prefix = single ? 0xf3 : 0xf2;
  if (inst->src.type == REG && inst->dst.type == REG_XMM) {
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
  }
  return p;
}

static unsigned char *assemble_cvttsd2si(Inst *inst, Code *code, bool single) {
  unsigned char *p = code->buf;
  unsigned char prefix = single ? 0xf3 : 0xf2;
  if (inst->src.type == REG_XMM && inst->dst.type == REG) {
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
  }
  return p;
}

static unsigned char *assemble_cvtsd2ss(Inst *inst, Code *code, bool single) {
  unsigned char *p = code->buf;
  unsigned char prefix = single ? 0xf3 : 0xf2;
  if (inst->src.type == REG_XMM && inst->dst.type == REG_XMM) {
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
  }
  return p;
}
#endif

bool assemble_inst(Inst *inst, const ParseInfo *info, Code *code) {
  unsigned char *p = code->buf;

  code->flag = 0;
  code->len = 0;

  switch(inst->op) {
  case NOOP:
    return true;
  case MOV:
    return assemble_mov(inst, info, code);
  case MOVB:
  case MOVW:
  case MOVL:
  case MOVQ:
    if (inst->src.type == IMMEDIATE && inst->dst.type == INDIRECT) {
      long offset = inst->dst.indirect.offset->fixnum;
      unsigned char sno = 0;
      unsigned char dno = opr_regno(&inst->dst.indirect.reg);
      unsigned char code = (offset == 0 && (dno & 7) != RBP - RAX) ? 0x00 : is_im8(offset) ? (unsigned char)0x40 : (unsigned char)0x80;
      short buf[] = {
        inst->op == MOVW ? 0x66 : (inst->op == MOVQ || dno >= 8) ? 0x48 | ((dno & 8) >> 3) | ((sno & 8) >> 1) : -1,
        0xc6 | (inst->op == MOVB ? 0 : 1),
        code | (dno & 7) | ((sno & 7) << 3),
        (dno & 7) == RSP - RAX ? 0x24 : -1,
      };

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
    }
    break;
  case MOVSX:
  case MOVZX:
    if (inst->src.type == REG && inst->dst.type == REG) {
      int s = inst->src.reg.no;
      int d = inst->dst.reg.no;
      unsigned char op = inst->op == MOVZX ? 0xb6 : 0xbe;
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
          return true;
        case REG32:
          if (opr_reg8(&inst->src.reg) && !inst->dst.reg.x) {
            MAKE_CODE(inst, code, 0x0f, op, 0xc0 + s + d * 8);
          } else {
            int pre = (!inst->src.reg.x ? 0x40 : 0x41) + (!inst->dst.reg.x ? 0 : 4);
            MAKE_CODE(inst, code, pre, 0x0f, op, 0xc0 + s + d * 8);
          }
          return true;
        case REG64:
          {
            int pre = (!inst->src.reg.x ? 0x48 : 0x49) + (!inst->dst.reg.x ? 0 : 4);
            MAKE_CODE(inst, code, pre, 0x0f, op, 0xc0 + s + d * 8);
            return true;
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
          return true;
        case REG64:
          {
            int pre = (!inst->src.reg.x ? 0x48 : 0x49) + (!inst->dst.reg.x ? 0 : 4);
            MAKE_CODE(inst, code, pre, 0x0f, op | 1, 0xc0 + s + d * 8);
            return true;
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
          return true;
        }
        break;
      default:
        break;
      }
      return assemble_error(info, "Illegal operand");
    }
    break;
  case LEA:
    if (inst->src.type == INDIRECT && inst->dst.type == REG) {
      if (inst->dst.reg.size != REG64)
        return assemble_error(info, "64 bit register expected for destination");

      assert(inst->src.indirect.offset != NULL);
      if (inst->src.indirect.reg.no == NOREG) {
        //
      } else if (inst->src.indirect.reg.no != RIP) {
        if (inst->src.indirect.offset->kind == EX_FIXNUM) {
          long offset = inst->src.indirect.offset->fixnum;
          enum RegSize size = inst->dst.reg.size;
          p = put_rex_indirect(
              p, size,
              opr_regno(&inst->dst.reg),
              opr_regno(&inst->src.indirect.reg),
              0x8c, 0x00, offset);
        }
      } else {
        if (inst->src.indirect.offset->kind != EX_FIXNUM) {
          int pre = !inst->dst.reg.x ? 0x48 : 0x4c;
          int d = inst->dst.reg.no;
          MAKE_CODE(inst, code, pre, 0x8d, 0x05 | (d << 3), IM32(0));
          return true;
        }
      }
    } else if (inst->src.type == INDIRECT_WITH_INDEX && inst->dst.type == REG) {
      if (inst->dst.reg.size != REG64)
        return assemble_error(info, "64 bit register expected for destination");

      Expr *offset_expr = inst->src.indirect_with_index.offset;
      Expr *scale_expr = inst->src.indirect_with_index.scale;
      if ((offset_expr == NULL || offset_expr->kind == EX_FIXNUM) &&
          (scale_expr == NULL || scale_expr->kind == EX_FIXNUM)) {
        long offset = offset_expr != NULL ? offset_expr->fixnum : 0;
        long scale = scale_expr != NULL ? scale_expr->fixnum : 1;
        if (is_im32(offset) && 1 <= scale && scale <= 8 && IS_POWER_OF_2(scale)) {
          const Reg *base_reg = &inst->src.indirect_with_index.base_reg;
          const Reg *index_reg = &inst->src.indirect_with_index.index_reg;
          p = put_rex_indirect_with_index(
              p,
              opr_regno(base_reg),
              opr_regno(index_reg),
              opr_regno(&inst->dst.reg),
              0x8c, 0x00, offset, scale);
        }
      }
    }
    break;
  case ADD:
    if (inst->src.type == REG && inst->dst.type == REG) {
      if (inst->dst.reg.size != inst->src.reg.size)
        return assemble_error(info, "Different source and destination register size");

      enum RegSize size = inst->src.reg.size;
      p = put_rex2(p, size, opr_regno(&inst->src.reg), opr_regno(&inst->dst.reg),
                   size == REG8 ? 0x00 : 0x01);
    } else if (inst->src.type == IMMEDIATE && inst->dst.type == REG) {
      long value = inst->src.immediate;
      if (is_im32(value)) {
        bool im8 = is_im8(value);
        enum RegSize size = inst->dst.reg.size;
        int d = opr_regno(&inst->dst.reg);
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
      }
    } else if (inst->src.type == INDIRECT && inst->dst.type == REG) {
      if (inst->src.indirect.offset->kind == EX_FIXNUM && inst->src.indirect.reg.no != RIP) {
        long offset = inst->src.indirect.offset->fixnum;
        enum RegSize size = inst->dst.reg.size;
        p = put_rex_indirect(
            p, size,
            opr_regno(&inst->dst.reg),
            opr_regno(&inst->src.indirect.reg),
            0x02, 0x00, offset);
      }
    } else if (inst->src.type == INDIRECT_WITH_INDEX && inst->dst.type == REG) {
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
      p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
    }
    break;
  case ADDQ:
    if (inst->src.type == IMMEDIATE && inst->dst.type == INDIRECT) {
      if (inst->dst.indirect.offset->kind == EX_FIXNUM &&
          inst->dst.indirect.reg.no != NOREG) {
        long value = inst->src.immediate;
        if (is_im32(value)) {
          long offset = inst->dst.indirect.offset->fixnum;
          unsigned char sno = 0;
          unsigned char dno = opr_regno(&inst->dst.indirect.reg);
          unsigned char code = (offset == 0 && (dno & 7) != RBP - RAX) ? 0x00 : is_im8(offset) ? (unsigned char)0x40 : (unsigned char)0x80;
          short buf[] = {
            (unsigned char)0x48 | ((dno & 8) >> 3) | ((sno & 8) >> 1),
            (is_im8(value) ? 0x83 : 0x81),
            code | (dno & 7) | ((sno & 7) << 3),
            (dno & 7) == RSP - RAX ? 0x24 : -1,
          };
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
        }
      }
    }
    break;
  case SUB:
    if (inst->src.type == REG && inst->dst.type == REG) {
      if (inst->dst.reg.size != inst->src.reg.size)
        return assemble_error(info, "Different source and destination register size");

      enum RegSize size = inst->src.reg.size;
      p = put_rex2(p, size, opr_regno(&inst->src.reg), opr_regno(&inst->dst.reg),
                   size == REG8 ? 0x28 : 0x29);
    } else if (inst->src.type == IMMEDIATE && inst->dst.type == REG) {
      long value = inst->src.immediate;
      if (is_im32(value)) {
        bool im8 = is_im8(value);
        enum RegSize size = inst->dst.reg.size;
        int d = opr_regno(&inst->dst.reg);
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
      }
    } else if (inst->src.type == INDIRECT && inst->dst.type == REG) {
      if (inst->src.indirect.offset->kind == EX_FIXNUM && inst->src.indirect.reg.no != RIP) {
        long offset = inst->src.indirect.offset->fixnum;
        enum RegSize size = inst->dst.reg.size;
        p = put_rex_indirect(
            p, size,
            opr_regno(&inst->dst.reg),
            opr_regno(&inst->src.indirect.reg),
            0x2a, 0x00, offset);
      }
    } else if (inst->src.type == INDIRECT_WITH_INDEX && inst->dst.type == REG) {
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
      p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
    }
    break;
  case SUBQ:
    if (inst->src.type == IMMEDIATE && inst->dst.type == INDIRECT) {
      if (inst->dst.indirect.offset->kind == EX_FIXNUM &&
          inst->dst.indirect.reg.no != NOREG) {
        long value = inst->src.immediate;
        if (is_im32(value)) {
          long offset = inst->dst.indirect.offset->fixnum;
          unsigned char sno = 0;
          unsigned char dno = opr_regno(&inst->dst.indirect.reg);
          unsigned char code = (offset == 0 && (dno & 7) != RBP - RAX) ? 0x28 : is_im8(offset) ? (unsigned char)0x40 : (unsigned char)0x80;
          short buf[] = {
            (unsigned char)0x48 | ((dno & 8) >> 3) | ((sno & 8) >> 1),
            (is_im8(value) ? 0x83 : 0x81),
            code | (dno & 7) | ((sno & 7) << 3),
            (dno & 7) == RSP - RAX ? 0x24 : -1,
          };
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
        }
      }
    }
    break;
  case MUL:
    if (inst->src.type == NOOPERAND || inst->dst.type != NOOPERAND)
      return assemble_error(info, "Illegal operand");

    if (inst->src.type == REG) {
      enum RegSize size = inst->src.reg.size;
      p = put_rex0(p, size, 0, opr_regno(&inst->src.reg),
                   0xf6 | (size == REG8 ? 0 : 1));
      *p++ = 0xe0 | inst->src.reg.no;
    }
    break;
  case DIV:
    if (inst->src.type == NOOPERAND || inst->dst.type != NOOPERAND)
      return assemble_error(info, "Illegal operand");

    if (inst->src.type == REG) {
      enum RegSize size = inst->src.reg.size;
      p = put_rex0(p, size, 0, opr_regno(&inst->src.reg),
                   0xf6 | (size == REG8 ? 0 : 1));
      *p++ = 0xf0 | inst->src.reg.no;
    }
    break;
  case IDIV:
    if (inst->src.type == NOOPERAND || inst->dst.type != NOOPERAND)
      return assemble_error(info, "Illegal operand");

    if (inst->src.type == REG) {
      enum RegSize size = inst->src.reg.size;
      p = put_rex0(p, size, 0, opr_regno(&inst->src.reg),
                   0xf6 | (size == REG8 ? 0 : 1));
      *p++ = 0xf8 | inst->src.reg.no;
    }
    break;
  case NEG:
    if (inst->src.type == NOOPERAND || inst->dst.type != NOOPERAND)
      return assemble_error(info, "Illegal operand");

    if (inst->src.type == REG) {
      p = put_rex1(p, inst->src.reg.size,
                   0xd8, opr_regno(&inst->src.reg), 0xf7);
    }
    break;
  case NOT:
    if (inst->src.type == NOOPERAND || inst->dst.type != NOOPERAND)
      return assemble_error(info, "Illegal operand");

    if (inst->src.type == REG) {
      p = put_rex1(p, inst->src.reg.size,
                   0xd0, opr_regno(&inst->src.reg), 0xf7);
    }
    break;
  case INC:
    if (inst->src.type == NOOPERAND || inst->dst.type != NOOPERAND)
      return assemble_error(info, "Illegal operand");

    if (inst->src.type == REG) {
      p = put_rex1(p, inst->src.reg.size,
                   0xc0, opr_regno(&inst->src.reg), 0xff);
    }
    break;
  case INCB:
  case INCW:
  case INCL:
  case INCQ:
    if (inst->src.type == NOOPERAND || inst->dst.type != NOOPERAND)
      return assemble_error(info, "Illegal operand");

    if (inst->src.type == INDIRECT &&
        inst->src.indirect.reg.no != NOREG && inst->src.indirect.reg.no != RIP) {
      enum RegSize size = inst->op + (REG8 - INCB);
      long offset = inst->src.indirect.offset->fixnum;
      p = put_rex_indirect(
          p, size,
          0, opr_regno(&inst->src.indirect.reg),
          0xfe, 0x00, offset);
    }
    break;
  case DEC:
    if (inst->src.type == NOOPERAND || inst->dst.type != NOOPERAND)
      return assemble_error(info, "Illegal operand");

    if (inst->src.type == REG) {
      p = put_rex1(p, inst->src.reg.size,
                   0xc8, opr_regno(&inst->src.reg), 0xff);
    }
    break;
  case DECB:
  case DECW:
  case DECL:
  case DECQ:
    if (inst->src.type == NOOPERAND)
      return assemble_error(info, "Illegal operand");

    if (inst->src.type == INDIRECT &&
        inst->src.indirect.reg.no != NOREG && inst->src.indirect.reg.no != RIP) {
      enum RegSize size = inst->op + (REG8 - DECB);
      long offset = inst->src.indirect.offset->fixnum;
      p = put_rex_indirect(
          p, size,
          1, opr_regno(&inst->src.indirect.reg),
          0xfe, 0x08, offset);
    }
    break;
  case AND:
    if (inst->src.type == REG && inst->dst.type == REG) {
      if (inst->dst.reg.size != inst->src.reg.size)
        return assemble_error(info, "Different source and destination register size");

      enum RegSize size = inst->dst.reg.size;
      p = put_rex2(p, size, opr_regno(&inst->src.reg), opr_regno(&inst->dst.reg),
                   size == REG8 ? 0x20 : 0x21);
    } else if (inst->src.type == IMMEDIATE && inst->dst.type == REG) {
      long value = inst->src.immediate;
      enum RegSize size = inst->dst.reg.size;
      if (is_im8(value) && (size != REG8 || opr_regno(&inst->dst.reg) != AL - AL)) {
        p = put_rex1(p, size,
                     0xe0, opr_regno(&inst->dst.reg),
                     0x83);
        *p++ = IM8(value);
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
      } else {
        return assemble_error(info, "Too large constant");
      }
    }
    break;
  case OR:
    if (inst->src.type == REG && inst->dst.type == REG) {
      if (inst->dst.reg.size != inst->src.reg.size)
        return assemble_error(info, "Different source and destination register size");

      enum RegSize size = inst->dst.reg.size;
      p = put_rex2(p, size, opr_regno(&inst->src.reg), opr_regno(&inst->dst.reg),
                   size == REG8 ? 0x08 : 0x09);
    } else if (inst->src.type == IMMEDIATE && inst->dst.type == REG) {
      long value = inst->src.immediate;
      enum RegSize size = inst->dst.reg.size;
      if (is_im8(value) && (size != REG8 || opr_regno(&inst->dst.reg) != AL - AL)) {
        p = put_rex1(p, size,
                     0xc8, opr_regno(&inst->dst.reg),
                     0x83);
        *p++ = IM8(value);
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
      } else {
        return assemble_error(info, "Too large constant");
      }
    }
    break;
  case XOR:
    if (inst->src.type == REG && inst->dst.type == REG) {
      if (inst->dst.reg.size != inst->src.reg.size)
        return assemble_error(info, "Different source and destination register size");

      enum RegSize size = inst->dst.reg.size;
      p = put_rex2(p, size, opr_regno(&inst->src.reg), opr_regno(&inst->dst.reg),
                   size == REG8 ? 0x30 : 0x31);
    } else if (inst->src.type == IMMEDIATE && inst->dst.type == REG) {
      long value = inst->src.immediate;
      enum RegSize size = inst->dst.reg.size;
      if (is_im8(value) && (size != REG8 || opr_regno(&inst->dst.reg) != AL - AL)) {
        p = put_rex1(p, size,
                     0xf0, opr_regno(&inst->dst.reg),
                     0x83);
        *p++ = IM8(value);
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
      } else {
        return assemble_error(info, "Too large constant");
      }
    }
    break;
  case SHL:
    if (inst->src.type == REG && inst->dst.type == REG) {
      if (opr_regno(&inst->src.reg) != CL - AL)
        return assemble_error(info, "`%cl` expected");

      enum RegSize size = inst->dst.reg.size;
      p = put_rex1(p, size, 0xe0, opr_regno(&inst->dst.reg),
                   size == REG8 ? 0xd2 : 0xd3);
    } else if (inst->src.type == IMMEDIATE && inst->dst.type == REG) {
      enum RegSize size = inst->dst.reg.size;
      if (inst->src.immediate == 1) {
        p = put_rex1(p, size, 0xe0, opr_regno(&inst->dst.reg),
                     size == REG8 ? 0xd0 : 0xd1);
      } else {
        p = put_rex1(p, size, 0xe0, opr_regno(&inst->dst.reg),
                     size == REG8 ? 0xc0 : 0xc1);
        *p++ = IM8(inst->src.immediate);
      }
    }
    break;
  case SHR:
    if (inst->src.type == REG && inst->dst.type == REG) {
      if (opr_regno(&inst->src.reg) != CL - AL)
        return assemble_error(info, "`%cl` expected");

      enum RegSize size = inst->dst.reg.size;
      p = put_rex1(p, size, 0xe8, opr_regno(&inst->dst.reg),
                   size == REG8 ? 0xd2 : 0xd3);
    } else if (inst->src.type == IMMEDIATE && inst->dst.type == REG) {
      enum RegSize size = inst->dst.reg.size;
      if (inst->src.immediate == 1) {
        p = put_rex1(p, size, 0xe8, opr_regno(&inst->dst.reg),
                     size == REG8 ? 0xd0 : 0xd1);
      } else {
        p = put_rex1(p, size, 0xe8, opr_regno(&inst->dst.reg),
                     size == REG8 ? 0xc0 : 0xc1);
        *p++ = IM8(inst->src.immediate);
      }
    }
    break;
  case SAR:
    if (inst->src.type == REG && inst->dst.type == REG) {
      if (opr_regno(&inst->src.reg) != CL - AL)
        return assemble_error(info, "`%cl` expected");

      enum RegSize size = inst->dst.reg.size;
      p = put_rex1(p, size, 0xf8, opr_regno(&inst->dst.reg),
                   size == REG8 ? 0xd2 : 0xd3);
    } else if (inst->src.type == IMMEDIATE && inst->dst.type == REG) {
      enum RegSize size = inst->dst.reg.size;
      if (inst->src.immediate == 1) {
        p = put_rex1(p, size, 0xf8, opr_regno(&inst->dst.reg),
                     size == REG8 ? 0xd0 : 0xd1);
      } else {
        p = put_rex1(p, size, 0xf8, opr_regno(&inst->dst.reg),
                     size == REG8 ? 0xc0 : 0xc1);
        *p++ = IM8(inst->src.immediate);
      }
    }
    break;
  case CMP:
    if (inst->src.type == REG && inst->dst.type == REG) {
      if (inst->dst.reg.size != inst->src.reg.size)
        return assemble_error(info, "Different source and destination register size");

      enum RegSize size = inst->src.reg.size;
      p = put_rex2(p, size, opr_regno(&inst->src.reg), opr_regno(&inst->dst.reg),
                   size == REG8 ? 0x38 : 0x39);
    } else if (inst->src.type == IMMEDIATE && inst->dst.type == REG) {
      long value = inst->src.immediate;
      if (is_im32(value)) {
        enum RegSize size = inst->dst.reg.size;
        bool im8 = is_im8(value);
        int d = opr_regno(&inst->dst.reg);
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
      }
    }
    break;
  case TEST:
    if (inst->src.type == REG && inst->dst.type == REG) {
      if (inst->dst.reg.size != inst->src.reg.size)
        return assemble_error(info, "Different source and destination register size");

      enum RegSize size = inst->src.reg.size;
      p = put_rex2(p, size, opr_regno(&inst->src.reg), opr_regno(&inst->dst.reg),
                   size == REG8 ? 0x84 : 0x85);
    }
    break;
  case CWTL:
    if (inst->src.type != NOOPERAND || inst->dst.type != NOOPERAND)
      return assemble_error(info, "Illegal operand");

    MAKE_CODE(inst, code, 0x98);
    return true;
  case CLTD:
    if (inst->src.type != NOOPERAND || inst->dst.type != NOOPERAND)
      return assemble_error(info, "Illegal operand");

    MAKE_CODE(inst, code, 0x99);
    return true;
  case CQTO:
    if (inst->src.type != NOOPERAND || inst->dst.type != NOOPERAND)
      return assemble_error(info, "Illegal operand");

    MAKE_CODE(inst, code, 0x48, 0x99);
    return true;

  case SETO: case SETNO: case SETB:  case SETAE:
  case SETE: case SETNE: case SETBE: case SETA:
  case SETS: case SETNS: case SETP:  case SETNP:
  case SETL: case SETGE: case SETLE: case SETG:
    if (inst->dst.type != NOOPERAND || inst->src.type != REG ||
        inst->src.reg.size != REG8)
      return assemble_error(info, "Illegal opeand");

    p = put_rex0(p, REG8, 0, opr_regno(&inst->src.reg), 0x0f);
    *p++ = 0x90 | (inst->op - SETO);
    *p++ = 0xc0 | inst->src.reg.no;
    break;
  case PUSH:
    if (inst->dst.type == NOOPERAND) {
      if (inst->src.type == REG && inst->src.reg.size == REG64) {
        p = put_rex0(p, REG32, 0, opr_regno(&inst->src.reg),
                     0x50 | inst->src.reg.no);
        break;
      } else if (inst->src.type == IMMEDIATE) {
        long value = inst->src.immediate;
        if (is_im8(value)) {
          *p++ = 0x6a;
          *p++ = IM8(value);
          break;
        } else if (is_im32(value)) {
          *p++ = 0x68;
          PUT_CODE(p, IM32(value));
          p += 4;
          break;
        }
      }
    }
    return assemble_error(info, "Illegal operand");
  case POP:
    if (inst->dst.type != NOOPERAND || inst->src.type != REG ||
        inst->src.reg.size != REG64)
      return assemble_error(info, "Illegal operand");

    p = put_rex0(p, REG32, 0, opr_regno(&inst->src.reg),
                 0x58 | inst->src.reg.no);
    break;
  case JMP:
    if (inst->dst.type != NOOPERAND)
      return assemble_error(info, "Illegal operand");

    switch (inst->src.type) {
    case DIRECT:
      //MAKE_CODE(inst, code, 0xe9, IM32(0));
      MAKE_CODE(inst, code, 0xeb, IM8(0));  // Short jmp in default.
      return true;
    case DEREF_REG:
      {
        int s = inst->src.reg.no;
        short buf[] = {
          inst->src.reg.x ? 0x41 : -1,
          0xff, 0xe0 | s,
        };
        p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
      }
      break;
    case DEREF_INDIRECT:
      {
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
            p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
            if (offset != 0) {
              if (is_im8(offset)) {
                *p++ = IM8(offset);
              } else {
                PUT_CODE(p, IM32(offset));
                p += 4;
              }
            }
          }
        }
      }
      break;
    case DEREF_INDIRECT_WITH_INDEX:
      {
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
            p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
            if (offset != 0) {
              if (is_im8(offset)) {
                *p++ = IM8(offset);
              } else {
                PUT_CODE(p, IM32(offset));
                p += 4;
              }
            }
          }
        }
      }
      break;
    default: break;
    }
    break;
  case JO: case JNO: case JB:  case JAE:
  case JE: case JNE: case JBE: case JA:
  case JS: case JNS: case JP:  case JNP:
  case JL: case JGE: case JLE: case JG:
    if (inst->src.type != DIRECT || inst->dst.type != NOOPERAND)
      return assemble_error(info, "Illegal operand");

    //MAKE_CODE(inst, code, 0x0f, 0x80 + (inst->op - JO), IM32(0));
    MAKE_CODE(inst, code, 0x70 + (inst->op - JO), IM8(0));  // Short jmp in default.
    return true;
  case CALL:
    if (inst->dst.type != NOOPERAND)
      return assemble_error(info, "Illegal operand");

    if (inst->src.type == DIRECT) {
      MAKE_CODE(inst, code, 0xe8, IM32(0));
      return true;
    } else if (inst->src.type == DEREF_REG) {
      int s = inst->src.reg.no;
      if (!inst->src.reg.x) {
        MAKE_CODE(inst, code, 0xff, 0xd0 + s);
      } else {
        MAKE_CODE(inst, code, 0x41, 0xff, 0xd0 + s);
      }
      return true;
    }
    break;
  case RET:
    if (inst->src.type != NOOPERAND || inst->dst.type != NOOPERAND)
      return assemble_error(info, "Illegal operand");

    MAKE_CODE(inst, code, 0xc3);
    return true;
  case INT:
    if (inst->dst.type != NOOPERAND)
      return assemble_error(info, "Illegal operand");

    if (inst->src.type == IMMEDIATE) {
      long value = inst->src.immediate;
      MAKE_CODE(inst, code, 0xcd, IM8(value));
      return true;
    }
    return true;
  case SYSCALL:
    if (inst->src.type != NOOPERAND || inst->dst.type != NOOPERAND)
      return assemble_error(info, "Illegal operand");

    MAKE_CODE(inst, code, 0x0f, 0x05);
    return true;
#ifndef __NO_FLONUM
  case MOVSD:
    p = assemble_movsd(inst, code, false);
    break;
  case ADDSD:
    p = assemble_bop_sd(inst, code, false, 0x58);
    break;
  case SUBSD:
    p = assemble_bop_sd(inst, code, false, 0x5c);
    break;
  case MULSD:
    p = assemble_bop_sd(inst, code, false, 0x59);
    break;
  case DIVSD:
    p = assemble_bop_sd(inst, code, false, 0x5e);
    break;
  case UCOMISD:
    p = assemble_ucomisd(inst, code, false);
    break;
  case CVTSI2SD:
    p = assemble_cvtsi2sd(inst, code, false);
    break;
  case CVTTSD2SI:
    p = assemble_cvttsd2si(inst, code, false);
    break;
  case SQRTSD:
    if (inst->src.type == REG_XMM && inst->dst.type == REG_XMM) {
      unsigned char sno = inst->src.regxmm - XMM0;
      unsigned char dno = inst->dst.regxmm - XMM0;
      short buf[] = {
        0xf2,
        sno >= 8 || dno >= 8 ? (unsigned char)0x40 | ((sno & 8) >> 3) | ((dno & 8) >> 1) : -1,
        0x0f,
        0x51,
        (unsigned char)0xc0 | ((dno & 7) << 3) | (sno & 7),
      };
      p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
    }
    break;

  case MOVSS:
    p = assemble_movsd(inst, code, true);
    break;
  case ADDSS:
    p = assemble_bop_sd(inst, code, true, 0x58);
    break;
  case SUBSS:
    p = assemble_bop_sd(inst, code, true, 0x5c);
    break;
  case MULSS:
    p = assemble_bop_sd(inst, code, true, 0x59);
    break;
  case DIVSS:
    p = assemble_bop_sd(inst, code, true, 0x5e);
    break;
  case UCOMISS:
    p = assemble_ucomisd(inst, code, true);
    break;
  case CVTSI2SS:
    p = assemble_cvtsi2sd(inst, code, true);
    break;
  case CVTTSS2SI:
    p = assemble_cvttsd2si(inst, code, true);
    break;

  case CVTSD2SS:
    p = assemble_cvtsd2ss(inst, code, false);
    break;
  case CVTSS2SD:
    p = assemble_cvtsd2ss(inst, code, true);
    break;
#endif
  default:
    break;
  }

  if (p > code->buf) {
    code->inst = inst;
    code->len = p - code->buf;
    assert((size_t)code->len <= sizeof(code->buf));
    return true;
  }

  char buf[64];
  snprintf(buf, sizeof(buf), "op=%2d: not handled", inst->op);
  assemble_error(info, buf);
  return false;
}

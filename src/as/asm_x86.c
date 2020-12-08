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

#ifndef __NO_FLONUM
static unsigned char *put_code_filtered(unsigned char *p, const short *buf, size_t count) {
  for (size_t i = 0; i < count; ++i) {
    short c = *buf++;
    if (c >= 0)
      *p++ = c;
  }
  return p;
}
#endif

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

  static const char kScaleTable[] = {-1, 0, 1, -1, 2, -1, -1, -1, 3};
  char scale_bit = kScaleTable[scale];

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

static unsigned char *put_rex_imm_indirect(
    unsigned char *p, enum RegSize size, int reg, int indirect_reg,
    unsigned char opcode, unsigned char op2, long value, long offset)
{
  p = put_rex_indirect(p, size, reg, indirect_reg, opcode, op2, offset);
  if (is_im8(value)) {
    *p++ = IM8(value);
  } else {
    PUT_CODE(p, IM32(value));
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
      if (inst->src.indirect.reg.no != RIP) {
        long offset = inst->src.indirect.offset->fixnum;
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
      if (inst->dst.indirect.reg.no != RIP) {
        long offset = inst->dst.indirect.offset->fixnum;
        enum RegSize size = inst->src.reg.size;
        p = put_rex_indirect(
            p, size,
            opr_regno(&inst->src.reg),
            opr_regno(&inst->dst.indirect.reg),
            0x88, 0x00, offset);
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
static bool assemble_movsd(Inst *inst, const ParseInfo *info, Code *code) {
  unsigned char *p = code->buf;

  if (inst->src.type == REG_XMM && inst->dst.type == REG_XMM) {
    unsigned char sno = inst->src.regxmm - XMM0;
    unsigned char dno = inst->dst.regxmm - XMM0;
    short buf[] = {
      0xf2,
      sno >= 8 || dno >= 8 ? (unsigned char)0x40 | ((sno & 8) >> 3) | ((dno & 8) >> 1) : -1,
      0x0f,
      0x10,
      (unsigned char)0xc0 | ((dno & 7) << 3) | (sno & 7),
    };
    p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
  } else if (inst->src.type == INDIRECT && inst->dst.type == REG_XMM) {
    if (inst->src.indirect.offset->kind == EX_FIXNUM) {
      if (inst->src.indirect.reg.no != RIP) {
        long offset = inst->src.indirect.offset->fixnum;
        unsigned char sno = opr_regno(&inst->src.indirect.reg);
        unsigned char dno = inst->dst.regxmm - XMM0;
        int d = dno & 7;
        int s = sno & 7;
        unsigned char code = (offset == 0 && s != RBP - RAX) ? (unsigned char)0x00 : is_im8(offset) ? (unsigned char)0x40 : (unsigned char)0x80;

        short buf[] = {
          0xf2,
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
      if (inst->dst.indirect.reg.no != RIP) {
        long offset = inst->dst.indirect.offset->fixnum;
        unsigned char sno = inst->src.regxmm - XMM0;
        unsigned char dno = opr_regno(&inst->dst.indirect.reg);
        int d = dno & 7;
        int s = sno & 7;
        unsigned char code = (offset == 0 && d != RBP - RAX) ? (unsigned char)0x00 : is_im8(offset) ? (unsigned char)0x40 : (unsigned char)0x80;

        short buf[] = {
          0xf2,
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

  if (p > code->buf) {
    code->inst = inst;
    code->len = p - code->buf;
    assert((size_t)code->len <= sizeof(code->buf));
    return true;
  }

  return assemble_error(info, "Illegal operand");
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
  case MOVSX:
  case MOVZX:
    if (inst->src.type == REG && inst->dst.type == REG) {
      int s = inst->src.reg.no;
      int d = inst->dst.reg.no;
      unsigned char op = inst->op == MOVZX ? 0xb6 : 0xbe;
      switch (inst->src.reg.size) {
      case REG8:
        switch (inst->dst.reg.size) {
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
      if (inst->src.indirect.reg.no != RIP) {
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
      if (inst->src.indirect.offset->kind == EX_FIXNUM &&
          inst->src.indirect.reg.no != RIP) {
        long offset = inst->src.indirect.offset->fixnum;
        enum RegSize size = inst->dst.reg.size;
        p = put_rex_indirect(
            p, size,
            opr_regno(&inst->dst.reg),
            opr_regno(&inst->src.indirect.reg),
            0x02, 0x00, offset);
      }
    }
    break;
  case ADDQ:
    if (inst->src.type == IMMEDIATE && inst->dst.type == INDIRECT) {
      if (inst->dst.indirect.offset->kind == EX_FIXNUM) {
        long value = inst->src.immediate;
        if (is_im32(value)) {
          long offset = inst->dst.indirect.offset->fixnum;
          enum RegSize size = REG64;  // caz ADDQ
          p = put_rex_imm_indirect(
              p, size,
              0, opr_regno(&inst->dst.indirect.reg),
              (is_im8(value) ? 0x83 : 0x81), 0x00, value, offset);
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
    }
    break;
  case SUBQ:
    if (inst->src.type == IMMEDIATE && inst->dst.type == INDIRECT) {
      if (inst->dst.indirect.offset->kind == EX_FIXNUM) {
        long value = inst->src.immediate;
        if (is_im32(value)) {
          long offset = inst->dst.indirect.offset->fixnum;
          enum RegSize size = REG64;  // caz SUBQ
          p = put_rex_imm_indirect(
              p, size,
              0, opr_regno(&inst->dst.indirect.reg),
              (is_im8(value) ? 0x83 : 0x81), 0x28, value, offset);
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
        inst->src.indirect.reg.no != RIP) {
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
        inst->src.indirect.reg.no != RIP) {
      enum RegSize size = inst->op + (REG8 - DECB);
      long offset = inst->src.indirect.offset->fixnum;
      p = put_rex_indirect(
          p, size,
          0, opr_regno(&inst->src.indirect.reg),
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
                       0x81);
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
                       0x81);
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
                       0x81);
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
    if (inst->src.type != DIRECT || inst->dst.type != NOOPERAND)
      return assemble_error(info, "Illegal operand");
    //MAKE_CODE(inst, code, 0xe9, IM32(0));
    MAKE_CODE(inst, code, 0xeb, IM8(0));  // Short jmp in default.
    return true;
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
      int s = inst->src.deref_reg.no;
      if (!inst->src.deref_reg.x) {
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
    return assemble_movsd(inst, info, code);
  case ADDSD:
    if (inst->src.type == REG_XMM && inst->dst.type == REG_XMM) {
      unsigned char sno = inst->src.regxmm - XMM0;
      unsigned char dno = inst->dst.regxmm - XMM0;
      short buf[] = {
        0xf2,
        sno >= 8 || dno >= 8 ? (unsigned char)0x40 | ((sno & 8) >> 3) | ((dno & 8) >> 1) : -1,
        0x0f,
        0x58,
        (unsigned char)0xc0 | ((dno & 7) << 3) | (sno & 7),
      };
      p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
    }
    break;
  case SUBSD:
    if (inst->src.type == REG_XMM && inst->dst.type == REG_XMM) {
      unsigned char sno = inst->src.regxmm - XMM0;
      unsigned char dno = inst->dst.regxmm - XMM0;
      short buf[] = {
        0xf2,
        sno >= 8 || dno >= 8 ? (unsigned char)0x40 | ((sno & 8) >> 3) | ((dno & 8) >> 1) : -1,
        0x0f,
        0x5c,
        (unsigned char)0xc0 | ((dno & 7) << 3) | (sno & 7),
      };
      p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
    }
    break;
  case MULSD:
    if (inst->src.type == REG_XMM && inst->dst.type == REG_XMM) {
      unsigned char sno = inst->src.regxmm - XMM0;
      unsigned char dno = inst->dst.regxmm - XMM0;
      short buf[] = {
        0xf2,
        sno >= 8 || dno >= 8 ? (unsigned char)0x40 | ((sno & 8) >> 3) | ((dno & 8) >> 1) : -1,
        0x0f,
        0x59,
        (unsigned char)0xc0 | ((dno & 7) << 3) | (sno & 7),
      };
      p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
    }
    break;
  case DIVSD:
    if (inst->src.type == REG_XMM && inst->dst.type == REG_XMM) {
      unsigned char sno = inst->src.regxmm - XMM0;
      unsigned char dno = inst->dst.regxmm - XMM0;
      short buf[] = {
        0xf2,
        sno >= 8 || dno >= 8 ? (unsigned char)0x40 | ((sno & 8) >> 3) | ((dno & 8) >> 1) : -1,
        0x0f,
        0x5e,
        (unsigned char)0xc0 | ((dno & 7) << 3) | (sno & 7),
      };
      p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
    }
    break;
  case UCOMISD:
    if (inst->src.type == REG_XMM && inst->dst.type == REG_XMM) {
      unsigned char sno = inst->src.regxmm - XMM0;
      unsigned char dno = inst->dst.regxmm - XMM0;
      short buf[] = {
        0x66,
        sno >= 8 || dno >= 8 ? (unsigned char)0x40 | ((sno & 8) >> 3) | ((dno & 8) >> 1) : -1,
        0x0f,
        0x2e,
        (unsigned char)0xc0 | ((dno & 7) << 3) | (sno & 7),
      };
      p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
    }
    break;
  case CVTSI2SD:
    if (inst->src.type == REG && inst->dst.type == REG_XMM) {
      unsigned char sno = opr_regno(&inst->src.reg);
      unsigned char dno = inst->dst.regxmm - XMM0;
      short buf[] = {
        0xf2,
        sno >= 8 || dno >= 8 || inst->src.reg.size == REG64 ? (unsigned char)0x40 | ((sno & 8) >> 3) | ((dno & 8) >> 1) | (inst->src.reg.size == REG64 ? 8 : 0) : -1,
        0x0f,
        0x2a,
        (unsigned char)0xc0 | ((dno & 7) << 3) | (sno & 7),
      };
      p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
    }
    break;
  case CVTTSD2SI:
    if (inst->src.type == REG_XMM && inst->dst.type == REG) {
      unsigned char sno = inst->src.regxmm - XMM0;
      unsigned char dno = opr_regno(&inst->dst.reg);
      short buf[] = {
        0xf2,
        sno >= 8 || dno >= 8 || inst->dst.reg.size == REG64 ? (unsigned char)0x40 | ((sno & 8) >> 3) | ((dno & 8) >> 1) | (inst->dst.reg.size == REG64 ? 8 : 0) : -1,
        0x0f,
        0x2c,
        (unsigned char)0xc0 | ((dno & 7) << 3) | (sno & 7),
      };
      p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
    }
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

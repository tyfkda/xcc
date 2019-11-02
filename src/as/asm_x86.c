#include "asm_x86.h"

#include <assert.h>
#include <stdlib.h>  // exit
#include <string.h>  // memcpy

#include "inst.h"
#include "util.h"

#ifndef PUT_CODE
#define PUT_CODE(p, ...)  do { unsigned char buf[] = {__VA_ARGS__}; memcpy(p, buf, sizeof(buf)); } while (0)
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

static bool assemble_error(const char *rawline, const char *message) {
  fprintf(stderr, "%s\n", message);
  fprintf(stderr, "%s\n", rawline);
  return false;
}

static unsigned char *put_rex0(unsigned char *p, enum RegSize size,
                               int sno, int dno, unsigned char opcode)
{
  if (size == REG16)
    *p++ = 0x66;
  if (sno >= 8 || dno >= 8 ||
      (size == REG8 && (sno >= 4 || dno >= 4)) ||
      size == REG64)
    *p++ = 0x40 | ((dno & 8) >> 3) | ((sno & 8) >> 1) | (size != REG64 ? 0 : 8);
  *p++ = opcode;
  return p;
}

static unsigned char *put_rex2(unsigned char *p, enum RegSize size,
                               int sno, int dno, unsigned char opcode)
{
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

static bool assemble_mov(Inst *inst, const char *rawline, Code *code) {
  unsigned char *p = code->buf;

  if (inst->src.type == REG && inst->dst.type == REG) {
    if (inst->dst.reg.size != inst->src.reg.size)
      return assemble_error(rawline, "Different source and destination register size");

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
    if (inst->src.indirect.label == NULL) {
      if (inst->src.indirect.reg.no != RIP) {
        long offset = inst->src.indirect.offset;
        enum RegSize size = inst->dst.reg.size;
        p = put_rex_indirect(
            p, size,
            opr_regno(&inst->dst.reg),
            opr_regno(&inst->src.indirect.reg),
            0x8a, 0x00, offset);
      }
    }
  } else if (inst->src.type == REG && inst->dst.type == INDIRECT) {
    if (inst->dst.indirect.label == NULL) {
      if (inst->dst.indirect.reg.no != RIP) {
        long offset = inst->dst.indirect.offset;
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

  return assemble_error(rawline, "Illegal operand");
}

bool assemble_inst(Inst *inst, const char *rawline, Code *code) {
  unsigned char *p = code->buf;

  code->flag = 0;
  code->len = 0;

  switch(inst->op) {
  case NOOP:
    return true;
  case MOV:
    return assemble_mov(inst, rawline, code);
  case MOVSX:
    if (inst->src.type == REG && inst->dst.type == REG) {
      int s = inst->src.reg.no;
      int d = inst->dst.reg.no;
      switch (inst->src.reg.size) {
      case REG8:
        switch (inst->dst.reg.size) {
        case REG32:
          if (opr_reg8(&inst->src.reg) && !inst->dst.reg.x) {
            MAKE_CODE(inst, code, 0x0f, 0xbe, 0xc0 + s + d * 8);
          } else {
            int pre = (!inst->src.reg.x ? 0x40 : 0x41) + (!inst->dst.reg.x ? 0 : 4);
            MAKE_CODE(inst, code, pre, 0x0f, 0xbe, 0xc0 + s + d * 8);
          }
          return true;
        case REG64:
          {
            int pre = (!inst->src.reg.x ? 0x48 : 0x49) + (!inst->dst.reg.x ? 0 : 4);
            MAKE_CODE(inst, code, pre, 0x0f, 0xbe, 0xc0 + s + d * 8);
            return true;
          }
        default:
          break;
        }
      case REG16:
        switch (inst->dst.reg.size) {
        case REG32:
          if (!inst->src.reg.x && !inst->dst.reg.x) {
            MAKE_CODE(inst, code, 0x0f, 0xbf, 0xc0 + s + d * 8);
          } else {
            int pre = (!inst->src.reg.x ? 0x40 : 0x41) + (!inst->dst.reg.x ? 0 : 4);
            MAKE_CODE(inst, code, pre, 0x0f, 0xbf, 0xc0 + s + d * 8);
          }
          return true;
        case REG64:
          {
            int pre = (!inst->src.reg.x ? 0x48 : 0x49) + (!inst->dst.reg.x ? 0 : 4);
            MAKE_CODE(inst, code, pre, 0x0f, 0xbf, 0xc0 + s + d * 8);
            return true;
          }
        default:
          break;
        }
      case REG32:
        if (inst->dst.reg.size == REG64) {
          int pre = (!inst->src.reg.x ? 0x48 : 0x49) + (!inst->dst.reg.x ? 0 : 4);
          MAKE_CODE(inst, code, pre, 0x63, 0xc0 + s + d * 8);
          return true;
        }
      default:
        break;
      }
      return assemble_error(rawline, "Illegal operand");
    }
    break;
  case LEA:
    if (inst->src.type == INDIRECT && inst->dst.type == REG) {
      if (inst->dst.reg.size != REG64)
        return assemble_error(rawline, "64 bit register expected for destination");

      int d = inst->dst.reg.no;
      if (inst->src.indirect.reg.no != RIP) {
        if (inst->src.indirect.label == NULL) {
          long offset = inst->src.indirect.offset;
          enum RegSize size = inst->dst.reg.size;
          p = put_rex_indirect(
              p, size,
              opr_regno(&inst->dst.reg),
              opr_regno(&inst->src.indirect.reg),
              0x8c, 0x00, offset);
        }
      } else {
        int pre = !inst->dst.reg.x ? 0x48 : 0x4c;
        if (inst->src.indirect.offset == 0) {
          MAKE_CODE(inst, code, pre, 0x8d, 0x05 | (d << 3), IM32(-1));
          return true;
        }
      }
    }
    break;
  case ADD:
    if (inst->src.type == REG && inst->dst.type == REG) {
      if (inst->dst.reg.size != inst->src.reg.size)
        return assemble_error(rawline, "Different source and destination register size");

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
          p = put_rex2(p, size, 0, d, im8 ? 0x83 : 0x81);

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
      if (inst->src.indirect.label == NULL &&
          inst->src.indirect.reg.no != RIP) {
        long offset = inst->src.indirect.offset;
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
      if (inst->dst.indirect.label == NULL) {
        long value = inst->src.immediate;
        if (is_im32(value)) {
          long offset = inst->dst.indirect.offset;
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
        return assemble_error(rawline, "Different source and destination register size");

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
          p = put_rex2(p, size, 5, d, im8 ? 0x83 : 0x81);  // 0xe8 = 0xc0 | (5 << 3)

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
      if (inst->dst.indirect.label == NULL) {
        long value = inst->src.immediate;
        if (is_im32(value)) {
          long offset = inst->dst.indirect.offset;
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
      return assemble_error(rawline, "Illegal operand");

    if (inst->src.type == REG) {
      enum RegSize size = inst->src.reg.size;
      p = put_rex0(p, size, 0, opr_regno(&inst->src.reg),
                   0xf6 | (size == REG8 ? 0 : 1));
      *p++ = 0xe0 | inst->src.reg.no;
    }
    break;
  case IDIV:
    if (inst->src.type == NOOPERAND || inst->dst.type != NOOPERAND)
      return assemble_error(rawline, "Illegal operand");

    if (inst->src.type == REG) {
      enum RegSize size = inst->src.reg.size;
      p = put_rex0(p, size, 0, opr_regno(&inst->src.reg),
                   0xf6 | (size == REG8 ? 0 : 1));
      *p++ = 0xf8 | inst->src.reg.no;
    }
    break;
  case NEG:
    if (inst->src.type == NOOPERAND || inst->dst.type != NOOPERAND)
      return assemble_error(rawline, "Illegal operand");

    if (inst->src.type == REG) {
      p = put_rex2(p, inst->src.reg.size,
                   3, opr_regno(&inst->src.reg), 0xf7);  // 0xd8 = 0xc0 | (3 << 3)
    }
    break;
  case NOT:
    if (inst->src.type == NOOPERAND || inst->dst.type != NOOPERAND)
      return assemble_error(rawline, "Illegal operand");

    if (inst->src.type == REG) {
      p = put_rex2(p, inst->src.reg.size,
                   2, opr_regno(&inst->src.reg), 0xf7);  // 0xd0 = 0xc0 | (2 << 3)
    }
    break;
  case INC:
    if (inst->src.type == NOOPERAND || inst->dst.type != NOOPERAND)
      return assemble_error(rawline, "Illegal operand");

    if (inst->src.type == REG) {
      p = put_rex2(p, inst->src.reg.size,
                   0, opr_regno(&inst->src.reg), 0xff);
    }
    break;
  case INCL:
  case INCQ:
    if (inst->src.type == NOOPERAND || inst->dst.type != NOOPERAND)
      return assemble_error(rawline, "Illegal operand");

    if (inst->src.type == INDIRECT &&
        inst->src.indirect.reg.no != RIP) {
      enum RegSize size = inst->op + (REG32 - INCL);
      long offset = inst->src.indirect.offset;
      p = put_rex_indirect(
          p, size,
          0, opr_regno(&inst->src.indirect.reg),
          0xfe, 0x00, offset);
    }
    break;
  case DEC:
    if (inst->src.type == NOOPERAND || inst->dst.type != NOOPERAND)
      return assemble_error(rawline, "Illegal operand");

    if (inst->src.type == REG) {
      p = put_rex2(p, inst->src.reg.size,
                   1, opr_regno(&inst->src.reg), 0xff);  // 0xc8 = 0xc0 | (1 << 3)
    }
    break;
  case DECL:
  case DECQ:
    if (inst->src.type == NOOPERAND)
      return assemble_error(rawline, "Illegal operand");

    if (inst->src.type == INDIRECT &&
        inst->src.indirect.reg.no != RIP) {
      enum RegSize size = inst->op + (REG32 - DECL);
      long offset = inst->src.indirect.offset;
      p = put_rex_indirect(
          p, size,
          0, opr_regno(&inst->src.indirect.reg),
          0xfe, 0x08, offset);
    }
    break;
  case AND:
    if (inst->src.type == REG && inst->dst.type == REG) {
      if (inst->dst.reg.size != inst->src.reg.size)
        return assemble_error(rawline, "Different source and destination register size");

      enum RegSize size = inst->src.reg.size;
      p = put_rex2(p, size, opr_regno(&inst->src.reg), opr_regno(&inst->dst.reg),
                   size == REG8 ? 0x20 : 0x21);
    }
    break;
  case OR:
    if (inst->src.type == REG && inst->dst.type == REG) {
      if (inst->dst.reg.size != inst->src.reg.size)
        return assemble_error(rawline, "Different source and destination register size");

      enum RegSize size = inst->src.reg.size;
      p = put_rex2(p, size, opr_regno(&inst->src.reg), opr_regno(&inst->dst.reg),
                   size == REG8 ? 0x08 : 0x09);
    }
    break;
  case XOR:
    if (inst->src.type == REG && inst->dst.type == REG) {
      if (inst->dst.reg.size != inst->src.reg.size)
        return assemble_error(rawline, "Different source and destination register size");

      enum RegSize size = inst->src.reg.size;
      p = put_rex2(p, size, opr_regno(&inst->src.reg), opr_regno(&inst->dst.reg),
                   size == REG8 ? 0x30 : 0x31);
    }
    break;
  case SHL:
    if (inst->src.type == REG && inst->dst.type == REG) {
      if (opr_regno(&inst->src.reg) != CL - AL)
        return assemble_error(rawline, "`%cl` expected");

      enum RegSize size = inst->dst.reg.size;
      p = put_rex2(p, size, 4, opr_regno(&inst->dst.reg),  // 0xe0 = 0xc0 | (4 << 3)
                   size == REG8 ? 0xd2 : 0xd3);
    }
    break;
  case SHR:
    if (inst->src.type == REG && inst->dst.type == REG) {
      if (opr_regno(&inst->src.reg) != CL - AL)
        return assemble_error(rawline, "`%cl` expected");

      enum RegSize size = inst->dst.reg.size;
      p = put_rex2(p, size, 5, opr_regno(&inst->dst.reg),  // 0xe8 = 0xc0 | (5 << 3)
                   size == REG8 ? 0xd2 : 0xd3);
    }
    break;
  case CMP:
    if (inst->src.type == REG && inst->dst.type == REG) {
      if (inst->dst.reg.size != inst->src.reg.size)
        return assemble_error(rawline, "Different source and destination register size");

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
          p = put_rex0(p, size, 0, d, im8 ? 0x3c : 0x3d);
        else
          p = put_rex2(p, size, 7, d, im8 ? 0x83 : 0x81);  // 0xf8 = 0xc0 | (7 << 3)

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
        return assemble_error(rawline, "Different source and destination register size");

      enum RegSize size = inst->src.reg.size;
      p = put_rex2(p, size, opr_regno(&inst->src.reg), opr_regno(&inst->dst.reg),
                   size == REG8 ? 0x84 : 0x85);
    }
    break;
  case CLTD:
    if (inst->src.type != NOOPERAND || inst->dst.type != NOOPERAND)
      return assemble_error(rawline, "Illegal operand");

    MAKE_CODE(inst, code, 0x99);
    return true;
  case CQTO:
    if (inst->src.type != NOOPERAND || inst->dst.type != NOOPERAND)
      return assemble_error(rawline, "Illegal operand");

    MAKE_CODE(inst, code, 0x48, 0x99);
    return true;

  case SETO: case SETNO: case SETB:  case SETAE:
  case SETE: case SETNE: case SETBE: case SETA:
  case SETS: case SETNS: case SETP:  case SETNP:
  case SETL: case SETGE: case SETLE: case SETG:
    if (inst->dst.type != NOOPERAND || inst->src.type != REG ||
        inst->src.reg.size != REG8)
      return assemble_error(rawline, "Illegal opeand");

    p = put_rex0(p, REG8, 0, opr_regno(&inst->src.reg), 0x0f);
    *p++ = 0x90 | (inst->op - SETO);
    *p++ = 0xc0 | inst->src.reg.no;
    break;
  case PUSH:
    if (inst->dst.type != NOOPERAND || inst->src.type != REG ||
        inst->src.reg.size != REG64)
      return assemble_error(rawline, "Illegal operand");

    p = put_rex0(p, REG32, 0, opr_regno(&inst->src.reg),
                 0x50 | inst->src.reg.no);
    break;
  case POP:
    if (inst->dst.type != NOOPERAND || inst->src.type != REG ||
        inst->src.reg.size != REG64)
      return assemble_error(rawline, "Illegal operand");

    p = put_rex0(p, REG32, 0, opr_regno(&inst->src.reg),
                 0x58 | inst->src.reg.no);
    break;
  case JMP:
    if (inst->src.type != LABEL || inst->dst.type != NOOPERAND)
      return assemble_error(rawline, "Illegal operand");
    //MAKE_CODE(inst, code, 0xe9, IM32(-1));
    MAKE_CODE(inst, code, 0xeb, IM8(-1));  // Short jmp in default.
    return true;
  case JO: case JNO: case JB:  case JAE:
  case JE: case JNE: case JBE: case JA:
  case JS: case JNS: case JP:  case JNP:
  case JL: case JGE: case JLE: case JG:
    if (inst->src.type != LABEL || inst->dst.type != NOOPERAND)
      return assemble_error(rawline, "Illegal operand");

    //MAKE_CODE(inst, code, 0x0f, 0x80 + (inst->op - JO), IM32(-1));
    MAKE_CODE(inst, code, 0x70 + (inst->op - JO), IM8(-1));  // Short jmp in default.
    return true;
  case CALL:
    if (inst->dst.type != NOOPERAND)
      return assemble_error(rawline, "Illegal operand");

    if (inst->src.type == LABEL) {
      MAKE_CODE(inst, code, 0xe8, IM32(-1));
      return true;
    } if (inst->src.type == DEREF_REG) {
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
      return assemble_error(rawline, "Illegal operand");

    MAKE_CODE(inst, code, 0xc3);
    return true;
  case INT:
    if (inst->dst.type != NOOPERAND)
      return assemble_error(rawline, "Illegal operand");

    if (inst->src.type == IMMEDIATE) {
      long value = inst->src.immediate;
      MAKE_CODE(inst, code, 0xcd, IM8(value));
      return true;
    }
    return true;
  case SYSCALL:
    if (inst->src.type != NOOPERAND || inst->dst.type != NOOPERAND)
      return assemble_error(rawline, "Illegal operand");

    MAKE_CODE(inst, code, 0x0f, 0x05);
    return true;
  default:
    break;
  }

  if (p > code->buf) {
    code->inst = inst;
    code->len = p - code->buf;
    assert((size_t)code->len <= sizeof(code->buf));
    return true;
  }

  fprintf(stderr, "op=%2d: not handled\n", inst->op);
  return false;
}

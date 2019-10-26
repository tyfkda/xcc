#include "asm_x86.h"

#include <assert.h>
#include <stdlib.h>  // exit
#include <string.h>  // memcpy

#include "inst.h"
#include "util.h"

#ifndef MAKE_CODE
#define MAKE_CODE(inst, code, ...)  do { unsigned char buf[] = {__VA_ARGS__}; make_code(inst, code, buf, sizeof(buf)); } while (0)
#endif
#ifndef PUT_CODE
#define PUT_CODE(p, ...)  do { unsigned char buf[] = {__VA_ARGS__}; memcpy(p, buf, sizeof(buf)); } while (0)
#endif

#define IM8(x)   (x)
#define IM16(x)  (x), ((x) >> 8)
#define IM32(x)  (x), ((x) >> 8), ((x) >> 16), ((x) >> 24)
#define IM64(x)  (x), ((x) >> 8), ((x) >> 16), ((x) >> 24), ((x) >> 32), ((x) >> 40), ((x) >> 48), ((x) >> 56)

static void make_code(Inst *inst, Code *code, unsigned char *buf, int len) {
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

static bool assemble_mov(Inst *inst, const char *rawline, Code *code) {
  unsigned char *p = code->buf;

  if (inst->src.type == REG && inst->dst.type == REG) {
    if (inst->dst.u.reg.size != inst->src.u.reg.size)
      return assemble_error(rawline, "Different source and destination register size");

    enum RegSize size = inst->src.u.reg.size;
    p = put_rex2(p, size, opr_regno(&inst->src.u.reg), opr_regno(&inst->dst.u.reg),
                 size == REG8 ? 0x88 : 0x89);
  } else if (inst->src.type == IMMEDIATE && inst->dst.type == REG) {
    enum RegSize size = inst->dst.u.reg.size;
    if (size == REG64 && is_im32(inst->src.u.immediate)) {
      int d = inst->dst.u.reg.no;
      int pre = !inst->dst.u.reg.x ? 0x48 : 0x49;
      MAKE_CODE(inst, code, pre, 0xc7, 0xc0 | d, IM32(inst->src.u.immediate));
      return true;
    }

    p = put_rex0(p, size, 0, opr_regno(&inst->dst.u.reg),
                 0xb0 | (size == REG8 ? 0 : 8) | inst->dst.u.reg.no);
    switch (size) {
    case REG8:  PUT_CODE(p, IM8(inst->src.u.immediate)); break;
    case REG16: PUT_CODE(p, IM16(inst->src.u.immediate)); break;
    case REG32: PUT_CODE(p, IM32(inst->src.u.immediate)); break;
    case REG64: PUT_CODE(p, IM64(inst->src.u.immediate)); break;
    default: assert(false); break;
    }
    p += 1 << size;
  } else if (inst->src.type == INDIRECT && inst->dst.type == REG) {
    if (inst->src.u.indirect.label == NULL) {
      if (inst->src.u.indirect.reg.no != RIP) {
        int s = inst->src.u.indirect.reg.no;
        int d = inst->dst.u.reg.no;
        long offset = inst->src.u.indirect.offset;
        if (inst->dst.u.reg.size == REG8) {
          if (!inst->src.u.indirect.reg.x && opr_reg8(&inst->dst.u.reg)) {
            if (s != RSP - RAX) {
              if (offset == 0 && s != RBP - RAX) {
                MAKE_CODE(inst, code, 0x8a, 0x00 + s + d * 8);
                return true;
              } else if (is_im8(offset)) {
                MAKE_CODE(inst, code, 0x8a, 0x40 + s + d * 8, IM8(offset));
                return true;
              } else if (is_im32(offset)) {
                MAKE_CODE(inst, code, 0x8a, 0x80 + s + d * 8, IM32(offset));
                return true;
              }
            } else {
              if (offset == 0) {
                MAKE_CODE(inst, code, 0x8a, 0x04 + d * 8, 0x24);
                return true;
              } else if (is_im8(offset)) {
                MAKE_CODE(inst, code, 0x8a, 0x44 + d * 8, 0x24, IM8(offset));
                return true;
              } else if (is_im32(offset)) {
                MAKE_CODE(inst, code, 0x8a, 0x84 + d * 8, 0x24, IM32(offset));
                return true;
              }
            }
          } else {
            int pre = (!inst->src.u.indirect.reg.x ? 0x40 : 0x41) + (!inst->dst.u.reg.x ? 0 : 4);
            if (s != RSP - RAX) {
              if (offset == 0 && s != RBP - RAX) {
                MAKE_CODE(inst, code, pre, 0x8a, 0x00 + s + d * 8);
                return true;
              } else if (is_im8(offset)) {
                MAKE_CODE(inst, code, pre, 0x8a, 0x40 + s + d * 8, IM8(offset));
                return true;
              } else if (is_im32(offset)) {
                MAKE_CODE(inst, code, pre, 0x8a, 0x80 + s + d * 8, IM32(offset));
                return true;
              }
            } else {
              if (offset == 0) {
                MAKE_CODE(inst, code, pre, 0x8a, 0x04 + d * 8, 0x24);
                return true;
              } else if (is_im8(offset)) {
                MAKE_CODE(inst, code, pre, 0x8a, 0x44 + d * 8, 0x24, IM8(offset));
                return true;
              } else if (is_im32(offset)) {
                MAKE_CODE(inst, code, pre, 0x8a, 0x84 + d * 8, 0x24, IM32(offset));
                return true;
              }
            }
          }
        } else if (inst->dst.u.reg.size == REG16) {
          if (!inst->dst.u.reg.x && !inst->src.u.indirect.reg.x) {
            if (s != RSP - RAX) {
              if (offset == 0 && s != RBP - RAX) {
                MAKE_CODE(inst, code, 0x66, 0x8b, 0x00 + s + d * 8);
                return true;
              } else if (is_im8(offset)) {
                MAKE_CODE(inst, code, 0x66, 0x8b, 0x40 + s + d * 8, IM8(offset));
                return true;
              } else if (is_im32(offset)) {
                MAKE_CODE(inst, code, 0x66, 0x8b, 0x80 + s + d * 8, IM32(offset));
                return true;
              }
            } else {
              if (offset == 0) {
                MAKE_CODE(inst, code, 0x66, 0x8b, 0x04 + d * 8, 0x24);
                return true;
              } else if (is_im8(offset)) {
                MAKE_CODE(inst, code, 0x66, 0x8b, 0x44 + d * 8, 0x24, IM8(offset));
                return true;
              } else if (is_im32(offset)) {
                MAKE_CODE(inst, code, 0x66, 0x8b, 0x84 + d * 8, 0x24, IM32(offset));
                return true;
              }
            }
          } else {
            int pre = (!inst->src.u.indirect.reg.x ? 0x40 : 0x41) + (!inst->dst.u.reg.x ? 0 : 4);
            if (s != RSP - RAX) {
              if (offset == 0 && s != RBP - RAX) {
                MAKE_CODE(inst, code, 0x66, pre, 0x8b, 0x00 + s + d * 8);
                return true;
              } else if (is_im8(offset)) {
                MAKE_CODE(inst, code, 0x66, pre, 0x8b, 0x40 + s + d * 8, IM8(offset));
                return true;
              } else if (is_im32(offset)) {
                MAKE_CODE(inst, code, 0x66, pre, 0x8b, 0x80 + s + d * 8, IM32(offset));
                return true;
              }
            } else {
              if (offset == 0) {
                MAKE_CODE(inst, code, 0x66, pre, 0x8b, 0x04 + d * 8, 0x24);
                return true;
              } else if (is_im8(offset)) {
                MAKE_CODE(inst, code, 0x66, pre, 0x8b, 0x44 + d * 8, 0x24, IM8(offset));
                return true;
              } else if (is_im32(offset)) {
                MAKE_CODE(inst, code, 0x66, pre, 0x8b, 0x84 + d * 8, 0x24, IM32(offset));
                return true;
              }
            }
          }
        } else if (inst->dst.u.reg.size == REG32) {
          if (!inst->src.u.indirect.reg.x && !inst->dst.u.reg.x) {
            if (s != RSP - RAX) {
              if (offset == 0 && s != RBP - RAX) {
                MAKE_CODE(inst, code, 0x8b, 0x00 + s + d * 8);
                return true;
              } else if (is_im8(offset)) {
                MAKE_CODE(inst, code, 0x8b, 0x40 + s + d * 8, IM8(offset));
                return true;
              } else if (is_im32(offset)) {
                MAKE_CODE(inst, code, 0x8b, 0x80 + s + d * 8, IM32(offset));
                return true;
              }
            } else {
              if (offset == 0) {
                MAKE_CODE(inst, code, 0x8b, 0x04 + d * 8, 0x24);
                return true;
              } else if (is_im8(offset)) {
                MAKE_CODE(inst, code, 0x8b, 0x44 + d * 8, 0x24, IM8(offset));
                return true;
              } else if (is_im32(offset)) {
                MAKE_CODE(inst, code, 0x8b, 0x84 + d * 8, 0x24, IM32(offset));
                return true;
              }
            }
          } else {
            int pre = (!inst->src.u.indirect.reg.x ? 0x40 : 0x41) + (!inst->dst.u.reg.x ? 0 : 4);
            if (s != RSP - RAX) {
              if (offset == 0 && s != RBP - RAX) {
                MAKE_CODE(inst, code, pre, 0x8b, 0x00 + s + d * 8);
                return true;
              } else if (is_im8(offset)) {
                MAKE_CODE(inst, code, pre, 0x8b, 0x40 + s + d * 8, IM8(offset));
                return true;
              } else if (is_im32(offset)) {
                MAKE_CODE(inst, code, pre, 0x8b, 0x80 + s + d * 8, IM32(offset));
                return true;
              }
            } else {
              if (offset == 0) {
                MAKE_CODE(inst, code, pre, 0x8b, 0x04 + d * 8, 0x24);
                return true;
              } else if (is_im8(offset)) {
                MAKE_CODE(inst, code, pre, 0x8b, 0x44 + d * 8, 0x24, IM8(offset));
                return true;
              } else if (is_im32(offset)) {
                MAKE_CODE(inst, code, pre, 0x8b, 0x84 + d * 8, 0x24, IM32(offset));
                return true;
              }
            }
          }
        } else if (inst->dst.u.reg.size == REG64) {
          int pre = (!inst->src.u.indirect.reg.x ? 0x48 : 0x49) + (!inst->dst.u.reg.x ? 0 : 4);
          if (s != RSP - RAX) {
            if (offset == 0 && s != RBP - RAX) {
              MAKE_CODE(inst, code, pre, 0x8b, 0x00 + s + d * 8);
              return true;
            } else if (is_im8(offset)) {
              MAKE_CODE(inst, code, pre, 0x8b, 0x40 + s + d * 8, IM8(offset));
              return true;
            } else if (is_im32(offset)) {
              MAKE_CODE(inst, code, pre, 0x8b, 0x80 + s + d * 8, IM32(offset));
              return true;
            }
          } else {
            if (offset == 0) {
              MAKE_CODE(inst, code, pre, 0x8b, 0x04 + d * 8, 0x24);
              return true;
            } else if (is_im8(offset)) {
              MAKE_CODE(inst, code, pre, 0x8b, 0x44 + d * 8, 0x24, IM8(offset));
              return true;
            } else if (is_im32(offset)) {
              MAKE_CODE(inst, code, pre, 0x8b, 0x84 + d * 8, 0x24, IM32(offset));
              return true;
            }
          }
        }
      }
    }
  } else if (inst->src.type == REG && inst->dst.type == INDIRECT &&
             inst->dst.u.indirect.reg.no != RIP) {
    if (inst->dst.u.indirect.label == NULL) {
      int s = inst->src.u.reg.no;
      int d = inst->dst.u.indirect.reg.no;
      long offset = inst->dst.u.indirect.offset;
      if (inst->src.u.reg.size == REG8) {
        if (opr_reg8(&inst->src.u.reg) && !inst->dst.u.indirect.reg.x) {
          if (d != RSP - RAX) {
            if (offset == 0 && d != RBP - RAX) {
              MAKE_CODE(inst, code, 0x88, 0x00 + d + s * 8);
              return true;
            } else if (is_im8(offset)) {
              MAKE_CODE(inst, code, 0x88, 0x40 + d + s * 8, IM8(offset));
              return true;
            } else if (is_im32(offset)) {
              MAKE_CODE(inst, code, 0x89, 0x80 + d + s * 8, IM32(offset));
              return true;
            }
          } else {
            if (offset == 0) {
              MAKE_CODE(inst, code, 0x88, 0x04 + s * 8, 0x24);
              return true;
            } else if (is_im8(offset)) {
              MAKE_CODE(inst, code, 0x88, 0x44 + s * 8, 0x24, IM8(offset));
              return true;
            } else if (is_im32(offset)) {
              MAKE_CODE(inst, code, 0x89, 0x84 + s * 8, 0x24, IM32(offset));
              return true;
            }
          }
        } else {
          int pre = (!inst->dst.u.indirect.reg.x ? 0x40 : 0x41) + (!inst->src.u.reg.x ? 0 : 4);
          if (d != RSP - RAX) {
            if (offset == 0 && d != RBP - RAX) {
              MAKE_CODE(inst, code, pre, 0x88, 0x00 + d + s * 8);
              return true;
            } else if (is_im8(offset)) {
              MAKE_CODE(inst, code, pre, 0x88, 0x40 + d + s * 8, IM8(offset));
              return true;
            } else if (is_im32(offset)) {
              MAKE_CODE(inst, code, pre, 0x89, 0x80 + d + s * 8, IM32(offset));
              return true;
            }
          } else {
            if (offset == 0) {
              MAKE_CODE(inst, code, pre, 0x88, 0x04 + s * 8, 0x24);
              return true;
            } else if (is_im8(offset)) {
              MAKE_CODE(inst, code, pre, 0x88, 0x44 + s * 8, 0x24, IM8(offset));
              return true;
            } else if (is_im32(offset)) {
              MAKE_CODE(inst, code, pre, 0x89, 0x84 + s * 8, 0x24, IM32(offset));
              return true;
            }
          }
        }
      } else if (inst->src.u.reg.size == REG16) {
        if (!inst->src.u.reg.x && !inst->dst.u.indirect.reg.x) {
          if (d != RSP - RAX) {
            if (offset == 0 && d != RBP - RAX) {
              MAKE_CODE(inst, code, 0x66, 0x89, 0x00 + d + s * 8);
              return true;
            } else if (is_im8(offset)) {
              MAKE_CODE(inst, code, 0x66, 0x89, 0x40 + d + s * 8, IM8(offset));
              return true;
            } else if (is_im32(offset)) {
              MAKE_CODE(inst, code, 0x66, 0x89, 0x80 + d + s * 8, IM32(offset));
              return true;
            }
          } else {
            if (offset == 0) {
              MAKE_CODE(inst, code, 0x66, 0x89, 0x04 + s * 8, 0x24);
              return true;
            } else if (is_im8(offset)) {
              MAKE_CODE(inst, code, 0x66, 0x89, 0x44 + s * 8, 0x24, IM8(offset));
              return true;
            } else if (is_im32(offset)) {
              MAKE_CODE(inst, code, 0x66, 0x89, 0x84 + s * 8, 0x24, IM32(offset));
              return true;
            }
          }
        } else {
          int pre = (!inst->dst.u.indirect.reg.x ? 0x40 : 0x41) + (!inst->src.u.reg.x ? 0 : 4);
          if (d != RSP - RAX) {
            if (offset == 0 && d != RBP - RAX) {
              MAKE_CODE(inst, code, 0x66, pre, 0x89, 0x00 + d + s * 8);
              return true;
            } else if (is_im8(offset)) {
              MAKE_CODE(inst, code, 0x66, pre, 0x89, 0x40 + d + s * 8, IM8(offset));
              return true;
            } else if (is_im32(offset)) {
              MAKE_CODE(inst, code, 0x66, pre, 0x89, 0x80 + d + s * 8, IM32(offset));
              return true;
            }
          } else {
            if (offset == 0) {
              MAKE_CODE(inst, code, 0x66, pre, 0x89, 0x04 + s * 8, 0x24);
              return true;
            } else if (is_im8(offset)) {
              MAKE_CODE(inst, code, 0x66, pre, 0x89, 0x44 + s * 8, 0x24, IM8(offset));
              return true;
            } else if (is_im32(offset)) {
              MAKE_CODE(inst, code, 0x66, pre, 0x89, 0x84 + s * 8, 0x24, IM32(offset));
              return true;
            }
          }
        }
      } else if (inst->src.u.reg.size == REG32) {
        if (!inst->dst.u.indirect.reg.x && !inst->src.u.reg.x) {
          if (d != RSP - RAX) {
            if (offset == 0 && d != RBP - RAX) {
              MAKE_CODE(inst, code, 0x89, 0x00 + d + s * 8);
              return true;
            } else if (is_im8(offset)) {
              MAKE_CODE(inst, code, 0x89, 0x40 + d + s * 8, IM8(offset));
              return true;
            } else if (is_im32(offset)) {
              MAKE_CODE(inst, code, 0x89, 0x80 + d + s * 8, IM32(offset));
              return true;
            }
          } else {
            if (offset == 0) {
              MAKE_CODE(inst, code, 0x89, 0x04 + s * 8, 0x24);
              return true;
            } else if (is_im8(offset)) {
              MAKE_CODE(inst, code, 0x89, 0x44 + s * 8, 0x24, IM8(offset));
              return true;
            } else if (is_im32(offset)) {
              MAKE_CODE(inst, code, 0x89, 0x84 + s * 8, 0x24, IM32(offset));
              return true;
            }
          }
        } else {
          int pre = (!inst->dst.u.indirect.reg.x ? 0x40 : 0x41) + (!inst->src.u.reg.x ? 0 : 4);
          if (d != RSP - RAX) {
            if (offset == 0 && d != RBP - RAX) {
              MAKE_CODE(inst, code, pre, 0x89, 0x00 + d + s * 8);
              return true;
            } else if (is_im8(offset)) {
              MAKE_CODE(inst, code, pre, 0x89, 0x40 + d + s * 8, IM8(offset));
              return true;
            } else if (is_im32(offset)) {
              MAKE_CODE(inst, code, pre, 0x89, 0x80 + d + s * 8, IM32(offset));
              return true;
            }
          } else {
            if (offset == 0) {
              MAKE_CODE(inst, code, pre, 0x89, 0x04 + s * 8, 0x24);
              return true;
            } else if (is_im8(offset)) {
              MAKE_CODE(inst, code, pre, 0x89, 0x44 + s * 8, 0x24, IM8(offset));
              return true;
            } else if (is_im32(offset)) {
              MAKE_CODE(inst, code, pre, 0x89, 0x84 + s * 8, 0x24, IM32(offset));
              return true;
            }
          }
        }
      } else if (inst->src.u.reg.size == REG64) {
        int pre = (!inst->dst.u.indirect.reg.x ? 0x48 : 0x49) + (!inst->src.u.reg.x ? 0 : 4);
        if (d != RSP - RAX) {
          if (offset == 0 && d != RBP - RAX) {
            MAKE_CODE(inst, code, pre, 0x89, 0x00 + d + s * 8);
            return true;
          } else if (is_im8(offset)) {
            MAKE_CODE(inst, code, pre, 0x89, 0x40 + d + s * 8, IM8(offset));
            return true;
          } else if (is_im32(offset)) {
            MAKE_CODE(inst, code, pre, 0x89, 0x80 + d + s * 8, IM32(offset));
            return true;
          }
        } else {
          if (offset == 0) {
            MAKE_CODE(inst, code, pre, 0x89, 0x04 + s * 8, 0x24);
            return true;
          } else if (is_im8(offset)) {
            MAKE_CODE(inst, code, pre, 0x89, 0x44 + s * 8, 0x24, IM8(offset));
            return true;
          } else if (is_im32(offset)) {
            MAKE_CODE(inst, code, pre, 0x89, 0x84 + s * 8, 0x24, IM32(offset));
            return true;
          }
        }
      }
    }
  }

  if (p > code->buf) {
    code->inst = inst;
    code->len = p - code->buf;
    return true;
  }

  return assemble_error(rawline, "Illegal operand");
}

bool assemble_inst(Inst *inst, const char *rawline, Code *code) {
  unsigned char *p = code->buf;

  code->len = 0;

  switch(inst->op) {
  case NOOP:
    return true;
  case MOV:
    return assemble_mov(inst, rawline, code);
  case MOVSX:
    if (inst->src.type == REG && inst->dst.type == REG) {
      int s = inst->src.u.reg.no;
      int d = inst->dst.u.reg.no;
      switch (inst->src.u.reg.size) {
      case REG8:
        switch (inst->dst.u.reg.size) {
        case REG32:
          if (opr_reg8(&inst->src.u.reg) && !inst->dst.u.reg.x) {
            MAKE_CODE(inst, code, 0x0f, 0xbe, 0xc0 + s + d * 8);
          } else {
            int pre = (!inst->src.u.reg.x ? 0x40 : 0x41) + (!inst->dst.u.reg.x ? 0 : 4);
            MAKE_CODE(inst, code, pre, 0x0f, 0xbe, 0xc0 + s + d * 8);
          }
          return true;
        case REG64:
          {
            int pre = (!inst->src.u.reg.x ? 0x48 : 0x49) + (!inst->dst.u.reg.x ? 0 : 4);
            MAKE_CODE(inst, code, pre, 0x0f, 0xbe, 0xc0 + s + d * 8);
            return true;
          }
        default:
          break;
        }
      case REG16:
        switch (inst->dst.u.reg.size) {
        case REG32:
          if (!inst->src.u.reg.x && !inst->dst.u.reg.x) {
            MAKE_CODE(inst, code, 0x0f, 0xbf, 0xc0 + s + d * 8);
          } else {
            int pre = (!inst->src.u.reg.x ? 0x40 : 0x41) + (!inst->dst.u.reg.x ? 0 : 4);
            MAKE_CODE(inst, code, pre, 0x0f, 0xbf, 0xc0 + s + d * 8);
          }
          return true;
        case REG64:
          {
            int pre = (!inst->src.u.reg.x ? 0x48 : 0x49) + (!inst->dst.u.reg.x ? 0 : 4);
            MAKE_CODE(inst, code, pre, 0x0f, 0xbf, 0xc0 + s + d * 8);
            return true;
          }
        default:
          break;
        }
      case REG32:
        if (inst->dst.u.reg.size == REG64) {
          int pre = (!inst->src.u.reg.x ? 0x48 : 0x49) + (!inst->dst.u.reg.x ? 0 : 4);
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
      if (inst->dst.u.reg.size != REG64)
        return assemble_error(rawline, "64 bit register expected for destination");

      int d = inst->dst.u.reg.no;
      if (inst->src.u.indirect.reg.no != RIP) {
        int s = inst->src.u.indirect.reg.no;
        int pre = (!inst->src.u.indirect.reg.x ? 0x48 : 0x49) + (!inst->dst.u.reg.x ? 0 : 4);
        if (inst->src.u.indirect.label == NULL) {
          long offset = inst->src.u.indirect.offset;
          if (inst->src.u.indirect.reg.no != RSP - RAX) {
            if (offset == 0 && inst->src.u.indirect.reg.no != RBP - RAX) {
              MAKE_CODE(inst, code, pre, 0x8d, 0x00 + s + d * 8);
              return true;
            } else if (is_im8(offset)) {
              MAKE_CODE(inst, code, pre, 0x8d, 0x40 + s + d * 8, IM8(offset));
              return true;
            } else if (is_im32(offset)) {
              MAKE_CODE(inst, code, pre, 0x8d, 0x80 + s + d * 8, IM32(offset));
              return true;
            }
          } else {
            if (offset == 0) {
              MAKE_CODE(inst, code, pre, 0x8d, 0x04 + d * 8, 0x24);
              return true;
            } else if (is_im8(offset)) {
              MAKE_CODE(inst, code, pre, 0x8d, 0x44 + d * 8, 0x24, IM8(offset));
              return true;
            } else if (is_im32(offset)) {
              MAKE_CODE(inst, code, pre, 0x8d, 0x84 + d * 8, 0x24, IM32(offset));
              return true;
            }
          }
        }
      } else {
        int pre = !inst->dst.u.reg.x ? 0x48 : 0x4c;
        if (inst->src.u.indirect.offset == 0) {
          MAKE_CODE(inst, code, pre, 0x8d, 0x05 + d * 8, IM32(-1));
          return true;
        }
      }
    }
    break;
  case ADD:
    if (inst->src.type == REG && inst->dst.type == REG) {
      if (inst->dst.u.reg.size != inst->src.u.reg.size)
        return assemble_error(rawline, "Different source and destination register size");

      enum RegSize size = inst->src.u.reg.size;
      p = put_rex2(p, size, opr_regno(&inst->src.u.reg), opr_regno(&inst->dst.u.reg),
                   size == REG8 ? 0x00 : 0x01);
    } else if (inst->src.type == IMMEDIATE && inst->dst.type == REG) {
      if (inst->dst.u.reg.size == REG64) {
        int pre = !inst->dst.u.reg.x ? 0x48 : 0x49;
        int d = inst->dst.u.reg.no;
        if (is_im8(inst->src.u.immediate)) {
          MAKE_CODE(inst, code, pre, 0x83, 0xc0 + d, IM8(inst->src.u.immediate));
          return true;
        } else if (is_im32(inst->src.u.immediate)) {
          if (opr_regno(&inst->dst.u.reg) == RAX - RAX)
            MAKE_CODE(inst, code, pre, 0x05, IM32(inst->src.u.immediate));
          else
            MAKE_CODE(inst, code, pre, 0x81, 0xc0 + d, IM32(inst->src.u.immediate));
          return true;
        }
      }
    } else if (inst->src.type == INDIRECT && inst->dst.type == REG) {
      if (!inst->src.u.indirect.reg.x && inst->src.u.indirect.label == NULL &&
          inst->dst.u.reg.size == REG64) {
        int pre = (!inst->src.u.indirect.reg.x ? 0x48 : 0x49) + (!inst->dst.u.reg.x ? 0 : 4);
        int s = inst->src.u.indirect.reg.no;
        int d = inst->dst.u.reg.no;
        long offset = inst->src.u.indirect.offset;
        if (inst->src.u.indirect.reg.no != RSP - RAX) {
          if (offset == 0 && inst->src.u.indirect.reg.no != RBP - RAX) {
            MAKE_CODE(inst, code, pre, 0x03, 0x00 + s + d * 8);
          } else if (is_im8(offset)) {
            MAKE_CODE(inst, code, pre, 0x03, 0x40 + s + d * 8, IM8(offset));
          } else if (is_im32(offset)) {
            MAKE_CODE(inst, code, pre, 0x03, 0x80 + s + d * 8, IM8(offset));
          }
        } else {
          if (offset == 0 && inst->src.u.indirect.reg.no != RBP - RAX) {
            MAKE_CODE(inst, code, pre, 0x03, 0x04 + d * 8, 0x24);
          } else if (is_im8(offset)) {
            MAKE_CODE(inst, code, pre, 0x03, 0x44 + d * 8, 0x24, IM8(offset));
          } else if (is_im32(offset)) {
            MAKE_CODE(inst, code, pre, 0x03, 0x84 + d * 8, 0x24, IM8(offset));
          }
        }
      }
    }
    break;
  case ADDQ:
    if (inst->src.type == IMMEDIATE && inst->dst.type == INDIRECT) {
      if (!inst->dst.u.indirect.reg.x && inst->dst.u.indirect.label == NULL) {
        int pre = !inst->dst.u.indirect.reg.x ? 0x48 : 0x49;
        long value = inst->src.u.immediate;
        int d = inst->dst.u.indirect.reg.no;
        long offset = inst->dst.u.indirect.offset;
        if (is_im8(value)) {
          if (d != RSP - RAX) {
            if (offset == 0 && d != RBP - RAX) {
              MAKE_CODE(inst, code, pre, 0x83, 0x00 + d, IM8(value));
              return true;
            } else if (is_im8(offset)) {
              MAKE_CODE(inst, code, pre, 0x83, 0x40 + d, IM8(offset), IM8(value));
              return true;
            } else if (is_im32(offset)) {
              MAKE_CODE(inst, code, pre, 0x83, 0x80 + d, IM32(offset), IM8(value));
              return true;
            }
          } else {
            if (offset == 0) {
              MAKE_CODE(inst, code, pre, 0x83, 0x04, 0x24, IM8(value));
              return true;
            } else if (is_im8(offset)) {
              MAKE_CODE(inst, code, pre, 0x83, 0x44, 0x24, IM8(offset), IM8(value));
              return true;
            } else if (is_im32(offset)) {
              MAKE_CODE(inst, code, pre, 0x83, 0x84, 0x24, IM32(offset), IM8(value));
              return true;
            }
          }
        } else if (is_im32(value)) {
          if (d != RSP - RAX) {
            if (offset == 0 && d != RBP - RAX) {
              MAKE_CODE(inst, code, pre, 0x81, 0x00 + d, IM32(value));
              return true;
            } else if (is_im8(offset)) {
              MAKE_CODE(inst, code, pre, 0x81, 0x40 + d, IM8(offset), IM32(value));
              return true;
            } else if (is_im32(offset)) {
              MAKE_CODE(inst, code, pre, 0x81, 0x80 + d, IM32(offset), IM32(value));
              return true;
            }
          } else {
            if (offset == 0) {
              MAKE_CODE(inst, code, pre, 0x81, 0x04, 0x24, IM32(value));
              return true;
            } else if (is_im8(offset)) {
              MAKE_CODE(inst, code, pre, 0x81, 0x44, 0x24, IM8(offset), IM32(value));
              return true;
            } else if (is_im32(offset)) {
              MAKE_CODE(inst, code, pre, 0x81, 0x84, 0x24, IM32(offset), IM32(value));
              return true;
            }
          }
        }
      }
    }
    break;
  case SUB:
    if (inst->src.type == REG && inst->dst.type == REG) {
      if (inst->dst.u.reg.size != inst->src.u.reg.size)
        return assemble_error(rawline, "Different source and destination register size");

      enum RegSize size = inst->src.u.reg.size;
      p = put_rex2(p, size, opr_regno(&inst->src.u.reg), opr_regno(&inst->dst.u.reg),
                   size == REG8 ? 0x28 : 0x29);
    } else if (inst->src.type == IMMEDIATE && inst->dst.type == REG) {
      if (inst->dst.u.reg.size == REG64) {
        int pre = !inst->dst.u.reg.x ? 0x48 : 0x49;
        int d = inst->dst.u.reg.no;
        if (is_im8(inst->src.u.immediate)) {
          MAKE_CODE(inst, code, pre, 0x83, 0xe8 + d, IM8(inst->src.u.immediate));
          return true;
        } else if (is_im32(inst->src.u.immediate)) {
          if (opr_regno(&inst->dst.u.reg) == RAX - RAX)
            MAKE_CODE(inst, code, pre, 0x2d, IM32(inst->src.u.immediate));
          else
            MAKE_CODE(inst, code, pre, 0x81, 0xe8 + d, IM32(inst->src.u.immediate));
          return true;
        }
      }
    }
    break;
  case SUBQ:
    if (inst->src.type == IMMEDIATE && inst->dst.type == INDIRECT) {
      if (!inst->dst.u.indirect.reg.x && inst->dst.u.indirect.label == NULL) {
        long value = inst->src.u.immediate;
        int d = inst->dst.u.indirect.reg.no;
        long offset = inst->dst.u.indirect.offset;
        if (is_im8(value)) {
          if (inst->dst.u.indirect.reg.no != RSP - RAX) {
            if (offset == 0 && inst->dst.u.indirect.reg.no != RBP - RAX) {
              MAKE_CODE(inst, code, 0x48, 0x83, 0x28 + d, IM8(value));
              return true;
            } else if (is_im8(offset)) {
              MAKE_CODE(inst, code, 0x48, 0x83, 0x68 + d, IM8(offset), IM8(value));
              return true;
            } else if (is_im32(offset)) {
              MAKE_CODE(inst, code, 0x48, 0x83, 0xa8 + d, IM32(offset), IM8(value));
              return true;
            }
          } else {
            if (offset == 0) {
              MAKE_CODE(inst, code, 0x48, 0x83, 0x2c, 0x24, IM8(value));
              return true;
            } else if (is_im8(offset)) {
              MAKE_CODE(inst, code, 0x48, 0x83, 0x6c, 0x24, IM8(offset), IM8(value));
              return true;
            } else if (is_im32(offset)) {
              MAKE_CODE(inst, code, 0x48, 0x83, 0xac, 0x24, IM32(offset), IM8(value));
              return true;
            }
          }
        } else if (is_im32(value)) {
          if (inst->dst.u.indirect.reg.no != RSP - RAX) {
            if (offset == 0 && inst->dst.u.indirect.reg.no != RBP - RAX) {
              MAKE_CODE(inst, code, 0x48, 0x81, 0x28 + d, IM32(value));
              return true;
            } else if (is_im8(offset)) {
              MAKE_CODE(inst, code, 0x48, 0x81, 0x68 + d, IM8(offset), IM32(value));
              return true;
            } else if (is_im32(offset)) {
              MAKE_CODE(inst, code, 0x48, 0x81, 0xa8 + d, IM32(offset), IM32(value));
              return true;
            }
          } else {
            if (offset == 0) {
              MAKE_CODE(inst, code, 0x48, 0x81, 0x2c, 0x24, IM32(value));
              return true;
            } else if (is_im8(offset)) {
              MAKE_CODE(inst, code, 0x48, 0x81, 0x6c, 0x24, IM8(offset), IM32(value));
              return true;
            } else if (is_im32(offset)) {
              MAKE_CODE(inst, code, 0x48, 0x81, 0xac, 0x24, IM32(offset), IM32(value));
              return true;
            }
          }
        }
      }
    }
    break;
  case MUL:
    if (inst->src.type == NOOPERAND || inst->dst.type != NOOPERAND)
      return assemble_error(rawline, "Illegal operand");

    if (inst->src.type == REG) {
      enum RegSize size = inst->src.u.reg.size;
      p = put_rex0(p, size, 0, opr_regno(&inst->src.u.reg),
                   0xf6 | (size == REG8 ? 0 : 1));
      *p++ = 0xe0 | inst->src.u.reg.no;
    }
    break;
  case IDIV:
    if (inst->src.type == NOOPERAND || inst->dst.type != NOOPERAND)
      return assemble_error(rawline, "Illegal operand");

    if (inst->src.type == REG) {
      enum RegSize size = inst->src.u.reg.size;
      p = put_rex0(p, size, 0, opr_regno(&inst->src.u.reg),
                   0xf6 | (size == REG8 ? 0 : 1));
      *p++ = 0xf8 | inst->src.u.reg.no;
    }
    break;
  case NEG:
    if (inst->src.type == NOOPERAND || inst->dst.type != NOOPERAND)
      return assemble_error(rawline, "Illegal operand");

    if (inst->src.type == REG) {
      int s = inst->src.u.reg.no;
      if (inst->src.u.reg.size == REG16) {
        if (!inst->src.u.reg.x) {
          MAKE_CODE(inst, code, 0x66, 0xf7, 0xd8 + s);
        } else {
          MAKE_CODE(inst, code, 0x66, 0x41, 0xf7, 0xd8 + s);
        }
        return true;
      } else if (inst->src.u.reg.size == REG32) {
        if (!inst->src.u.reg.x) {
          MAKE_CODE(inst, code, 0xf7, 0xd8 + s);
        } else {
          MAKE_CODE(inst, code, 0x41, 0xf7, 0xd8 + s);
        }
        return true;
      } else if (inst->src.u.reg.size == REG64) {
        int pre = !inst->src.u.reg.x ? 0x48 : 0x49;
        MAKE_CODE(inst, code, pre, 0xf7, 0xd8 + s);
        return true;
      }
    }
    break;
  case NOT:
    if (inst->src.type == NOOPERAND || inst->dst.type != NOOPERAND)
      return assemble_error(rawline, "Illegal operand");

    if (inst->src.type == REG) {
      int s = inst->src.u.reg.no;
      if (inst->src.u.reg.size == REG16) {
        if (!inst->src.u.reg.x) {
          MAKE_CODE(inst, code, 0x66, 0xf7, 0xd0 + s);
        } else {
          MAKE_CODE(inst, code, 0x66, 0x41, 0xf7, 0xd0 + s);
        }
        return true;
      } else if (inst->src.u.reg.size == REG32) {
        if (!inst->src.u.reg.x) {
          MAKE_CODE(inst, code, 0xf7, 0xd0 + s);
        } else {
          MAKE_CODE(inst, code, 0x41, 0xf7, 0xd0 + s);
        }
        return true;
      }
    }
    break;
  case INC:
    if (inst->src.type == NOOPERAND || inst->dst.type != NOOPERAND)
      return assemble_error(rawline, "Illegal operand");

    if (inst->src.type == REG) {
      if (inst->src.u.reg.size == REG32) {
        int s = inst->src.u.reg.no;
        if (!inst->src.u.reg.x) {
          MAKE_CODE(inst, code, 0xff, 0xc0 + s);
        } else {
          MAKE_CODE(inst, code, 0x41, 0xff, 0xc0 + s);
        }
        return true;
      } else if (inst->src.u.reg.size == REG64) {
        int pre = !inst->src.u.reg.x ? 0x48 : 0x49;
        int s = inst->src.u.reg.no;
        MAKE_CODE(inst, code, pre, 0xff, 0xc0 + s);
        return true;
      }
    }
    break;
  case INCL:
    if (inst->src.type == NOOPERAND || inst->dst.type != NOOPERAND)
      return assemble_error(rawline, "Illegal operand");

    if (inst->src.type == INDIRECT &&
        inst->src.u.indirect.reg.no != RIP) {
      int s = inst->src.u.indirect.reg.no;
      long offset = inst->src.u.indirect.offset;
      if (!inst->src.u.indirect.reg.x) {
        if (s != RSP - RAX) {
          if (offset == 0 && s != RBP - RAX) {
            MAKE_CODE(inst, code, 0xff, 0x00 + s);
            return true;
          } else if (is_im8(offset)) {
            MAKE_CODE(inst, code, 0xff, 0x40 + s, IM8(offset));
            return true;
          } else if (is_im32(offset)) {
            MAKE_CODE(inst, code, 0xff, 0x80 + s, IM32(offset));
            return true;
          }
        } else {
          if (offset == 0) {
            MAKE_CODE(inst, code, 0xff, 0x04, 0x24);
            return true;
          } else if (is_im8(offset)) {
            MAKE_CODE(inst, code, 0xff, 0x44, 0x24, IM8(offset));
            return true;
          } else if (is_im32(offset)) {
            MAKE_CODE(inst, code, 0xff, 0x84, 0x24, IM32(offset));
            return true;
          }
        }
      } else {
        if (s != RSP - RAX) {
          if (offset == 0 && s != RBP - RAX) {
            MAKE_CODE(inst, code, 0x41, 0xff, 0x00 + s);
            return true;
          } else if (is_im8(offset)) {
            MAKE_CODE(inst, code, 0x41, 0xff, 0x40 + s, IM8(offset));
            return true;
          } else if (is_im32(offset)) {
            MAKE_CODE(inst, code, 0x41, 0xff, 0x80 + s, IM32(offset));
            return true;
          }
        } else {
          if (offset == 0) {
            MAKE_CODE(inst, code, 0x41, 0xff, 0x04, 0x24);
            return true;
          } else if (is_im8(offset)) {
            MAKE_CODE(inst, code, 0x41, 0xff, 0x44, 0x24, IM8(offset));
            return true;
          } else if (is_im32(offset)) {
            MAKE_CODE(inst, code, 0x41, 0xff, 0x84, 0x24, IM32(offset));
            return true;
          }
        }
      }
    }
    break;
  case INCQ:
    if (inst->src.type == NOOPERAND || inst->dst.type != NOOPERAND)
      return assemble_error(rawline, "Illegal operand");

    if (inst->src.type == INDIRECT &&
        inst->src.u.indirect.reg.no != RIP) {
      int pre = !inst->src.u.indirect.reg.x ? 0x48 : 0x49;
      int s = inst->src.u.indirect.reg.no;
      long offset = inst->src.u.indirect.offset;
      if (inst->src.u.indirect.reg.no != RSP - RAX) {
        if (offset == 0 && inst->src.u.indirect.reg.no != RBP - RAX) {
          MAKE_CODE(inst, code, pre, 0xff, 0x00 + s);
          return true;
        } else if (is_im8(offset)) {
          MAKE_CODE(inst, code, pre, 0xff, 0x40 + s, IM8(offset));
          return true;
        } else if (is_im32(offset)) {
          MAKE_CODE(inst, code, pre, 0xff, 0x80 + s, IM32(offset));
          return true;
        }
      } else {
        if (offset == 0) {
          MAKE_CODE(inst, code, pre, 0xff, 0x04, 0x24);
          return true;
        } else if (is_im8(offset)) {
          MAKE_CODE(inst, code, pre, 0xff, 0x44, 0x24, IM8(offset));
          return true;
        } else if (is_im32(offset)) {
          MAKE_CODE(inst, code, pre, 0xff, 0x84, 0x24, IM32(offset));
          return true;
        }
      }
    }
    break;
  case DEC:
    if (inst->src.type == NOOPERAND || inst->dst.type != NOOPERAND)
      return assemble_error(rawline, "Illegal operand");

    if (inst->src.type == REG) {
      if (inst->src.u.reg.size == REG32) {
        int s = inst->src.u.reg.no;
        if (!inst->src.u.reg.x) {
          MAKE_CODE(inst, code, 0xff, 0xc8 + s);
        } else {
          MAKE_CODE(inst, code, 0x41, 0xff, 0xc8 + s);
        }
        return true;
      } else if (inst->src.u.reg.size == REG64) {
        int pre = !inst->src.u.reg.x ? 0x48 : 0x49;
        int s = inst->src.u.reg.no;
        MAKE_CODE(inst, code, pre, 0xff, 0xc8 + s);
        return true;
      }
    }
    break;
  case DECL:
    if (inst->src.type == NOOPERAND)
      return assemble_error(rawline, "Illegal operand");

    if (inst->src.type == INDIRECT &&
        inst->src.u.indirect.reg.no != RIP) {
      int s = inst->src.u.indirect.reg.no;
      long offset = inst->src.u.indirect.offset;
      if (!inst->src.u.indirect.reg.x) {
        if (s != RSP - RAX) {
          if (offset == 0 && s != RBP - RAX) {
            MAKE_CODE(inst, code, 0xff, 0x08 + s);
            return true;
          } else if (is_im8(offset)) {
            MAKE_CODE(inst, code, 0xff, 0x48 + s, IM8(offset));
            return true;
          } else if (is_im32(offset)) {
            MAKE_CODE(inst, code, 0xff, 0x88 + s, IM32(offset));
            return true;
          }
        } else {
          if (offset == 0) {
            MAKE_CODE(inst, code, 0xff, 0x0c, 0x24);
            return true;
          } else if (is_im8(offset)) {
            MAKE_CODE(inst, code, 0xff, 0x4c, 0x24, IM8(offset));
            return true;
          } else if (is_im32(offset)) {
            MAKE_CODE(inst, code, 0xff, 0x8c, 0x24, IM32(offset));
            return true;
          }
        }
      } else {
        if (s != RSP - RAX) {
          if (offset == 0 && s != RBP - RAX) {
            MAKE_CODE(inst, code, 0x41, 0xff, 0x08 + s);
            return true;
          } else if (is_im8(offset)) {
            MAKE_CODE(inst, code, 0x41, 0xff, 0x48 + s, IM8(offset));
            return true;
          } else if (is_im32(offset)) {
            MAKE_CODE(inst, code, 0x41, 0xff, 0x88 + s, IM32(offset));
            return true;
          }
        } else {
          if (offset == 0) {
            MAKE_CODE(inst, code, 0x41, 0xff, 0x0c, 0x24);
            return true;
          } else if (is_im8(offset)) {
            MAKE_CODE(inst, code, 0x41, 0xff, 0x4c, 0x24, IM8(offset));
            return true;
          } else if (is_im32(offset)) {
            MAKE_CODE(inst, code, 0x41, 0xff, 0x8c, 0x24, IM32(offset));
            return true;
          }
        }
      }
    }
    break;
  case DECQ:
    if (inst->src.type == NOOPERAND)
      return assemble_error(rawline, "Illegal operand");

    if (inst->src.type == INDIRECT &&
        inst->src.u.indirect.reg.no != RIP) {
      int pre = !inst->src.u.indirect.reg.x ? 0x48 : 0x49;
      int s = inst->src.u.indirect.reg.no;
      long offset = inst->src.u.indirect.offset;
      if (inst->src.u.indirect.reg.no != RSP - RAX) {
        if (offset == 0 && inst->src.u.indirect.reg.no != RBP - RAX) {
          MAKE_CODE(inst, code, pre, 0xff, 0x08 + s);
          return true;
        } else if (is_im8(offset)) {
          MAKE_CODE(inst, code, pre, 0xff, 0x48 + s, IM8(offset));
          return true;
        } else if (is_im32(offset)) {
          MAKE_CODE(inst, code, pre, 0xff, 0x88 + s, IM32(offset));
          return true;
        }
      } else {
        if (offset == 0) {
          MAKE_CODE(inst, code, pre, 0xff, 0x0c, 0x24);
          return true;
        } else if (is_im8(offset)) {
          MAKE_CODE(inst, code, pre, 0xff, 0x4c, 0x24, IM8(offset));
          return true;
        } else if (is_im32(offset)) {
          MAKE_CODE(inst, code, pre, 0xff, 0x8c, 0x24, IM32(offset));
          return true;
        }
      }
    }
    break;
  case AND:
    if (inst->src.type == REG && inst->dst.type == REG) {
      if (inst->dst.u.reg.size != inst->src.u.reg.size)
        return assemble_error(rawline, "Different source and destination register size");

      int s = inst->src.u.reg.no;
      int d = inst->dst.u.reg.no;
      switch (inst->src.u.reg.size) {
      case REG16:
        if (!inst->src.u.reg.x && !inst->dst.u.reg.x) {
          MAKE_CODE(inst, code, 0x66, 0x21, 0xc0 + s * 8 + d);
        } else {
          int pre = (!inst->dst.u.reg.x ? 0x40 : 0x41) + (!inst->src.u.reg.x ? 0 : 4);
          MAKE_CODE(inst, code, 0x66, pre, 0x21, 0xc0 + s * 8 + d);
        }
        return true;
      case REG32:
        if (!inst->src.u.reg.x && !inst->dst.u.reg.x) {
          MAKE_CODE(inst, code, 0x21, 0xc0 + s * 8 + d);
        } else {
          int pre = (!inst->dst.u.reg.x ? 0x40 : 0x41) + (!inst->src.u.reg.x ? 0 : 4);
          MAKE_CODE(inst, code, pre, 0x21, 0xc0 + s * 8 + d);
        }
        return true;
      case REG64:
        {
          int pre = (!inst->dst.u.reg.x ? 0x48 : 0x49) + (!inst->src.u.reg.x ? 0 : 4);
          MAKE_CODE(inst, code, pre, 0x21, 0xc0 + s * 8 + d);
          return true;
        }
      default:
        break;
      }
    }
    break;
  case OR:
    if (inst->src.type == REG && inst->dst.type == REG) {
      if (inst->dst.u.reg.size != inst->src.u.reg.size)
        return assemble_error(rawline, "Different source and destination register size");

      int s = inst->src.u.reg.no;
      int d = inst->dst.u.reg.no;
      switch (inst->src.u.reg.size) {
      case REG8:
        if (opr_reg8(&inst->src.u.reg) && opr_reg8(&inst->dst.u.reg)) {
          MAKE_CODE(inst, code, 0x08, 0xc0 + s * 8 + d);
        } else {
          int pre = (!inst->dst.u.reg.x ? 0x40 : 0x41) + (!inst->src.u.reg.x ? 0 : 4);
          MAKE_CODE(inst, code, pre, 0x08, 0xc0 + s * 8 + d);
        }
        return true;
      case REG16:
        if (!inst->src.u.reg.x && !inst->dst.u.reg.x) {
          MAKE_CODE(inst, code, 0x66, 0x09, 0xc0 + s * 8 + d);
        } else {
          int pre = (!inst->dst.u.reg.x ? 0x40 : 0x41) + (!inst->src.u.reg.x ? 0 : 4);
          MAKE_CODE(inst, code, 0x66, pre, 0x09, 0xc0 + s * 8 + d);
        }
        return true;
      case REG32:
        if (!inst->src.u.reg.x && !inst->dst.u.reg.x) {
          MAKE_CODE(inst, code, 0x09, 0xc0 + s * 8 + d);
        } else {
          int pre = (!inst->dst.u.reg.x ? 0x40 : 0x41) + (!inst->src.u.reg.x ? 0 : 4);
          MAKE_CODE(inst, code, pre, 0x09, 0xc0 + s * 8 + d);
        }
        return true;
      case REG64:
        {
          int pre = (!inst->dst.u.reg.x ? 0x48 : 0x49) + (!inst->src.u.reg.x ? 0 : 4);
          MAKE_CODE(inst, code, pre, 0x09, 0xc0 + s * 8 + d);
          return true;
        }
      default:
        break;
      }
    }
    break;
  case XOR:
    if (inst->src.type == REG && inst->dst.type == REG) {
      if (inst->dst.u.reg.size != inst->src.u.reg.size)
        return assemble_error(rawline, "Different source and destination register size");

      int s = inst->src.u.reg.no;
      int d = inst->dst.u.reg.no;
      switch (inst->src.u.reg.size) {
      case REG8:
        if (opr_reg8(&inst->src.u.reg) && opr_reg8(&inst->dst.u.reg)) {
          MAKE_CODE(inst, code, 0x30, 0xc0 + s * 8 + d);
        } else {
          int pre = (!inst->dst.u.reg.x ? 0x40 : 0x41) + (!inst->src.u.reg.x ? 0 : 4);
          MAKE_CODE(inst, code, pre, 0x30, 0xc0 + s * 8 + d);
        }
        return true;
      case REG16:
        if (!inst->src.u.reg.x && !inst->dst.u.reg.x) {
          MAKE_CODE(inst, code, 0x66, 0x31, 0xc0 + s * 8 + d);
        } else {
          int pre = (!inst->dst.u.reg.x ? 0x40 : 0x41) + (!inst->src.u.reg.x ? 0 : 4);
          MAKE_CODE(inst, code, 0x66, pre, 0x31, 0xc0 + s * 8 + d);
        }
        return true;
      case REG32:
        if (!inst->src.u.reg.x && !inst->dst.u.reg.x) {
          MAKE_CODE(inst, code, 0x31, 0xc0 + s * 8 + d);
        } else {
          int pre = (!inst->dst.u.reg.x ? 0x40 : 0x41) + (!inst->src.u.reg.x ? 0 : 4);
          MAKE_CODE(inst, code, pre, 0x31, 0xc0 + s * 8 + d);
        }
        return true;
      case REG64:
        {
          int pre = (!inst->dst.u.reg.x ? 0x48 : 0x49) + (!inst->src.u.reg.x ? 0 : 4);
          MAKE_CODE(inst, code, pre, 0x31, 0xc0 + s * 8 + d);
          return true;
        }
      default:
        break;
      }
    }
    break;
  case SHL:
    if (inst->src.type == REG && inst->dst.type == REG) {
      if (opr_regno(&inst->src.u.reg) != CL - AL)
        return assemble_error(rawline, "`%cl` expected");

      int d = inst->dst.u.reg.no;
      switch (inst->dst.u.reg.size) {
      case REG8:
        if (opr_reg8(&inst->dst.u.reg)) {
          MAKE_CODE(inst, code, 0xd2, 0xe0 + d);
        } else {
          int pre = !inst->dst.u.reg.x ? 0x40 : 0x41;
          MAKE_CODE(inst, code, pre, 0xd2, 0xe0 + d);
        }
        return true;
      case REG16:
        if (!inst->dst.u.reg.x) {
          MAKE_CODE(inst, code, 0x66, 0xd3, 0xe0 + d);
        } else {
          MAKE_CODE(inst, code, 0x66, 0x41, 0xd3, 0xe0 + d);
        }
        return true;
      case REG32:
        if (!inst->dst.u.reg.x) {
          MAKE_CODE(inst, code, 0xd3, 0xe0 + d);
        } else {
          MAKE_CODE(inst, code, 0x41, 0xd3, 0xe0 + d);
        }
        return true;
      case REG64:
        {
          int pre = inst->dst.u.reg.size == REG64 ? 0x48 : 0x49;
          MAKE_CODE(inst, code, pre, 0xd3, 0xe0 + d);
          return true;
        }
      default:
        break;
      }
    }
    break;
  case SHR:
    if (inst->src.type == REG && inst->dst.type == REG) {
      if (opr_regno(&inst->src.u.reg) != CL - AL)
        return assemble_error(rawline, "`%cl` expected");

      int d = inst->dst.u.reg.no;
      switch (inst->dst.u.reg.size) {
      case REG32:
        if (!inst->dst.u.reg.x) {
          MAKE_CODE(inst, code, 0xd3, 0xe8 + d);
        } else {
          MAKE_CODE(inst, code, 0x41, 0xd3, 0xe8 + d);
        }
        return true;
      case REG64:
        {
          int pre = !inst->dst.u.reg.x ? 0x48 : 0x49;
          MAKE_CODE(inst, code, pre, 0xd3, 0xe8 + d);
          return true;
        }
      default:
        break;
      }
    }
    break;
  case CMP:
    if (inst->src.type == REG && inst->dst.type == REG) {
      if (inst->dst.u.reg.size != inst->src.u.reg.size)
        return assemble_error(rawline, "Different source and destination register size");

      int s = inst->src.u.reg.no;
      int d = inst->dst.u.reg.no;
      switch (inst->src.u.reg.size) {
      case REG8:
        if (opr_reg8(&inst->src.u.reg) && opr_reg8(&inst->dst.u.reg)) {
          MAKE_CODE(inst, code, 0x38, 0xc0 + s * 8 + d);
        } else {
          int pre = (!inst->dst.u.reg.x ? 0x40 : 0x41) + (!inst->src.u.reg.x ? 0 : 4);
          MAKE_CODE(inst, code, pre, 0x38, 0xc0 + s * 8 + d);
        }
        return true;
      case REG32:
        if (!inst->src.u.reg.x && !inst->dst.u.reg.x) {
          MAKE_CODE(inst, code, 0x39, 0xc0 + s * 8 + d);
        } else {
          int pre = (!inst->dst.u.reg.x ? 0x40 : 0x41) + (!inst->src.u.reg.x ? 0 : 4);
          MAKE_CODE(inst, code, pre, 0x39, 0xc0 + s * 8 + d);
        }
        return true;
      case REG64:
        {
          int pre = (!inst->dst.u.reg.x ? 0x48 : 0x49) + (!inst->src.u.reg.x ? 0 : 4);
          MAKE_CODE(inst, code, pre, 0x39, 0xc0 + s * 8 + d);
          return true;
        }
      default:
        break;
      }
    } else if (inst->src.type == IMMEDIATE && inst->dst.type == REG) {
      long value = inst->src.u.immediate;
      if (inst->dst.u.reg.size == REG8) {
        int d = inst->dst.u.reg.no;
        if (opr_regno(&inst->dst.u.reg) == AL - AL) {
          MAKE_CODE(inst, code, 0x3c, IM8(value));
        } else if (opr_reg8(&inst->dst.u.reg)) {
          MAKE_CODE(inst, code, 0x80, 0xf8 + d, IM8(value));
        } else {
          MAKE_CODE(inst, code, 0x41, 0x80, 0xf8 + d, IM8(value));
        }
        return true;
      } else if (inst->dst.u.reg.size == REG32) {
        int d = inst->dst.u.reg.no;
        if (!inst->dst.u.reg.x) {
          if (is_im8(value)) {
            MAKE_CODE(inst, code, 0x83, 0xf8 + d, IM8(value));
            return true;
          } else if (is_im32(value)) {
            if (opr_regno(&inst->dst.u.reg) == EAX - EAX) {
              MAKE_CODE(inst, code, 0x3d, IM32(value));
              return true;
            } else {
              MAKE_CODE(inst, code, 0x81, 0xf8 + d, IM32(value));
              return true;
            }
          }
        } else {
          if (is_im8(value)) {
            MAKE_CODE(inst, code, 0x41, 0x83, 0xf8 + d, IM8(value));
            return true;
          } else if (is_im32(value)) {
            MAKE_CODE(inst, code, 0x41, 0x81, 0xf8 + d, IM32(value));
            return true;
          }
        }
      } else if (inst->dst.u.reg.size == REG64) {
        int d = inst->dst.u.reg.no;
        int pre = !inst->dst.u.reg.x ? 0x48 : 0x49;
        if (is_im8(value)) {
          MAKE_CODE(inst, code, pre, 0x83, 0xf8 + d, IM8(value));
          return true;
        } else if (is_im32(value)) {
          if (opr_regno(&inst->dst.u.reg) == EAX - EAX) {
            MAKE_CODE(inst, code, pre, 0x3d, IM32(value));
            return true;
          } else {
            MAKE_CODE(inst, code, pre, 0x81, 0xf8 + d, IM32(value));
            return true;
          }
        }
      }
    }
    break;
  case TEST:
    if (inst->src.type == REG && inst->dst.type == REG) {
      if (inst->dst.u.reg.size != inst->src.u.reg.size)
        return assemble_error(rawline, "Different source and destination register size");

      int s = inst->src.u.reg.no;
      int d = inst->dst.u.reg.no;
      switch (inst->src.u.reg.size) {
      case REG8:
        if (opr_reg8(&inst->src.u.reg) && opr_reg8(&inst->dst.u.reg)) {
          MAKE_CODE(inst, code, 0x84, 0xc0 + s * 8 + d);
        } else {
          int pre = (!inst->dst.u.reg.x ? 0x40 : 0x41) + (!inst->src.u.reg.x ? 0 : 4);
          MAKE_CODE(inst, code, pre, 0x84, 0xc0 + s * 8 + d);
        }
        return true;
      case REG16:
        if (!inst->src.u.reg.x && !inst->dst.u.reg.x) {
          MAKE_CODE(inst, code, 0x66, 0x85, 0xc0 + s * 8 + d);
        } else {
          int pre = (!inst->dst.u.reg.x ? 0x40 : 0x41) + (!inst->src.u.reg.x ? 0 : 4);
          MAKE_CODE(inst, code, 0x66, pre, 0x85, 0xc0 + s * 8 + d);
        }
        return true;
      case REG32:
        if (!inst->src.u.reg.x && !inst->dst.u.reg.x) {
          MAKE_CODE(inst, code, 0x85, 0xc0 + s * 8 + d);
        } else {
          int pre = (!inst->dst.u.reg.x ? 0x40 : 0x41) + (!inst->src.u.reg.x ? 0 : 4);
          MAKE_CODE(inst, code, pre, 0x85, 0xc0 + s * 8 + d);
        }
        return true;
      case REG64:
        {
          int pre = (!inst->src.u.reg.x ? 0x48 : 0x49) + (!inst->dst.u.reg.x ? 0 : 4);
          MAKE_CODE(inst, code, pre, 0x85, 0xc0 + s * 8 + d);
          return true;
        }
      default:
        break;
      }
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
    {
      if (inst->dst.type != NOOPERAND || inst->src.type != REG ||
          inst->src.u.reg.size != REG8)
        return assemble_error(rawline, "Illegal opeand");

      int s = inst->src.u.reg.no;
      if (opr_reg8(&inst->src.u.reg)) {
        MAKE_CODE(inst, code, 0x0f, 0x90 + (inst->op - SETO), 0xc0 + s);
      } else {
        int pre = !inst->src.u.reg.x ? 0x40 : 0x41;
        MAKE_CODE(inst, code, pre, 0x0f, 0x90 + (inst->op - SETO), 0xc0 + s);
      }
      return true;
    }
  case PUSH:
    {
      if (inst->dst.type != NOOPERAND || inst->src.type != REG ||
          inst->src.u.reg.size != REG64)
        return assemble_error(rawline, "Illegal operand");

      int d = inst->src.u.reg.no;
      if (!inst->src.u.reg.x) {
        MAKE_CODE(inst, code, 0x50 + d);
      } else {
        MAKE_CODE(inst, code, 0x41, 0x50 + d);
      }
      return true;
    }
  case POP:
    {
      if (inst->dst.type != NOOPERAND || inst->src.type != REG ||
          inst->src.u.reg.size != REG64)
        return assemble_error(rawline, "Illegal operand");

      int d = inst->src.u.reg.no;
      if (!inst->src.u.reg.x) {
        MAKE_CODE(inst, code, 0x58 + d);
      } else {
        MAKE_CODE(inst, code, 0x41, 0x58 + d);
      }
      return true;
    }
  case JMP:
    if (inst->src.type != LABEL || inst->dst.type != NOOPERAND)
      return assemble_error(rawline, "Illegal operand");
    MAKE_CODE(inst, code, 0xe9, IM32(-1));
    return true;
  case JO: case JNO: case JB:  case JAE:
  case JE: case JNE: case JBE: case JA:
  case JS: case JNS: case JP:  case JNP:
  case JL: case JGE: case JLE: case JG:
    if (inst->src.type != LABEL || inst->dst.type != NOOPERAND)
      return assemble_error(rawline, "Illegal operand");

    // TODO: Handle short jump.
    MAKE_CODE(inst, code, 0x0f, 0x80 + (inst->op - JO), IM32(-1));
    return true;
  case CALL:
    if (inst->dst.type != NOOPERAND)
      return assemble_error(rawline, "Illegal operand");

    if (inst->src.type == LABEL) {
      MAKE_CODE(inst, code, 0xe8, IM32(-1));
      return true;
    } if (inst->src.type == DEREF_REG) {
      int s = inst->src.u.deref_reg.no;
      if (!inst->src.u.deref_reg.x) {
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
      long value = inst->src.u.immediate;
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
    return true;
  }

  fprintf(stderr, "op=%2d: not handled\n", inst->op);
  return false;
}

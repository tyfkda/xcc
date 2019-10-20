#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdint.h>  // uintptr_t
#include <stdio.h>
#include <stdlib.h>  // calloc
#include <string.h>
#include <strings.h>  // strncasecmp

#include "elfutil.h"
#include "gen.h"
#include "inst.h"
#include "util.h"

#define PROG_START   (0x100)

#if defined(__XV6)
// XV6
#include "../kernel/syscall.h"
#include "../kernel/traps.h"

#define START_ADDRESS    0x1000

#elif defined(__linux__)
// Linux

#include <sys/stat.h>

#define START_ADDRESS    (0x01000000 + PROG_START)

#else

#error Target not supported

#endif

#define LOAD_ADDRESS    START_ADDRESS

typedef struct {
  Inst *inst;
  char len;
  unsigned char buf[15];
} Code;

#ifndef MAKE_CODE
#define MAKE_CODE(inst, code, ...)  do { unsigned char buf[] = {__VA_ARGS__}; make_code(inst, code, buf, sizeof(buf)); } while (0)
#endif

void make_code(Inst *inst, Code *code, unsigned char *buf, int len) {
  assert(len <= (int)sizeof(code->buf));
  code->inst = inst;
  code->len = len;
  memcpy(code->buf, buf, len);
}

typedef struct {
  size_t len;
  unsigned char *buf;
} Data;

#define IM8(x)   (x)
#define IM16(x)  (x), ((x) >> 8)
#define IM32(x)  (x), ((x) >> 8), ((x) >> 16), ((x) >> 24)
#define IM64(x)  (x), ((x) >> 8), ((x) >> 16), ((x) >> 24), ((x) >> 32), ((x) >> 40), ((x) >> 48), ((x) >> 56)

typedef struct {
  const char *rawline;
  const char *label;

  Inst inst;

  enum DirectiveType dir;
  const char *directive_line;
} Line;

enum IRType {
  IR_LABEL,
  IR_CODE,
  IR_DATA,
  IR_BSS,
  IR_ALIGN,
  IR_LOC_REL32,
  IR_LOC_ABS64,
};

typedef struct {
  enum IRType type;
  union {
    const char *label;
    Code code;
    Data data;
    size_t bss;
    int align;
    int section;
    struct {
      const char *label;
      int ofs;
      int base;
    } loc;
  } u;
} IR;

static IR *new_ir_label(const char *label) {
  IR *ir = malloc(sizeof(*ir));
  ir->type = IR_LABEL;
  ir->u.label = label;
  return ir;
}

static IR *new_ir_code(const Code *code) {
  IR *ir = malloc(sizeof(*ir));
  ir->type = IR_CODE;
  ir->u.code = *code;
  return ir;
}

static IR *new_ir_data(const void *data, size_t size) {
  IR *ir = malloc(sizeof(*ir));
  ir->type = IR_DATA;
  ir->u.data.len = size;
  ir->u.data.buf = (unsigned char*)data;
  return ir;
}

static IR *new_ir_bss(size_t size) {
  IR *ir = malloc(sizeof(*ir));
  ir->type = IR_BSS;
  ir->u.bss = size;
  return ir;
}

static IR *new_ir_align(int align) {
  IR *ir = malloc(sizeof(*ir));
  ir->type = IR_ALIGN;
  ir->u.align = align;
  return ir;
}

static IR *new_ir_loc_rel32(const char *label, int ofs, int base) {
  IR *ir = malloc(sizeof(*ir));
  ir->type = IR_LOC_REL32;
  ir->u.loc.label = label;
  ir->u.loc.ofs = ofs;
  ir->u.loc.base = base;
  return ir;
}

static IR *new_ir_loc_abs64(const char *label, int ofs) {
  IR *ir = malloc(sizeof(*ir));
  ir->type = IR_LOC_ABS64;
  ir->u.loc.label = label;
  ir->u.loc.ofs = ofs;
  ir->u.loc.base = 0;
  return ir;
}

bool err;

static char unescape_char(char c) {
  switch (c) {
  case '0':  return '\0';
  case 'n':  return '\n';
  case 't':  return '\t';
  case 'r':  return '\r';
  case '"':  return '"';
  case '\'':  return '\'';
  default:
    return c;
  }
}

static size_t unescape_string(const char *p, char *dst) {
  size_t len = 0;
  for (; *p != '"'; ++p, ++len) {
    char c = *p;
    if (c == '\0')
      error("string not closed");
    if (c == '\\') {
      // TODO: Handle \x...
      c = unescape_char(*(++p));
    }
    if (dst != NULL)
      *dst++ = c;
  }
  return len;
}

static void handle_directive(enum DirectiveType dir, const char *p, Vector **section_irs) {
  Vector *irs = section_irs[current_section];

  switch (dir) {
  case DT_ASCII:
    {
      if (*p != '"')
        error("`\"' expected");
      ++p;
      size_t len = unescape_string(p, NULL);
      char *str = malloc(len);
      unescape_string(p, str);

      vec_push(irs, new_ir_data(str, len));
    }
    break;

  case DT_COMM:
    {
      const char *label = parse_label(&p);
      if (label == NULL)
        error(".comm: label expected");
      p = skip_whitespace(p);
      if (*p != ',')
        error(".comm: `,' expected");
      p = skip_whitespace(p + 1);
      long count;
      if (!parse_immediate(&p, &count))
        error(".comm: count expected");
      current_section = SEC_BSS;
      irs = section_irs[current_section];
      vec_push(irs, new_ir_label(label));
      vec_push(irs, new_ir_bss(count));
    }
    break;

  case DT_TEXT:
    current_section = SEC_CODE;
    break;

  case DT_DATA:
    current_section = SEC_DATA;
    break;

  case DT_ALIGN:
    {
      long align;
      if (!parse_immediate(&p, &align))
        error(".align: number expected");
      vec_push(irs, new_ir_align(align));
    }
    break;

  case DT_BYTE:
  case DT_WORD:
  case DT_LONG:
  case DT_QUAD:
    {
      long value;
      if (parse_immediate(&p, &value)) {
        // TODO: Target endian.
        int size = 1 << (dir - DT_BYTE);
        unsigned char *buf = malloc(size);
        for (int i = 0; i < size; ++i)
          buf[i] = value >> (8 * i);
        vec_push(irs, new_ir_data(buf, size));
      } else {
        const char *label = parse_label(&p);
        if (label != NULL) {
          if (dir == DT_QUAD) {
            vec_push(irs, new_ir_loc_abs64(label, 0));
            void *buf = calloc(sizeof(void*), 1);
            vec_push(irs, new_ir_data(buf, sizeof(void*)));
          } else {
            error("label can use only in .quad");
          }
        } else {
          error(".quad: number or label expected");
        }
      }
    }
    break;

  case DT_SECTION:
  case DT_GLOBL:
  case DT_EXTERN:
    break;

  default:
    fprintf(stderr, "Unhandled directive: %d, %s\n", dir, p);
    break;
  }
}

static Line *parse_line(const char *rawline) {
  Line *line = malloc(sizeof(*line));
  line->rawline = rawline;
  line->label = NULL;
  line->inst.op = NOOP;
  line->inst.src.type = line->inst.dst.type = NOOPERAND;
  line->dir = NODIRECTIVE;

  const char *p = rawline;
  line->label = parse_label(&p);
  if (line->label != NULL) {
    if (*p != ':')
      error("`:' expected");
    ++p;
  }

  p = skip_whitespace(p);
  if (*p == '.') {
    ++p;
    enum DirectiveType dir = parse_directive(&p);
    if (dir == NODIRECTIVE)
      error("Unknown directive");
    line->dir = dir;
    line->directive_line = p;
  } else if (*p != '\0') {
    parse_inst(&p, &line->inst);
    if (*p != '\0' && !(*p == '/' && p[1] == '/')) {
      fprintf(stderr, "Syntax error: %s\n", p);
      err = true;
    }
  }
  return line;
}

static char opr_regno(const Operand *opr) {
  return opr->u.reg.no | (opr->u.reg.x << 3);
}

static bool opr_reg8(const Operand *opr) {
  assert(opr->u.reg.size == REG8);
  return opr_regno(opr) < 4;
}

static bool assemble_mov(Line *line, Code *code) {
  if (line->inst.src.type == REG && line->inst.dst.type == REG) {
    if (line->inst.src.u.reg.size == REG8 && line->inst.dst.u.reg.size == REG8) {
      int s = line->inst.src.u.reg.no;
      int d = line->inst.dst.u.reg.no;
      if (opr_reg8(&line->inst.src) && opr_reg8(&line->inst.dst)) {
        MAKE_CODE(&line->inst, code, 0x88, 0xc0 + d + s * 8);
      } else {
        int pre = (!line->inst.dst.u.reg.x ? 0x40 : 0x41) + (!line->inst.src.u.reg.x ? 0 : 4);
        MAKE_CODE(&line->inst, code, pre, 0x88, 0xc0 + d + s * 8);
      }
      return true;
    } else if (line->inst.src.u.reg.size == REG16 && line->inst.dst.u.reg.size == REG16) {
      int s = line->inst.src.u.reg.no;
      int d = line->inst.dst.u.reg.no;
      if (!line->inst.src.u.reg.x && !line->inst.dst.u.reg.x) {
        MAKE_CODE(&line->inst, code, 0x66, 0x89, 0xc0 + d + s * 8);
      } else {
        int pre = (!line->inst.dst.u.reg.x ? 0x40 : 0x41) + (!line->inst.src.u.reg.x ? 0 : 4);
        MAKE_CODE(&line->inst, code, 0x66, pre, 0x89, 0xc0 + d + s * 8);
      }
      return true;
    } else if (line->inst.src.u.reg.size == REG32 && line->inst.dst.u.reg.size == REG32) {
      int s = line->inst.src.u.reg.no;
      int d = line->inst.dst.u.reg.no;
      if (!line->inst.src.u.reg.x && !line->inst.dst.u.reg.x) {
        MAKE_CODE(&line->inst, code, 0x89, 0xc0 + d + s * 8);
      } else {
        int pre = (!line->inst.dst.u.reg.x ? 0x40 : 0x41) + (!line->inst.src.u.reg.x ? 0 : 4);
        MAKE_CODE(&line->inst, code, pre, 0x89, 0xc0 + d + s * 8);
      }
      return true;
    } else if (line->inst.src.u.reg.size == REG64 && line->inst.dst.u.reg.size == REG64) {
      int pre = (!line->inst.dst.u.reg.x ? 0x48 : 0x49) + (!line->inst.src.u.reg.x ? 0 : 4);
      int s = line->inst.src.u.reg.no;
      int d = line->inst.dst.u.reg.no;
      MAKE_CODE(&line->inst, code, pre, 0x89, 0xc0 + d + s * 8);
      return true;
    }
  } else if (line->inst.src.type == IMMEDIATE && line->inst.dst.type == REG) {
    if (line->inst.dst.u.reg.size == REG8) {
      int d = line->inst.dst.u.reg.no;
      if (opr_reg8(&line->inst.dst)) {
        MAKE_CODE(&line->inst, code, 0xb0 + d, IM8(line->inst.src.u.immediate));
      } else {
        int pre = !line->inst.dst.u.reg.x ? 0x40 : 0x41;
        MAKE_CODE(&line->inst, code, pre, 0xb0 + d, IM8(line->inst.src.u.immediate));
      }
      return true;
    } else if (line->inst.dst.u.reg.size == REG16) {
      int d = line->inst.dst.u.reg.no;
      if (!line->inst.dst.u.reg.x) {
        MAKE_CODE(&line->inst, code, 0x66, 0xb8 + d, IM16(line->inst.src.u.immediate));
      } else {
        MAKE_CODE(&line->inst, code, 0x66, 0x41, 0xb8 + d, IM16(line->inst.src.u.immediate));
      }
      return true;
    } else if (line->inst.dst.u.reg.size == REG32) {
      int d = line->inst.dst.u.reg.no;
      if (!line->inst.dst.u.reg.x) {
        MAKE_CODE(&line->inst, code, 0xb8 + d, IM32(line->inst.src.u.immediate));
      } else {
        MAKE_CODE(&line->inst, code, 0x41, 0xb8 + d, IM32(line->inst.src.u.immediate));
      }
      return true;
    } else if (line->inst.dst.u.reg.size == REG64) {
      int pre = !line->inst.dst.u.reg.x ? 0x48 : 0x49;
      int d = line->inst.dst.u.reg.no;
      if (is_im32(line->inst.src.u.immediate)) {
        MAKE_CODE(&line->inst, code, pre, 0xc7, 0xc0 + d, IM32(line->inst.src.u.immediate));
      } else {
        MAKE_CODE(&line->inst, code, pre, 0xb8 + d, IM64(line->inst.src.u.immediate));
      }
      return true;
    }
  } else if (line->inst.src.type == INDIRECT && line->inst.dst.type == REG) {
    if (line->inst.src.u.indirect.label == NULL) {
      if (line->inst.src.u.indirect.reg.no != RIP) {
        int s = line->inst.src.u.indirect.reg.no;
        long offset = line->inst.src.u.indirect.offset;
        if (line->inst.dst.u.reg.size == REG8) {
          if (!line->inst.src.u.indirect.reg.x && opr_reg8(&line->inst.dst)) {
            int d = line->inst.dst.u.reg.no;
            if (s != RSP - RAX) {
              if (offset == 0 && s != RBP - RAX) {
                MAKE_CODE(&line->inst, code, 0x8a, 0x00 + s + d * 8);
                return true;
              } else if (is_im8(offset)) {
                MAKE_CODE(&line->inst, code, 0x8a, 0x40 + s + d * 8, IM8(offset));
                return true;
              } else if (is_im32(offset)) {
                MAKE_CODE(&line->inst, code, 0x8a, 0x80 + s + d * 8, IM32(offset));
                return true;
              }
            } else {
              if (offset == 0) {
                MAKE_CODE(&line->inst, code, 0x8a, 0x04 + d * 8, 0x24);
                return true;
              } else if (is_im8(offset)) {
                MAKE_CODE(&line->inst, code, 0x8a, 0x44 + d * 8, 0x24, IM8(offset));
                return true;
              } else if (is_im32(offset)) {
                MAKE_CODE(&line->inst, code, 0x8a, 0x84 + d * 8, 0x24, IM32(offset));
                return true;
              }
            }
          } else {
            int pre = (!line->inst.src.u.indirect.reg.x ? 0x40 : 0x41) + (!line->inst.dst.u.reg.x ? 0 : 4);
            int d = line->inst.dst.u.reg.no;
            if (s != RSP - RAX) {
              if (offset == 0 && s != RBP - RAX) {
                MAKE_CODE(&line->inst, code, pre, 0x8a, 0x00 + s + d * 8);
                return true;
              } else if (is_im8(offset)) {
                MAKE_CODE(&line->inst, code, pre, 0x8a, 0x40 + s + d * 8, IM8(offset));
                return true;
              } else if (is_im32(offset)) {
                MAKE_CODE(&line->inst, code, pre, 0x8a, 0x80 + s + d * 8, IM32(offset));
                return true;
              }
            } else {
              if (offset == 0) {
                MAKE_CODE(&line->inst, code, pre, 0x8a, 0x04 + d * 8, 0x24);
                return true;
              } else if (is_im8(offset)) {
                MAKE_CODE(&line->inst, code, pre, 0x8a, 0x44 + d * 8, 0x24, IM8(offset));
                return true;
              } else if (is_im32(offset)) {
                MAKE_CODE(&line->inst, code, pre, 0x8a, 0x84 + d * 8, 0x24, IM32(offset));
                return true;
              }
            }
          }
        } else if (line->inst.dst.u.reg.size == REG16) {
          int d = line->inst.dst.u.reg.no;
          if (!line->inst.dst.u.reg.x && !line->inst.src.u.indirect.reg.x) {
            if (s != RSP - RAX) {
              if (offset == 0 && s != RBP - RAX) {
                MAKE_CODE(&line->inst, code, 0x66, 0x8b, 0x00 + s + d * 8);
                return true;
              } else if (is_im8(offset)) {
                MAKE_CODE(&line->inst, code, 0x66, 0x8b, 0x40 + s + d * 8, IM8(offset));
                return true;
              } else if (is_im32(offset)) {
                MAKE_CODE(&line->inst, code, 0x66, 0x8b, 0x80 + s + d * 8, IM32(offset));
                return true;
              }
            } else {
              if (offset == 0) {
                MAKE_CODE(&line->inst, code, 0x66, 0x8b, 0x04 + d * 8, 0x24);
                return true;
              } else if (is_im8(offset)) {
                MAKE_CODE(&line->inst, code, 0x66, 0x8b, 0x44 + d * 8, 0x24, IM8(offset));
                return true;
              } else if (is_im32(offset)) {
                MAKE_CODE(&line->inst, code, 0x66, 0x8b, 0x84 + d * 8, 0x24, IM32(offset));
                return true;
              }
            }
          } else {
            int pre = (!line->inst.src.u.indirect.reg.x ? 0x40 : 0x41) + (!line->inst.dst.u.reg.x ? 0 : 4);
            if (s != RSP - RAX) {
              if (offset == 0 && s != RBP - RAX) {
                MAKE_CODE(&line->inst, code, 0x66, pre, 0x8b, 0x00 + s + d * 8);
                return true;
              } else if (is_im8(offset)) {
                MAKE_CODE(&line->inst, code, 0x66, pre, 0x8b, 0x40 + s + d * 8, IM8(offset));
                return true;
              } else if (is_im32(offset)) {
                MAKE_CODE(&line->inst, code, 0x66, pre, 0x8b, 0x80 + s + d * 8, IM32(offset));
                return true;
              }
            } else {
              if (offset == 0) {
                MAKE_CODE(&line->inst, code, 0x66, pre, 0x8b, 0x04 + d * 8, 0x24);
                return true;
              } else if (is_im8(offset)) {
                MAKE_CODE(&line->inst, code, 0x66, pre, 0x8b, 0x44 + d * 8, 0x24, IM8(offset));
                return true;
              } else if (is_im32(offset)) {
                MAKE_CODE(&line->inst, code, 0x66, pre, 0x8b, 0x84 + d * 8, 0x24, IM32(offset));
                return true;
              }
            }
          }
        } else if (line->inst.dst.u.reg.size == REG32) {
          int d = line->inst.dst.u.reg.no;
          if (!line->inst.src.u.indirect.reg.x && !line->inst.dst.u.reg.x) {
            if (s != RSP - RAX) {
              if (offset == 0 && s != RBP - RAX) {
                MAKE_CODE(&line->inst, code, 0x8b, 0x00 + s + d * 8);
                return true;
              } else if (is_im8(offset)) {
                MAKE_CODE(&line->inst, code, 0x8b, 0x40 + s + d * 8, IM8(offset));
                return true;
              } else if (is_im32(offset)) {
                MAKE_CODE(&line->inst, code, 0x8b, 0x80 + s + d * 8, IM32(offset));
                return true;
              }
            } else {
              if (offset == 0) {
                MAKE_CODE(&line->inst, code, 0x8b, 0x04 + d * 8, 0x24);
                return true;
              } else if (is_im8(offset)) {
                MAKE_CODE(&line->inst, code, 0x8b, 0x44 + d * 8, 0x24, IM8(offset));
                return true;
              } else if (is_im32(offset)) {
                MAKE_CODE(&line->inst, code, 0x8b, 0x84 + d * 8, 0x24, IM32(offset));
                return true;
              }
            }
          } else {
            int pre = (!line->inst.src.u.indirect.reg.x ? 0x40 : 0x41) + (!line->inst.dst.u.reg.x ? 0 : 4);
            if (s != RSP - RAX) {
              if (offset == 0 && s != RBP - RAX) {
                MAKE_CODE(&line->inst, code, pre, 0x8b, 0x00 + s + d * 8);
                return true;
              } else if (is_im8(offset)) {
                MAKE_CODE(&line->inst, code, pre, 0x8b, 0x40 + s + d * 8, IM8(offset));
                return true;
              } else if (is_im32(offset)) {
                MAKE_CODE(&line->inst, code, pre, 0x8b, 0x80 + s + d * 8, IM32(offset));
                return true;
              }
            } else {
              if (offset == 0) {
                MAKE_CODE(&line->inst, code, pre, 0x8b, 0x04 + d * 8, 0x24);
                return true;
              } else if (is_im8(offset)) {
                MAKE_CODE(&line->inst, code, pre, 0x8b, 0x44 + d * 8, 0x24, IM8(offset));
                return true;
              } else if (is_im32(offset)) {
                MAKE_CODE(&line->inst, code, pre, 0x8b, 0x84 + d * 8, 0x24, IM32(offset));
                return true;
              }
            }
          }
        } else if (line->inst.dst.u.reg.size == REG64) {
          int d = line->inst.dst.u.reg.no;
          int pre = (!line->inst.src.u.indirect.reg.x ? 0x48 : 0x49) + (!line->inst.dst.u.reg.x ? 0 : 4);
          if (s != RSP - RAX) {
            if (offset == 0 && s != RBP - RAX) {
              MAKE_CODE(&line->inst, code, pre, 0x8b, 0x00 + s + d * 8);
              return true;
            } else if (is_im8(offset)) {
              MAKE_CODE(&line->inst, code, pre, 0x8b, 0x40 + s + d * 8, IM8(offset));
              return true;
            } else if (is_im32(offset)) {
              MAKE_CODE(&line->inst, code, pre, 0x8b, 0x80 + s + d * 8, IM32(offset));
              return true;
            }
          } else {
            if (offset == 0) {
              MAKE_CODE(&line->inst, code, pre, 0x8b, 0x04 + d * 8, 0x24);
              return true;
            } else if (is_im8(offset)) {
              MAKE_CODE(&line->inst, code, pre, 0x8b, 0x44 + d * 8, 0x24, IM8(offset));
              return true;
            } else if (is_im32(offset)) {
              MAKE_CODE(&line->inst, code, pre, 0x8b, 0x84 + d * 8, 0x24, IM32(offset));
              return true;
            }
          }
        }
      }
    }
  } else if (line->inst.src.type == REG && line->inst.dst.type == INDIRECT &&
             line->inst.dst.u.indirect.reg.no != RIP) {
    if (line->inst.dst.u.indirect.label == NULL) {
      int d = line->inst.dst.u.indirect.reg.no;
      long offset = line->inst.dst.u.indirect.offset;
      if (line->inst.src.u.reg.size == REG8) {
        int s = line->inst.src.u.reg.no;
        if (opr_reg8(&line->inst.src) && !line->inst.dst.u.indirect.reg.x) {
          if (d != RSP - RAX) {
            if (offset == 0 && d != RBP - RAX) {
              MAKE_CODE(&line->inst, code, 0x88, 0x00 + d + s * 8);
              return true;
            } else if (is_im8(offset)) {
              MAKE_CODE(&line->inst, code, 0x88, 0x40 + d + s * 8, IM8(offset));
              return true;
            } else if (is_im32(offset)) {
              MAKE_CODE(&line->inst, code, 0x89, 0x80 + d + s * 8, IM32(offset));
              return true;
            }
          } else {
            if (offset == 0) {
              MAKE_CODE(&line->inst, code, 0x88, 0x04 + s * 8, 0x24);
              return true;
            } else if (is_im8(offset)) {
              MAKE_CODE(&line->inst, code, 0x88, 0x44 + s * 8, 0x24, IM8(offset));
              return true;
            } else if (is_im32(offset)) {
              MAKE_CODE(&line->inst, code, 0x89, 0x84 + s * 8, 0x24, IM32(offset));
              return true;
            }
          }
        } else {
          int pre = (!line->inst.dst.u.indirect.reg.x ? 0x40 : 0x41) + (!line->inst.src.u.reg.x ? 0 : 4);
          if (d != RSP - RAX) {
            if (offset == 0 && d != RBP - RAX) {
              MAKE_CODE(&line->inst, code, pre, 0x88, 0x00 + d + s * 8);
              return true;
            } else if (is_im8(offset)) {
              MAKE_CODE(&line->inst, code, pre, 0x88, 0x40 + d + s * 8, IM8(offset));
              return true;
            } else if (is_im32(offset)) {
              MAKE_CODE(&line->inst, code, pre, 0x89, 0x80 + d + s * 8, IM32(offset));
              return true;
            }
          } else {
            if (offset == 0) {
              MAKE_CODE(&line->inst, code, pre, 0x88, 0x04 + s * 8, 0x24);
              return true;
            } else if (is_im8(offset)) {
              MAKE_CODE(&line->inst, code, pre, 0x88, 0x44 + s * 8, 0x24, IM8(offset));
              return true;
            } else if (is_im32(offset)) {
              MAKE_CODE(&line->inst, code, pre, 0x89, 0x84 + s * 8, 0x24, IM32(offset));
              return true;
            }
          }
        }
      } else if (line->inst.src.u.reg.size == REG16) {
        int s = line->inst.src.u.reg.no;
        if (!line->inst.src.u.reg.x && !line->inst.dst.u.indirect.reg.x) {
          if (d != RSP - RAX) {
            if (offset == 0 && d != RBP - RAX) {
              MAKE_CODE(&line->inst, code, 0x66, 0x89, 0x00 + d + s * 8);
              return true;
            } else if (is_im8(offset)) {
              MAKE_CODE(&line->inst, code, 0x66, 0x89, 0x40 + d + s * 8, IM8(offset));
              return true;
            } else if (is_im32(offset)) {
              MAKE_CODE(&line->inst, code, 0x66, 0x89, 0x80 + d + s * 8, IM32(offset));
              return true;
            }
          } else {
            if (offset == 0) {
              MAKE_CODE(&line->inst, code, 0x66, 0x89, 0x04 + s * 8, 0x24);
              return true;
            } else if (is_im8(offset)) {
              MAKE_CODE(&line->inst, code, 0x66, 0x89, 0x44 + s * 8, 0x24, IM8(offset));
              return true;
            } else if (is_im32(offset)) {
              MAKE_CODE(&line->inst, code, 0x66, 0x89, 0x84 + s * 8, 0x24, IM32(offset));
              return true;
            }
          }
        } else {
          int pre = (!line->inst.dst.u.indirect.reg.x ? 0x40 : 0x41) + (!line->inst.src.u.reg.x ? 0 : 4);
          if (d != RSP - RAX) {
            if (offset == 0 && d != RBP - RAX) {
              MAKE_CODE(&line->inst, code, 0x66, pre, 0x89, 0x00 + d + s * 8);
              return true;
            } else if (is_im8(offset)) {
              MAKE_CODE(&line->inst, code, 0x66, pre, 0x89, 0x40 + d + s * 8, IM8(offset));
              return true;
            } else if (is_im32(offset)) {
              MAKE_CODE(&line->inst, code, 0x66, pre, 0x89, 0x80 + d + s * 8, IM32(offset));
              return true;
            }
          } else {
            if (offset == 0) {
              MAKE_CODE(&line->inst, code, 0x66, pre, 0x89, 0x04 + s * 8, 0x24);
              return true;
            } else if (is_im8(offset)) {
              MAKE_CODE(&line->inst, code, 0x66, pre, 0x89, 0x44 + s * 8, 0x24, IM8(offset));
              return true;
            } else if (is_im32(offset)) {
              MAKE_CODE(&line->inst, code, 0x66, pre, 0x89, 0x84 + s * 8, 0x24, IM32(offset));
              return true;
            }
          }
        }
      } else if (line->inst.src.u.reg.size == REG32) {
        int s = line->inst.src.u.reg.no;
        if (!line->inst.dst.u.indirect.reg.x && !line->inst.src.u.reg.x) {
          if (d != RSP - RAX) {
            if (offset == 0 && d != RBP - RAX) {
              MAKE_CODE(&line->inst, code, 0x89, 0x00 + d + s * 8);
              return true;
            } else if (is_im8(offset)) {
              MAKE_CODE(&line->inst, code, 0x89, 0x40 + d + s * 8, IM8(offset));
              return true;
            } else if (is_im32(offset)) {
              MAKE_CODE(&line->inst, code, 0x89, 0x80 + d + s * 8, IM32(offset));
              return true;
            }
          } else {
            if (offset == 0) {
              MAKE_CODE(&line->inst, code, 0x89, 0x04 + s * 8, 0x24);
              return true;
            } else if (is_im8(offset)) {
              MAKE_CODE(&line->inst, code, 0x89, 0x44 + s * 8, 0x24, IM8(offset));
              return true;
            } else if (is_im32(offset)) {
              MAKE_CODE(&line->inst, code, 0x89, 0x84 + s * 8, 0x24, IM32(offset));
              return true;
            }
          }
        } else {
          int pre = (!line->inst.dst.u.indirect.reg.x ? 0x40 : 0x41) + (!line->inst.src.u.reg.x ? 0 : 4);
          if (d != RSP - RAX) {
            if (offset == 0 && d != RBP - RAX) {
              MAKE_CODE(&line->inst, code, pre, 0x89, 0x00 + d + s * 8);
              return true;
            } else if (is_im8(offset)) {
              MAKE_CODE(&line->inst, code, pre, 0x89, 0x40 + d + s * 8, IM8(offset));
              return true;
            } else if (is_im32(offset)) {
              MAKE_CODE(&line->inst, code, pre, 0x89, 0x80 + d + s * 8, IM32(offset));
              return true;
            }
          } else {
            if (offset == 0) {
              MAKE_CODE(&line->inst, code, pre, 0x89, 0x04 + s * 8, 0x24);
              return true;
            } else if (is_im8(offset)) {
              MAKE_CODE(&line->inst, code, pre, 0x89, 0x44 + s * 8, 0x24, IM8(offset));
              return true;
            } else if (is_im32(offset)) {
              MAKE_CODE(&line->inst, code, pre, 0x89, 0x84 + s * 8, 0x24, IM32(offset));
              return true;
            }
          }
        }
      } else if (line->inst.src.u.reg.size == REG64) {
        int pre = (!line->inst.dst.u.indirect.reg.x ? 0x48 : 0x49) + (!line->inst.src.u.reg.x ? 0 : 4);
        int s = line->inst.src.u.reg.no;
        if (d != RSP - RAX) {
          if (offset == 0 && d != RBP - RAX) {
            MAKE_CODE(&line->inst, code, pre, 0x89, 0x00 + d + s * 8);
            return true;
          } else if (is_im8(offset)) {
            MAKE_CODE(&line->inst, code, pre, 0x89, 0x40 + d + s * 8, IM8(offset));
            return true;
          } else if (is_im32(offset)) {
            MAKE_CODE(&line->inst, code, pre, 0x89, 0x80 + d + s * 8, IM32(offset));
            return true;
          }
        } else {
          if (offset == 0) {
            MAKE_CODE(&line->inst, code, pre, 0x89, 0x04 + s * 8, 0x24);
            return true;
          } else if (is_im8(offset)) {
            MAKE_CODE(&line->inst, code, pre, 0x89, 0x44 + s * 8, 0x24, IM8(offset));
            return true;
          } else if (is_im32(offset)) {
            MAKE_CODE(&line->inst, code, pre, 0x89, 0x84 + s * 8, 0x24, IM32(offset));
            return true;
          }
        }
      }
    }
  }
  return false;
}

static void assemble_line(Line *line, Code *code, IR **pir) {
  code->len = 0;

  switch(line->inst.op) {
  case NOOP:
    return;
  case MOV:
    if (assemble_mov(line, code))
      return;
    break;
  case MOVSX:
    if (line->inst.src.type == REG && line->inst.dst.type == REG) {
      int s = line->inst.src.u.reg.no;
      int d = line->inst.dst.u.reg.no;
      if (line->inst.src.u.reg.size == REG8) {
        if (line->inst.dst.u.reg.size == REG32) {
          if (opr_reg8(&line->inst.src) && !line->inst.dst.u.reg.x) {
            MAKE_CODE(&line->inst, code, 0x0f, 0xbe, 0xc0 + s + d * 8);
          } else {
            int pre = (!line->inst.src.u.reg.x ? 0x40 : 0x41) + (!line->inst.dst.u.reg.x ? 0 : 4);
            MAKE_CODE(&line->inst, code, pre, 0x0f, 0xbe, 0xc0 + s + d * 8);
          }
          return;
        } else if (line->inst.dst.u.reg.size == REG64) {
          int pre = (!line->inst.src.u.reg.x ? 0x48 : 0x49) + (!line->inst.dst.u.reg.x ? 0 : 4);
          MAKE_CODE(&line->inst, code, pre, 0x0f, 0xbe, 0xc0 + s + d * 8);
          return;
        }
      } else if (line->inst.src.u.reg.size == REG16) {
        if (line->inst.dst.u.reg.size == REG32) {
          if (!line->inst.src.u.reg.x && !line->inst.dst.u.reg.x) {
            MAKE_CODE(&line->inst, code, 0x0f, 0xbf, 0xc0 + s + d * 8);
          } else {
            int pre = (!line->inst.src.u.reg.x ? 0x40 : 0x41) + (!line->inst.dst.u.reg.x ? 0 : 4);
            MAKE_CODE(&line->inst, code, pre, 0x0f, 0xbf, 0xc0 + s + d * 8);
          }
          return;
        } else if (line->inst.dst.u.reg.size == REG64) {
          int pre = (!line->inst.src.u.reg.x ? 0x48 : 0x49) + (!line->inst.dst.u.reg.x ? 0 : 4);
          MAKE_CODE(&line->inst, code, pre, 0x0f, 0xbf, 0xc0 + s + d * 8);
          return;
        }
      } else if (line->inst.src.u.reg.size == REG32) {
        if (line->inst.dst.u.reg.size == REG64) {
          int pre = (!line->inst.src.u.reg.x ? 0x48 : 0x49) + (!line->inst.dst.u.reg.x ? 0 : 4);
          MAKE_CODE(&line->inst, code, pre, 0x63, 0xc0 + s + d * 8);
          return;
        }
      }
    }
    break;
  case LEA:
    if (line->inst.src.type == INDIRECT &&
        line->inst.dst.type == REG && line->inst.dst.u.reg.size == REG64) {
      int d = line->inst.dst.u.reg.no;
      if (line->inst.src.u.indirect.reg.no != RIP) {
        int s = line->inst.src.u.indirect.reg.no;
        int pre = (!line->inst.src.u.indirect.reg.x ? 0x48 : 0x49) + (!line->inst.dst.u.reg.x ? 0 : 4);
        if (line->inst.src.u.indirect.label == NULL) {
          long offset = line->inst.src.u.indirect.offset;
          if (line->inst.src.u.indirect.reg.no != RSP - RAX) {
            if (offset == 0 && line->inst.src.u.indirect.reg.no != RBP - RAX) {
              MAKE_CODE(&line->inst, code, pre, 0x8d, 0x00 + s + d * 8);
              return;
            } else if (is_im8(offset)) {
              MAKE_CODE(&line->inst, code, pre, 0x8d, 0x40 + s + d * 8, IM8(offset));
              return;
            } else if (is_im32(offset)) {
              MAKE_CODE(&line->inst, code, pre, 0x8d, 0x80 + s + d * 8, IM32(offset));
              return;
            }
          } else {
            if (offset == 0) {
              MAKE_CODE(&line->inst, code, pre, 0x8d, 0x04 + d * 8, 0x24);
              return;
            } else if (is_im8(offset)) {
              MAKE_CODE(&line->inst, code, pre, 0x8d, 0x44 + d * 8, 0x24, IM8(offset));
              return;
            } else if (is_im32(offset)) {
              MAKE_CODE(&line->inst, code, pre, 0x8d, 0x84 + d * 8, 0x24, IM32(offset));
              return;
            }
          }
        }
      } else {
        int pre = !line->inst.dst.u.reg.x ? 0x48 : 0x4c;
        if (line->inst.src.u.indirect.offset == 0) {
          *pir = new_ir_loc_rel32(line->inst.src.u.indirect.label, 3, 7);
          MAKE_CODE(&line->inst, code, pre, 0x8d, 0x05 + d * 8, IM32(-1));
          return;
        }
      }
    }
    break;
  case ADD:
    if (line->inst.src.type == REG && line->inst.dst.type == REG) {
      int s = line->inst.src.u.reg.no;
      int d = line->inst.dst.u.reg.no;
      if (line->inst.src.u.reg.size == REG8 && line->inst.dst.u.reg.size == REG8) {
        if (opr_reg8(&line->inst.src) && opr_reg8(&line->inst.dst)) {
          MAKE_CODE(&line->inst, code, 0x00, 0xc0 + s * 8 + d);
        } else {
          int pre = (!line->inst.dst.u.reg.x ? 0x40 : 0x41) + (!line->inst.src.u.reg.x ? 0 : 4);
          MAKE_CODE(&line->inst, code, pre, 0x00, 0xc0 + s * 8 + d);
        }
        return;
      } else if (line->inst.src.u.reg.size == REG32 && line->inst.dst.u.reg.size == REG32) {
        if (!line->inst.src.u.reg.x && !line->inst.dst.u.reg.x) {
          MAKE_CODE(&line->inst, code, 0x01, 0xc0 + s * 8 + d);
        } else {
          int pre = (!line->inst.dst.u.reg.x ? 0x40 : 0x41) + (!line->inst.src.u.reg.x ? 0 : 4);
          MAKE_CODE(&line->inst, code, pre, 0x01, 0xc0 + s * 8 + d);
        }
        return;
      } else if (line->inst.src.u.reg.size == REG64 && line->inst.dst.u.reg.size == REG64) {
        int pre = (!line->inst.dst.u.reg.x ? 0x48 : 0x49) + (!line->inst.src.u.reg.x ? 0 : 4);
        MAKE_CODE(&line->inst, code, pre, 0x01, 0xc0 + s * 8 + d);
        return;
      }
    } else if (line->inst.src.type == IMMEDIATE && line->inst.dst.type == REG) {
      if (line->inst.dst.u.reg.size == REG64) {
        int pre = !line->inst.dst.u.reg.x ? 0x48 : 0x49;
        int d = line->inst.dst.u.reg.no;
        if (is_im8(line->inst.src.u.immediate)) {
          MAKE_CODE(&line->inst, code, pre, 0x83, 0xc0 + d, IM8(line->inst.src.u.immediate));
          return;
        } else if (is_im32(line->inst.src.u.immediate)) {
          if (opr_regno(&line->inst.dst) == RAX - RAX)
            MAKE_CODE(&line->inst, code, pre, 0x05, IM32(line->inst.src.u.immediate));
          else
            MAKE_CODE(&line->inst, code, pre, 0x81, 0xc0 + d, IM32(line->inst.src.u.immediate));
          return;
        }
      }
    } else if (line->inst.src.type == INDIRECT && line->inst.dst.type == REG) {
      if (!line->inst.src.u.indirect.reg.x && line->inst.src.u.indirect.label == NULL &&
          line->inst.dst.u.reg.size == REG64) {
        int pre = (!line->inst.src.u.indirect.reg.x ? 0x48 : 0x49) + (!line->inst.dst.u.reg.x ? 0 : 4);
        int s = line->inst.src.u.indirect.reg.no;
        int d = line->inst.dst.u.reg.no;
        long offset = line->inst.src.u.indirect.offset;
        if (line->inst.src.u.indirect.reg.no != RSP - RAX) {
          if (offset == 0 && line->inst.src.u.indirect.reg.no != RBP - RAX) {
            MAKE_CODE(&line->inst, code, pre, 0x03, 0x00 + s + d * 8);
          } else if (is_im8(offset)) {
            MAKE_CODE(&line->inst, code, pre, 0x03, 0x40 + s + d * 8, IM8(offset));
          } else if (is_im32(offset)) {
            MAKE_CODE(&line->inst, code, pre, 0x03, 0x80 + s + d * 8, IM8(offset));
          }
        } else {
          if (offset == 0 && line->inst.src.u.indirect.reg.no != RBP - RAX) {
            MAKE_CODE(&line->inst, code, pre, 0x03, 0x04 + d * 8, 0x24);
          } else if (is_im8(offset)) {
            MAKE_CODE(&line->inst, code, pre, 0x03, 0x44 + d * 8, 0x24, IM8(offset));
          } else if (is_im32(offset)) {
            MAKE_CODE(&line->inst, code, pre, 0x03, 0x84 + d * 8, 0x24, IM8(offset));
          }
        }
      }
    }
    break;
  case ADDQ:
    if (line->inst.src.type == IMMEDIATE && line->inst.dst.type == INDIRECT) {
      if (!line->inst.dst.u.indirect.reg.x && line->inst.dst.u.indirect.label == NULL) {
        int pre = !line->inst.dst.u.indirect.reg.x ? 0x48 : 0x49;
        long value = line->inst.src.u.immediate;
        int d = line->inst.dst.u.indirect.reg.no;
        long offset = line->inst.dst.u.indirect.offset;
        if (is_im8(value)) {
          if (d != RSP - RAX) {
            if (offset == 0 && d != RBP - RAX) {
              MAKE_CODE(&line->inst, code, pre, 0x83, 0x00 + d, IM8(value));
              return;
            } else if (is_im8(offset)) {
              MAKE_CODE(&line->inst, code, pre, 0x83, 0x40 + d, IM8(offset), IM8(value));
              return;
            } else if (is_im32(offset)) {
              MAKE_CODE(&line->inst, code, pre, 0x83, 0x80 + d, IM32(offset), IM8(value));
              return;
            }
          } else {
            if (offset == 0) {
              MAKE_CODE(&line->inst, code, pre, 0x83, 0x04, 0x24, IM8(value));
              return;
            } else if (is_im8(offset)) {
              MAKE_CODE(&line->inst, code, pre, 0x83, 0x44, 0x24, IM8(offset), IM8(value));
              return;
            } else if (is_im32(offset)) {
              MAKE_CODE(&line->inst, code, pre, 0x83, 0x84, 0x24, IM32(offset), IM8(value));
              return;
            }
          }
        } else if (is_im32(value)) {
          if (d != RSP - RAX) {
            if (offset == 0 && d != RBP - RAX) {
              MAKE_CODE(&line->inst, code, pre, 0x81, 0x00 + d, IM32(value));
              return;
            } else if (is_im8(offset)) {
              MAKE_CODE(&line->inst, code, pre, 0x81, 0x40 + d, IM8(offset), IM32(value));
              return;
            } else if (is_im32(offset)) {
              MAKE_CODE(&line->inst, code, pre, 0x81, 0x80 + d, IM32(offset), IM32(value));
              return;
            }
          } else {
            if (offset == 0) {
              MAKE_CODE(&line->inst, code, pre, 0x81, 0x04, 0x24, IM32(value));
              return;
            } else if (is_im8(offset)) {
              MAKE_CODE(&line->inst, code, pre, 0x81, 0x44, 0x24, IM8(offset), IM32(value));
              return;
            } else if (is_im32(offset)) {
              MAKE_CODE(&line->inst, code, pre, 0x81, 0x84, 0x24, IM32(offset), IM32(value));
              return;
            }
          }
        }
      }
    }
    break;
  case SUB:
    if (line->inst.src.type == REG && line->inst.dst.type == REG) {
      int s = line->inst.src.u.reg.no;
      int d = line->inst.dst.u.reg.no;
      if (line->inst.src.u.reg.size == REG8 && line->inst.dst.u.reg.size == REG8) {
        if (opr_reg8(&line->inst.src) && opr_reg8(&line->inst.dst)) {
          MAKE_CODE(&line->inst, code, 0x28, 0xc0 + s * 8 + d);
        } else {
          int pre = (!line->inst.dst.u.reg.x ? 0x40 : 0x41) + (!line->inst.src.u.reg.x ? 0 : 4);
          MAKE_CODE(&line->inst, code, pre, 0x28, 0xc0 + s * 8 + d);
        }
        return;
      } else if (line->inst.src.u.reg.size == REG32 && line->inst.dst.u.reg.size == REG32) {
        if (!line->inst.src.u.reg.x && !line->inst.dst.u.reg.x) {
          MAKE_CODE(&line->inst, code, 0x29, 0xc0 + s * 8 + d);
        } else {
          int pre = (!line->inst.dst.u.reg.x ? 0x40 : 0x41) + (!line->inst.src.u.reg.x ? 0 : 4);
          MAKE_CODE(&line->inst, code, pre, 0x29, 0xc0 + s * 8 + d);
        }
        return;
      } else if (line->inst.src.u.reg.size == REG64 && line->inst.dst.u.reg.size == REG64) {
        int pre = (!line->inst.dst.u.reg.x ? 0x48 : 0x49) + (!line->inst.src.u.reg.x ? 0 : 4);
        MAKE_CODE(&line->inst, code, pre, 0x29, 0xc0 + s * 8 + d);
        return;
      }
    } else if (line->inst.src.type == IMMEDIATE && line->inst.dst.type == REG) {
      if (line->inst.dst.u.reg.size == REG64) {
        int pre = !line->inst.dst.u.reg.x ? 0x48 : 0x49;
        int d = line->inst.dst.u.reg.no;
        if (is_im8(line->inst.src.u.immediate)) {
          MAKE_CODE(&line->inst, code, pre, 0x83, 0xe8 + d, IM8(line->inst.src.u.immediate));
          return;
        } else if (is_im32(line->inst.src.u.immediate)) {
          if (opr_regno(&line->inst.dst) == RAX - RAX)
            MAKE_CODE(&line->inst, code, pre, 0x2d, IM32(line->inst.src.u.immediate));
          else
            MAKE_CODE(&line->inst, code, pre, 0x81, 0xe8 + d, IM32(line->inst.src.u.immediate));
          return;
        }
      }
    }
    break;
  case SUBQ:
    if (line->inst.src.type == IMMEDIATE && line->inst.dst.type == INDIRECT) {
      if (!line->inst.dst.u.indirect.reg.x && line->inst.dst.u.indirect.label == NULL) {
        long value = line->inst.src.u.immediate;
        int d = line->inst.dst.u.indirect.reg.no;
        long offset = line->inst.dst.u.indirect.offset;
        if (is_im8(value)) {
          if (line->inst.dst.u.indirect.reg.no != RSP - RAX) {
            if (offset == 0 && line->inst.dst.u.indirect.reg.no != RBP - RAX) {
              MAKE_CODE(&line->inst, code, 0x48, 0x83, 0x28 + d, IM8(value));
              return;
            } else if (is_im8(offset)) {
              MAKE_CODE(&line->inst, code, 0x48, 0x83, 0x68 + d, IM8(offset), IM8(value));
              return;
            } else if (is_im32(offset)) {
              MAKE_CODE(&line->inst, code, 0x48, 0x83, 0xa8 + d, IM32(offset), IM8(value));
              return;
            }
          } else {
            if (offset == 0) {
              MAKE_CODE(&line->inst, code, 0x48, 0x83, 0x2c, 0x24, IM8(value));
              return;
            } else if (is_im8(offset)) {
              MAKE_CODE(&line->inst, code, 0x48, 0x83, 0x6c, 0x24, IM8(offset), IM8(value));
              return;
            } else if (is_im32(offset)) {
              MAKE_CODE(&line->inst, code, 0x48, 0x83, 0xac, 0x24, IM32(offset), IM8(value));
              return;
            }
          }
        } else if (is_im32(value)) {
          if (line->inst.dst.u.indirect.reg.no != RSP - RAX) {
            if (offset == 0 && line->inst.dst.u.indirect.reg.no != RBP - RAX) {
              MAKE_CODE(&line->inst, code, 0x48, 0x81, 0x28 + d, IM32(value));
              return;
            } else if (is_im8(offset)) {
              MAKE_CODE(&line->inst, code, 0x48, 0x81, 0x68 + d, IM8(offset), IM32(value));
              return;
            } else if (is_im32(offset)) {
              MAKE_CODE(&line->inst, code, 0x48, 0x81, 0xa8 + d, IM32(offset), IM32(value));
              return;
            }
          } else {
            if (offset == 0) {
              MAKE_CODE(&line->inst, code, 0x48, 0x81, 0x2c, 0x24, IM32(value));
              return;
            } else if (is_im8(offset)) {
              MAKE_CODE(&line->inst, code, 0x48, 0x81, 0x6c, 0x24, IM8(offset), IM32(value));
              return;
            } else if (is_im32(offset)) {
              MAKE_CODE(&line->inst, code, 0x48, 0x81, 0xac, 0x24, IM32(offset), IM32(value));
              return;
            }
          }
        }
      }
    }
    break;
  case MUL:
    if (line->inst.src.type == REG && line->inst.dst.type == NOOPERAND) {
      if (line->inst.src.u.reg.size == REG32) {
        int s = line->inst.src.u.reg.no;
        if (!line->inst.src.u.reg.x) {
          MAKE_CODE(&line->inst, code, 0xf7, 0xe0 + s);
        } else {
          MAKE_CODE(&line->inst, code, 0x41, 0xf7, 0xe0 + s);
        }
        return;
      } else if (line->inst.src.u.reg.size == REG64) {
        int s = line->inst.src.u.reg.no;
        int pre = !line->inst.src.u.reg.x ? 0x48 : 0x49;
        MAKE_CODE(&line->inst, code, pre, 0xf7, 0xe0 + s);
        return;
      }
    }
    break;
  case IDIV:
    if (line->inst.src.type == REG && line->inst.dst.type == NOOPERAND) {
      if (line->inst.src.u.reg.size == REG32) {
        int s = line->inst.src.u.reg.no;
        if (!line->inst.src.u.reg.x) {
          MAKE_CODE(&line->inst, code, 0xf7, 0xf8 + s);
        } else {
          MAKE_CODE(&line->inst, code, 0x41, 0xf7, 0xf8 + s);
        }
        return;
      } else if (line->inst.src.u.reg.size == REG64) {
        int pre = !line->inst.src.u.reg.x ? 0x48 : 0x49;
        int s = line->inst.src.u.reg.no;
        MAKE_CODE(&line->inst, code, pre, 0xf7, 0xf8 + s);
        return;
      }
    }
    break;
  case NEG:
    if (line->inst.src.type == REG && line->inst.dst.type == NOOPERAND) {
      int s = line->inst.src.u.reg.no;
      if (line->inst.src.u.reg.size == REG16) {
        if (!line->inst.src.u.reg.x) {
          MAKE_CODE(&line->inst, code, 0x66, 0xf7, 0xd8 + s);
        } else {
          MAKE_CODE(&line->inst, code, 0x66, 0x41, 0xf7, 0xd8 + s);
        }
        return;
      } else if (line->inst.src.u.reg.size == REG32) {
        if (!line->inst.src.u.reg.x) {
          MAKE_CODE(&line->inst, code, 0xf7, 0xd8 + s);
        } else {
          MAKE_CODE(&line->inst, code, 0x41, 0xf7, 0xd8 + s);
        }
        return;
      } else if (line->inst.src.u.reg.size == REG64) {
        int pre = !line->inst.src.u.reg.x ? 0x48 : 0x49;
        MAKE_CODE(&line->inst, code, pre, 0xf7, 0xd8 + s);
        return;
      }
    }
    break;
  case NOT:
    if (line->inst.src.type == REG && line->inst.dst.type == NOOPERAND) {
      int s = line->inst.src.u.reg.no;
      if (line->inst.src.u.reg.size == REG16) {
        if (!line->inst.src.u.reg.x) {
          MAKE_CODE(&line->inst, code, 0x66, 0xf7, 0xd0 + s);
        } else {
          MAKE_CODE(&line->inst, code, 0x66, 0x41, 0xf7, 0xd0 + s);
        }
        return;
      } else if (line->inst.src.u.reg.size == REG32) {
        if (!line->inst.src.u.reg.x) {
          MAKE_CODE(&line->inst, code, 0xf7, 0xd0 + s);
        } else {
          MAKE_CODE(&line->inst, code, 0x41, 0xf7, 0xd0 + s);
        }
        return;
      }
    }
    break;
  case INC:
    if (line->inst.src.type == REG && line->inst.dst.type == NOOPERAND) {
      if (line->inst.src.u.reg.size == REG32) {
        int s = line->inst.src.u.reg.no;
        if (!line->inst.src.u.reg.x) {
          MAKE_CODE(&line->inst, code, 0xff, 0xc0 + s);
        } else {
          MAKE_CODE(&line->inst, code, 0x41, 0xff, 0xc0 + s);
        }
        return;
      } else if (line->inst.src.u.reg.size == REG64) {
        int pre = !line->inst.src.u.reg.x ? 0x48 : 0x49;
        int s = line->inst.src.u.reg.no;
        MAKE_CODE(&line->inst, code, pre, 0xff, 0xc0 + s);
        return;
      }
    }
    break;
  case INCL:
    if (line->inst.src.type == INDIRECT && line->inst.dst.type == NOOPERAND &&
        line->inst.src.u.indirect.reg.no != RIP) {
      int s = line->inst.src.u.indirect.reg.no;
      long offset = line->inst.src.u.indirect.offset;
      if (!line->inst.src.u.indirect.reg.x) {
        if (s != RSP - RAX) {
          if (offset == 0 && s != RBP - RAX) {
            MAKE_CODE(&line->inst, code, 0xff, 0x00 + s);
            return;
          } else if (is_im8(offset)) {
            MAKE_CODE(&line->inst, code, 0xff, 0x40 + s, IM8(offset));
            return;
          } else if (is_im32(offset)) {
            MAKE_CODE(&line->inst, code, 0xff, 0x80 + s, IM32(offset));
            return;
          }
        } else {
          if (offset == 0) {
            MAKE_CODE(&line->inst, code, 0xff, 0x04, 0x24);
            return;
          } else if (is_im8(offset)) {
            MAKE_CODE(&line->inst, code, 0xff, 0x44, 0x24, IM8(offset));
            return;
          } else if (is_im32(offset)) {
            MAKE_CODE(&line->inst, code, 0xff, 0x84, 0x24, IM32(offset));
            return;
          }
        }
      } else {
        if (s != RSP - RAX) {
          if (offset == 0 && s != RBP - RAX) {
            MAKE_CODE(&line->inst, code, 0x41, 0xff, 0x00 + s);
            return;
          } else if (is_im8(offset)) {
            MAKE_CODE(&line->inst, code, 0x41, 0xff, 0x40 + s, IM8(offset));
            return;
          } else if (is_im32(offset)) {
            MAKE_CODE(&line->inst, code, 0x41, 0xff, 0x80 + s, IM32(offset));
            return;
          }
        } else {
          if (offset == 0) {
            MAKE_CODE(&line->inst, code, 0x41, 0xff, 0x04, 0x24);
            return;
          } else if (is_im8(offset)) {
            MAKE_CODE(&line->inst, code, 0x41, 0xff, 0x44, 0x24, IM8(offset));
            return;
          } else if (is_im32(offset)) {
            MAKE_CODE(&line->inst, code, 0x41, 0xff, 0x84, 0x24, IM32(offset));
            return;
          }
        }
      }
    }
    break;
  case INCQ:
    if (line->inst.src.type == INDIRECT && line->inst.dst.type == NOOPERAND &&
        line->inst.src.u.indirect.reg.no != RIP) {
      int pre = !line->inst.src.u.indirect.reg.x ? 0x48 : 0x49;
      int s = line->inst.src.u.indirect.reg.no;
      long offset = line->inst.src.u.indirect.offset;
      if (line->inst.src.u.indirect.reg.no != RSP - RAX) {
        if (offset == 0 && line->inst.src.u.indirect.reg.no != RBP - RAX) {
          MAKE_CODE(&line->inst, code, pre, 0xff, 0x00 + s);
          return;
        } else if (is_im8(offset)) {
          MAKE_CODE(&line->inst, code, pre, 0xff, 0x40 + s, IM8(offset));
          return;
        } else if (is_im32(offset)) {
          MAKE_CODE(&line->inst, code, pre, 0xff, 0x80 + s, IM32(offset));
          return;
        }
      } else {
        if (offset == 0) {
          MAKE_CODE(&line->inst, code, pre, 0xff, 0x04, 0x24);
          return;
        } else if (is_im8(offset)) {
          MAKE_CODE(&line->inst, code, pre, 0xff, 0x44, 0x24, IM8(offset));
          return;
        } else if (is_im32(offset)) {
          MAKE_CODE(&line->inst, code, pre, 0xff, 0x84, 0x24, IM32(offset));
          return;
        }
      }
    }
    break;
  case DEC:
    if (line->inst.src.type == REG && line->inst.dst.type == NOOPERAND) {
      if (line->inst.src.u.reg.size == REG32) {
        int s = line->inst.src.u.reg.no;
        if (!line->inst.src.u.reg.x) {
          MAKE_CODE(&line->inst, code, 0xff, 0xc8 + s);
        } else {
          MAKE_CODE(&line->inst, code, 0x41, 0xff, 0xc8 + s);
        }
        return;
      } else if (line->inst.src.u.reg.size == REG64) {
        int pre = !line->inst.src.u.reg.x ? 0x48 : 0x49;
        int s = line->inst.src.u.reg.no;
        MAKE_CODE(&line->inst, code, pre, 0xff, 0xc8 + s);
        return;
      }
    }
    break;
  case DECL:
    if (line->inst.src.type == INDIRECT && line->inst.dst.type == NOOPERAND &&
        line->inst.src.u.indirect.reg.no != RIP) {
      int s = line->inst.src.u.indirect.reg.no;
      long offset = line->inst.src.u.indirect.offset;
      if (!line->inst.src.u.indirect.reg.x) {
        if (s != RSP - RAX) {
          if (offset == 0 && s != RBP - RAX) {
            MAKE_CODE(&line->inst, code, 0xff, 0x08 + s);
            return;
          } else if (is_im8(offset)) {
            MAKE_CODE(&line->inst, code, 0xff, 0x48 + s, IM8(offset));
            return;
          } else if (is_im32(offset)) {
            MAKE_CODE(&line->inst, code, 0xff, 0x88 + s, IM32(offset));
            return;
          }
        } else {
          if (offset == 0) {
            MAKE_CODE(&line->inst, code, 0xff, 0x0c, 0x24);
            return;
          } else if (is_im8(offset)) {
            MAKE_CODE(&line->inst, code, 0xff, 0x4c, 0x24, IM8(offset));
            return;
          } else if (is_im32(offset)) {
            MAKE_CODE(&line->inst, code, 0xff, 0x8c, 0x24, IM32(offset));
            return;
          }
        }
      } else {
        if (s != RSP - RAX) {
          if (offset == 0 && s != RBP - RAX) {
            MAKE_CODE(&line->inst, code, 0x41, 0xff, 0x08 + s);
            return;
          } else if (is_im8(offset)) {
            MAKE_CODE(&line->inst, code, 0x41, 0xff, 0x48 + s, IM8(offset));
            return;
          } else if (is_im32(offset)) {
            MAKE_CODE(&line->inst, code, 0x41, 0xff, 0x88 + s, IM32(offset));
            return;
          }
        } else {
          if (offset == 0) {
            MAKE_CODE(&line->inst, code, 0x41, 0xff, 0x0c, 0x24);
            return;
          } else if (is_im8(offset)) {
            MAKE_CODE(&line->inst, code, 0x41, 0xff, 0x4c, 0x24, IM8(offset));
            return;
          } else if (is_im32(offset)) {
            MAKE_CODE(&line->inst, code, 0x41, 0xff, 0x8c, 0x24, IM32(offset));
            return;
          }
        }
      }
    }
    break;
  case DECQ:
    if (line->inst.src.type == INDIRECT && line->inst.dst.type == NOOPERAND &&
        line->inst.src.u.indirect.reg.no != RIP) {
      int pre = !line->inst.src.u.indirect.reg.x ? 0x48 : 0x49;
      int s = line->inst.src.u.indirect.reg.no;
      long offset = line->inst.src.u.indirect.offset;
      if (line->inst.src.u.indirect.reg.no != RSP - RAX) {
        if (offset == 0 && line->inst.src.u.indirect.reg.no != RBP - RAX) {
          MAKE_CODE(&line->inst, code, pre, 0xff, 0x08 + s);
          return;
        } else if (is_im8(offset)) {
          MAKE_CODE(&line->inst, code, pre, 0xff, 0x48 + s, IM8(offset));
          return;
        } else if (is_im32(offset)) {
          MAKE_CODE(&line->inst, code, pre, 0xff, 0x88 + s, IM32(offset));
          return;
        }
      } else {
        if (offset == 0) {
          MAKE_CODE(&line->inst, code, pre, 0xff, 0x0c, 0x24);
          return;
        } else if (is_im8(offset)) {
          MAKE_CODE(&line->inst, code, pre, 0xff, 0x4c, 0x24, IM8(offset));
          return;
        } else if (is_im32(offset)) {
          MAKE_CODE(&line->inst, code, pre, 0xff, 0x8c, 0x24, IM32(offset));
          return;
        }
      }
    }
    break;
  case AND:
    if (line->inst.src.type == REG && line->inst.dst.type == REG) {
      int s = line->inst.src.u.reg.no;
      int d = line->inst.dst.u.reg.no;
      if (line->inst.src.u.reg.size == REG16 && line->inst.dst.u.reg.size == REG16) {
        if (!line->inst.src.u.reg.x && !line->inst.dst.u.reg.x) {
          MAKE_CODE(&line->inst, code, 0x66, 0x21, 0xc0 + s * 8 + d);
        } else {
          int pre = (!line->inst.dst.u.reg.x ? 0x40 : 0x41) + (!line->inst.src.u.reg.x ? 0 : 4);
          MAKE_CODE(&line->inst, code, 0x66, pre, 0x21, 0xc0 + s * 8 + d);
        }
        return;
      } if (line->inst.src.u.reg.size == REG32 && line->inst.dst.u.reg.size == REG32) {
        if (!line->inst.src.u.reg.x && !line->inst.dst.u.reg.x) {
          MAKE_CODE(&line->inst, code, 0x21, 0xc0 + s * 8 + d);
        } else {
          int pre = (!line->inst.dst.u.reg.x ? 0x40 : 0x41) + (!line->inst.src.u.reg.x ? 0 : 4);
          MAKE_CODE(&line->inst, code, pre, 0x21, 0xc0 + s * 8 + d);
        }
        return;
      } else if (line->inst.src.u.reg.size == REG64 && line->inst.dst.u.reg.size == REG64) {
        int pre = (!line->inst.dst.u.reg.x ? 0x48 : 0x49) + (!line->inst.src.u.reg.x ? 0 : 4);
        MAKE_CODE(&line->inst, code, pre, 0x21, 0xc0 + s * 8 + d);
        return;
      }
    }
    break;
  case OR:
    if (line->inst.src.type == REG && line->inst.dst.type == REG) {
      int s = line->inst.src.u.reg.no;
      int d = line->inst.dst.u.reg.no;
      if (line->inst.src.u.reg.size == REG8 && line->inst.dst.u.reg.size == REG8) {
        if (opr_reg8(&line->inst.src) && opr_reg8(&line->inst.dst)) {
          MAKE_CODE(&line->inst, code, 0x08, 0xc0 + s * 8 + d);
        } else {
          int pre = (!line->inst.dst.u.reg.x ? 0x40 : 0x41) + (!line->inst.src.u.reg.x ? 0 : 4);
          MAKE_CODE(&line->inst, code, pre, 0x08, 0xc0 + s * 8 + d);
        }
        return;
      } else if (line->inst.src.u.reg.size == REG16 && line->inst.dst.u.reg.size == REG16) {
        if (!line->inst.src.u.reg.x && !line->inst.dst.u.reg.x) {
          MAKE_CODE(&line->inst, code, 0x66, 0x09, 0xc0 + s * 8 + d);
        } else {
          int pre = (!line->inst.dst.u.reg.x ? 0x40 : 0x41) + (!line->inst.src.u.reg.x ? 0 : 4);
          MAKE_CODE(&line->inst, code, 0x66, pre, 0x09, 0xc0 + s * 8 + d);
        }
        return;
      } else if (line->inst.src.u.reg.size == REG32 && line->inst.dst.u.reg.size == REG32) {
        if (!line->inst.src.u.reg.x && !line->inst.dst.u.reg.x) {
          MAKE_CODE(&line->inst, code, 0x09, 0xc0 + s * 8 + d);
        } else {
          int pre = (!line->inst.dst.u.reg.x ? 0x40 : 0x41) + (!line->inst.src.u.reg.x ? 0 : 4);
          MAKE_CODE(&line->inst, code, pre, 0x09, 0xc0 + s * 8 + d);
        }
        return;
      } else if (line->inst.src.u.reg.size == REG64 && line->inst.dst.u.reg.size == REG64) {
        int pre = (!line->inst.dst.u.reg.x ? 0x48 : 0x49) + (!line->inst.src.u.reg.x ? 0 : 4);
        MAKE_CODE(&line->inst, code, pre, 0x09, 0xc0 + s * 8 + d);
        return;
      }
    }
    break;
  case XOR:
    if (line->inst.src.type == REG && line->inst.dst.type == REG) {
      int s = line->inst.src.u.reg.no;
      int d = line->inst.dst.u.reg.no;
      if (line->inst.src.u.reg.size == REG8 && line->inst.dst.u.reg.size == REG8) {
        if (opr_reg8(&line->inst.src) && opr_reg8(&line->inst.dst)) {
          MAKE_CODE(&line->inst, code, 0x30, 0xc0 + s * 8 + d);
        } else {
          int pre = (!line->inst.dst.u.reg.x ? 0x40 : 0x41) + (!line->inst.src.u.reg.x ? 0 : 4);
          MAKE_CODE(&line->inst, code, pre, 0x30, 0xc0 + s * 8 + d);
        }
        return;
      } else if (line->inst.src.u.reg.size == REG16 && line->inst.dst.u.reg.size == REG16) {
        if (!line->inst.src.u.reg.x && !line->inst.dst.u.reg.x) {
          MAKE_CODE(&line->inst, code, 0x66, 0x31, 0xc0 + s * 8 + d);
        } else {
          int pre = (!line->inst.dst.u.reg.x ? 0x40 : 0x41) + (!line->inst.src.u.reg.x ? 0 : 4);
          MAKE_CODE(&line->inst, code, 0x66, pre, 0x31, 0xc0 + s * 8 + d);
        }
        return;
      } else if (line->inst.src.u.reg.size == REG32 && line->inst.dst.u.reg.size == REG32) {
        if (!line->inst.src.u.reg.x && !line->inst.dst.u.reg.x) {
          MAKE_CODE(&line->inst, code, 0x31, 0xc0 + s * 8 + d);
        } else {
          int pre = (!line->inst.dst.u.reg.x ? 0x40 : 0x41) + (!line->inst.src.u.reg.x ? 0 : 4);
          MAKE_CODE(&line->inst, code, pre, 0x31, 0xc0 + s * 8 + d);
        }
        return;
      } else if (line->inst.src.u.reg.size == REG64 && line->inst.dst.u.reg.size == REG64) {
        int pre = (!line->inst.dst.u.reg.x ? 0x48 : 0x49) + (!line->inst.src.u.reg.x ? 0 : 4);
        MAKE_CODE(&line->inst, code, pre, 0x31, 0xc0 + s * 8 + d);
        return;
      }
    }
    break;
  case SHL:
    if (line->inst.src.type == REG && line->inst.dst.type == REG &&
        opr_regno(&line->inst.src) == CL - AL) {
      int d = line->inst.dst.u.reg.no;
      if (line->inst.dst.u.reg.size == REG8) {
        if (opr_reg8(&line->inst.dst)) {
          MAKE_CODE(&line->inst, code, 0xd2, 0xe0 + d);
        } else {
          int pre = !line->inst.dst.u.reg.x ? 0x40 : 0x41;
          MAKE_CODE(&line->inst, code, pre, 0xd2, 0xe0 + d);
        }
        return;
      } else if (line->inst.dst.u.reg.size == REG16) {
        if (!line->inst.dst.u.reg.x) {
          MAKE_CODE(&line->inst, code, 0x66, 0xd3, 0xe0 + d);
        } else {
          MAKE_CODE(&line->inst, code, 0x66, 0x41, 0xd3, 0xe0 + d);
        }
        return;
      } else if (line->inst.dst.u.reg.size == REG32) {
        if (!line->inst.dst.u.reg.x) {
          MAKE_CODE(&line->inst, code, 0xd3, 0xe0 + d);
        } else {
          MAKE_CODE(&line->inst, code, 0x41, 0xd3, 0xe0 + d);
        }
        return;
      } else if (line->inst.dst.u.reg.size == REG64) {
        int pre = line->inst.dst.u.reg.size == REG64 ? 0x48 : 0x49;
        MAKE_CODE(&line->inst, code, pre, 0xd3, 0xe0 + d);
        return;
      }
    }
    break;
  case SHR:
    if (line->inst.src.type == REG && line->inst.dst.type == REG &&
        opr_regno(&line->inst.src) == CL - AL) {
      int d = line->inst.dst.u.reg.no;
      if (line->inst.dst.u.reg.size == REG32) {
        if (!line->inst.dst.u.reg.x) {
          MAKE_CODE(&line->inst, code, 0xd3, 0xe8 + d);
        } else {
          MAKE_CODE(&line->inst, code, 0x41, 0xd3, 0xe8 + d);
        }
        return;
      } else if (line->inst.dst.u.reg.size == REG64) {
        int pre = !line->inst.dst.u.reg.x ? 0x48 : 0x49;
        MAKE_CODE(&line->inst, code, pre, 0xd3, 0xe8 + d);
        return;
      }
    }
    break;
  case CMP:
    if (line->inst.src.type == REG && line->inst.dst.type == REG) {
      int s = line->inst.src.u.reg.no;
      int d = line->inst.dst.u.reg.no;
      if (line->inst.src.u.reg.size == REG8 && line->inst.dst.u.reg.size == REG8) {
        if (opr_reg8(&line->inst.src) && opr_reg8(&line->inst.dst)) {
          MAKE_CODE(&line->inst, code, 0x38, 0xc0 + s * 8 + d);
        } else {
          int pre = (!line->inst.dst.u.reg.x ? 0x40 : 0x41) + (!line->inst.src.u.reg.x ? 0 : 4);
          MAKE_CODE(&line->inst, code, pre, 0x38, 0xc0 + s * 8 + d);
        }
        return;
      } else if (line->inst.src.u.reg.size == REG32 && line->inst.dst.u.reg.size == REG32) {
        if (!line->inst.src.u.reg.x && !line->inst.dst.u.reg.x) {
          MAKE_CODE(&line->inst, code, 0x39, 0xc0 + s * 8 + d);
        } else {
          int pre = (!line->inst.dst.u.reg.x ? 0x40 : 0x41) + (!line->inst.src.u.reg.x ? 0 : 4);
          MAKE_CODE(&line->inst, code, pre, 0x39, 0xc0 + s * 8 + d);
        }
        return;
      } else if (line->inst.src.u.reg.size == REG64 && line->inst.dst.u.reg.size == REG64) {
        int pre = (!line->inst.dst.u.reg.x ? 0x48 : 0x49) + (!line->inst.src.u.reg.x ? 0 : 4);
        MAKE_CODE(&line->inst, code, pre, 0x39, 0xc0 + s * 8 + d);
        return;
      }
    } else if (line->inst.src.type == IMMEDIATE && line->inst.dst.type == REG) {
      long value = line->inst.src.u.immediate;
      if (line->inst.dst.u.reg.size == REG8) {
        int d = line->inst.dst.u.reg.no;
        if (opr_regno(&line->inst.dst) == AL - AL) {
          MAKE_CODE(&line->inst, code, 0x3c, IM8(value));
        } else if (opr_reg8(&line->inst.dst)) {
          MAKE_CODE(&line->inst, code, 0x80, 0xf8 + d, IM8(value));
        } else {
          MAKE_CODE(&line->inst, code, 0x41, 0x80, 0xf8 + d, IM8(value));
        }
        return;
      } else if (line->inst.dst.u.reg.size == REG32) {
        int d = line->inst.dst.u.reg.no;
        if (!line->inst.dst.u.reg.x) {
          if (is_im8(value)) {
            MAKE_CODE(&line->inst, code, 0x83, 0xf8 + d, IM8(value));
            return;
          } else if (is_im32(value)) {
            if (opr_regno(&line->inst.dst) == EAX - EAX) {
              MAKE_CODE(&line->inst, code, 0x3d, IM32(value));
              return;
            } else {
              MAKE_CODE(&line->inst, code, 0x81, 0xf8 + d, IM32(value));
              return;
            }
          }
        } else {
          if (is_im8(value)) {
            MAKE_CODE(&line->inst, code, 0x41, 0x83, 0xf8 + d, IM8(value));
            return;
          } else if (is_im32(value)) {
            MAKE_CODE(&line->inst, code, 0x41, 0x81, 0xf8 + d, IM32(value));
            return;
          }
        }
      } else if (line->inst.dst.u.reg.size == REG64) {
        int d = line->inst.dst.u.reg.no;
        int pre = !line->inst.dst.u.reg.x ? 0x48 : 0x49;
        if (is_im8(value)) {
          MAKE_CODE(&line->inst, code, pre, 0x83, 0xf8 + d, IM8(value));
          return;
        } else if (is_im32(value)) {
          if (opr_regno(&line->inst.dst) == EAX - EAX) {
            MAKE_CODE(&line->inst, code, pre, 0x3d, IM32(value));
            return;
          } else {
            MAKE_CODE(&line->inst, code, pre, 0x81, 0xf8 + d, IM32(value));
            return;
          }
        }
      }
    }
    break;
  case TEST:
    if (line->inst.src.type == REG && line->inst.dst.type == REG) {
      int s = line->inst.src.u.reg.no;
      int d = line->inst.dst.u.reg.no;
      if (line->inst.src.u.reg.size == REG8 && line->inst.dst.u.reg.size == REG8) {
        if (opr_reg8(&line->inst.src) && opr_reg8(&line->inst.dst)) {
          MAKE_CODE(&line->inst, code, 0x84, 0xc0 + s * 8 + d);
        } else {
          int pre = (!line->inst.dst.u.reg.x ? 0x40 : 0x41) + (!line->inst.src.u.reg.x ? 0 : 4);
          MAKE_CODE(&line->inst, code, pre, 0x84, 0xc0 + s * 8 + d);
        }
        return;
      } else if (line->inst.src.u.reg.size == REG16 && line->inst.dst.u.reg.size == REG16) {
        if (!line->inst.src.u.reg.x && !line->inst.dst.u.reg.x) {
          MAKE_CODE(&line->inst, code, 0x66, 0x85, 0xc0 + s * 8 + d);
        } else {
          int pre = (!line->inst.dst.u.reg.x ? 0x40 : 0x41) + (!line->inst.src.u.reg.x ? 0 : 4);
          MAKE_CODE(&line->inst, code, 0x66, pre, 0x85, 0xc0 + s * 8 + d);
        }
        return;
      } else if (line->inst.src.u.reg.size == REG32 && line->inst.dst.u.reg.size == REG32) {
        if (!line->inst.src.u.reg.x && !line->inst.dst.u.reg.x) {
          MAKE_CODE(&line->inst, code, 0x85, 0xc0 + s * 8 + d);
        } else {
          int pre = (!line->inst.dst.u.reg.x ? 0x40 : 0x41) + (!line->inst.src.u.reg.x ? 0 : 4);
          MAKE_CODE(&line->inst, code, pre, 0x85, 0xc0 + s * 8 + d);
        }
        return;
      } else if (line->inst.src.u.reg.size == REG64 && line->inst.dst.u.reg.size == REG64) {
        int pre = (!line->inst.src.u.reg.x ? 0x48 : 0x49) + (!line->inst.dst.u.reg.x ? 0 : 4);
        MAKE_CODE(&line->inst, code, pre, 0x85, 0xc0 + s * 8 + d);
        return;
      }
    }
    break;
  case CLTD:
    if (line->inst.src.type == NOOPERAND && line->inst.dst.type == NOOPERAND) {
      MAKE_CODE(&line->inst, code, 0x99);
      return;
    }
    break;
  case CQTO:
    if (line->inst.src.type == NOOPERAND && line->inst.dst.type == NOOPERAND) {
      MAKE_CODE(&line->inst, code, 0x48, 0x99);
      return;
    }
    break;

  case SETO: case SETNO: case SETB:  case SETAE:
  case SETE: case SETNE: case SETBE: case SETA:
  case SETS: case SETNS: case SETP:  case SETNP:
  case SETL: case SETGE: case SETLE: case SETG:
    if (line->inst.src.type == REG && line->inst.dst.type == NOOPERAND) {
      int s = line->inst.src.u.reg.no;
      if (line->inst.src.u.reg.size == REG8) {
        if (opr_reg8(&line->inst.src)) {
          MAKE_CODE(&line->inst, code, 0x0f, 0x90 + (line->inst.op - SETO), 0xc0 + s);
        } else {
          int pre = !line->inst.src.u.reg.x ? 0x40 : 0x41;
          MAKE_CODE(&line->inst, code, pre, 0x0f, 0x90 + (line->inst.op - SETO), 0xc0 + s);
        }
        return;
      }
    }
    break;
  case PUSH:
    if (line->inst.src.type == REG && line->inst.dst.type == NOOPERAND) {
      if (line->inst.src.u.reg.size == REG64) {
        int d = line->inst.src.u.reg.no;
        if (!line->inst.src.u.reg.x) {
          MAKE_CODE(&line->inst, code, 0x50 + d);
        } else {
          MAKE_CODE(&line->inst, code, 0x41, 0x50 + d);
        }
        return;
      }
    }
    break;
  case POP:
    if (line->inst.src.type == REG && line->inst.dst.type == NOOPERAND) {
      if (line->inst.src.u.reg.size == REG64) {
        int d = line->inst.src.u.reg.no;
        if (!line->inst.src.u.reg.x) {
          MAKE_CODE(&line->inst, code, 0x58 + d);
        } else {
          MAKE_CODE(&line->inst, code, 0x41, 0x58 + d);
        }
        return;
      }
    }
    break;
  case JMP:
    if (line->inst.src.type != LABEL || line->inst.dst.type != NOOPERAND)
      error("Illegal oprand: JMP");
    *pir = new_ir_loc_rel32(line->inst.src.u.label, 1, 5);
    MAKE_CODE(&line->inst, code, 0xe9, IM32(-1));
    return;
  case JO: case JNO: case JB:  case JAE:
  case JE: case JNE: case JBE: case JA:
  case JS: case JNS: case JP:  case JNP:
  case JL: case JGE: case JLE: case JG:
    if (line->inst.src.type == LABEL && line->inst.dst.type == NOOPERAND) {
      // TODO: Handle short jump.
      *pir = new_ir_loc_rel32(line->inst.src.u.label, 2, 6);
      MAKE_CODE(&line->inst, code, 0x0f, 0x80 + (line->inst.op - JO), IM32(-1));
      return;
    }
    break;
  case CALL:
    if (line->inst.src.type == LABEL && line->inst.dst.type == NOOPERAND) {
      *pir = new_ir_loc_rel32(line->inst.src.u.label, 1, 5);
      MAKE_CODE(&line->inst, code, 0xe8, IM32(-1));
      return;
    } if (line->inst.src.type == DEREF_REG && line->inst.dst.type == NOOPERAND) {
      int s = line->inst.src.u.deref_reg.no;
      if (!line->inst.src.u.deref_reg.x) {
        MAKE_CODE(&line->inst, code, 0xff, 0xd0 + s);
      } else {
        MAKE_CODE(&line->inst, code, 0x41, 0xff, 0xd0 + s);
      }
      return;
    }
    break;
  case RET:
    MAKE_CODE(&line->inst, code, 0xc3);
    return;
  case INT:
    if (line->inst.src.type == IMMEDIATE && line->inst.dst.type == NOOPERAND) {
      long value = line->inst.src.u.immediate;
      MAKE_CODE(&line->inst, code, 0xcd, IM8(value));
      return;
    }
    return;
  case SYSCALL:
    MAKE_CODE(&line->inst, code, 0x0f, 0x05);
    return;
  default:
    break;
  }

  fprintf(stderr, "op=%2d: not handled: %s\n", line->inst.op, line->rawline);
  err = true;
}

static void assemble_file(FILE *fp, Vector **section_irs) {
  for (int i = 0; i < SECTION_COUNT; ++i)
    section_irs[i] = new_vector();

  for (;;) {
    char *rawline = NULL;
    size_t capa = 0;
    ssize_t len = getline_(&rawline, &capa, fp, 0);
    if (len == EOF)
      break;

    Vector *irs = section_irs[current_section];
    Line *line = parse_line(rawline);
    if (line->label != NULL)
      vec_push(irs, new_ir_label(line->label));
    if (line->dir == NODIRECTIVE) {
      IR *ir = NULL;
      Code code;
      assemble_line(line, &code, &ir);
      if (ir != NULL)
        vec_push(irs, ir);
      if (code.len > 0)
        vec_push(irs, new_ir_code(&code));
    } else {
      handle_directive(line->dir, line->directive_line, section_irs);
    }
  }
}

static void emit_irs(Vector **section_irs) {
  for (int sec = 0; sec < SECTION_COUNT; ++sec) {
    Vector *irs = section_irs[sec];
    for (int i = 0, len = irs->len; i < len; ++i) {
      IR *ir = irs->data[i];
      switch (ir->type) {
      case IR_LABEL:
        add_label(sec, ir->u.label);
        break;
      case IR_CODE:
        add_code(ir->u.code.buf, ir->u.code.len);
        break;
      case IR_DATA:
        add_section_data(sec, ir->u.data.buf, ir->u.data.len);
        break;
      case IR_BSS:
        add_bss(ir->u.bss);
        break;
      case IR_ALIGN:
        align_section_size(sec, ir->u.align);
        break;
      case IR_LOC_REL32:
        add_loc_rel32(ir->u.loc.label, ir->u.loc.ofs, ir->u.loc.base);
        break;
      case IR_LOC_ABS64:
        add_loc_abs64(sec, ir->u.loc.label, ir->u.loc.ofs);
        break;
      default:  assert(false); break;
      }
    }
  }
}

static void assemble(FILE *fp) {
  Vector *section_irs[SECTION_COUNT];
  assemble_file(fp, section_irs);
  emit_irs(section_irs);
}

static void put_padding(FILE* fp, uintptr_t start) {
  long cur = ftell(fp);
   if (start > (size_t)cur) {
    size_t size = start - (uintptr_t)cur;
    char* buf = calloc(1, size);
    fwrite(buf, size, 1, fp);
    free(buf);
  }
}

int main(int argc, char* argv[]) {
  const char *ofn = "a.out";
  int iarg;

  for (iarg = 1; iarg < argc; ++iarg) {
    if (*argv[iarg] != '-')
      break;
    if (strncmp(argv[iarg], "-o", 2) == 0)
      ofn = strdup_(argv[iarg] + 2);
  }

  current_section = SEC_CODE;
  init_gen();

  FILE* fp = fopen(ofn, "wb");
  if (fp == NULL) {
    fprintf(stderr, "Failed to open output file: %s\n", ofn);
    return 1;
  }

  if (iarg < argc) {
    for (int i = iarg; i < argc; ++i) {
      FILE *fp = fopen(argv[i], "r");
      if (fp == NULL)
        error("Cannot open %s\n", argv[i]);
      assemble(fp);
      fclose(fp);
    }
  } else {
    assemble(stdin);
  }

  if (err) {
    if (fp != NULL) {
      fclose(fp);
      remove(ofn);
    }
    return 1;
  }

  resolve_label_locations(LOAD_ADDRESS);

  size_t codefilesz, codememsz;
  size_t datafilesz, datamemsz;
  uintptr_t codeloadadr, dataloadadr;
  get_section_size(0, &codefilesz, &codememsz, &codeloadadr);
  get_section_size(1, &datafilesz, &datamemsz, &dataloadadr);

  uintptr_t entry = label_adr("_start");
  if (entry == (uintptr_t)-1)
    error("Cannot find label: `%s'", "_start");

  int phnum = datamemsz > 0 ? 2 : 1;

  out_elf_header(fp, entry, phnum);
  out_program_header(fp, 0, PROG_START, codeloadadr, codefilesz, codememsz);
  if (phnum > 1)
    out_program_header(fp, 1, ALIGN(PROG_START + codefilesz, 0x1000), dataloadadr, datafilesz, datamemsz);

  put_padding(fp, PROG_START);
  output_section(fp, 0);
  if (phnum > 1) {
    put_padding(fp, ALIGN(PROG_START + codefilesz, 0x1000));
    output_section(fp, 1);
  }
  fclose(fp);

#if !defined(__XV6) && defined(__linux__)
  if (chmod(ofn, 0755) == -1) {
    perror("chmod failed\n");
    return 1;
  }
#endif
  return 0;
}

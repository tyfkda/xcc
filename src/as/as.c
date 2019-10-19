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

#ifndef ADD_CODE
#define ADD_CODE(...)  do { unsigned char buf[] = {__VA_ARGS__}; add_code(buf, sizeof(buf)); } while (0)
#endif

#define IM8(x)   (x)
#define IM16(x)  (x), ((x) >> 8)
#define IM32(x)  (x), ((x) >> 8), ((x) >> 16), ((x) >> 24)
#define IM64(x)  (x), ((x) >> 8), ((x) >> 16), ((x) >> 24), ((x) >> 32), ((x) >> 40), ((x) >> 48), ((x) >> 56)

typedef struct {
  const char *label;
  enum Opcode op;
  Operand src;
  Operand dst;

  enum DirectiveType dir;
  const char *directive_line;
} Line;

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

static void handle_directive(enum DirectiveType dir, const char *p) {
  switch (dir) {
  case DT_ASCII:
    {
      if (*p != '"')
        error("`\"' expected");
      ++p;
      size_t len = unescape_string(p, NULL);
      char *str = malloc(len);
      unescape_string(p, str);

      add_section_data(current_section, str, len);

      free(str);
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
      add_label(SEC_BSS, label);
      add_bss(count);
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
      align_section_size(current_section, align);
    }
    break;

  case DT_BYTE:
  case DT_WORD:
  case DT_LONG:
  case DT_QUAD:
    {
      long value;
      if (parse_immediate(&p, &value)) {
        switch (dir) {
        case DT_BYTE:
          {  // TODO: Target endian.
            int8_t x = value;
            add_section_data(current_section, &x, sizeof(x));
          }
          break;
        case DT_WORD:
          {  // TODO: Target endian.
            int16_t x = value;
            add_section_data(current_section, &x, sizeof(x));
          }
          break;
        case DT_LONG:
          {  // TODO: Target endian.
            int32_t x = value;
            add_section_data(current_section, &x, sizeof(x));
          }
          break;
        case DT_QUAD:
          {  // TODO: Target endian.
            int64_t x = value;
            add_section_data(current_section, &x, sizeof(x));
          }
          break;
        default:
          break;
        }
      } else {
        const char *label = parse_label(&p);
        if (label != NULL) {
          add_loc_abs64(current_section, label, 0);
          int64_t x = -1;
          add_section_data(current_section, &x, sizeof(x));
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

static void parse_line(const char *str, Line *line) {
  // Clear
  line->label = NULL;
  line->op = NOOP;
  line->src.type = line->dst.type = NOOPERAND;
  line->dir = NODIRECTIVE;

  const char *p = str;
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
    line->op = parse_opcode(&p);
    if (line->op != NOOP) {
      if (parse_operand(&p, &line->src)) {
        p = skip_whitespace(p);
        if (*p == ',') {
          p = skip_whitespace(p + 1);
          parse_operand(&p, &line->dst);
          p = skip_whitespace(p);
        }
      }
    }

    if (*p != '\0' && !(*p == '/' && p[1] == '/')) {
      fprintf(stderr, "Syntax error: %s\n", p);
      err = true;
    }
  }
}

static bool assemble_mov(const Line *line) {
  if (line->src.type == REG && line->dst.type == REG) {
    if (is_reg8ss(line->src.u.reg) && is_reg8ss(line->dst.u.reg)) {
      int s = (line->src.u.reg - AL) & 7;
      int d = (line->dst.u.reg - AL) & 7;
      if (is_reg8(line->src.u.reg) && is_reg8(line->dst.u.reg)) {
        ADD_CODE(0x88, 0xc0 + d + s * 8);
      } else {
        int pre = (is_reg8s(line->dst.u.reg) ? 0x40 : 0x41) + (is_reg8s(line->src.u.reg) ? 0 : 4);
        ADD_CODE(pre, 0x88, 0xc0 + d + s * 8);
      }
      return true;
    } else if (is_reg16s(line->src.u.reg) && is_reg16s(line->dst.u.reg)) {
      int s = (line->src.u.reg - AX) & 7;
      int d = (line->dst.u.reg - AX) & 7;
      if (is_reg16(line->src.u.reg) && is_reg16(line->dst.u.reg)) {
        ADD_CODE(0x66, 0x89, 0xc0 + d + s * 8);
      } else {
        int pre = (is_reg16(line->dst.u.reg) ? 0x40 : 0x41) + (is_reg16(line->src.u.reg) ? 0 : 4);
        ADD_CODE(0x66, pre, 0x89, 0xc0 + d + s * 8);
      }
      return true;
    } else if (is_reg32s(line->src.u.reg) && is_reg32s(line->dst.u.reg)) {
      int s = (line->src.u.reg - EAX) & 7;
      int d = (line->dst.u.reg - EAX) & 7;
      if (is_reg32(line->src.u.reg) && is_reg32(line->dst.u.reg)) {
        ADD_CODE(0x89, 0xc0 + d + s * 8);
      } else {
        int pre = (is_reg32(line->dst.u.reg) ? 0x40 : 0x41) + (is_reg32(line->src.u.reg) ? 0 : 4);
        ADD_CODE(pre, 0x89, 0xc0 + d + s * 8);
      }
      return true;
    } else if (is_reg64s(line->src.u.reg) && is_reg64s(line->dst.u.reg)) {
      int pre = (is_reg64(line->dst.u.reg) ? 0x48 : 0x49) + (is_reg64(line->src.u.reg) ? 0 : 4);
      int s = (line->src.u.reg - RAX) & 7;
      int d = (line->dst.u.reg - RAX) & 7;
      ADD_CODE(pre, 0x89, 0xc0 + d + s * 8);
      return true;
    }
  } else if (line->src.type == IMMEDIATE && line->dst.type == REG) {
    if (is_reg8ss(line->dst.u.reg)) {
      int d = (line->dst.u.reg - AL) & 7;
      if (is_reg8(line->dst.u.reg)) {
        ADD_CODE(0xb0 + d, IM8(line->src.u.immediate));
      } else {
        int pre = is_reg8s(line->dst.u.reg) ? 0x40 : 0x41;
        ADD_CODE(pre, 0xb0 + d, IM8(line->src.u.immediate));
      }
      return true;
    } else if (is_reg16s(line->dst.u.reg)) {
      int d = (line->dst.u.reg - AX) & 7;
      if (is_reg16(line->dst.u.reg)) {
        ADD_CODE(0x66, 0xb8 + d, IM16(line->src.u.immediate));
      } else {
        ADD_CODE(0x66, 0x41, 0xb8 + d, IM16(line->src.u.immediate));
      }
      return true;
    } else if (is_reg32(line->dst.u.reg)) {
      int d = line->dst.u.reg - EAX;
      ADD_CODE(0xb8 + d, IM32(line->src.u.immediate));
      return true;
    } else if (is_reg32x(line->dst.u.reg)) {
      int d = line->dst.u.reg - R8D;
      ADD_CODE(0x41, 0xb8 + d, IM32(line->src.u.immediate));
      return true;
    } else if (is_reg64s(line->dst.u.reg)) {
      int pre = is_reg64(line->dst.u.reg) ? 0x48 : 0x49;
      int d = (line->dst.u.reg - RAX) & 7;
      if (is_im32(line->src.u.immediate)) {
        ADD_CODE(pre, 0xc7, 0xc0 + d, IM32(line->src.u.immediate));
      } else {
        ADD_CODE(pre, 0xb8 + d, IM64(line->src.u.immediate));
      }
      return true;
    }
  } else if (line->src.type == INDIRECT && line->dst.type == REG) {
    if (line->src.u.indirect.label == NULL) {
      if (is_reg64s(line->src.u.indirect.reg)) {
        int s = (line->src.u.indirect.reg - RAX) & 7;
        long offset = line->src.u.indirect.offset;
        if (is_reg8ss(line->dst.u.reg)) {
          if (is_reg64(line->src.u.indirect.reg) && is_reg8(line->dst.u.reg)) {
            int d = (line->dst.u.reg - AL) & 7;
            if (s != RSP - RAX) {
              if (offset == 0 && s != RBP - RAX) {
                ADD_CODE(0x8a, 0x00 + s + d * 8);
                return true;
              } else if (is_im8(offset)) {
                ADD_CODE(0x8a, 0x40 + s + d * 8, IM8(offset));
                return true;
              } else if (is_im32(offset)) {
                ADD_CODE(0x8a, 0x80 + s + d * 8, IM32(offset));
                return true;
              }
            } else {
              if (offset == 0) {
                ADD_CODE(0x8a, 0x04 + d * 8, 0x24);
                return true;
              } else if (is_im8(offset)) {
                ADD_CODE(0x8a, 0x44 + d * 8, 0x24, IM8(offset));
                return true;
              } else if (is_im32(offset)) {
                ADD_CODE(0x8a, 0x84 + d * 8, 0x24, IM32(offset));
                return true;
              }
            }
          } else {
            int pre = (is_reg64(line->src.u.indirect.reg) ? 0x40 : 0x41) + (is_reg8s(line->dst.u.reg) ? 0 : 4);
            int d = (line->dst.u.reg - AL) & 7;
            if (s != RSP - RAX) {
              if (offset == 0 && s != RBP - RAX) {
                ADD_CODE(pre, 0x8a, 0x00 + s + d * 8);
                return true;
              } else if (is_im8(offset)) {
                ADD_CODE(pre, 0x8a, 0x40 + s + d * 8, IM8(offset));
                return true;
              } else if (is_im32(offset)) {
                ADD_CODE(pre, 0x8a, 0x80 + s + d * 8, IM32(offset));
                return true;
              }
            } else {
              if (offset == 0) {
                ADD_CODE(pre, 0x8a, 0x04 + d * 8, 0x24);
                return true;
              } else if (is_im8(offset)) {
                ADD_CODE(pre, 0x8a, 0x44 + d * 8, 0x24, IM8(offset));
                return true;
              } else if (is_im32(offset)) {
                ADD_CODE(pre, 0x8a, 0x84 + d * 8, 0x24, IM32(offset));
                return true;
              }
            }
          }
        } else if (is_reg16s(line->dst.u.reg)) {
          int d = (line->dst.u.reg - AX) & 7;
          if (is_reg16(line->dst.u.reg) && is_reg64(line->src.u.indirect.reg)) {
            if (s != RSP - RAX) {
              if (offset == 0 && s != RBP - RAX) {
                ADD_CODE(0x66, 0x8b, 0x00 + s + d * 8);
                return true;
              } else if (is_im8(offset)) {
                ADD_CODE(0x66, 0x8b, 0x40 + s + d * 8, IM8(offset));
                return true;
              } else if (is_im32(offset)) {
                ADD_CODE(0x66, 0x8b, 0x80 + s + d * 8, IM32(offset));
                return true;
              }
            } else {
              if (offset == 0) {
                ADD_CODE(0x66, 0x8b, 0x04 + d * 8, 0x24);
                return true;
              } else if (is_im8(offset)) {
                ADD_CODE(0x66, 0x8b, 0x44 + d * 8, 0x24, IM8(offset));
                return true;
              } else if (is_im32(offset)) {
                ADD_CODE(0x66, 0x8b, 0x84 + d * 8, 0x24, IM32(offset));
                return true;
              }
            }
          } else {
            int pre = (is_reg64(line->src.u.indirect.reg) ? 0x40 : 0x41) + (is_reg16(line->dst.u.reg) ? 0 : 4);
            if (s != RSP - RAX) {
              if (offset == 0 && s != RBP - RAX) {
                ADD_CODE(0x66, pre, 0x8b, 0x00 + s + d * 8);
                return true;
              } else if (is_im8(offset)) {
                ADD_CODE(0x66, pre, 0x8b, 0x40 + s + d * 8, IM8(offset));
                return true;
              } else if (is_im32(offset)) {
                ADD_CODE(0x66, pre, 0x8b, 0x80 + s + d * 8, IM32(offset));
                return true;
              }
            } else {
              if (offset == 0) {
                ADD_CODE(0x66, pre, 0x8b, 0x04 + d * 8, 0x24);
                return true;
              } else if (is_im8(offset)) {
                ADD_CODE(0x66, pre, 0x8b, 0x44 + d * 8, 0x24, IM8(offset));
                return true;
              } else if (is_im32(offset)) {
                ADD_CODE(0x66, pre, 0x8b, 0x84 + d * 8, 0x24, IM32(offset));
                return true;
              }
            }
          }
        } else if (is_reg32s(line->dst.u.reg)) {
          int d = (line->dst.u.reg - EAX) & 7;
          if (is_reg64(line->src.u.indirect.reg) && is_reg32(line->dst.u.reg)) {
            if (s != RSP - RAX) {
              if (offset == 0 && s != RBP - RAX) {
                ADD_CODE(0x8b, 0x00 + s + d * 8);
                return true;
              } else if (is_im8(offset)) {
                ADD_CODE(0x8b, 0x40 + s + d * 8, IM8(offset));
                return true;
              } else if (is_im32(offset)) {
                ADD_CODE(0x8b, 0x80 + s + d * 8, IM32(offset));
                return true;
              }
            } else {
              if (offset == 0) {
                ADD_CODE(0x8b, 0x04 + d * 8, 0x24);
                return true;
              } else if (is_im8(offset)) {
                ADD_CODE(0x8b, 0x44 + d * 8, 0x24, IM8(offset));
                return true;
              } else if (is_im32(offset)) {
                ADD_CODE(0x8b, 0x84 + d * 8, 0x24, IM32(offset));
                return true;
              }
            }
          } else {
            int pre = (is_reg64(line->src.u.indirect.reg) ? 0x40 : 0x41) + (is_reg32(line->dst.u.reg) ? 0 : 4);
            if (s != RSP - RAX) {
              if (offset == 0 && s != RBP - RAX) {
                ADD_CODE(pre, 0x8b, 0x00 + s + d * 8);
                return true;
              } else if (is_im8(offset)) {
                ADD_CODE(pre, 0x8b, 0x40 + s + d * 8, IM8(offset));
                return true;
              } else if (is_im32(offset)) {
                ADD_CODE(pre, 0x8b, 0x80 + s + d * 8, IM32(offset));
                return true;
              }
            } else {
              if (offset == 0) {
                ADD_CODE(pre, 0x8b, 0x04 + d * 8, 0x24);
                return true;
              } else if (is_im8(offset)) {
                ADD_CODE(pre, 0x8b, 0x44 + d * 8, 0x24, IM8(offset));
                return true;
              } else if (is_im32(offset)) {
                ADD_CODE(pre, 0x8b, 0x84 + d * 8, 0x24, IM32(offset));
                return true;
              }
            }
          }
        } else if (is_reg64s(line->dst.u.reg)) {
          int d = (line->dst.u.reg - RAX) & 7;
          int pre = (is_reg64(line->src.u.indirect.reg) ? 0x48 : 0x49) + (is_reg64(line->dst.u.reg) ? 0 : 4);
          if (s != RSP - RAX) {
            if (offset == 0 && s != RBP - RAX) {
              ADD_CODE(pre, 0x8b, 0x00 + s + d * 8);
              return true;
            } else if (is_im8(offset)) {
              ADD_CODE(pre, 0x8b, 0x40 + s + d * 8, IM8(offset));
              return true;
            } else if (is_im32(offset)) {
              ADD_CODE(pre, 0x8b, 0x80 + s + d * 8, IM32(offset));
              return true;
            }
          } else {
            if (offset == 0) {
              ADD_CODE(pre, 0x8b, 0x04 + d * 8, 0x24);
              return true;
            } else if (is_im8(offset)) {
              ADD_CODE(pre, 0x8b, 0x44 + d * 8, 0x24, IM8(offset));
              return true;
            } else if (is_im32(offset)) {
              ADD_CODE(pre, 0x8b, 0x84 + d * 8, 0x24, IM32(offset));
              return true;
            }
          }
        }
      }
    }
  } else if (line->src.type == REG && line->dst.type == INDIRECT &&
             is_reg64s(line->dst.u.indirect.reg)) {
    if (line->dst.u.indirect.label == NULL) {
      int d = (line->dst.u.indirect.reg - RAX) & 7;
      long offset = line->dst.u.indirect.offset;
      if (is_reg8ss(line->src.u.reg)) {
        int s = (line->src.u.reg - AL) & 7;
        if (is_reg8(line->src.u.reg) && is_reg64(line->dst.u.indirect.reg)) {
          if (d != RSP - RAX) {
            if (offset == 0 && d != RBP - RAX) {
              ADD_CODE(0x88, 0x00 + d + s * 8);
              return true;
            } else if (is_im8(offset)) {
              ADD_CODE(0x88, 0x40 + d + s * 8, IM8(offset));
              return true;
            } else if (is_im32(offset)) {
              ADD_CODE(0x89, 0x80 + d + s * 8, IM32(offset));
              return true;
            }
          } else {
            if (offset == 0) {
              ADD_CODE(0x88, 0x04 + s * 8, 0x24);
              return true;
            } else if (is_im8(offset)) {
              ADD_CODE(0x88, 0x44 + s * 8, 0x24, IM8(offset));
              return true;
            } else if (is_im32(offset)) {
              ADD_CODE(0x89, 0x84 + s * 8, 0x24, IM32(offset));
              return true;
            }
          }
        } else {
          int pre = (is_reg64(line->dst.u.indirect.reg) ? 0x40 : 0x41) + (is_reg8s(line->src.u.reg) ? 0 : 4);
          if (d != RSP - RAX) {
            if (offset == 0 && d != RBP - RAX) {
              ADD_CODE(pre, 0x88, 0x00 + d + s * 8);
              return true;
            } else if (is_im8(offset)) {
              ADD_CODE(pre, 0x88, 0x40 + d + s * 8, IM8(offset));
              return true;
            } else if (is_im32(offset)) {
              ADD_CODE(pre, 0x89, 0x80 + d + s * 8, IM32(offset));
              return true;
            }
          } else {
            if (offset == 0) {
              ADD_CODE(pre, 0x88, 0x04 + s * 8, 0x24);
              return true;
            } else if (is_im8(offset)) {
              ADD_CODE(pre, 0x88, 0x44 + s * 8, 0x24, IM8(offset));
              return true;
            } else if (is_im32(offset)) {
              ADD_CODE(pre, 0x89, 0x84 + s * 8, 0x24, IM32(offset));
              return true;
            }
          }
        }
      } else if (is_reg16s(line->src.u.reg)) {
        int s = (line->src.u.reg - AX) & 7;
        if (is_reg16(line->src.u.reg) && is_reg64(line->dst.u.indirect.reg)) {
          if (d != RSP - RAX) {
            if (offset == 0 && d != RBP - RAX) {
              ADD_CODE(0x66, 0x89, 0x00 + d + s * 8);
              return true;
            } else if (is_im8(offset)) {
              ADD_CODE(0x66, 0x89, 0x40 + d + s * 8, IM8(offset));
              return true;
            } else if (is_im32(offset)) {
              ADD_CODE(0x66, 0x89, 0x80 + d + s * 8, IM32(offset));
              return true;
            }
          } else {
            if (offset == 0) {
              ADD_CODE(0x66, 0x89, 0x04 + s * 8, 0x24);
              return true;
            } else if (is_im8(offset)) {
              ADD_CODE(0x66, 0x89, 0x44 + s * 8, 0x24, IM8(offset));
              return true;
            } else if (is_im32(offset)) {
              ADD_CODE(0x66, 0x89, 0x84 + s * 8, 0x24, IM32(offset));
              return true;
            }
          }
        } else {
          int pre = (is_reg64(line->dst.u.indirect.reg) ? 0x40 : 0x41) + (is_reg16(line->src.u.reg) ? 0 : 4);
          if (d != RSP - RAX) {
            if (offset == 0 && d != RBP - RAX) {
              ADD_CODE(0x66, pre, 0x89, 0x00 + d + s * 8);
              return true;
            } else if (is_im8(offset)) {
              ADD_CODE(0x66, pre, 0x89, 0x40 + d + s * 8, IM8(offset));
              return true;
            } else if (is_im32(offset)) {
              ADD_CODE(0x66, pre, 0x89, 0x80 + d + s * 8, IM32(offset));
              return true;
            }
          } else {
            if (offset == 0) {
              ADD_CODE(0x66, pre, 0x89, 0x04 + s * 8, 0x24);
              return true;
            } else if (is_im8(offset)) {
              ADD_CODE(0x66, pre, 0x89, 0x44 + s * 8, 0x24, IM8(offset));
              return true;
            } else if (is_im32(offset)) {
              ADD_CODE(0x66, pre, 0x89, 0x84 + s * 8, 0x24, IM32(offset));
              return true;
            }
          }
        }
      } else if (is_reg32s(line->src.u.reg)) {
        int s = (line->src.u.reg - EAX) & 7;
        if (is_reg64(line->dst.u.indirect.reg) && is_reg32(line->src.u.reg)) {
          if (d != RSP - RAX) {
            if (offset == 0 && d != RBP - RAX) {
              ADD_CODE(0x89, 0x00 + d + s * 8);
              return true;
            } else if (is_im8(offset)) {
              ADD_CODE(0x89, 0x40 + d + s * 8, IM8(offset));
              return true;
            } else if (is_im32(offset)) {
              ADD_CODE(0x89, 0x80 + d + s * 8, IM32(offset));
              return true;
            }
          } else {
            if (offset == 0) {
              ADD_CODE(0x89, 0x04 + s * 8, 0x24);
              return true;
            } else if (is_im8(offset)) {
              ADD_CODE(0x89, 0x44 + s * 8, 0x24, IM8(offset));
              return true;
            } else if (is_im32(offset)) {
              ADD_CODE(0x89, 0x84 + s * 8, 0x24, IM32(offset));
              return true;
            }
          }
        } else {
          int pre = (is_reg64(line->dst.u.indirect.reg) ? 0x40 : 0x41) + (is_reg32(line->src.u.reg) ? 0 : 4);
          if (d != RSP - RAX) {
            if (offset == 0 && d != RBP - RAX) {
              ADD_CODE(pre, 0x89, 0x00 + d + s * 8);
              return true;
            } else if (is_im8(offset)) {
              ADD_CODE(pre, 0x89, 0x40 + d + s * 8, IM8(offset));
              return true;
            } else if (is_im32(offset)) {
              ADD_CODE(pre, 0x89, 0x80 + d + s * 8, IM32(offset));
              return true;
            }
          } else {
            if (offset == 0) {
              ADD_CODE(pre, 0x89, 0x04 + s * 8, 0x24);
              return true;
            } else if (is_im8(offset)) {
              ADD_CODE(pre, 0x89, 0x44 + s * 8, 0x24, IM8(offset));
              return true;
            } else if (is_im32(offset)) {
              ADD_CODE(pre, 0x89, 0x84 + s * 8, 0x24, IM32(offset));
              return true;
            }
          }
        }
      } else if (is_reg64s(line->src.u.reg)) {
        int pre = (is_reg64(line->dst.u.indirect.reg) ? 0x48 : 0x49) + (is_reg64(line->src.u.reg) ? 0 : 4);
        int s = (line->src.u.reg - RAX) & 7;
        if (d != RSP - RAX) {
          if (offset == 0 && d != RBP - RAX) {
            ADD_CODE(pre, 0x89, 0x00 + d + s * 8);
            return true;
          } else if (is_im8(offset)) {
            ADD_CODE(pre, 0x89, 0x40 + d + s * 8, IM8(offset));
            return true;
          } else if (is_im32(offset)) {
            ADD_CODE(pre, 0x89, 0x80 + d + s * 8, IM32(offset));
            return true;
          }
        } else {
          if (offset == 0) {
            ADD_CODE(pre, 0x89, 0x04 + s * 8, 0x24);
            return true;
          } else if (is_im8(offset)) {
            ADD_CODE(pre, 0x89, 0x44 + s * 8, 0x24, IM8(offset));
            return true;
          } else if (is_im32(offset)) {
            ADD_CODE(pre, 0x89, 0x84 + s * 8, 0x24, IM32(offset));
            return true;
          }
        }
      }
    }
  }
  return false;
}

static void assemble_line(const Line *line, const char *rawline) {
  if (line->label != NULL)
    add_label(current_section, line->label);

  switch(line->op) {
  case NOOP:
    return;
  case MOV:
    if (assemble_mov(line))
      return;
    break;
  case MOVSX:
    if (line->src.type == REG && line->dst.type == REG) {
      if (is_reg8ss(line->src.u.reg)) {
        if (is_reg32s(line->dst.u.reg)) {
          int s = (line->src.u.reg - AL) & 7;
          int d = (line->dst.u.reg - EAX) & 7;
          if (is_reg8(line->src.u.reg) && is_reg32(line->dst.u.reg)) {
            ADD_CODE(0x0f, 0xbe, 0xc0 + s + d * 8);
          } else {
            int pre = (is_reg8s(line->src.u.reg) ? 0x40 : 0x41) + (is_reg32(line->dst.u.reg) ? 0 : 4);
            ADD_CODE(pre, 0x0f, 0xbe, 0xc0 + s + d * 8);
          }
          return;
        } else if (is_reg64s(line->dst.u.reg)) {
          int s = (line->src.u.reg - AL) & 7;
          int d = (line->dst.u.reg - RAX) & 7;
          int pre = (is_reg8s(line->src.u.reg) ? 0x48 : 0x49) + (is_reg64(line->dst.u.reg) ? 0 : 4);
          ADD_CODE(pre, 0x0f, 0xbe, 0xc0 + s + d * 8);
          return;
        }
      } else if (is_reg16s(line->src.u.reg)) {
        if (is_reg32s(line->dst.u.reg)) {
          int s = (line->src.u.reg - AX) & 7;
          int d = (line->dst.u.reg - EAX) & 7;
          if (is_reg16(line->src.u.reg) && is_reg32(line->dst.u.reg)) {
            ADD_CODE(0x0f, 0xbf, 0xc0 + s + d * 8);
          } else {
            int pre = (is_reg16(line->src.u.reg) ? 0x40 : 0x41) + (is_reg32(line->dst.u.reg) ? 0 : 4);
            ADD_CODE(pre, 0x0f, 0xbf, 0xc0 + s + d * 8);
          }
          return;
        } else if (is_reg64s(line->dst.u.reg)) {
          int pre = (is_reg16(line->src.u.reg) ? 0x48 : 0x49) + (is_reg64(line->dst.u.reg) ? 0 : 4);
          int s = (line->src.u.reg - AX) & 7;
          int d = (line->dst.u.reg - RAX) & 7;
          ADD_CODE(pre, 0x0f, 0xbf, 0xc0 + s + d * 8);
          return;
        }
      } else if (is_reg32s(line->src.u.reg)) {
        if (is_reg64s(line->dst.u.reg)) {
          int pre = (is_reg32(line->src.u.reg) ? 0x48 : 0x49) + (is_reg64(line->dst.u.reg) ? 0 : 4);
          int s = (line->src.u.reg - EAX) & 7;
          int d = (line->dst.u.reg - RAX) & 7;
          ADD_CODE(pre, 0x63, 0xc0 + s + d * 8);
          return;
        }
      }
    }
    break;
  case LEA:
    if (line->src.type == INDIRECT &&
        line->dst.type == REG && is_reg64s(line->dst.u.reg)) {
      int d = (line->dst.u.reg - RAX) & 7;
      if (is_reg64s(line->src.u.indirect.reg)) {
        int s = (line->src.u.indirect.reg - RAX) & 7;
        int pre = (is_reg64(line->src.u.reg) ? 0x48 : 0x49) + (is_reg64(line->dst.u.reg) ? 0 : 4);
        if (line->src.u.indirect.label == NULL) {
          long offset = line->src.u.indirect.offset;
          if (line->src.u.indirect.reg != RSP) {
            if (offset == 0 && line->src.u.indirect.reg != RBP) {
              ADD_CODE(pre, 0x8d, 0x00 + s + d * 8);
              return;
            } else if (is_im8(offset)) {
              ADD_CODE(pre, 0x8d, 0x40 + s + d * 8, IM8(offset));
              return;
            } else if (is_im32(offset)) {
              ADD_CODE(pre, 0x8d, 0x80 + s + d * 8, IM32(offset));
              return;
            }
          } else {
            if (offset == 0) {
              ADD_CODE(pre, 0x8d, 0x04 + d * 8, 0x24);
              return;
            } else if (is_im8(offset)) {
              ADD_CODE(pre, 0x8d, 0x44 + d * 8, 0x24, IM8(offset));
              return;
            } else if (is_im32(offset)) {
              ADD_CODE(pre, 0x8d, 0x84 + d * 8, 0x24, IM32(offset));
              return;
            }
          }
        }
      } else if (line->src.u.indirect.reg == RIP) {
        int pre = is_reg64(line->dst.u.reg) ? 0x48 : 0x4c;
        if (line->src.u.indirect.offset == 0) {
          ADD_LOC_REL32(line->src.u.indirect.label, 3, 7);
          ADD_CODE(pre, 0x8d, 0x05 + d * 8, IM32(-1));
          return;
        }
      }
    }
    break;
  case ADD:
    if (line->src.type == REG && line->dst.type == REG) {
      if (is_reg8ss(line->src.u.reg) && is_reg8ss(line->dst.u.reg)) {
        int s = (line->src.u.reg - AL) & 7;
        int d = (line->dst.u.reg - AL) & 7;
        if (is_reg8(line->src.u.reg) && is_reg8(line->dst.u.reg)) {
          ADD_CODE(0x00, 0xc0 + s * 8 + d);
        } else {
          int pre = (is_reg8s(line->dst.u.reg) ? 0x40 : 0x41) + (is_reg8s(line->src.u.reg) ? 0 : 4);
          ADD_CODE(pre, 0x00, 0xc0 + s * 8 + d);
        }
        return;
      } else if (is_reg32s(line->src.u.reg) && is_reg32s(line->dst.u.reg)) {
        int s = (line->src.u.reg - EAX) & 7;
        int d = (line->dst.u.reg - EAX) & 7;
        if (is_reg32(line->src.u.reg) && is_reg32(line->dst.u.reg)) {
          ADD_CODE(0x01, 0xc0 + s * 8 + d);
        } else {
          int pre = (is_reg32(line->dst.u.reg) ? 0x40 : 0x41) + (is_reg32(line->src.u.reg) ? 0 : 4);
          ADD_CODE(pre, 0x01, 0xc0 + s * 8 + d);
        }
        return;
      } else if (is_reg64s(line->src.u.reg) && is_reg64s(line->dst.u.reg)) {
        int pre = (is_reg64(line->dst.u.reg) ? 0x48 : 0x49) + (is_reg64(line->src.u.reg) ? 0 : 4);
        int s = (line->src.u.reg - RAX) & 7;
        int d = (line->dst.u.reg - RAX) & 7;
        ADD_CODE(pre, 0x01, 0xc0 + s * 8 + d);
        return;
      }
    } else if (line->src.type == IMMEDIATE && line->dst.type == REG) {
      if (is_reg64(line->dst.u.reg)) {
        if (is_im8(line->src.u.immediate)) {
          ADD_CODE(0x48, 0x83, 0xc0 + (line->dst.u.reg - RAX), IM8(line->src.u.immediate));
          return;
        } else if (is_im32(line->src.u.immediate)) {
          if (line->dst.u.reg == RAX)
            ADD_CODE(0x48, 0x05, IM32(line->src.u.immediate));
          else
            ADD_CODE(0x48, 0x81, 0xc0 + (line->dst.u.reg - RAX), IM32(line->src.u.immediate));
          return;
        }
      }
    } else if (line->src.type == INDIRECT && line->dst.type == REG) {
      if (is_reg64(line->src.u.indirect.reg) && line->src.u.indirect.label == NULL &&
          is_reg64(line->dst.u.reg)) {
        int s = line->src.u.indirect.reg - RAX;
        int d = line->dst.u.reg - RAX;
        long offset = line->src.u.indirect.offset;
        switch (line->src.u.indirect.reg) {
        case RSP:
        case RBP:
          // Not implemented
          break;
        default:
          if (offset == 0) {
            ADD_CODE(0x48, 0x03, 0x00 + s + d * 8);
          }
          return;
        }
      }
    }
    break;
  case ADDQ:
    if (line->src.type == IMMEDIATE && line->dst.type == INDIRECT) {
      if (is_reg64s(line->dst.u.indirect.reg) && line->dst.u.indirect.label == NULL) {
        int pre = is_reg64(line->dst.u.indirect.reg) ? 0x48 : 0x49;
        long value = line->src.u.immediate;
        int d = (line->dst.u.indirect.reg - RAX) & 7;
        long offset = line->dst.u.indirect.offset;
        if (is_im8(value)) {
          if (d != RSP - RAX) {
            if (offset == 0 && d != RBP - RAX) {
              ADD_CODE(pre, 0x83, 0x00 + d, IM8(value));
              return;
            } else if (is_im8(offset)) {
              ADD_CODE(pre, 0x83, 0x40 + d, IM8(offset), IM8(value));
              return;
            } else if (is_im32(offset)) {
              ADD_CODE(pre, 0x83, 0x80 + d, IM32(offset), IM8(value));
              return;
            }
          } else {
            if (offset == 0) {
              ADD_CODE(pre, 0x83, 0x04, 0x24, IM8(value));
              return;
            } else if (is_im8(offset)) {
              ADD_CODE(pre, 0x83, 0x44, 0x24, IM8(offset), IM8(value));
              return;
            } else if (is_im32(offset)) {
              ADD_CODE(pre, 0x83, 0x84, 0x24, IM32(offset), IM8(value));
              return;
            }
          }
        } else if (is_im32(value)) {
          if (d != RSP - RAX) {
            if (offset == 0 && d != RBP - RAX) {
              ADD_CODE(pre, 0x81, 0x00 + d, IM32(value));
              return;
            } else if (is_im8(offset)) {
              ADD_CODE(pre, 0x81, 0x40 + d, IM8(offset), IM32(value));
              return;
            } else if (is_im32(offset)) {
              ADD_CODE(pre, 0x81, 0x80 + d, IM32(offset), IM32(value));
              return;
            }
          } else {
            if (offset == 0) {
              ADD_CODE(pre, 0x81, 0x04, 0x24, IM32(value));
              return;
            } else if (is_im8(offset)) {
              ADD_CODE(pre, 0x81, 0x44, 0x24, IM8(offset), IM32(value));
              return;
            } else if (is_im32(offset)) {
              ADD_CODE(pre, 0x81, 0x84, 0x24, IM32(offset), IM32(value));
              return;
            }
          }
        }
      }
    }
    break;
  case SUB:
    if (line->src.type == REG && line->dst.type == REG) {
      if (is_reg8ss(line->src.u.reg) && is_reg8ss(line->dst.u.reg)) {
        int s = (line->src.u.reg - AL) & 7;
        int d = (line->dst.u.reg - AL) & 7;
        if (is_reg8(line->src.u.reg) && is_reg8(line->dst.u.reg)) {
          ADD_CODE(0x28, 0xc0 + s * 8 + d);
        } else {
          int pre = (is_reg8s(line->dst.u.reg) ? 0x40 : 0x41) + (is_reg8s(line->src.u.reg) ? 0 : 4);
          ADD_CODE(pre, 0x28, 0xc0 + s * 8 + d);
        }
        return;
      } else if (is_reg32s(line->src.u.reg) && is_reg32s(line->dst.u.reg)) {
        int s = (line->src.u.reg - EAX) & 7;
        int d = (line->dst.u.reg - EAX) & 7;
        if (is_reg32(line->src.u.reg) && is_reg32(line->dst.u.reg)) {
          ADD_CODE(0x29, 0xc0 + s * 8 + d);
        } else {
          int pre = (is_reg32(line->dst.u.reg) ? 0x40 : 0x41) + (is_reg32(line->src.u.reg) ? 0 : 4);
          ADD_CODE(pre, 0x29, 0xc0 + s * 8 + d);
        }
        return;
      } else if (is_reg64s(line->src.u.reg) && is_reg64s(line->dst.u.reg)) {
        int pre = (is_reg64(line->dst.u.reg) ? 0x48 : 0x49) + (is_reg64(line->src.u.reg) ? 0 : 4);
        int s = (line->src.u.reg - RAX) & 7;
        int d = (line->dst.u.reg - RAX) & 7;
        ADD_CODE(pre, 0x29, 0xc0 + s * 8 + d);
        return;
      }
    } else if (line->dst.type == REG && line->src.type == IMMEDIATE) {
      if (is_reg64(line->dst.u.reg)) {
        if (is_im8(line->src.u.immediate)) {
          ADD_CODE(0x48, 0x83, 0xe8 + (line->dst.u.reg - RAX), IM8(line->src.u.immediate));
          return;
        } else if (is_im32(line->src.u.immediate)) {
          if (line->dst.u.reg == RAX)
            ADD_CODE(0x48, 0x2d, IM32(line->src.u.immediate));
          else
            ADD_CODE(0x48, 0x81, 0xe8 + (line->dst.u.reg - RAX), IM32(line->src.u.immediate));
          return;
        }
      }
    }
    break;
  case SUBQ:
    if (line->src.type == IMMEDIATE && line->dst.type == INDIRECT) {
      if (is_reg64(line->dst.u.indirect.reg) && line->dst.u.indirect.label == NULL) {
        long value = line->src.u.immediate;
        int d = line->dst.u.indirect.reg - RAX;
        long offset = line->dst.u.indirect.offset;
        if (is_im8(value)) {
          if (line->dst.u.indirect.reg != RSP) {
            if (offset == 0 && line->dst.u.indirect.reg != RBP) {
              ADD_CODE(0x48, 0x83, 0x28 + d, IM8(value));
              return;
            } else if (is_im8(offset)) {
              ADD_CODE(0x48, 0x83, 0x68 + d, IM8(offset), IM8(value));
              return;
            } else if (is_im32(offset)) {
              ADD_CODE(0x48, 0x83, 0xa8 + d, IM32(offset), IM8(value));
              return;
            }
          } else {
            if (offset == 0) {
              ADD_CODE(0x48, 0x83, 0x2c, 0x24, IM8(value));
              return;
            } else if (is_im8(offset)) {
              ADD_CODE(0x48, 0x83, 0x6c, 0x24, IM8(offset), IM8(value));
              return;
            } else if (is_im32(offset)) {
              ADD_CODE(0x48, 0x83, 0xac, 0x24, IM32(offset), IM8(value));
              return;
            }
          }
        } else if (is_im32(value)) {
          if (line->dst.u.indirect.reg != RSP) {
            if (offset == 0 && line->dst.u.indirect.reg != RBP) {
              ADD_CODE(0x48, 0x81, 0x28 + d, IM32(value));
              return;
            } else if (is_im8(offset)) {
              ADD_CODE(0x48, 0x81, 0x68 + d, IM8(offset), IM32(value));
              return;
            } else if (is_im32(offset)) {
              ADD_CODE(0x48, 0x81, 0xa8 + d, IM32(offset), IM32(value));
              return;
            }
          } else {
            if (offset == 0) {
              ADD_CODE(0x48, 0x81, 0x2c, 0x24, IM32(value));
              return;
            } else if (is_im8(offset)) {
              ADD_CODE(0x48, 0x81, 0x6c, 0x24, IM8(offset), IM32(value));
              return;
            } else if (is_im32(offset)) {
              ADD_CODE(0x48, 0x81, 0xac, 0x24, IM32(offset), IM32(value));
              return;
            }
          }
        }
      }
    }
    break;
  case MUL:
    if (line->src.type == REG && line->dst.type == NOOPERAND) {
      if (is_reg32(line->src.u.reg)) {
        int s = line->src.u.reg - EAX;
        ADD_CODE(0xf7, 0xe0 + s);
        return;
      } else if (is_reg32x(line->src.u.reg)) {
        int s = line->src.u.reg - R8D;
        ADD_CODE(0x41, 0xf7, 0xe0 + s);
        return;
      } else if (is_reg64s(line->src.u.reg)) {
        int s = (line->src.u.reg - RAX) & 7;
        int pre = is_reg64(line->src.u.reg) ? 0x48 : 0x49;
        ADD_CODE(pre, 0xf7, 0xe0 + s);
        return;
      }
    }
    break;
  case IDIV:
    if (line->src.type == REG && line->dst.type == NOOPERAND) {
      if (is_reg32s(line->src.u.reg)) {
        int s = (line->src.u.reg - EAX) & 7;
        if (is_reg32(line->src.u.reg)) {
          ADD_CODE(0xf7, 0xf8 + s);
        } else {
          ADD_CODE(0x41, 0xf7, 0xf8 + s);
        }
        return;
      } else if (is_reg64s(line->src.u.reg)) {
        int pre = is_reg64(line->src.u.reg) ? 0x48 : 0x49;
        int s = (line->src.u.reg - RAX) & 7;
        ADD_CODE(pre, 0xf7, 0xf8 + s);
        return;
      }
    }
    break;
  case NEG:
    if (line->src.type == REG && line->dst.type == NOOPERAND) {
      if (is_reg16s(line->src.u.reg)) {
        int s = (line->src.u.reg - AX) & 7;
        if (is_reg16(line->src.u.reg)) {
          ADD_CODE(0x66, 0xf7, 0xd8 + s);
        } else {
          ADD_CODE(0x66, 0x41, 0xf7, 0xd8 + s);
        }
        return;
      } else if (is_reg32s(line->src.u.reg)) {
        int s = (line->src.u.reg - EAX) & 7;
        if (is_reg32(line->src.u.reg)) {
          ADD_CODE(0xf7, 0xd8 + s);
        } else {
          ADD_CODE(0x41, 0xf7, 0xd8 + s);
        }
        return;
      } else if (is_reg64s(line->src.u.reg)) {
        int s = (line->src.u.reg - RAX) & 7;
        int pre = is_reg64(line->src.u.reg) ? 0x48 : 0x49;
        ADD_CODE(pre, 0xf7, 0xd8 + s);
        return;
      }
    }
    break;
  case NOT:
    if (line->src.type == REG && line->dst.type == NOOPERAND) {
      if (is_reg16s(line->src.u.reg)) {
        int s = (line->src.u.reg - AX) & 7;
        if (is_reg16(line->src.u.reg)) {
          ADD_CODE(0x66, 0xf7, 0xd0 + s);
        } else {
          ADD_CODE(0x66, 0x41, 0xf7, 0xd0 + s);
        }
        return;
      } else if (is_reg32s(line->src.u.reg)) {
        int s = (line->src.u.reg - EAX) & 7;
        if (is_reg32(line->src.u.reg)) {
          ADD_CODE(0xf7, 0xd0 + s);
        } else {
          ADD_CODE(0x41, 0xf7, 0xd0 + s);
        }
        return;
      }
    }
    break;
  case INC:
    if (line->src.type == REG && line->dst.type == NOOPERAND) {
      if (is_reg32(line->src.u.reg)) {
        int s = line->src.u.reg - EAX;
        ADD_CODE(0xff, 0xc0 + s);
        return;
      } else if (is_reg64(line->src.u.reg)) {
        int s = line->src.u.reg - RAX;
        ADD_CODE(0x48, 0xff, 0xc0 + s);
        return;
      }
    }
    break;
  case INCL:
    if (line->src.type == INDIRECT && line->dst.type == NOOPERAND &&
        is_reg64s(line->src.u.reg)) {
      int s = (line->src.u.indirect.reg - RAX) & 7;
      long offset = line->src.u.indirect.offset;
      if (is_reg64(line->src.u.reg)) {
        if (s != RSP - RAX) {
          if (offset == 0 && s != RBP - RAX) {
            ADD_CODE(0xff, 0x00 + s);
            return;
          } else if (is_im8(offset)) {
            ADD_CODE(0xff, 0x40 + s, IM8(offset));
            return;
          } else if (is_im32(offset)) {
            ADD_CODE(0xff, 0x80 + s, IM32(offset));
            return;
          }
        } else {
          if (offset == 0) {
            ADD_CODE(0xff, 0x04, 0x24);
            return;
          } else if (is_im8(offset)) {
            ADD_CODE(0xff, 0x44, 0x24, IM8(offset));
            return;
          } else if (is_im32(offset)) {
            ADD_CODE(0xff, 0x84, 0x24, IM32(offset));
            return;
          }
        }
      } else {
        if (s != RSP - RAX) {
          if (offset == 0 && s != RBP - RAX) {
            ADD_CODE(0x41, 0xff, 0x00 + s);
            return;
          } else if (is_im8(offset)) {
            ADD_CODE(0x41, 0xff, 0x40 + s, IM8(offset));
            return;
          } else if (is_im32(offset)) {
            ADD_CODE(0x41, 0xff, 0x80 + s, IM32(offset));
            return;
          }
        } else {
          if (offset == 0) {
            ADD_CODE(0x41, 0xff, 0x04, 0x24);
            return;
          } else if (is_im8(offset)) {
            ADD_CODE(0x41, 0xff, 0x44, 0x24, IM8(offset));
            return;
          } else if (is_im32(offset)) {
            ADD_CODE(0x41, 0xff, 0x84, 0x24, IM32(offset));
            return;
          }
        }
      }
    }
    break;
  case INCQ:
    if (line->src.type == INDIRECT && line->dst.type == NOOPERAND &&
        is_reg64s(line->src.u.reg)) {
      int pre = is_reg64(line->src.u.reg) ? 0x48 : 0x49;
      int s = (line->src.u.indirect.reg - RAX) & 7;
      long offset = line->src.u.indirect.offset;
      if (line->src.u.indirect.reg != RSP) {
        if (offset == 0 && line->src.u.indirect.reg != RBP) {
          ADD_CODE(pre, 0xff, 0x00 + s);
          return;
        } else if (is_im8(offset)) {
          ADD_CODE(pre, 0xff, 0x40 + s, IM8(offset));
          return;
        } else if (is_im32(offset)) {
          ADD_CODE(pre, 0xff, 0x80 + s, IM32(offset));
          return;
        }
      } else {
        if (offset == 0) {
          ADD_CODE(pre, 0xff, 0x04, 0x24);
          return;
        } else if (is_im8(offset)) {
          ADD_CODE(pre, 0xff, 0x44, 0x24, IM8(offset));
          return;
        } else if (is_im32(offset)) {
          ADD_CODE(pre, 0xff, 0x84, 0x24, IM32(offset));
          return;
        }
      }
    }
    break;
  case DEC:
    if (line->src.type == REG && line->dst.type == NOOPERAND) {
      if (is_reg32(line->src.u.reg)) {
        int s = line->src.u.reg - EAX;
        ADD_CODE(0xff, 0xc8 + s);
        return;
      } else if (is_reg64(line->src.u.reg)) {
        int s = line->src.u.reg - RAX;
        ADD_CODE(0x48, 0xff, 0xc8 + s);
        return;
      }
    }
    break;
  case DECL:
    if (line->src.type == INDIRECT && line->dst.type == NOOPERAND &&
        is_reg64s(line->src.u.reg)) {
      int s = (line->src.u.indirect.reg - RAX) & 7;
      long offset = line->src.u.indirect.offset;
      if (is_reg64(line->src.u.reg)) {
        if (s != RSP - RAX) {
          if (offset == 0 && s != RBP - RAX) {
            ADD_CODE(0xff, 0x08 + s);
            return;
          } else if (is_im8(offset)) {
            ADD_CODE(0xff, 0x48 + s, IM8(offset));
            return;
          } else if (is_im32(offset)) {
            ADD_CODE(0xff, 0x88 + s, IM32(offset));
            return;
          }
        } else {
          if (offset == 0) {
            ADD_CODE(0xff, 0x0c, 0x24);
            return;
          } else if (is_im8(offset)) {
            ADD_CODE(0xff, 0x4c, 0x24, IM8(offset));
            return;
          } else if (is_im32(offset)) {
            ADD_CODE(0xff, 0x8c, 0x24, IM32(offset));
            return;
          }
        }
      } else {
        if (s != RSP - RAX) {
          if (offset == 0 && s != RBP - RAX) {
            ADD_CODE(0x41, 0xff, 0x08 + s);
            return;
          } else if (is_im8(offset)) {
            ADD_CODE(0x41, 0xff, 0x48 + s, IM8(offset));
            return;
          } else if (is_im32(offset)) {
            ADD_CODE(0x41, 0xff, 0x88 + s, IM32(offset));
            return;
          }
        } else {
          if (offset == 0) {
            ADD_CODE(0x41, 0xff, 0x0c, 0x24);
            return;
          } else if (is_im8(offset)) {
            ADD_CODE(0x41, 0xff, 0x4c, 0x24, IM8(offset));
            return;
          } else if (is_im32(offset)) {
            ADD_CODE(0x41, 0xff, 0x8c, 0x24, IM32(offset));
            return;
          }
        }
      }
    }
    break;
  case DECQ:
    if (line->src.type == INDIRECT && line->dst.type == NOOPERAND &&
        is_reg64s(line->src.u.reg)) {
      int pre = is_reg64(line->src.u.reg) ? 0x48 : 0x49;
      int s = (line->src.u.indirect.reg - RAX) & 7;
      long offset = line->src.u.indirect.offset;
      if (line->src.u.indirect.reg != RSP) {
        if (offset == 0 && line->src.u.indirect.reg != RBP) {
          ADD_CODE(pre, 0xff, 0x08 + s);
          return;
        } else if (is_im8(offset)) {
          ADD_CODE(pre, 0xff, 0x48 + s, IM8(offset));
          return;
        } else if (is_im32(offset)) {
          ADD_CODE(pre, 0xff, 0x88 + s, IM32(offset));
          return;
        }
      } else {
        if (offset == 0) {
          ADD_CODE(pre, 0xff, 0x0c, 0x24);
          return;
        } else if (is_im8(offset)) {
          ADD_CODE(pre, 0xff, 0x4c, 0x24, IM8(offset));
          return;
        } else if (is_im32(offset)) {
          ADD_CODE(pre, 0xff, 0x8c, 0x24, IM32(offset));
          return;
        }
      }
    }
    break;
  case AND:
    if (line->src.type == REG && line->dst.type == REG) {
      if (is_reg16s(line->src.u.reg) && is_reg16s(line->dst.u.reg)) {
        int s = (line->src.u.reg - AX) & 7;
        int d = (line->dst.u.reg - AX) & 7;
        if (is_reg16(line->src.u.reg) && is_reg16(line->dst.u.reg)) {
          ADD_CODE(0x66, 0x21, 0xc0 + s * 8 + d);
        } else {
          int pre = (is_reg16(line->dst.u.reg) ? 0x40 : 0x41) + (is_reg16(line->src.u.reg) ? 0 : 4);
          ADD_CODE(0x66, pre, 0x21, 0xc0 + s * 8 + d);
        }
        return;
      } if (is_reg32s(line->src.u.reg) && is_reg32s(line->dst.u.reg)) {
        int s = (line->src.u.reg - EAX) & 7;
        int d = (line->dst.u.reg - EAX) & 7;
        if (is_reg32(line->src.u.reg) && is_reg32(line->dst.u.reg)) {
          ADD_CODE(0x21, 0xc0 + s * 8 + d);
        } else {
          int pre = (is_reg32(line->dst.u.reg) ? 0x40 : 0x41) + (is_reg32(line->src.u.reg) ? 0 : 4);
          ADD_CODE(pre, 0x21, 0xc0 + s * 8 + d);
        }
        return;
      } else if (is_reg64s(line->src.u.reg) && is_reg64s(line->dst.u.reg)) {
        int s = (line->src.u.reg - RAX) & 7;
        int d = (line->dst.u.reg - RAX) & 7;
        int pre = (is_reg64(line->dst.u.reg) ? 0x48 : 0x49) + (is_reg64(line->src.u.reg) ? 0 : 4);
        ADD_CODE(pre, 0x21, 0xc0 + s * 8 + d);
        return;
      }
    }
    break;
  case OR:
    if (line->src.type == REG && line->dst.type == REG) {
      if (is_reg16s(line->src.u.reg) && is_reg16s(line->dst.u.reg)) {
        int s = (line->src.u.reg - AX) & 7;
        int d = (line->dst.u.reg - AX) & 7;
        if (is_reg16(line->src.u.reg) && is_reg16(line->dst.u.reg)) {
          ADD_CODE(0x66, 0x09, 0xc0 + s * 8 + d);
        } else {
          int pre = (is_reg16(line->dst.u.reg) ? 0x40 : 0x41) + (is_reg16(line->src.u.reg) ? 0 : 4);
          ADD_CODE(0x66, pre, 0x09, 0xc0 + s * 8 + d);
        }
        return;
      } else if (is_reg32s(line->src.u.reg) && is_reg32s(line->dst.u.reg)) {
        int s = (line->src.u.reg - EAX) & 7;
        int d = (line->dst.u.reg - EAX) & 7;
        if (is_reg32(line->src.u.reg) && is_reg32(line->dst.u.reg)) {
          ADD_CODE(0x09, 0xc0 + s * 8 + d);
        } else {
          int pre = (is_reg32(line->dst.u.reg) ? 0x40 : 0x41) + (is_reg32(line->src.u.reg) ? 0 : 4);
          ADD_CODE(pre, 0x09, 0xc0 + s * 8 + d);
        }
        return;
      } else if (is_reg64s(line->src.u.reg) && is_reg64s(line->dst.u.reg)) {
        int pre = (is_reg64(line->dst.u.reg) ? 0x48 : 0x49) + (is_reg64(line->src.u.reg) ? 0 : 4);
        int s = (line->src.u.reg - RAX) & 7;
        int d = (line->dst.u.reg - RAX) & 7;
        ADD_CODE(pre, 0x09, 0xc0 + s * 8 + d);
        return;
      }
    }
    break;
  case XOR:
    if (line->src.type == REG && line->dst.type == REG) {
      if (is_reg8ss(line->src.u.reg) && is_reg8ss(line->dst.u.reg)) {
        int s = (line->src.u.reg - AL) & 7;
        int d = (line->dst.u.reg - AL) & 7;
        if (is_reg8(line->src.u.reg) && is_reg8(line->dst.u.reg)) {
          ADD_CODE(0x30, 0xc0 + s * 8 + d);
        } else {
          int pre = (is_reg8s(line->dst.u.reg) ? 0x40 : 0x41) + (is_reg8s(line->src.u.reg) ? 0 : 4);
          ADD_CODE(pre, 0x30, 0xc0 + s * 8 + d);
        }
        return;
      } else if (is_reg16s(line->src.u.reg) && is_reg16s(line->dst.u.reg)) {
        int s = (line->src.u.reg - AX) & 7;
        int d = (line->dst.u.reg - AX) & 7;
        if (is_reg16(line->src.u.reg) && is_reg16(line->dst.u.reg)) {
          ADD_CODE(0x66, 0x31, 0xc0 + s * 8 + d);
        } else {
          int pre = (is_reg16(line->dst.u.reg) ? 0x40 : 0x41) + (is_reg16(line->src.u.reg) ? 0 : 4);
          ADD_CODE(0x66, pre, 0x31, 0xc0 + s * 8 + d);
        }
        return;
      } else if (is_reg32s(line->src.u.reg) && is_reg32s(line->dst.u.reg)) {
        int s = (line->src.u.reg - EAX) & 7;
        int d = (line->dst.u.reg - EAX) & 7;
        if (is_reg32(line->src.u.reg) && is_reg32(line->dst.u.reg)) {
          ADD_CODE(0x31, 0xc0 + s * 8 + d);
        } else {
          int pre = (is_reg32(line->dst.u.reg) ? 0x40 : 0x41) + (is_reg32(line->src.u.reg) ? 0 : 4);
          ADD_CODE(pre, 0x31, 0xc0 + s * 8 + d);
        }
        return;
      } else if (is_reg64s(line->src.u.reg) && is_reg64s(line->dst.u.reg)) {
        int pre = (is_reg64(line->dst.u.reg) ? 0x48 : 0x49) + (is_reg64(line->src.u.reg) ? 0 : 4);
        int s = (line->src.u.reg - RAX) & 7;
        int d = (line->dst.u.reg - RAX) & 7;
        ADD_CODE(pre, 0x31, 0xc0 + s * 8 + d);
        return;
      }
    }
    break;
  case SHL:
    if (line->src.type == REG && line->dst.type == REG &&
        line->src.u.reg == CL) {
      if (is_reg16s(line->dst.u.reg)) {
        int d = (line->dst.u.reg - AX) & 7;
        if (is_reg16(line->dst.u.reg)) {
          ADD_CODE(0x66, 0xd3, 0xe0 + d);
        } else {
          ADD_CODE(0x66, 0x41, 0xd3, 0xe0 + d);
        }
        return;
      } else if (is_reg32s(line->dst.u.reg)) {
        int d = (line->dst.u.reg - EAX) & 7;
        if (is_reg32(line->dst.u.reg)) {
          ADD_CODE(0xd3, 0xe0 + d);
        } else {
          ADD_CODE(0x41, 0xd3, 0xe0 + d);
        }
        return;
      } else if (is_reg64s(line->dst.u.reg)) {
        int pre = is_reg64s(line->dst.u.reg) ? 0x48 : 0x49;
        int d = (line->dst.u.reg - RAX) & 7;
        ADD_CODE(pre, 0xd3, 0xe0 + d);
        return;
      }
    }
    break;
  case SHR:
    if (line->src.type == REG && line->dst.type == REG &&
        line->src.u.reg == CL) {
      if (is_reg32s(line->dst.u.reg)) {
        int d = (line->dst.u.reg - EAX) & 7;
        if (is_reg32(line->dst.u.reg)) {
          ADD_CODE(0xd3, 0xe8 + d);
        } else {
          ADD_CODE(0x41, 0xd3, 0xe8 + d);
        }
        return;
      } else if (is_reg64s(line->dst.u.reg)) {
        int pre = is_reg64(line->dst.u.reg) ? 0x48 : 0x49;
        int d = (line->dst.u.reg - RAX) & 7;
        ADD_CODE(pre, 0xd3, 0xe8 + d);
        return;
      }
    }
    break;
  case CMP:
    if (line->src.type == REG && line->dst.type == REG) {
      if (is_reg8ss(line->src.u.reg) && is_reg8ss(line->dst.u.reg)) {
        int s = (line->src.u.reg - AL) & 7;
        int d = (line->dst.u.reg - AL) & 7;
        if (is_reg8(line->src.u.reg) && is_reg8(line->dst.u.reg)) {
          ADD_CODE(0x38, 0xc0 + s * 8 + d);
        } else {
          int pre = (is_reg8s(line->dst.u.reg) ? 0x40 : 0x41) + (is_reg8s(line->src.u.reg) ? 0 : 4);
          ADD_CODE(pre, 0x38, 0xc0 + s * 8 + d);
        }
        return;
      } else if (is_reg32s(line->src.u.reg) && is_reg32s(line->dst.u.reg)) {
        int s = (line->src.u.reg - EAX) & 7;
        int d = (line->dst.u.reg - EAX) & 7;
        if (is_reg32(line->src.u.reg) && is_reg32(line->dst.u.reg)) {
          ADD_CODE(0x39, 0xc0 + s * 8 + d);
        } else {
          int pre = (is_reg32(line->dst.u.reg) ? 0x40 : 0x41) + (is_reg32(line->src.u.reg) ? 0 : 4);
          ADD_CODE(pre, 0x39, 0xc0 + s * 8 + d);
        }
        return;
      } else if (is_reg64s(line->src.u.reg) && is_reg64s(line->dst.u.reg)) {
        int pre = (is_reg64(line->dst.u.reg) ? 0x48 : 0x49) + (is_reg64(line->src.u.reg) ? 0 : 4);
        int s = (line->src.u.reg - RAX) & 7;
        int d = (line->dst.u.reg - RAX) & 7;
        ADD_CODE(pre, 0x39, 0xc0 + s * 8 + d);
        return;
      }
    } else if (line->src.type == IMMEDIATE && line->dst.type == REG) {
      long value = line->src.u.immediate;
      if (is_reg8ss(line->dst.u.reg)) {
        int d = (line->dst.u.reg - AL) & 7;
        if (line->dst.u.reg == AL) {
          ADD_CODE(0x3c, IM8(value));
        } else if (is_reg8(line->dst.u.reg)) {
          ADD_CODE(0x80, 0xf8 + d, IM8(value));
        } else {
          ADD_CODE(0x41, 0x80, 0xf8 + d, IM8(value));
        }
        return;
      } else if (is_reg32(line->dst.u.reg)) {
        int d = line->dst.u.reg - EAX;
        if (is_im8(value)) {
          ADD_CODE(0x83, 0xf8 + d, IM8(value));
          return;
        } else if (is_im32(value)) {
          if (line->dst.u.reg == EAX) {
            ADD_CODE(0x3d, IM32(value));
            return;
          } else {
            ADD_CODE(0x81, 0xf8 + d, IM32(value));
            return;
          }
        }
      } else if (is_reg32x(line->dst.u.reg)) {
        int d = line->dst.u.reg - R8D;
        if (is_im8(value)) {
          ADD_CODE(0x41, 0x83, 0xf8 + d, IM8(value));
          return;
        } else if (is_im32(value)) {
          ADD_CODE(0x41, 0x81, 0xf8 + d, IM32(value));
          return;
        }
      } else if (is_reg64s(line->dst.u.reg)) {
        int d = (line->dst.u.reg - RAX) & 7;
        int pre = is_reg64(line->dst.u.reg) ? 0x48 : 0x49;
        if (is_im8(value)) {
          ADD_CODE(pre, 0x83, 0xf8 + d, IM8(value));
          return;
        } else if (is_im32(value)) {
          if (line->dst.u.reg == EAX) {
            ADD_CODE(pre, 0x3d, IM32(value));
            return;
          } else {
            ADD_CODE(pre, 0x81, 0xf8 + d, IM32(value));
            return;
          }
        }
      }
    }
    break;
  case TEST:
    if (line->src.type == REG && line->dst.type == REG) {
      if (is_reg8ss(line->src.u.reg) && is_reg8ss(line->dst.u.reg)) {
        int s = (line->src.u.reg - AL) & 7;
        int d = (line->dst.u.reg - AL) & 7;
        if (is_reg8(line->src.u.reg) && is_reg8(line->dst.u.reg)) {
          ADD_CODE(0x84, 0xc0 + s * 8 + d);
        } else {
          int pre = (is_reg8s(line->dst.u.reg) ? 0x40 : 0x41) + (is_reg8s(line->src.u.reg) ? 0 : 4);
          ADD_CODE(pre, 0x84, 0xc0 + s * 8 + d);
        }
        return;
      } else if (is_reg16s(line->src.u.reg) && is_reg16s(line->dst.u.reg)) {
        int s = (line->src.u.reg - AX) & 7;
        int d = (line->dst.u.reg - AX) & 7;
        if (is_reg16(line->src.u.reg) && is_reg16(line->dst.u.reg)) {
          ADD_CODE(0x66, 0x85, 0xc0 + s * 8 + d);
        } else {
          int pre = (is_reg16(line->dst.u.reg) ? 0x40 : 0x41) + (is_reg16(line->src.u.reg) ? 0 : 4);
          ADD_CODE(0x66, pre, 0x85, 0xc0 + s * 8 + d);
        }
        return;
      } else if (is_reg32s(line->src.u.reg) && is_reg32s(line->dst.u.reg)) {
        int s = (line->src.u.reg - EAX) & 7;
        int d = (line->dst.u.reg - EAX) & 7;
        if (is_reg32(line->src.u.reg) && is_reg32(line->dst.u.reg)) {
          ADD_CODE(0x85, 0xc0 + s * 8 + d);
        } else {
          int pre = (is_reg32(line->dst.u.reg) ? 0x40 : 0x41) + (is_reg32(line->src.u.reg) ? 0 : 4);
          ADD_CODE(pre, 0x85, 0xc0 + s * 8 + d);
        }
        return;
      } else if (is_reg64s(line->src.u.reg) && is_reg64s(line->dst.u.reg)) {
        int pre = (is_reg64(line->src.u.reg) ? 0x48 : 0x49) + (is_reg64(line->dst.u.reg) ? 0 : 4);
        int s = (line->src.u.reg - RAX) & 7;
        int d = (line->dst.u.reg - RAX) & 7;
        ADD_CODE(pre, 0x85, 0xc0 + s * 8 + d);
        return;
      }
    }
    break;
  case CLTD:
    if (line->src.type == NOOPERAND && line->dst.type == NOOPERAND) {
      ADD_CODE(0x99);
      return;
    }
    break;
  case CQTO:
    if (line->src.type == NOOPERAND && line->dst.type == NOOPERAND) {
      ADD_CODE(0x48, 0x99);
      return;
    }
    break;

  case SETO: case SETNO: case SETB:  case SETAE:
  case SETE: case SETNE: case SETBE: case SETA:
  case SETS: case SETNS: case SETP:  case SETNP:
  case SETL: case SETGE: case SETLE: case SETG:
    if (line->src.type == REG && line->dst.type == NOOPERAND) {
      if (is_reg8(line->src.u.reg)) {
        int s = line->src.u.reg - AL;
        ADD_CODE(0x0f, 0x90 + (line->op - SETO), 0xc0 + s);
        return;
      } else if (is_reg8x(line->src.u.reg)) {
        int s = line->src.u.reg - AL;
        ADD_CODE(0x41, 0x0f, 0x90 + (line->op - SETO), 0xc0 + s);
        return;
      }
    }
    break;
  case PUSH:
    if (line->src.type == REG && line->dst.type == NOOPERAND) {
      if (is_reg64(line->src.u.reg)) {
        ADD_CODE(0x50 + (line->src.u.reg - RAX));
        return;
      } else if (is_reg64x(line->src.u.reg)) {
        ADD_CODE(0x41, 0x50 + (line->src.u.reg - R8));
        return;
      }
    }
    break;
  case POP:
    if (line->src.type == REG && line->dst.type == NOOPERAND) {
      if (is_reg64(line->src.u.reg)) {
        ADD_CODE(0x58 + (line->src.u.reg - RAX));
        return;
      } else if (is_reg64x(line->src.u.reg)) {
        ADD_CODE(0x41, 0x58 + (line->src.u.reg - R8));
        return;
      }
    }
    break;
  case JMP:
    if (line->src.type != LABEL || line->dst.type != NOOPERAND)
      error("Illegal oprand: JMP");
    ADD_LOC_REL32(line->src.u.label, 1, 5);
    ADD_CODE(0xe9, IM32(-1));
    return;
  case JO: case JNO: case JB:  case JAE:
  case JE: case JNE: case JBE: case JA:
  case JS: case JNS: case JP:  case JNP:
  case JL: case JGE: case JLE: case JG:
    if (line->src.type == LABEL && line->dst.type == NOOPERAND) {
      // TODO: Handle short jump.
      ADD_LOC_REL32(line->src.u.label, 2, 6);
      ADD_CODE(0x0f, 0x80 + (line->op - JO), IM32(-1));
      return;
    }
    break;
  case CALL:
    if (line->src.type == LABEL && line->dst.type == NOOPERAND) {
      ADD_LOC_REL32(line->src.u.label, 1, 5);
      ADD_CODE(0xe8, IM32(-1));
      return;
    } if (line->src.type == DEREF_REG && line->dst.type == NOOPERAND) {
      if (is_reg64(line->src.u.deref_reg)) {
        int s = line->src.u.deref_reg - RAX;
        ADD_CODE(0xff, 0xd0 + s);
        return;
      } else if (is_reg64s(line->src.u.deref_reg)) {
        int s = line->src.u.deref_reg - R8;
        ADD_CODE(0x41, 0xff, 0xd0 + s);
        return;
      }
    }
    break;
  case RET:
    ADD_CODE(0xc3);
    return;
  case INT:
    if (line->src.type == IMMEDIATE && line->dst.type == NOOPERAND) {
      long value = line->src.u.immediate;
      ADD_CODE(0xcd, IM8(value));
      return;
    }
    return;
  case SYSCALL:
    ADD_CODE(0x0f, 0x05);
    return;
  default:
    break;
  }

  fprintf(stderr, "op=%2d: not handled: %s\n", line->op, rawline);
  err = true;
}

static void assemble(FILE *fp) {
  for (;;) {
    char *rawline = NULL;
    size_t capa = 0;
    ssize_t len = getline_(&rawline, &capa, fp, 0);
    if (len == EOF)
      break;

    Line line;
    parse_line(rawline, &line);
    if (line.dir == NODIRECTIVE) {
      assemble_line(&line, rawline);
    } else {
      handle_directive(line.dir, line.directive_line);
    }
  }
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

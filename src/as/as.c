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

static void init(uintptr_t adr) {
  init_gen(adr);
}

enum Opcode {
  NOOP,
  MOV,
  MOVSX,
  LEA,
  CALL,
  JMP,
  JO,
  JNO,
  JB,
  JAE,
  JE,
  JNE,
  JBE,
  JA,
  JS,
  JNS,
  JP,
  JNP,
  JL,
  JGE,
  JLE,
  JG,
  RET,
  PUSH,
  POP,
  ADD,
  ADDQ,
  SUB,
  MUL,
  DIV,
  XOR,
  NEG,
  NOT,
  INC,
  INCL,
  DEC,
  DECL,
  SHL,
  SHR,
  AND,
  OR,
  CMP,
  TEST,
  SETE,
  SETNE,
  SYSCALL,
};

static const char *kOpTable[] = {
  "mov",
  "movsx",
  "lea",
  "call",
  "jmp",
  "jo",
  "jno",
  "jb",
  "jae",
  "je",
  "jne",
  "jbe",
  "ja",
  "js",
  "jns",
  "jp",
  "jnp",
  "jl",
  "jge",
  "jle",
  "jg",
  "ret",
  "push",
  "pop",
  "add",
  "addq",
  "sub",
  "mul",
  "div",
  "xor",
  "neg",
  "not",
  "inc",
  "incl",
  "dec",
  "decl",
  "shl",
  "shr",
  "and",
  "or",
  "cmp",
  "test",
  "sete",
  "setne",
  "syscall",
};

enum RegType {
  NOREG,

  AL,
  CL,
  DL,
  BL,
  SPL,
  BPL,
  SIL,
  DIL,

  EAX,
  ECX,
  EDX,
  EBX,
  ESP,
  EBP,
  ESI,
  EDI,

  RAX,
  RCX,
  RDX,
  RBX,
  RSP,
  RBP,
  RSI,
  RDI,

  RIP,
};

static const struct {
  const char *name;
  enum RegType reg;
} kRegisters[] = {
  {"al", AL},
  {"cl", CL},
  {"dl", DL},
  {"bl", BL},
  {"spl", SPL},
  {"bpl", BPL},
  {"sil", SIL},
  {"dil", DIL},

  {"eax", EAX},
  {"ecx", ECX},
  {"edx", EDX},
  {"ebx", EBX},
  {"esp", ESP},
  {"ebp", EBP},
  {"esi", ESI},
  {"edi", EDI},

  {"rax", RAX},
  {"rcx", RCX},
  {"rdx", RDX},
  {"rbx", RBX},
  {"rsp", RSP},
  {"rbp", RBP},
  {"rsi", RSI},
  {"rdi", RDI},

  {"rip", RIP},
};

static bool is_reg8(enum RegType reg) {
  return reg >= AL && reg <= BL;
}

static bool is_reg8x(enum RegType reg) {
  return reg >= AL && reg <= DIL;
}

static bool is_reg32(enum RegType reg) {
  return reg >= EAX && reg <= EDI;
}

static bool is_reg64(enum RegType reg) {
  return reg >= RAX && reg <= RDI;
}

enum OperandType {
  NOOPERAND,
  REG,
  INDIRECT,
  IMMEDIATE,
  LABEL,
};

typedef struct {
  enum OperandType type;
  union {
    enum RegType reg;
    long immediate;
    const char *label;
    struct {
      enum RegType reg;
      const char *label;
      long offset;
    } indirect;
  } u;
} Operand;

enum DirectiveType {
  NODIRECTIVE,
  DT_ASCII,
  DT_SECTION,
  DT_DATA,
  DT_GLOBL,
  DT_EXTERN,
};

static const char *kDirectiveTable[] = {
  "ascii",
  "section",
  "data",
  "globl",
  "extern",
};

typedef struct {
  const char *label;
  enum Opcode op;
  Operand src;
  Operand dst;
} Line;

static bool is_label_first_chr(char c) {
  return isalpha(c) || c == '_' || c == '.';
}

static bool is_label_chr(char c) {
  return is_label_first_chr(c) || isdigit(c);
}

static const char *skip_whitespace(const char *p) {
  while (isspace(*p))
    ++p;
  return p;
}

static const char *parse_label(const char **pp) {
  const char *p = *pp;
  const char *start = p;
  if (!is_label_first_chr(*p))
    return NULL;

  do {
    ++p;
  } while (is_label_chr(*p));
  *pp = p;
  return strndup_(start, p - start);
}

static int find_match_index(const char **pp, const char **table, size_t count) {
  const char *p = *pp;
  const char *start = p;

  while (isalpha(*p))
    ++p;
  if (*p == '\0' || isspace(*p)) {
    size_t n = p - start;
    for (size_t i = 0; i < count; ++i) {
      const char *name = table[i];
      size_t len = strlen(name);
      if (n == len && strncasecmp(start, name, n) == 0) {
        *pp = skip_whitespace(p);
        return i;
      }
    }
  }
  return -1;
}

static enum Opcode parse_directive(const char **pp) {
  return find_match_index(pp, kDirectiveTable, sizeof(kDirectiveTable) / sizeof(*kDirectiveTable)) + 1;
}

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

      add_code((unsigned char*)str, len);  // TODO: Detect section.

      free(str);
    }
    break;

  case DT_SECTION:
  case DT_DATA:
  case DT_GLOBL:
  case DT_EXTERN:
    break;

  default:
    fprintf(stderr, "Unhandled directive: %d, %s\n", dir, p);
    break;
  }
}

static enum Opcode parse_opcode(const char **pp) {
  return find_match_index(pp, kOpTable, sizeof(kOpTable) / sizeof(*kOpTable)) + 1;
}

static enum RegType parse_register(const char **pp) {
  const char *p = *pp;
  for (int i = 0, len = sizeof(kRegisters) / sizeof(*kRegisters); i < len; ++i) {
    const char *name = kRegisters[i].name;
    size_t n = strlen(name);
    if (strncmp(p, name, n) == 0) {
      *pp = p + n;
      return kRegisters[i].reg;
    }
  }
  return NOREG;
}

static bool parse_immediate(const char **pp, Operand *operand) {
  long num = strtol(*pp, (char**)pp, 10);
  operand->type = IMMEDIATE;
  operand->u.immediate = num;
  return true;
}

static bool parse_operand(const char **pp, Operand *operand) {
  const char *p = *pp;
  if (*p == '%') {
    *pp = p + 1;
    enum RegType reg = parse_register(pp);
    if (reg == NOREG)
      error("Illegal register");
    operand->type = REG;
    operand->u.reg = reg;
    return true;
  }

  if (*p == '$') {
    *pp = p + 1;
    return parse_immediate(pp, operand);
  }

  bool has_offset = false;
  long offset = 0;
  const char *label = parse_label(pp);
  if (label == NULL) {
    bool neg = false;
    if (*p == '-') {
      neg = true;
      ++p;
    }
    if (isdigit(*p)) {
      offset = strtol(p, (char**)pp, 10);
      if (*pp > p)
        has_offset = true;
      if (neg)
        offset = -offset;
    } else if (neg) {
      error("Illegal `-'");
    }
  }
  p = skip_whitespace(*pp);
  if (*p != '(') {
    if (label != NULL) {
      operand->type = LABEL;
      operand->u.label = label;
      *pp = p;
      return true;
    }
    if (has_offset)
      error("direct number not implemented");
  } else {
    if (p[1] == '%') {
      *pp = p + 2;
      enum RegType reg = parse_register(pp);
      if (reg == NOREG)
        error("Register expected");
      p = skip_whitespace(*pp);
      if (*p != ')')
        error("`)' expected");
      *pp = ++p;
      operand->type = INDIRECT;
      operand->u.indirect.reg = reg;
      operand->u.indirect.label = label;
      operand->u.indirect.offset = offset;
      return true;
    }
    error("Illegal `('");
  }

  return false;
}

static void parse_line(const char *str, Line *line) {
  // Clear
  line->label = NULL;
  line->op = NOOP;
  line->src.type = line->dst.type = NOOPERAND;

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
    handle_directive(dir, p);
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
      //error("Syntax error");
      fprintf(stderr, "Not handled: %s\n", p);
    }
  }
}

static bool is_im8(long x) {
  return x < (1L << 7) && x >= -(1L << 7);
}

static bool is_im32(long x) {
  return x < (1L << 31) && x >= -(1L << 31);
}

static bool assemble_mov(const Line *line) {
  if (line->src.type == REG && line->dst.type == REG) {
    if (is_reg32(line->src.u.reg) && is_reg32(line->dst.u.reg)) {
      int s = line->src.u.reg - EAX;
      int d = line->dst.u.reg - EAX;
      ADD_CODE(0x89, 0xc0 + d + s * 8);
      return true;
    } else if (is_reg64(line->src.u.reg) && is_reg64(line->dst.u.reg)) {
      int s = line->src.u.reg - RAX;
      int d = line->dst.u.reg - RAX;
      ADD_CODE(0x48, 0x89, 0xc0 + d + s * 8);
      return true;
    }
  } else if (line->src.type == IMMEDIATE && line->dst.type == REG) {
    if (is_reg8(line->dst.u.reg)) {
      int d = line->dst.u.reg - AL;
      ADD_CODE(0xb0 + d, IM8(line->src.u.immediate));
      return true;
    } else if (is_reg32(line->dst.u.reg)) {
      int d = line->dst.u.reg - EAX;
      ADD_CODE(0xb8 + d, IM32(line->src.u.immediate));
      return true;
    } else if (is_reg64(line->dst.u.reg)) {
      int d = line->dst.u.reg - RAX;
      if (is_im32(line->src.u.immediate)) {
        ADD_CODE(0x48, 0xc7, 0xc0 + d, IM32(line->src.u.immediate));
      } else {
        ADD_CODE(0x48, 0xb8 + d, IM64(line->src.u.immediate));
      }
      return true;
    }
  } else if (line->src.type == INDIRECT && line->dst.type == REG) {
    if (line->src.u.indirect.label == NULL) {
      if (is_reg64(line->src.u.indirect.reg)) {
        int s = line->src.u.indirect.reg - RAX;
        long offset = line->src.u.indirect.offset;
        if (is_reg8(line->dst.u.reg)) {
          int d = line->dst.u.reg - AL;
          if (line->src.u.reg != RSP) {
            if (offset == 0 && line->src.u.reg != RBP) {
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
              ADD_CODE(0x8a, 0x04, 0x24);
              return true;
            } else if (is_im8(offset)) {
              ADD_CODE(0x8a, 0x44, 0x24, IM8(offset));
              return true;
            } else if (is_im32(offset)) {
              ADD_CODE(0x8a, 0x84, 0x24, IM32(offset));
              return true;
            }
          }
        } else if (is_reg32(line->dst.u.reg)) {
          int d = line->dst.u.reg - EAX;
          if (line->src.u.reg != RSP) {
            if (offset == 0 && line->src.u.reg != RBP) {
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
              ADD_CODE(0x8b, 0x04, 0x24);
              return true;
            } else if (is_im8(offset)) {
              ADD_CODE(0x8b, 0x44, 0x24, IM8(offset));
              return true;
            } else if (is_im32(offset)) {
              ADD_CODE(0x8b, 0x84, 0x24, IM32(offset));
              return true;
            }
          }
        } else if (is_reg64(line->dst.u.reg)) {
          int d = line->dst.u.reg - RAX;
          if (line->src.u.reg != RSP) {
            if (offset == 0 && line->src.u.reg != RBP) {
              ADD_CODE(0x48, 0x8b, 0x00 + s + d * 8);
              return true;
            } else if (is_im8(offset)) {
              ADD_CODE(0x48, 0x8b, 0x40 + s + d * 8, IM8(offset));
              return true;
            } else if (is_im32(offset)) {
              ADD_CODE(0x48, 0x8b, 0x80 + s + d * 8, IM32(offset));
              return true;
            }
          } else {
            if (offset == 0) {
              ADD_CODE(0x48, 0x8b, 0x04, 0x24);
              return true;
            } else if (is_im8(offset)) {
              ADD_CODE(0x48, 0x8b, 0x44, 0x24, IM8(offset));
              return true;
            } else if (is_im32(offset)) {
              ADD_CODE(0x48, 0x8b, 0x84, 0x24, IM32(offset));
              return true;
            }
          }
        }
      }
    }
  } else if (line->src.type == REG && line->dst.type == INDIRECT &&
             is_reg64(line->dst.u.indirect.reg)) {
    if (line->dst.u.indirect.label == NULL) {
      int d = line->dst.u.indirect.reg - RAX;
      long offset = line->dst.u.indirect.offset;
      if (is_reg8(line->src.u.reg)) {
        int s = line->src.u.reg - AL;
        if (line->dst.u.indirect.reg != RSP) {
          if (offset == 0 && line->dst.u.indirect.reg != RBP) {
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
            ADD_CODE(0x88, 0x04, 0x24);
            return true;
          } else if (is_im8(offset)) {
            ADD_CODE(0x88, 0x44, 0x24, IM8(offset));
            return true;
          } else if (is_im32(offset)) {
            ADD_CODE(0x89, 0x84, 0x24, IM32(offset));
            return true;
          }
        }
      } else if (is_reg32(line->src.u.reg)) {
        int s = line->src.u.reg - EAX;
        if (line->dst.u.indirect.reg != RSP) {
          if (offset == 0 && line->dst.u.indirect.reg != RBP) {
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
            ADD_CODE(0x89, 0x04, 0x24);
            return true;
          } else if (is_im8(offset)) {
            ADD_CODE(0x89, 0x44, 0x24, IM8(offset));
            return true;
          } else if (is_im32(offset)) {
            ADD_CODE(0x89, 0x84, 0x24, IM32(offset));
            return true;
          }
        }
      } else if (is_reg64(line->src.u.reg)) {
        int s = line->src.u.reg - RAX;
        if (line->dst.u.indirect.reg != RSP) {
          if (offset == 0 && line->dst.u.indirect.reg != RBP) {
            ADD_CODE(0x48, 0x89, 0x00 + d + s * 8);
            return true;
          } else if (is_im8(offset)) {
            ADD_CODE(0x48, 0x89, 0x40 + d + s * 8, IM8(offset));
            return true;
          } else if (is_im32(offset)) {
            ADD_CODE(0x48, 0x89, 0x80 + d + s * 8, IM32(offset));
            return true;
          }
        } else {
          if (offset == 0) {
            ADD_CODE(0x48, 0x89, 0x04, 0x24);
            return true;
          } else if (is_im8(offset)) {
            ADD_CODE(0x48, 0x89, 0x44, 0x24, IM8(offset));
            return true;
          } else if (is_im32(offset)) {
            ADD_CODE(0x48, 0x89, 0x84, 0x24, IM32(offset));
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
    add_label(line->label);

  switch(line->op) {
  case NOOP:
    return;
  case MOV:
    if (assemble_mov(line))
      return;
    break;
  case MOVSX:
    if (line->src.type == REG && line->dst.type == REG) {
      if (is_reg8(line->src.u.reg) && is_reg64(line->dst.u.reg)) {
        int s = line->src.u.reg - AL;
        int d = line->dst.u.reg - RAX;
        ADD_CODE(0x48, 0x0f, 0xbe, 0xc0 + s + d * 8);
        return;
      } else if (is_reg32(line->src.u.reg) && is_reg64(line->dst.u.reg)) {
        int s = line->src.u.reg - EAX;
        int d = line->dst.u.reg - RAX;
        ADD_CODE(0x48, 0x63, 0xc0 + s + d * 8);
        return;
      }
    }
    break;
  case LEA:
    if (line->src.type == INDIRECT &&
        line->dst.type == REG && is_reg64(line->dst.u.reg)) {
      int d = line->dst.u.reg - RAX;
      if (is_reg64(line->src.u.indirect.reg)) {
        int s = line->src.u.indirect.reg - RAX;
        if (line->src.u.indirect.label == NULL) {
          long offset = line->src.u.indirect.offset;
          if (line->src.u.indirect.reg != RSP) {
            if (offset == 0 && line->src.u.indirect.reg != RBP) {
              ADD_CODE(0x48, 0x8d, 0x00 + s + d * 8);
              return;
            } else if (is_im8(offset)) {
              ADD_CODE(0x48, 0x8d, 0x40 + s + d * 8, IM8(offset));
              return;
            } else if (is_im32(offset)) {
              ADD_CODE(0x48, 0x8d, 0x80 + s + d * 8, IM32(offset));
              return;
            }
          } else {
            if (offset == 0) {
              ADD_CODE(0x48, 0x8d, 0x04 + d * 8, 0x24);
              return;
            } else if (is_im8(offset)) {
              ADD_CODE(0x48, 0x8d, 0x44 + d * 8, 0x24, IM8(offset));
              return;
            } else if (is_im32(offset)) {
              ADD_CODE(0x48, 0x8d, 0x84 + d * 8, 0x24, IM32(offset));
              return;
            }
          }
        }
      } else if (line->src.u.indirect.reg == RIP) {
        if (line->src.u.indirect.offset == 0) {
          ADD_LOC_REL32(line->src.u.indirect.label, 3, 7);
          ADD_CODE(0x48, 0x8d, 0x05 + d * 8, IM32(-1));
          return;
        }
      }
    }
    break;
  case ADD:
    if (line->src.type == REG && line->dst.type == REG) {
      if (is_reg32(line->src.u.reg) && is_reg32(line->dst.u.reg)) {
        int s = line->src.u.reg - EAX;
        int d = line->dst.u.reg - EAX;
        ADD_CODE(0x01, 0xc0 + s * 8 + d);
        return;
      } else if (is_reg64(line->src.u.reg) && is_reg64(line->dst.u.reg)) {
        int s = line->src.u.reg - RAX;
        int d = line->dst.u.reg - RAX;
        ADD_CODE(0x48, 0x01, 0xc0 + s * 8 + d);
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
      if (is_reg64(line->src.u.indirect.reg) && is_reg64(line->dst.u.reg)) {
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
  case SUB:
    if (line->src.type == REG && line->dst.type == REG) {
      if (is_reg32(line->src.u.reg) && is_reg32(line->dst.u.reg)) {
        int s = line->src.u.reg - EAX;
        int d = line->dst.u.reg - EAX;
        ADD_CODE(0x29, 0xc0 + s * 8 + d);
        return;
      } else if (is_reg64(line->src.u.reg) && is_reg64(line->dst.u.reg)) {
        int s = line->src.u.reg - RAX;
        int d = line->dst.u.reg - RAX;
        ADD_CODE(0x48, 0x29, 0xc0 + s * 8 + d);
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
  case MUL:
    if (line->src.type == REG && line->dst.type == NOOPERAND) {
      if (is_reg32(line->src.u.reg)) {
        int s = line->src.u.reg - EAX;
        ADD_CODE(0xf7, 0xe0 + s);
        return;
      } else if (is_reg64(line->src.u.reg)) {
        int s = line->src.u.reg - RAX;
        ADD_CODE(0x48, 0xf7, 0xe0 + s);
        return;
      }
    }
    break;
  case DIV:
    if (line->src.type == REG && line->dst.type == NOOPERAND) {
      if (is_reg32(line->src.u.reg)) {
        int s = line->src.u.reg - EAX;
        ADD_CODE(0xf7, 0xf0 + s);
        return;
      } else if (is_reg64(line->src.u.reg)) {
        int s = line->src.u.reg - RAX;
        ADD_CODE(0x48, 0xf7, 0xf0 + s);
        return;
      }
    }
    break;
  case XOR:
    if (line->src.type == REG && line->dst.type == REG) {
      if (is_reg32(line->src.u.reg) && is_reg32(line->dst.u.reg)) {
        int s = line->src.u.reg - EAX;
        int d = line->dst.u.reg - EAX;
        ADD_CODE(0x31, 0xc0 + s * 8 + d);
        return;
      }
    }
    break;
  case NEG:
    if (line->src.type == REG && line->dst.type == NOOPERAND) {
      if (is_reg64(line->src.u.reg)) {
        int s = line->src.u.reg - RAX;
        ADD_CODE(0x48, 0xf7, 0xd8 + s);
        return;
      }
    }
    break;
  case INCL:
    if (line->src.type == INDIRECT && line->dst.type == NOOPERAND &&
        is_reg64(line->src.u.reg)) {
      int s = line->src.u.indirect.reg - RAX;
      long offset = line->src.u.indirect.offset;
      if (line->src.u.indirect.reg != RSP) {
        if (offset == 0 && line->src.u.indirect.reg != RBP) {
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
    }
    break;
  case CMP:
    if (line->src.type == REG && line->dst.type == REG) {
      if (is_reg8(line->src.u.reg) && is_reg8(line->dst.u.reg)) {
        int s = line->src.u.reg - AL;
        int d = line->dst.u.reg - AL;
        ADD_CODE(0x38, 0xc0 + s * 8 + d);
        return;
      } else if (is_reg8x(line->src.u.reg) && is_reg8x(line->dst.u.reg)) {
        int s = line->src.u.reg - AL;
        int d = line->dst.u.reg - AL;
        ADD_CODE(0x40, 0x38, 0xc0 + s * 8 + d);
        return;
      } else if (is_reg32(line->src.u.reg) && is_reg32(line->dst.u.reg)) {
        int s = line->src.u.reg - EAX;
        int d = line->dst.u.reg - EAX;
        ADD_CODE(0x39, 0xc0 + s * 8 + d);
        return;
      } else if (is_reg64(line->src.u.reg) && is_reg64(line->dst.u.reg)) {
        int s = line->src.u.reg - RAX;
        int d = line->dst.u.reg - RAX;
        ADD_CODE(0x48, 0x39, 0xc0 + s * 8 + d);
        return;
      }
    }
    break;
  case TEST:
    if (line->src.type == REG && line->dst.type == REG) {
      if (is_reg32(line->src.u.reg) && is_reg32(line->src.u.reg)) {
        int s = line->src.u.reg - EAX;
        int d = line->dst.u.reg - EAX;
        ADD_CODE(0x85, 0xc0 + s * 8 + d);
        return;
      }
    }
    break;
  case PUSH:
    if (line->src.type != REG || line->dst.type != NOOPERAND ||
        !is_reg64(line->src.u.reg))
      error("Illegal oprand: PUSH");
    ADD_CODE(0x50 + (line->src.u.reg - RAX));
    return;
  case POP:
    if (line->src.type != REG || line->dst.type != NOOPERAND ||
        !is_reg64(line->src.u.reg))
      error("Illegal oprand: POP");
    ADD_CODE(0x58 + (line->src.u.reg - RAX));
    return;
  case JMP:
    if (line->src.type != LABEL || line->dst.type != NOOPERAND)
      error("Illegal oprand: JMP");
    ADD_LOC_REL32(line->src.u.label, 1, 5);
    ADD_CODE(0xe9, IM32(-1));
    return;
  case JO:
  case JNO:
  case JB:
  case JAE:
  case JE:
  case JNE:
  case JBE:
  case JA:
  case JS:
  case JNS:
  case JP:
  case JNP:
  case JL:
  case JGE:
  case JLE:
  case JG:
    if (line->src.type != LABEL || line->dst.type != NOOPERAND)
      error("Illegal oprand: Jxx");
    // TODO: Handle short jump.
    ADD_LOC_REL32(line->src.u.label, 2, 6);
    ADD_CODE(0x0f, 0x80 + (line->op - JO), IM32(-1));
    return;
  case CALL:
    if (line->src.type != LABEL || line->dst.type != NOOPERAND)
      error("Illegal oprand: CALL");
    ADD_LOC_REL32(line->src.u.label, 1, 5);
    ADD_CODE(0xe8, IM32(-1));
    return;
  case RET:
    ADD_CODE(0xc3);
    return;
  case SYSCALL:
    ADD_CODE(0x0f, 0x05);
    return;
  default:
    break;
  }

  printf("op=%2d: not handled: %s\n", line->op, rawline);
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
    assemble_line(&line, rawline);
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

  init(LOAD_ADDRESS);

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

  resolve_label_locations();

  size_t codefilesz, codememsz;
  size_t datafilesz, datamemsz;
  uintptr_t codeloadadr, dataloadadr;
  get_section_size(0, &codefilesz, &codememsz, &codeloadadr);
  get_section_size(1, &datafilesz, &datamemsz, &dataloadadr);

  fprintf(stderr, "codesize: %d, %d\n", (int)codefilesz, (int)codememsz);
  fprintf(stderr, "datasize: %d, %d\n", (int)datafilesz, (int)datamemsz);

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

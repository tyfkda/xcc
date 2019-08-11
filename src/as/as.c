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
  JE,
  JNE,
  JGE,
  JL,
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
  "*noop*",
  "mov",
  "movsx",
  "lea",
  "call",
  "jmp",
  "je",
  "jne",
  "jge",
  "jl",
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

static enum Opcode parse_opcode(const char **pp) {
  const char *p = *pp;
  const char *start = p;

  while (isalpha(*p))
    ++p;
  if (*p == '\0' || isspace(*p)) {
    size_t n = p - start;
    for (int i = 0, len = sizeof(kOpTable) / sizeof(*kOpTable); i < len; ++i) {
      const char *op = kOpTable[i];
      size_t len = strlen(op);
      if (n == len && strncasecmp(start, op, n) == 0) {
        *pp = skip_whitespace(p);
        return (enum Opcode)i;
      }
    }
  }
  return NOOP;
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
    if (isdigit(*p)) {
      offset = strtol(p, (char**)pp, 10);
      if (*pp > p)
        has_offset = true;
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
  if (*p != '\0') {
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
  }

  if (*p != '\0' && !(*p == '/' && p[1] == '/')) {
    //error("Syntax error");
    fprintf(stderr, "Not handled: %s\n", p);
  }
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
    if (is_reg32(line->dst.u.reg)) {
      ADD_CODE(0xb8 + line->dst.u.reg - EAX, IM32(line->src.u.immediate));
      return true;
    }
  } else if (line->src.type == INDIRECT && line->dst.type == REG) {
    if (line->src.u.indirect.label == NULL && line->src.u.indirect.offset == 0) {
      if (is_reg64(line->src.u.indirect.reg) && is_reg64(line->dst.u.reg)) {
        int d = line->dst.u.reg - RAX;
        switch (line->src.u.indirect.reg) {
        case RSP:
          ADD_CODE(0x48, 0x8b, 0x04 + d * 8, 0x24);
          break;
        case RBP:
          ADD_CODE(0x48, 0x8b, 0x45 + d * 8, IM8(0));
          break;
        default:
          {
            int s = line->src.u.indirect.reg - RAX;
            ADD_CODE(0x48, 0x8b, 0x00 + s + d * 8);
          }
          break;
        }
        return true;
      }
    }
  }
  return false;
}

static bool is_im8(long x) {
  return x < (1L << 7) && x >= (1L << 7);
}

static bool is_im32(long x) {
  return x < (1L << 31) && x >= -(1L << 31);
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
    if (line->src.type != REG || line->src.type != REG)
      error("Illegal oprand: MOVSX");
    if (is_reg32(line->src.u.reg) && is_reg64(line->dst.u.reg)) {
      int s = line->src.u.reg - EAX;
      int d = line->dst.u.reg - RAX;
      ADD_CODE(0x48, 0x63, 0xc0 + s + d * 8);
      return;
    }
    break;
  case LEA:
    {
      if (line->dst.type != REG || !is_reg64(line->dst.u.reg) ||
          line->src.type != INDIRECT)
        error("Illegal oprand: LEA");
      int d = line->dst.u.reg - RAX;
      if (line->src.u.indirect.label == NULL) {
        if (line->src.u.indirect.reg == RSP) {
          long offset = line->src.u.indirect.offset;
          if (offset == 0) {
            ADD_CODE(0x48, 0x8d, 0x04 + d * 8, 0x24);
          } else if (is_im8(offset)) {
            ADD_CODE(0x48, 0x8d, 0x44 + d * 8, 0x24, IM8(offset));
          } else if (is_im32(offset)) {
            ADD_CODE(0x48, 0x8d, 0x84 + d * 8, 0x24, IM32(offset));
          }
          return;
        }
      } else {
        if (line->src.u.indirect.reg == RIP) {
          assert(line->src.u.indirect.offset == 0);
          ADD_LOC_REL32(line->src.u.indirect.label, 3, 7);
          ADD_CODE(0x48, 0x8d, 0x05 + d * 8, IM32(-1));
          return;
        }
      }
    }
    break;
  case ADD:
    if (line->dst.type != REG || line->src.type != IMMEDIATE)
      error("Illegal oprand: ADD");
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
    break;
  case SUB:
    if (line->dst.type != REG || line->src.type != IMMEDIATE)
      error("Illegal oprand: SUB");
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
    break;
  case XOR:
    if (line->src.type != REG || line->dst.type != REG)
      error("Illegal oprand: XOR");
    if (is_reg32(line->src.u.reg) && is_reg32(line->dst.u.reg)) {
      int s = line->src.u.reg - EAX;
      int d = line->dst.u.reg - EAX;
      ADD_CODE(0x31, 0xc0 + s + d * 8);
      return;
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
